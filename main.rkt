#lang racket

(provide (all-defined-out))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/pretty)
(require (lib "eopl.ss" "eopl"))

; ----------------------------------- DATATYPES ------------------------------
(define-datatype thunk thunk?
  (a-thunk
   (expression expr?)
   (environment list?)
   (saved-store list?)
   )
  )

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst list?))
  (proc-val
   (proc proc?))
  (non-val)
  )

(define (expval->val exp1)
  (cases expval exp1
    (list-val (lst) lst)
    (bool-val (bool) bool)
    (num-val (num) num)
    (non-val () 'None)
    (proc-val (p) p)))

(define-datatype proc proc?
  (procedure
   (id symbol?)
   (params list?)
   (body list?))
  )
(define-datatype statement statement?
  (s-statement
   (s simple-statement?))
  (c-statement
   (s compound-statement?))
  )

(define-datatype simple-statement simple-statement?
  (assignment-statement
   (id symbol?)
   (right-hand expr?))
  (return-statement
   (body (lambda (x) (or (expr? x) (null? x)))))
  (global-statement
   (id symbol?))
  (pass-statement)
  (break-statement)
  (continue-statement)
  (print-statement
   (arg list?))
  (evaluate-statement
   (address string?))
  )

(define-datatype compound-statement compound-statement?
  (function-def-statement
   (id symbol?)
   (params list?)
   (body list?))
  (if-statement
   (condition expr?)
   (body list?)
   (else-body list?))
  (for-statement
   (id symbol?)
   (iterable expr?)
   (body list?))
  )
(define-datatype param param?
  (param-with-default
   (id symbol?)
   (expression expr?)
   )
  )

(define-datatype expr expr?
  (disjunction-exp
   (body disjunct?))
  )

(define-datatype disjunct disjunct?
  (simple-disjunct
   (x conjunct?))
  (compound-disjunct
   (x1 disjunct?)
   (x2 conjunct?))
  )

(define-datatype conjunct conjunct?
  (simple-conjunct
   (x inversion?))
  (compound-conjunct
   (x1 conjunct?)
   (x2 inversion?))
  )

(define-datatype inversion inversion?
  (not-inversion
   (x inversion?))
  (comparison-inversion
   (comp comparison?))
  )

(define-datatype comparison comparison?
  (simple-comp
   (x sum?))
  (compound-comp
   (x1 sum?)
   (x2 list?))
  )

(define-datatype comp-op-sum-pair comp-op-sum-pair?
  (eq-sum
   (x sum?))
  (lt-sum
   (x sum?))
  (gt-sum
   (x sum?))
  )

(define-datatype sum sum?
  (addition-sum
   (left-hand sum?)
   (right-hand term?))
  (subtraction-sum
   (left-hand sum?)
   (right-hand term?))
  (simple-sum
   (x term?))
  )

(define-datatype term term?
  (multiplication-factor
   (left-hand term?)
   (right-hand factor?))
  (division-factor
   (left-hand term?)
   (right-hand factor?))
  (simple-term
   (x factor?))
  )

(define-datatype factor factor?
  (plus-factor
   (x factor?))
  (minus-factor
   (x factor?))
  (simple-factor
   (x power?))
  )

(define-datatype power power?
  (to-power
   (left-hand atom?)
   (right-hand factor?))
  (simple-power
   (x primary?))
  )

(define-datatype primary primary?
  (atom-primary
   (x atom?))
  (expression-primary
   (x primary?)
   (expression expr?))
  (empty-primary
   (x primary?))
  (argument-primary
   (x primary?)
   (arguments list?))
  )
(define-datatype atom atom?
  (id-atom
   (x symbol?))
  (boolean-atom
   (x symbol?))
  (none-atom)
  (number-atom
   (x number?))
  (list-atom
   (x list?))
  )

(define (report-unbound-var) (display "unbound variable"))



; ----------------------------------- LEXICAL SPEC --------------------------------
(define simple-python-lexer
           (lexer
            (";" (token-semicolon))
            ("=" (token-assignto))
            ("+" (token-plus))
            ("-" (token-minus))
            ("*" (token-multiply))
            ("/" (token-divide))
            ("**" (token-power))
            ("==" (token-equals))
            ("<" (token-lessthan))
            (">" (token-greaterthan))
            ("(" (token-opening-paranthesis))
            (")" (token-closing-paranthesis))
            ("[" (token-opening-bracket))
            ("]" (token-closing-bracket))
            (":" (token-colon))
            ("," (token-comma))
            ((:or "\"" "'") (token-double-quote))
            ("pass" (token-pass))
            ("break" (token-break))
            ("continue" (token-continue))
            ("return" (token-return))
            ("global" (token-global))
            ("print" (token-print))
            ("evaluate" (token-evaluate))
            ("def" (token-def))
            ("if" (token-IF))
            ("else" (token-ELSE))
            ("for" (token-FOR))
            ("in" (token-IN))
            ("or" (token-OR))
            ("and" (token-AND))
            ("not" (token-NOT))
            ("True" (token-TRUE))
            ("False" (token-FALSE))
            ("None" (token-NONE))
            ((:or (:+ (char-range #\0 #\9))
                  (:: (:+ (char-range #\0 #\9)) #\.
                      (:+ (char-range #\0 #\9))))
             (token-NUM (string->number lexeme)))
            ((::
              (:or (char-range #\a #\z)
                   (char-range #\A #\Z)
                   #\_)
              (:*
               (:or (char-range #\a #\z)
                    (char-range #\A #\Z)
                    (char-range #\0 #\9)
                    #\_)))
             (token-ID lexeme))
            ((:: (:or "\"" "'")
                 (complement (:or "\"" "'"))
                 (:or "\"" "'"))
             (token-FILE-ADDRESS lexeme))
            (whitespace (simple-python-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (NUM ID FILE-ADDRESS))
(define-empty-tokens b (EOF
                        semicolon
                        assignto
                        plus
                        minus
                        multiply
                        divide
                        power
                        equals
                        lessthan
                        greaterthan
                        opening-paranthesis
                        closing-paranthesis
                        opening-bracket
                        closing-bracket
                        colon
                        comma
                        double-quote
                        pass
                        break
                        continue
                        return
                        global
                        print
                        evaluate
                        def
                        IF
                        ELSE
                        FOR
                        IN
                        OR
                        AND
                        NOT
                        TRUE
                        FALSE
                        NONE))


; --------------------------------- THE GRAMMER -----------------------------


(define simple-python-parser
           (parser
            (start Program)
            (end EOF)
            (error void)
            (tokens a b)
            ;(suppress)
            (grammar
             (Program ((Statements) $1))
             (Statements ((Statement semicolon) (list $1))
                         ((Statements Statement semicolon) (append $1 (list $2))))
             (Statement ((Compound-stmt) (c-statement $1))
                        ((Simple-stmt) (s-statement $1)))
             (Simple-stmt ((Assignment) $1)
                          ((Return-stmt) $1)
                          ((Global-stmt) $1)
                          ((pass) (pass-statement))
                          ((break) (break-statement))
                          ((continue) (continue-statement))
                          ((print-stmt) $1)
                          ((Evaluate-stmt) $1))
             (Compound-stmt ((Function-def) $1)
                            ((If-stmt) $1)
                            ((For-stmt) $1))
             (Assignment ((ID assignto Expression)
                          (assignment-statement (string->symbol $1) $3)))
             (Return-stmt ((return) (return-statement '()))
                          ((return Expression) (return-statement $2)))
             (Global-stmt ((global ID) (global-statement (string->symbol $2))))
             (Function-def ((def ID opening-paranthesis Params closing-paranthesis colon Statements)
                            (function-def-statement (string->symbol $2) $4 $7))
                           ((def ID opening-paranthesis closing-paranthesis colon Statements)
                            (function-def-statement (string->symbol $2) (list) $6)))
             (Params ((Param-with-default) (list $1))
                     ((Params comma Param-with-default) (append $1 (list $3))))
             (Param-with-default ((ID assignto Expression)
                                  (param-with-default (string->symbol $1) $3)))
             (If-stmt ((IF Expression colon Statements Else-block)
                       (if-statement $2 $4 $5)))
             (Else-block ((ELSE colon Statements) $3))
             (For-stmt ((FOR ID IN Expression colon Statements)
                        (for-statement (string->symbol $2) $4 $6)))
             (Expression ((Disjunction) (disjunction-exp $1)))
             (Disjunction ((Conjunction) (simple-disjunct $1))
                          ((Disjunction OR Conjunction) (compound-disjunct $1 $3)))
             (Conjunction ((Inversion) (simple-conjunct $1))
                          ((Conjunction AND Inversion) (compound-conjunct $1 $3)))
             (Inversion ((NOT Inversion) (not-inversion $2))
                        ((Comparison) (comparison-inversion $1)))
             (Comparison ((Sum Compare-op-sum-pairs) (compound-comp $1 $2))
                         ((Sum) (simple-comp $1)))
             (Compare-op-sum-pairs ((Compare-op-sum-pair) (list $1))
                                   ((Compare-op-sum-pairs Compare-op-sum-pair)
                                    (append $1 (list $2))))
             (Compare-op-sum-pair ((Eq-sum) $1)
                                  ((Lt-sum) $1)
                                  ((Gt-sum) $1))
             (Eq-sum ((equals Sum) (eq-sum $2)))
             (Lt-sum ((lessthan Sum) (lt-sum $2)))
             (Gt-sum ((greaterthan Sum) (gt-sum $2)))
             (Sum ((Sum plus Term) (addition-sum $1 $3))
                  ((Sum minus Term) (subtraction-sum $1 $3))
                  ((Term) (simple-sum $1)))
             (Term ((Term multiply Factor) (multiplication-factor $1 $3))
                   ((Term divide Factor) (division-factor $1 $3))
                   ((Factor) (simple-term $1)))
             (Factor ((plus Factor) (plus-factor $2))
                     ((minus Factor) (minus-factor $2))
                     ((Power) (simple-factor $1)))
             (Power ((Atom power Factor) (to-power $1 $3))
                    ((Primary) (simple-power $1)))
             (Primary ((Atom) (atom-primary $1))
                      ((Primary opening-bracket Expression closing-bracket)
                       (expression-primary $1 $3))
                      ((Primary opening-paranthesis closing-paranthesis)
                       (empty-primary $1))
                      ((Primary opening-paranthesis Arguments closing-paranthesis)
                       (argument-primary $1 $3)))
             (Arguments ((Expression) (list $1))
                        ((Arguments comma Expression) (append $1 (list $3))))
             (Atom ((ID) (id-atom (string->symbol $1)))
                   ((TRUE) (boolean-atom 'True))
                   ((FALSE) (boolean-atom 'False))
                   ((NONE) (none-atom))
                   ((NUM) (number-atom $1))
                   ((List) (list-atom $1)))
             (Atom-list ((Atom) (list $1))
                        ((Atom-list comma Atom) (append $1 (list $3))))
             (List ((opening-bracket Expressions closing-bracket) $2)
                   ((opening-bracket closing-bracket) (list)))
             (Expressions ((Expressions comma Expression) (append $1 (list $3)))
                          ((Expression) (list $1)))
             (print-stmt ((print opening-paranthesis Expressions closing-paranthesis)
                          (print-statement $3)))
             (Evaluate-stmt
              ((evaluate opening-paranthesis double-quote FILE-ADDRESS double-quote closing-paranthesis)
               (evaluate-statement (substring $4 1 (- (string-length $4) 1)))))
             )))

; ----------------------------------- INTERPRETER --------------------------------

(define (empty-environment) (list))

(define (extend-environment var ref env) (cons (list var ref) env))

(define (apply-environment var env)
  (if (or
       (null? env)
       (and
        (null? (rest (first env)))
        (or (eqv? (first (first env)) '__RUNTIME_SCOPE) (eqv? (first (first env)) '__RUNTIME_GLOBAL_SCOPE))))
      '()
      (if (eqv? var (first (first env)))
          (second (first env))
          (apply-environment var (rest env))
          )
      )
  )
(define (get-global-environment env)
  (if (null? env)
      env
      (if (contains-scope env)
          (if (and (null? (rest (first env))) (eqv? (first (first env)) '__RUNTIME_GLOBAL_SCOPE))
              (rest env)
              (get-global-environment (rest env))
              )
          env
          )
      )
  )

(define (contains-scope env)
  (if (null? env)
      #f
      (if
       (and
        (null? (rest (first env)))
        (or
         (eqv? (first (first env)) '__RUNTIME_SCOPE)
         (eqv? (first (first env)) '__RUNTIME_GLOBAL_SCOPE)))
          #t
          (contains-scope (rest env))
          )
      )
  )
(define the-store (list))

(define (refrence? v) (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref)
  )



(define (setref! ref new-val)
  (set! the-store (list-set the-store ref new-val))
  )



(define (environment-contains-global-scope env)
  (if (null? env)
      #f
      (if (eqv? (first (first env)) '__RUNTIME_GLOBAL_SCOPE)
          #t
          (environment-contains-global-scope (rest env))
          )
      )
  )

(define (add-environment-scope env)
  (if (environment-contains-global-scope env)
      (cons (list '__RUNTIME_SCOPE) env)
      (cons (list '__RUNTIME_GLOBAL_SCOPE) env)
      )
  )




(define (extend-env-with-args params args env prev-env)
  (if (null? args)
      (extend-env-only-params params env prev-env)
      (extend-env-with-args (rest params) (rest args)
                            (extend-env-with-arg (first params) (first args) env prev-env)
                            prev-env)
      )
  )
(define (extend-env-with-arg param1 arg1 env prev-env)
  (cases param param1
    (param-with-default (id exp1)
                        (extend-environment id (newref (a-thunk arg1 prev-env the-store)) env))
    )
  )
(define (extend-env-only-params params env prev-env)
  (if (null? params)
      env
      (cases param (first params)
        (param-with-default (id exp1)
                            (extend-env-only-params (rest params) (extend-environment id (newref (a-thunk exp1 prev-env the-store)) env) prev-env))
        )
      )
  )

(define (value-of-program program)
  (begin
    (define env (empty-environment))
    (value-of-statements program env)
    )
 )

(define (value-of-statements statements env)
  (if (null? statements)
      env
      (let ((result (value-of-statement (first statements) env)))
        (if (null? result)
            (value-of-statements (rest statements) result)
            (cond
              [(eqv? 'Return (first result))
               result]
              [else
               (value-of-statements (rest statements) result)])
            )
        )
      )
  )
(define (value-of-statements-function statements env)
  (let ((result (value-of-statements statements env)))
    (if (null? result)
        (non-val)
        (if (eqv? 'Return (first result))
            (second result) 
            (non-val)
            )
        )
    )
  )

(define (value-of-statement s env)
  (cases statement s
    (s-statement (s)
                 (value-of-s-statement s env))
    (c-statement (s)
                 (value-of-c-statement s env))
    )
  )

(define (value-of-s-statement s env)
  (cases simple-statement s
    (assignment-statement (id right-hand)
                          (value-of-assignment-statement id right-hand env))
    (return-statement (body)
                      (value-of-return-statement body env))
    (global-statement (id)
                      (value-of-global-statement id env))
    (pass-statement ()
                    env)
    (break-statement ()
                     env)
    (continue-statement ()
                        env)
    (print-statement (lst)
                     (begin
                       (display (map (lambda (x) (value-of-expression x env)) lst))
                       env))
    (evaluate-statement (address)
                        (let*
                          ((program (open-input-file "./test.plpy"))
                           (result (value-of-program (parse program))))
                          (begin
                            (display result)
                            env)))
    )
  )

(define (value-of-c-statement s env)
  (cases compound-statement s
    (function-def-statement (id params body)
                            (value-of-function-def-statement id params body env))
    (if-statement (condition body else-body)
                  (value-of-if-statement condition body else-body env value-of-statements))
    (for-statement (id iterable body)
                   (value-of-for-statement id iterable body env)
                   )
    )
  )

(define (value-of-assignment-statement id right-hand env)
  (let ((prev-dec (apply-environment id env)))
    (if (null? prev-dec)
        (let ((new-ref (newref (a-thunk right-hand env the-store))))
          (extend-environment id new-ref env)
          )
        (begin
          (setref! prev-dec (a-thunk right-hand env the-store))
          env)
        )
    )
  )
(define (value-of-return-statement body env)
  (if (null? body)
      (list 'Return (non-val))
      (list 'Return (value-of-expression body env))
      )
  )

(define (value-of-global-statement id env)
  (let ((global-ref (apply-environment id (get-global-environment env))))
    (extend-environment id global-ref env)
    )
  )
(define (value-of-function-def-statement id params body env)
  (let ((new-ref (newref (proc-val (procedure id params body)))))
    (extend-environment id new-ref env)
    )
  )
(define (value-of-if-statement condition body else-body env f)
  (let ((condition-result (value-of-expression condition env)))
    (if (expval->val condition-result)
        (f body env)
        (f else-body env)
        )
    )
  )
(define (value-of-for-statement id iterable body env)
  (let ((iterable-value (reverse (expval->val (value-of-expression iterable env)))))
        (let ((result (value-of-for-body id iterable-value body env)))
          (if (or (eqv? result 'break) (eqv? result 'continue))
              env
              result)
          )
    )
  )
(define (value-of-for-body id iterable body env)
  (if (null? iterable)
      env
      (begin
        (let ((result (value-of-for-body id (rest iterable) body env)))
          (cond
            [(eqv? result 'break)
             'break]
            [(eqv? result 'continue)
             (value-of-statements-inside-for body (extend-environment id (newref (a-thunk (first iterable) env the-store)) env))]
            [(null? result)
             (value-of-statements-inside-for body (extend-environment id (newref (a-thunk (first iterable) env the-store)) env))]
            [(eqv? 'Return (first result))
             result]
            [else
             (value-of-statements-inside-for body (extend-environment id (newref (a-thunk (first iterable) env the-store)) env))]
            )
          )
        )
      )
  )
(define (value-of-statements-inside-for statements env)
  (if (null? statements)
      env
      (let ((to-execute (first statements)))
        (cases statement to-execute
          (s-statement (s)
                       (cases simple-statement s
                         (break-statement ()
                                          'break)
                         (continue-statement ()
                                             'continue)
                         (return-statement (e1)
                                           (value-of-return-statement e1 env))
                         (else
                          (begin
                            (value-of-statements-inside-for (rest statements) (value-of-statement to-execute env))
                          env))
                         )
                       )
          (c-statement (s)
                       (cases compound-statement s
                         (if-statement (condition body else-body)
                                       (let ((result (value-of-if-statement condition body else-body env value-of-statements-inside-for)))
                                         (cond
                                           [(eqv? result 'break)
                                            'break]
                                           [(eqv? result 'continue)
                                            'continue]
                                           [(null? result)
                                            (value-of-statements-inside-for (rest statements) result)]
                                           [(eqv? 'Return (first result))
                                            result]
                                           [else
                                            (value-of-statements-inside-for (rest statements) result)]
                                           )))
                         (else
                          (begin
                            (value-of-statements-inside-for (rest statements) (value-of-statement to-execute env))
                            env))
                         )
                       )
          )
        )
      )
  )


#|(define (evaluate path)
  ( (call-with-input-file path (lambda (in) (port->string in)))))|#
  



(define (value-of-expression exp1 env)
  (cases expr exp1
    (disjunction-exp (body)
                     (value-of-disjunction body env))))

(define (value-of-disjunction body env)
  (cases disjunct body
    (simple-disjunct (x)
                     (value-of-conjunction x env))
    (compound-disjunct (x1 x2)
                       (let ((val-x1 (value-of-disjunction x1 env))
                             (val-x2 (value-of-conjunction x2 env)))
                         (if (expval->val val-x1)
                             val-x1
                             val-x2)))))

(define (value-of-conjunction body env)
  (cases conjunct body
    (simple-conjunct (x)
                     (value-of-inversion x env))
    (compound-conjunct (x1 x2)
                       (let ((val-x1 (value-of-conjunction x1 env))
                             (val-x2 (value-of-inversion x2 env)))
                         (if (not (expval->val val-x1))
                             val-x1
                             val-x2)))))

(define (value-of-inversion body env)
  (cases inversion body
    (not-inversion (x)
                   (bool-val (not (expval->val (value-of-inversion x env)))))
    (comparison-inversion (comp)
                           (value-of-comparison comp env))))

(define (value-of-comparison body env)
  (cases comparison body
    (simple-comp (x)
                 (value-of-sum x env))
    (compound-comp (x1 x2)
                   (value-of-comp-op-sum-pairs (value-of-sum x1 env) x2 env))))
(define (extract-sum cosp)
  (cases comp-op-sum-pair cosp
    (eq-sum (s) s)
    (lt-sum (s) s)
    (gt-sum (s) s)))

(define (value-of-comp-op-sum-pairs precursor body env)
  (cond
    [(null? body) (bool-val #t)]
    [else (let ((result (expval->val (value-of-comp-op-sum-pair precursor (first body) env))))
             (bool-val (and (expval->val (value-of-comp-op-sum-pairs (value-of-sum (extract-sum (first body)) env) (rest body) env)) result)))]))

(define (value-of-comp-op-sum-pair precursor body env)
  (cases comp-op-sum-pair body
    (eq-sum (x) (bool-val (equal? (expval->val precursor) (expval->val (value-of-sum x env)))))
    (lt-sum (x) (bool-val (< (expval->val precursor) (expval->val (value-of-sum x env)))))
    (gt-sum (x) (bool-val (> (expval->val precursor) (expval->val (value-of-sum x env)))))))

(define (value-of-sum body env)
  (cases sum body
    (addition-sum (left-hand right-hand) (add-or (value-of-sum left-hand env)  (value-of-term right-hand env) env))
    (subtraction-sum (left-hand right-hand)
                     (num-val (- (expval->val (value-of-sum left-hand env)) (expval->val (value-of-term right-hand env)))))
    (simple-sum (x) (value-of-term x env))))

(define (add-or left-hand right-hand env)
  (cases expval left-hand
    (num-val (num) (num-val (+ (expval->val right-hand) num)))
    (bool-val (bool) (if (eqv? #t bool)
                         (bool-val #t)
                          right-hand ))
    (list-val (lst) (list-val (append lst (expval->val right-hand ))))
    (else (non-val))))

(define (value-of-term body env)
  (cases term body
    (multiplication-factor (left-hand right-hand)
                           (mult-and (value-of-term left-hand env) right-hand env))
    (division-factor (left-hand right-hand)
                     (num-val (/ (expval->val (value-of-term left-hand env))
                        (expval->val (value-of-factor right-hand env)))))
    (simple-term (x) (value-of-factor x env))))

(define (mult-and left-hand right-hand env)
  (cases expval left-hand
    (num-val (num) (num-val (* num (expval->val (value-of-factor right-hand env)))))
    (bool-val (bool) (if (eqv? #f bool)
                         (bool-val #f)
                          (value-of-factor right-hand env)))
    (else (non-val))))

(define (value-of-factor body env)
  (cases factor body
    (plus-factor (x) (value-of-factor x env))
    (minus-factor (x) (num-val (- (expval->val (value-of-factor x env)))))
    (simple-factor (x) (value-of-power x env))))

(define (value-of-power body env)
  (cases power body
    (to-power (left-hand right-hand) (value-of-to-power left-hand right-hand env))
    (simple-power (x) (value-of-primary x env))))

(define (value-of-to-power left-hand right-hand env)
  (let ((right-hand-val (expval->val (value-of-factor right-hand env)))
        (left-hand-val (expval->val (value-of-atom left-hand env))))
    (num-val (expt left-hand-val right-hand-val))
    )
  )
(define (value-of-atom val env)
  (cases atom val
    (id-atom (x)
             (let ((ref (apply-environment x env)))
               (let ((value (deref ref)))
                 (if (thunk? value)
                     (cases thunk value
                       (a-thunk (exp1 prev-env saved-store)
                                (let ((curr-store the-store))
                                  (set! the-store saved-store)
                                  (let ((result (value-of-expression exp1 prev-env)))
                                    (begin
                                      (set! the-store curr-store)
                                      result)
                                    ))))
                     value))))
  (boolean-atom (x)
                (bool-val (to-boolean x)))
  (none-atom ()
             (non-val))
  (number-atom (x)
               (num-val x))
  (list-atom (x)
             (list-val x))
    )
  )
(define (to-boolean x)
  (eqv? x 'True))
     
(define (value-of-primary body env)
  (cases primary body
    (atom-primary (x) (value-of-atom x env))
    (expression-primary (x exp1) (value-of-expression-primary x exp1 env))
    (empty-primary (x) (value-of-empty-primary x env))
    (argument-primary (x args) (value-of-argument-primary x args env))))

(define (value-of-expression-primary x exp1 env)
  (value-of-expression (list-ref (expval->val (value-of-primary x env)) (expval->val (value-of-expression exp1 env))) env)
  )
(define (value-of-empty-primary x env)
  (let ((val (expval->val (value-of-primary x env))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body (list) (add-environment-scope env) env)
                 )
      )
    )
  )
(define (apply-procedure id params body args env prev-env)
  (value-of-statements-function body (extend-environment id
                                                (newref (proc-val (procedure id params body)))
                                                (extend-env-with-args params args env prev-env)))
  )
  
(define (value-of-argument-primary x args env)
  (let ((val (expval->val (value-of-primary x env))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body args (add-environment-scope env) env)
                 )
      )
    )
  )
(define (value-of-thunk t)
  (cases thunk t
    (a-thunk (exp1 prev-env saved-store)
             (let ((curr-store the-store))
               (set! the-store saved-store)
               (let ((result (value-of-expression exp1 prev-env)))
                 (begin
                   (set! the-store curr-store)
                   result)
                 )
               )          
             )
    )
  )


(define (make-num num) (simple-sum
               (simple-term
                (simple-factor
                 (simple-power
                  (atom-primary
                   (number-atom
                    num)))))))

(define (deref ref)
   (list-ref the-store ref))
(define (sne num)
  (disjunction-exp
   (simple-disjunct
    (simple-conjunct
     (comparison-inversion
      (simple-comp
       (simple-sum
        (simple-term
         (simple-factor
          (simple-power
           (atom-primary
            (number-atom
             num))))))))))))

(define for-list
  (disjunction-exp
   (simple-disjunct
    (simple-conjunct
     (comparison-inversion
      (simple-comp
       (simple-sum
        (simple-term
         (simple-factor
          (simple-power
           (atom-primary
            (list-atom
             (list
              (sne 1)
              (sne 2)
              (sne 3)
              (sne 4))))))))))))))

(define test
  (list
   (s-statement
    (assignment-statement
      'a
      (disjunction-exp
        (simple-disjunct
         (simple-conjunct
          (comparison-inversion
           (simple-comp
           (simple-sum
            (simple-term
             (plus-factor
              (simple-factor
               (simple-power
                (atom-primary
                 (number-atom
                  155))))))))))))))
   (c-statement
    (function-def-statement
     'p1
     (list)
     (list
      (s-statement
       (global-statement
        'a
        )
       )
      (c-statement
       (for-statement
        'i
        for-list
        (list
         (c-statement
          (if-statement
           (disjunction-exp
            (simple-disjunct
             (simple-conjunct
              (comparison-inversion
               (compound-comp
                (simple-sum
                 (simple-term
                  (simple-factor
                   (simple-power
                    (atom-primary
                   (id-atom
                    'i))
                    ))))
                (list
                 (eq-sum
                  (make-num 2))))))))
           (list
            (s-statement
             (assignment-statement
              'b
              (disjunction-exp
               (simple-disjunct
                (simple-conjunct
                 (comparison-inversion
                  (simple-comp
                   (simple-sum
                    (simple-term
                     (plus-factor
                      (simple-factor
                       (simple-power
                        (atom-primary
                         (number-atom
                          170))))))))))))))
            (s-statement
             (return-statement (sne 77)))
            (s-statement
             (print-statement (list 123123))))
           (list
          (s-statement
           (print-statement (list 12)))))))
        )
       )
      )
     )
    )
    (c-statement
     (if-statement
      (disjunction-exp
       (simple-disjunct
        (simple-conjunct
         (comparison-inversion
          (compound-comp
           (simple-sum
            (simple-term
             (simple-factor
              (simple-power
               (empty-primary
                (atom-primary
                 (id-atom
                  'p1)))))))
              (list
               (eq-sum
                (make-num 77))))))))
         (list
          (s-statement
           (print-statement (list 'True))))
         (list
          (s-statement
           (print-statement (list 'False))))))
   
               
    (c-statement
     (for-statement
      'i
      for-list
      (list
       (c-statement
        (if-statement
         (disjunction-exp
          (simple-disjunct
           (simple-conjunct
            (comparison-inversion
             (compound-comp
              (simple-sum
               (simple-term
                (simple-factor
                 (simple-power
                  (atom-primary
                   (id-atom
                   'a))
                  ))))
              (list
               (eq-sum
                (make-num 155))))))))
         (list
          (s-statement
           (print-statement (list 123123))))
         (list
          (s-statement
           (print-statement (list 12)))))))
        )
       )
      )
     )


; ----------------------------------- TEST ---------------------------------------

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define parse (lambda (program)
                (let ((my-lexer (lex-this simple-python-lexer program)))
                  (simple-python-parser my-lexer))))
(define (evaluate address)
  (value-of-program (parse (open-input-file address))))

(define program "./in4.txt")
(define result (evaluate program))
(define test-output (open-output-file "./out4-program.txt" #:exists 'replace))
(pretty-print result test-output)
(close-output-port test-output)