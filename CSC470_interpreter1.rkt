; Enviroment Initializers
(define empty-scope
  (lambda () (list 'empty-scope)))

(define extend-scope
  (lambda (name value scope)
    (list 'extend-scope name value scope)))

(define extend-scope*
  (lambda (lon lov scope)
    (cond
      ((null? lon) scope)
      (else (extend-scope* (cdr lon) (cdr lov) (extend-scope (car lon) (car lov) scope))))))

(define get-name
  (lambda (scope) (cadr scope)))

(define get-value
  (lambda (scope) (caddr scope)))

(define get-scope
  (lambda (scope) (cadddr scope)))

(define empty-scope?
  (lambda (env) (eq? 'empty-scope (car env))))

(define apply-scope
  (lambda (var-name scope)
    (cond
      ((empty-scope? scope) #f)
      (else
       (if (eq? var-name (get-name scope))
           (get-value scope)
           (apply-scope var-name (get-scope scope)))))))

(define has-binding?
  (lambda (var-name scope)
    (not (eq? (apply-scope var-name scope) #f))))

(define push-scope
  (lambda (scope env)
    (cons scope env)))

(define peek-scope
  (lambda (env)
    (if (null? env)
        #f
        (car env))))

(define remove-scope
  (lambda (env)
    (cdr env)))


(define empty-env
  (lambda () '((empty-scope))))

(define extend-env
  (lambda (var-name var-value env)
    (let* ((top-scope (peek-scope env))
           (extended-scope (extend-scope var-name var-value top-scope))
           (temp-env (remove-scope env)))
      (push-scope extended-scope temp-env))))


;(define extend-scope
 ; (lambda (name value scope)
  ;  (list 'extend-scope name value scope)))


(define apply-env
  (lambda (var-name env)
    (cond
      ((null? env) #f)
      ((has-binding? var-name (peek-scope env))
       (apply-scope var-name (peek-scope env)))
      (else (apply-env var-name (remove-scope env))))))     
    

; Grammar Constructors
(define lit-exp
  (lambda (n)
    (list 'lit-exp n)))

(define var-exp
  (lambda (s)
    (list 'var-exp s)))

(define bool-exp
  (lambda (op left right)
    (list 'bool-exp op left right)))

(define repeat-exp
  (lambda (count exp)
    (list 'repeat-exp count exp)))

(define create-var-exp
  (lambda (var-name var-val)
    (list 'create-var-exp var-name var-val)))

(define print-exp
  (lambda (exp)
    (list 'print-exp exp)))

(define do-in-order-exp
  (lambda (lo-exp)
    (cons 'do-in-order-exp lo-exp)))

(define if-exp
  (lambda (bool-exp true-exp false-exp)
    (list 'if-exp bool-exp true-exp false-exp)))

(define math-exp
  (lambda (op left right)
    (list 'math-exp op left right)))

(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))

(define app-exp
  (lambda (lambda-exp param-value)
    (list 'app-exp lambda-exp param-value)))

; Grammar Extractors
(define lc-exp->type
  (lambda (lc-exp)
    (car lc-exp)))

(define lit-exp->value
  (lambda (lit-exp)
    (cadr lit-exp)))

(define var-exp->var-name
  (lambda (var-exp)
    (cadr var-exp)))

(define repeat-exp->count
  (lambda (repeat-exp)
    (cadr repeat-exp)))

(define repeat-exp->exp
  (lambda (repeat-exp)
    (caddr repeat-exp)))

(define create-var-exp->var-name
  (lambda (create-var-exp)
    (cadr create-var-exp)))

(define create-var-exp->var-val
  (lambda (create-var-exp)
    (caddr create-var-exp)))

(define print-exp->exp
  (lambda (print-exp)
    (cadr print-exp)))

(define do-in-order-exp->list-of-expressions
  (lambda (do-in-order-exp)
    (cdr do-in-order-exp)))

(define bool-exp->op
  (lambda (bool-exp)
    (cadr bool-exp)))

(define if-exp->bool-exp
  (lambda (if-exp)
    (cadr if-exp)))

(define if-exp->true-exp
  (lambda (if-exp)
    (caddr if-exp)))

(define if-exp->false-exp
  (lambda (if-exp)
    (cadddr if-exp)))

(define bool-exp->left
  (lambda (bool-exp)
    (caddr bool-exp)))

(define bool-exp->right
  (lambda (bool-exp)
    (cadddr bool-exp)))

(define math-exp->op
  (lambda (math-exp)
    (cadr math-exp)))

(define math-exp->left
  (lambda (math-exp)
    (caddr math-exp)))

(define math-exp->right
  (lambda (math-exp)
    (cadddr math-exp)))

(define lambda-exp->parameter-name
  (lambda (lambda-exp)
    (cadr lambda-exp)))

(define lambda-exp->body
  (lambda (lambda-exp)
    (caddr lambda-exp)))

(define app-exp->lambda-exp
  (lambda (app-exp)
    (cadr app-exp)))

(define app-exp->parameter-input
  (lambda (app-exp)
    (caddr app-exp)))

; Grammar Predicates
(define lit-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lit-exp)))

(define var-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'var-exp)))

(define print-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'print-exp)))

(define repeat-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'repeat-exp)))

(define create-var-exp?
  (lambda (lc-exp)
     (eq? (lc-exp->type lc-exp) 'create-var-exp)))

(define do-in-order-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'do-in-order-exp)))

(define bool-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'bool-exp)))

(define if-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'if-exp)))

(define math-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'math-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'app-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lambda-exp)))

;C0d3 Extractors
(define literal-exp->value
  (lambda (literal-exp)
    (cadr literal-exp)))

(define display-exp->exp
  (lambda (display-exp)
    (cadr display-exp)))

(define repeat-exp->times
  (lambda (repeat-exp)
    (cadr repeat-exp)))

(define repeat-exp->what
  (lambda (repeat-exp)
    (cadddr repeat-exp)))

(define remember-exp->var-name
  (lambda (remember-exp)
    (cadr remember-exp)))

(define remember-exp->var-val
  (lambda (remember-exp)
    (caddr remember-exp)))

(define do-in-order->list-of-expressions
  (lambda (do-in-order-exp)
    (cdr do-in-order-exp)))

(define test-exp->op
  (lambda (test-exp)
    (cadr test-exp)))

(define test-exp->left
  (lambda (test-exp)
    (caddr test-exp)))

(define test-exp->right
  (lambda (test-exp)
    (cadddr test-exp)))

(define question-exp->test-exp
  (lambda (question-exp)
    (cadr question-exp)))

(define question-exp->true-exp
  (lambda (question-exp)
    (cadddr question-exp)))

(define question-exp->false-exp
  (lambda (question-exp)
    (cadddr (cddr question-exp))))

(define do-math->op
  (lambda (do-math-exp)
    (cadr do-math-exp)))

(define do-math->left
  (lambda (do-math-exp)
    (caddr do-math-exp)))

(define do-math->right
  (lambda (do-math-exp)
    (cadddr do-math-exp)))

(define get-value-exp->value
  (lambda (get-val-exp)
    (cadr get-val-exp)))

(define func-exp->parameter
  (lambda (func-exp)
    (car (car (cdr (cdr func-exp))))))

(define func-exp->body
  (lambda (func-exp)
    (car (cdr (cdr (cdr (cdr func-exp)))))))

(define run-exp->func
  (lambda (run-exp)
    (cadr run-exp)))

(define run-exp->parameter
  (lambda (run-exp)
    (cadddr run-exp)))

; Parse/Unparse
; (func gets (x) does x)
; (Run (func (x) x) ‘with parameter)
; (Get-Value ‘A)
; (literal 5)
; (do-math '+ (literal 5) (literal 4))
; (test < (get-value a) (literal 7))
; (ask-question (test < (get-value a) (literal 7)) if-true-do-> (literal 1) if-false-do-> (literal 0))
; (display (literal 7)) NOTE: displayed values are in purple (resolved are in blue)
; (do-in-order c0d3*)
; (remember a 13)
; (repeat 10 times (display (literal 5))
; (update a (literal 15))
; (while bool-expression body) 
        
(define parse-expression
  (lambda (c0d3)
    (cond
      ((eq? (car c0d3) 'literal) (lit-exp (literal-exp->value c0d3)))
      ((eq? (car c0d3) 'display) (print-exp (parse-expression (display-exp->exp c0d3))))
      ((eq? (car c0d3) 'test) (bool-exp (test-exp->op c0d3)
                                        (parse-expression (test-exp->left c0d3))
                                        (parse-expression (test-exp->right c0d3))))
      ((eq? (car c0d3) 'repeat) (repeat-exp (parse-expression (repeat-exp->times c0d3))
                                            (parse-expression (repeat-exp->what c0d3))))
      ((eq? (car c0d3) 'remember) (create-var-exp
                                       (remember-exp->var-name c0d3)
                                       (parse-expression (remember-exp->var-val c0d3))))
      ((eq? (car c0d3) 'ask-question) (if-exp (parse-expression (question-exp->test-exp c0d3))
                                              (parse-expression (question-exp->true-exp c0d3))
                                              (parse-expression (question-exp->false-exp c0d3))))
      ((eq? (car c0d3) 'do-in-order) (do-in-order-exp
                                      (map parse-expression
                                       (do-in-order->list-of-expressions c0d3)))) 
      ((eq? (car c0d3) 'do-math) (math-exp (do-math->op c0d3) 
                                           (parse-expression (do-math->left c0d3))
                                           (parse-expression (do-math->right c0d3))))
      ((eq? (car c0d3) 'get-value) (var-exp (get-value-exp->value c0d3)))
      ((eq? (car c0d3) 'func) (lambda-exp (func-exp->parameter c0d3) (parse-expression (func-exp->body c0d3))))
      ((eq? (car c0d3) 'run) (app-exp
                              (parse-expression (run-exp->func c0d3))
                              (parse-expression (run-exp->parameter c0d3)))))))

; Language Helpers
(define do-math
  (lambda (op left right)
    (cond
      ((eq? op '+) (+ left right))
      ((eq? op '-) (- left right))
      ((eq? op '*) (* left right))
      ((eq? op '/) (/ left right)))))

(define resolve-boolean
  (lambda (op left right)
    (cond
      ((eq? op '<) (< left right))
      ((eq? op '<=) (<= left right))
      ((eq? op '>) (> left right))
      ((eq? op '>=) (>= left right))
      ((eq? op '==) (= left right))
      ((eq? op '!=) (not (= left right)))))) 

(define apply-expression
  (lambda (lcexp env)
    (cond
      ((lit-exp? lcexp) (lit-exp->value lcexp))
      ((var-exp? lcexp) (apply-scope (var-exp->var-name lcexp) (peek-scope env)))
      ((print-exp? lcexp) (write (apply-expression (print-exp->exp lcexp) env))) 
      ((repeat-exp? lcexp) (letrec ((theExpression (repeat-exp->exp lcexp))
                                 (theNumberOfTimesExpression (repeat-exp->count lcexp))
                                 (theEvaluatedExpression (apply-expression theExpression env))
                                 (theEvaluatedNumberOfTimesExpression (apply-expression theNumberOfTimesExpression env))  
                                 (repeatFunc (lambda (count)
                                               (if (= 0 count)
                                                   '()
                                                   (cons theEvaluatedExpression (repeatFunc (- count 1))))))) 
                             (repeatFunc theEvaluatedNumberOfTimesExpression))) ;(repeatFunc theEvaluatedNumberOfTimesExpression)))))
      ((create-var-exp? lcexp) (let ((name (create-var-exp->var-name lcexp))
                                     (val (apply-expression (create-var-exp->var-val lcexp) env))) 
                                 (extend-env name val (peek-scope env))))
      ((bool-exp? lcexp) (let ((op (bool-exp->op lcexp))
                               (left (apply-expression (bool-exp->left lcexp) env))
                               (right (apply-expression (bool-exp->right lcexp) env)))
                           (resolve-boolean op left right)))
      ((print-exp? lcexp) (apply-expression (print-exp->exp lcexp) env))
      ((do-in-order-exp? lcexp) (letrec ((loe (do-in-order-exp->list-of-expressions lcexp))
                                         (processList (lambda (loe currScope)
                                           (cond
                                             ((null? loe) '())
                                             ((create-var-exp? (car loe)) (processList
                                                                           (cdr loe)
                                                                           (apply-expression (car loe) currScope)))
                                             (else (cons (apply-expression (car loe) currScope)
                                                         (processList (cdr loe) currScope)))))))
                                  (processList loe env)))
      ((if-exp? lcexp) (let* ((bool-result (apply-expression (if-exp->bool-exp lcexp) env))
                             (true-exp (if-exp->true-exp lcexp))
                             (false-exp (if-exp->false-exp lcexp)))
                         (if bool-result
                             (apply-expression true-exp env)
                             (apply-expression false-exp env))))
      ((math-exp? lcexp) (let ((op (math-exp->op lcexp))
                               (left (apply-expression (math-exp->left lcexp) env))
                               (right (apply-expression (math-exp->right lcexp) env)))
                           (do-math op left right)))
      ((lambda-exp? lcexp) (apply-expression (lambda-exp->body lcexp) env))
      ((app-exp? lcexp) (let* ((the-lambda (app-exp->lambda-exp lcexp))
                              (the-lambda-param-name (lambda-exp->parameter-name the-lambda))
                              (the-parameter-value (apply-expression (app-exp->parameter-input lcexp) env))
                              (the-new-env (extend-env the-lambda-param-name the-parameter-value env)))
                          (apply-expression the-lambda the-new-env))))))



(define run-program
  (lambda (c0d3-src env)
    (apply-expression (parse-expression c0d3-src) env)))

(define myC0d3 '(run (func gets (a) does (do-math + (get-value a) (literal 2))) with (literal 5)))
(define c0d3-bool '(test > (literal 5) (literal 2)))
(define c0d3-if '(ask-question
                  (test < (get-value a) (literal 7))
                  if-true-do-> (ask-question (test > (get-value a) (literal 5)) if-true-do-> (literal 8) if-false-do-> (literal 9))
                  if-false-do-> (literal 0)))
(define env (extend-env 'a 6 (extend-env 'b 7 (empty-env)))) 
(run-program '(do-in-order (literal 7) (remember a (literal 13)) (literal 8) (display (get-value a))) env)
;(repeat-exp->exp (parse-expression '(repeat (literal 10) times (literal 13))))
;(run-program '(repeat (literal 10) times (literal 13)) env)
;(parse-expression '(do-in-order (remember a (literal 1)) while (test < (get-value a) (literal 7))
;(run-program '(repeat (literal 10) times (do-in-order (remember t (literal 3))
;                          (remember z (literal 21))
;                          (do-math + (get-value t) (get-value z))
;                          (remember r (literal 14))
;                          (get-value r))) env)
env
;(extend-env 'w 48 env)
