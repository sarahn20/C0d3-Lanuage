;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname CSC470_interpreter1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Enviroment Initializers
(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (name value env)
    (list 'extend-env name value env)))

(define extend-env*
  (lambda (lon lov env)
    (cond
      ((null? lon) env)
      (else (extend-env* (cdr lon) (cdr lov) (extend-env (car lon) (car lov) env))))))

(define get-name
  (lambda (env) (cadr env)))

(define get-value
  (lambda (env) (caddr env)))

(define get-env
  (lambda (env) (cadddr env)))

(define empty-env?
  (lambda (env) (eq? 'empty-env (car env))))

(define apply-env
  (lambda (var-name env)
    (cond
      ((empty-env? env) #f)
      (else
       (if (eq? var-name (get-name env))
           (get-value env)
           (apply-env var-name (get-env env)))))))

(define has-binding?
  (lambda (var-name env)
    (not (eq? (apply-env var-name env) #f))))

; Grammar Constructors
(define lit-exp
  (lambda (n)
    (list 'lit-exp n)))

(define var-exp
  (lambda (s)
    (list 'var-exp s)))

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

(define parse-expression
  (lambda (c0d3)
    (cond
      ((eq? (car c0d3) 'literal) (lit-exp (literal-exp->value c0d3)))
      ((eq? (car c0d3) 'get-value) (var-exp (get-value-exp->value c0d3)))
      ((eq? (car c0d3) 'func) (lambda-exp (func-exp->parameter c0d3) (parse-expression (func-exp->body c0d3))))
      ((eq? (car c0d3) 'run) (app-exp
                              (parse-expression (run-exp->func c0d3))
                              (parse-expression (run-exp->parameter c0d3)))))))

(define apply-expression
  (lambda (lcexp env)
    (cond
      ((lit-exp? lcexp) (lit-exp->value lcexp))
      ((var-exp? lcexp) (apply-env (var-exp->var-name lcexp) env))
      ((lambda-exp? lcexp) (apply-expression (lambda-exp->body lcexp) env))
      ((app-exp? lcexp) (let* ((the-lambda (app-exp->lambda-exp lcexp))
                              (the-lambda-param-name (lambda-exp->parameter-name the-lambda))
                              (the-parameter-value (apply-expression (app-exp->parameter-input lcexp) env))
                              (the-new-env (extend-env the-lambda-param-name the-parameter-value env)))
                          (apply-expression the-lambda the-new-env))))))
                          

(define run-program
  (lambda (c0d3-src env)
    (apply-expression (parse-expression c0d3-src) env)))

(define myC0d3 '(run (func gets (a) does (get-value a)) with (literal 5)))
(define env (extend-env* '(c d e) '(1 2 3) (empty-env)))
(parse-expression myC0d3)
(run-program myC0d3 env)


         