;global-env is a list of scopes where a scope is a list of name/value pairs (2-lists)

;extractors
(define class->super
  (lambda (class)
    (cadr class)))

(define class->global-env
  (lambda (class)
    (car (caddr class))))

;constructors

(define class
  (lambda (base-class list-of-field-names list-of-field-values list-of-method-names list-of-method-values)
    (letrec ((create-pairs-for-scope (lambda (lon lov)
                             (cond
                               ((null? lon) '())
                               (else (cons (list (car lon)
                                                 (car lov))
                                           (create-pairs-for-scope (cdr lon) (cdr lov)))))))
             (global-env (list (create-pairs-for-scope (append list-of-field-names list-of-method-names)
                                                       (append list-of-field-values list-of-method-values)))))
      (list 'class base-class global-env))))

(define empty-object
  (lambda ()
    (class '(no-parent) '() '() '(getValue) (list (lambda (class var-name)
                                                    (letrec ((theFunction (lambda (theVar theEnv theClass)
        (cond
          ((null? theEnv)(theFunction theVar (class->global-env (class->super theClass)) (class->super theClass)))
           ((eq? (caar theEnv) theVar) (cadar theEnv)) 
          (else (theFunction theVar (cdr theEnv) theClass)))))
     (myResult (list (theFunction var-name (class->global-env class) class))))
    (car myResult)))))))
                           
(define sendMessage
  (lambda (class function var-name)
    (letrec ((theFunction (lambda (theVar theEnv theClass)
        (cond
           ((null? theEnv)(theFunction theVar (class->global-env (class->super theClass)) (class->super theClass)))
           ((eq? (caar theEnv) theVar) (cadar theEnv)) 
          (else (theFunction theVar (cdr theEnv) theClass)))))
     (myResult (list (theFunction function (class->global-env class) class))))
     ((car myResult) class (car var-name)))))

(define Person
  (lambda (list-of-field-names list-of-values)
    (class (empty-object) list-of-field-names list-of-values '() '())))

(define p1 (Person '(fname lname age) '(Mike Litman 21)))
(define p2 (Person '(fname lname age) '(Dave Smith 18)))
(sendMessage p2 'getValue '(fname))
p2
