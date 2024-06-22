(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        
        (mul-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var types bodies)
          (if (or (null? types) (null? bodies))
              (eopl:error 'proc-exp no-body-err-msg)
              (proc-val (procedure var types bodies env))))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define (apply-procedure proc1 val)
    (cases proc proc1
      (procedure (var types bodies saved-env)
                 (let ((index (find-type-index val types)))
                   (if (= index -1)
                       (eopl:error 'apply-proc no-overloading-err-msg)
                       (let ((exp (list-ref bodies index)))
                         (value-of exp (extend-env var val saved-env))))))))
  
  ; Returns the index of the given expval type in the given types list.
  ; Returns -1 if the type was not found.
  (define (find-type-index val types)
    (cases expval val
      (num-val (v) (index-of "Int" types))
      (bool-val (v) (index-of "Bool" types))
      (proc-val (v) (index-of "Proc" types))))
  
  ; Returns the index of a given value in the list.
  ; Returns -1 if the value is not in the list.
  (define (index-of value lst)
    (letrec ((helper 
             (lambda (value lst index)
               (if (null? lst)
                   -1
                   (if (string=? value (car lst))
                       index
                       (helper value (cdr lst) (+ index 1)))))))
    (helper value lst 0)))

  )
