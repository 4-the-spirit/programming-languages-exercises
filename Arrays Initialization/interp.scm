(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
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
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let* ((ref (expval->ref (value-of exp1 env)))
                (old-val (deref ref))
                (new-val (value-of exp2 env))
                (type-err-msg "Type of a variable can't be changed")
                (random-value 23))
            (if (has-same-type? old-val new-val)
                (setref! ref new-val)
                (eopl:error 'value-of type-err-msg))))
        
        (arr-exp (typ size initexps)
            (let ((size-err-msg "Array initialization mismatch array size")
                  (type-err-msg "Array initialization mismatch array type"))
              (cond
                ((has-invalid-size initexps size env) (eopl:error 'value-of size-err-msg))
                ((has-invalid-exp initexps typ env) (eopl:error 'value-of type-err-msg))
                ; The array expression is valid, now we'll allocate the pointers on the heap (store).
                (else (let ((size-val (value-of size env))
                            (refs-lst (map (lambda (exp) (ref-val (newref (value-of exp env)))) initexps)))
                        (arr-val (new-array typ size-val refs-lst)))))))
        
        (index-exp (exp1 exp2)
            (let ((index-val (value-of exp2 env))
                  (array-val (value-of exp1 env))
                  (indexing-err-msg "Bad array indexing"))
              (if (or (not (is-valid-index? index-val array-val)) (not (arr-val? array-val)))
                  (eopl:error 'value-of indexing-err-msg)
                  (let* ((arr (cases array (expval->array array-val)
                                (new-array (type size pure-array) pure-array)))
                         (final-index (expval->ref (list-ref arr (expval->num (value-of exp2 env))))))
                    (ref-val final-index)))))
        
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (if (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
  
  ; Returns #t if the given expressions list has an element that is not of the given type.
  ; Otherwise returns #f.
  (define (has-invalid-exp exps type env)
    (if (null? exps)
        #f
        (let* ((first (value-of (car exps) env))
               (first-symbol (get-value-symbol first)))
          (if (not (string=? first-symbol type))
            #t
            (has-invalid-exp (cdr exps) type env)))))
  
  ; Returns #t if the given expressions list has an invalid size, otherwise returns #f.
  (define (has-invalid-size exps size env)
    (not (= (length exps) (expval->num (value-of size env)))))
  
  ; Returns the symbol of the given value.
  (define (get-value-symbol val)
    (cases expval val
      (num-val (v) "#")
      (bool-val (v) "?")
      (proc-val (v) "@")
      (else "")))
  
  ; Returns #t if the given index is not greater than the last index of the array and greater
  ; or equal to zero, otherwise returns #f.
  (define (is-valid-index? index-val array-val)
    (let* ((arr (expval->array array-val))
           (size (cases array arr (new-array (type size-val refs-lst) (expval->num size-val))))
           (last-index (- size 1))
           (index (expval->num index-val)))
      (if (or (> index last-index) (> 0 index))
          #f
          #t)))
  
  ; Returns #t if the given values are of the same type, otherwise returns #f.
  (define (has-same-type? val1 val2)
    (let ((val1-type (cases expval val1
                       (num-val (v) "num-val")
                       (bool-val (v) "bool-val")
                       (proc-val (v) "proc-val")
                       (ref-val (v) "ref-val")
                       (arr-val (v) "arr-val")))
          (val2-type (cases expval val2
                       (num-val (v) "num-val")
                       (bool-val (v) "bool-val")
                       (proc-val (v) "proc-val")
                       (ref-val (v) "ref-val")
                       (arr-val (v) "arr-val"))))
      (string=? val1-type val2-type)))
 
  )
  


  
