(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
  
;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    (arr-val
     (arr array?))
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))
  
  (define (expval->array val)
    (cases expval val
      (arr-val (v) v)
      (else (expval-extractor-error 'array val))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))
  
  (define-datatype array array?
    (new-array
     (type is-array-type?)
     (size is-valid-size?)
     (ref-vals are-ref-vals?)))
  
  ; Returns #t if the given string is a valid array type, otherwise returns #f.
  (define (is-array-type? type)
    (or (string=? type "#") (string=? type "?") (string=? type "@")))
  
  ; Returns #t if the given number is a positive integer, otherwise returns #f.
  (define (is-valid-size? size)
    (and (integer? (expval->num size)) (> (expval->num size) 0)))
  
  ; Returns #t if the given list is a non-empty list of reference values, otherwise returns #f.
  (define (are-ref-vals? vals-lst)
    (if (or (null? vals-lst) (not (expval? (car vals-lst))))
        #f
        (if (and (= (length vals-lst) 1) (ref-val? (car vals-lst)))
            #t
            (are-ref-vals? (cdr vals-lst)))))
  
  ; Returns #t if the given value is a ref-val, otherwise returns #f.
  (define (ref-val? val)
    (cases expval val
      (ref-val (v) #t)
      (else #f)))
  
  ; Returns #t if the given value is a arr-val, otherwise returns #f.
  (define (arr-val? val)
    (cases expval val
      (arr-val (v) #t)
      (else #f)))
     
  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))
  


)