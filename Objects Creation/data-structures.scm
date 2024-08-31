(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined))               ; too many things to list

; /////////////// New Errors ///////////////

  (define not-a-record-err-msg "Not a record")
  (define no-such-field-err-msg "There is no such field")
  
  (define valid-return-code 0)
  
; //////////////////////////////////////////
  
  
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
    (rec-val
      (rec record?))
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
      (bval reference?)                 ; new for implicit-refs
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))
  
  ; /////////////// New ADT ///////////////
  
  (define-datatype record record?
    (new-record
       (fields symbols-list?)
       (values refs-list?)))
  
  ; Returns #t if all the elements of the given list are symbols, and #f otherwise.
  (define (symbols-list? lst)
    (if (null? lst)
        #t
        (and (symbol? (car lst)) (symbols-list? (cdr lst)))))
  
  ; Returns #t if all the elements of the given list are ref-vals, and #f otherwise.
  (define (refs-list? lst)
    (if (null? lst)
        #t
        (and (ref-val? (car lst)) (refs-list? (cdr lst)))))
  
  (define (expval->record val)
    (cases expval val
      (rec-val (v) v)
      (else (expval-extractor-error 'record val))))
  
  ; Returns #t if the given expressed value is a rec-val, and #f otherwise.
  (define (rec-val? val)
    (cases expval val
      (rec-val (v) #t)
      (else #f)))
      
  (define (var-exp->var exp)
    (cases expression exp
      (var-exp (idnt) idnt)
      (else (eopl:error "Can't extract from the given expression"))))
  
  ; Returns #t if the given value is a ref-val, and #f otherwise.
  (define (ref-val? val)
    (cases expval val
      (ref-val (v) #t)
      (else #f)))
  
  ; ///////////////////////////////////////
    

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym val)              ; val is a denoted value-- a
                                        ; reference. 
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