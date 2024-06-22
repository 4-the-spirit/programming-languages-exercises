(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for proc-lang/ds-rep

  (require "lang.scm")                  ; for expression?

  (provide (all-defined))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
  
;;; /////////////// errors ///////////////
  
  (define duplicate-types-err-msg "Procedure definition error - duplicate types")
  (define no-overloading-err-msg "There is no body for such parameter")
  (define no-body-err-msg "Procedure definition error - no body")

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (var symbol?)
      (types types-list?)
      (bodies bodies-list?)
      (env environment?)))
  
  ; Returns true if the given list is a non-empty list of types. Otherwise returns false.
  (define (types-list? lst)
    (let ((types-lst '("Int" "Bool" "Proc"))
          (max-occurs 1))
      (cond
        ((null? lst) #f)
        ((all lst (lambda (str) (if (and (>= max-occurs (count-string lst str)) (>= (count-string types-lst str) 1)) #t #f))) #t)
        ((> (length lst) (length types-lst)) (eopl:error 'proc-exp duplicate-types-err-msg))
        (else #f))))
  
  ; Returns true if the given list is a non-empty list of expression values. Otherwise returns false.
  (define (bodies-list? lst)
    (if (null? lst)
        #f
        (if (all lst (lambda (e) (if (expression? e) #t #f)))
            #t
            #f)))
  
  ; Returns true if all the elements in the given list satisfy the predicate condition.
  ; Otherwise returns false.
  (define (all lst predicate?)
    (if (null? lst)
        #t
        (if (predicate? (car lst))
            (all (cdr lst) predicate?)
            #f)))
  
  ; Returns the number of times a string occurs in the given list of strings.
  (define (count-string str-lst string-element)
    (if (null? str-lst)
        0
        (if (string=? (car str-lst) string-element)
            (+ 1 (count-string (cdr str-lst) string-element))
            (count-string (cdr str-lst) string-element))))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

)