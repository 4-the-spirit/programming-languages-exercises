(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)          
          (let* ((val1 (value-of exp1 env))
                 (val2 (value-of exp2 env))
                 (are-ints (and (num-val? val1) (num-val? val2)))
                 (any-bool (or (bool-val? val1) (bool-val? val2)))
                 (any-error (or (excp-val? val1) (excp-val? val2))))
            (cond
              ; If both are integers -> extract them and return num-val.
              (are-ints (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (num-val (- num1 num2))))
              (any-bool (excp-val (Exception "not a number")))
              (any-error (if (excp-val? val1) val1 val2))
              (else (excp-val (Exception "environment"))))))
        
        ;\commentbox{\zerotestspec}     
        (zero?-exp (exp1)
            (let ((val1 (value-of exp1 env)))
                (cases expval val1
                  (num-val (num)
                      (if (zero? num)
                          (bool-val #t)
                          (bool-val #f)))
                  (excp-val (err) (excp-val err))
                  ; The evaluation of the expression worked but didn't return a number.
                  (else (excp-val (Exception "not a number"))))))
        
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (bool-val (v) 
                        (if (expval->bool val1)
                            (value-of exp2 env)
                            (value-of exp3 env)))
              (else (excp-val (Exception "not a boolean"))))))
        
        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (vars exps body)       
          (let* ((vals (values-of-exps exps env))
                 (index (find-error vals 0)))
            (if (>= index 0)
                ; There exists an error in the list of values.
                (list-ref vals index)
                (value-of body
                    (extend-env vars vals env)))))
            
        (let*-exp (vars exps body)
         (if (null? vars) 
           (value-of body env)
           (let* ((v1 (value-of (car exps) env))
                  (e1 (extend-env (list (car vars)) (list v1) env)))
             (cases expval v1
               (excp-val (err) (excp-val err))
               (else (value-of (let*-exp (cdr vars) (cdr exps) body) e1)))
            )))
        
        ;; ======================================
        
        (throw-exp (excpt)
            (excp-val (Exception excpt)))
        
        (try-exp (exp1 excpts excptexps finexps)
                 (let* ((val1 (value-of exp1 env))
                       (finally-exists (length finexps))
                       (return-val val1))
                   
                   ; 1. If an error exists, find a 'catch' statement that matches it.
                   ; 2. Find a Solution: search for the error in 'excpts' & change the 'return-val' if found a treatment.
                   ; 3. Execute the finally if exists.
                   ; 4. Return 'return-val'.
                   
                   (begin
                     (if (excp-val? val1)
                         (let* ((err-msg (get-err-msg val1))
                               (err-index (get-err-index excpts err-msg 0))
                               (err-exp 0))
                           ; Check if we found a catch statement for the error that was raised.
                           (if (>= err-index 0)
                               (begin
                                 (set! err-exp (list-ref excptexps err-index))
                                 (set! return-val (value-of err-exp env)))
                               0))
                         ; If it's not an error, we'll take care of it afterwards.
                         0)
                     ; Execute the finally statement.
                     (if finally-exists
                         (value-of (car finexps) env)
                         0)
                     return-val)))

        )))

(define values-of-exps
    (lambda (exps env)
      (map (lambda(x) (value-of x env)) exps)))
  
; If there exists an error of type 'excp-val' in the given list, 
; its index would be returned. Otherwise, returns -1. 
(define (find-error val-exps start)
  (if (null? val-exps)
      -1
      (let ((first (car val-exps)))
        (if (excp-val? first)
            start
            (find-error (cdr val-exps) (+ start 1))))))

; Returns the message of a given error. If not an error was given, returns -1.
(define (get-err-msg err-exp)
  (cases expval err-exp
    (excp-val (err) (cases exception err
                      (Exception (msg) msg)))
    (else -1)))

; Returns the index of the given error message in the errors messages list.
; If the error wasn't found in the list, returns -1.
(define (get-err-index err-messages err-msg start)
  (if (null? err-messages)
      -1
      (let ((first (car err-messages)))
        (if (string=? first err-msg)
            start
            (get-err-index (cdr err-messages) err-msg (+ start 1))))))
  
  )

