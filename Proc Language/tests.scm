(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
 
      ; ========== Sub-Question 1 ==========
      (q1-test1 "let abs-equal = proc (x) proc (y) 
                               if zero?(-(x,y)) then zero?(0) 
                               else 
                                   if zero?(-(x,-(0,y))) then zero?(0) 
                                   else zero?(1)
                               in ((abs-equal 4) -4)" #t)
      
      (q1-test2 "let abs-equal = proc (x) proc (y) 
                               if zero?(-(x,y)) then zero?(0) 
                               else 
                                   if zero?(-(x,-(0,y))) then zero?(0) 
                                   else zero?(1)
                               in ((abs-equal 6) 6)" #t)
      
      (q1-test3 "let abs-equal = proc (x) proc (y) 
                               if zero?(-(x,y)) then zero?(0) 
                               else 
                                   if zero?(-(x,-(0,y))) then zero?(0) 
                                   else zero?(1)
                               in ((abs-equal 7) 9)" #f)
      
      (q1-test4 "let abs-equal = proc (x) proc (y) 
                               if zero?(-(x,y)) then zero?(0) 
                               else 
                                   if zero?(-(x,-(0,y))) then zero?(0) 
                                   else zero?(1)
                               in ((abs-equal 0) 0)" #t)
      
      ; ========== Sub-Question 2 ==========
      ; (q2 (num-val 12))
      
      ; ========== Sub-Question 3 ==========
      (q3-test1 "let div = proc (func)
                       proc (x)
                         proc(count)
                           if zero?(x) then count
                           else
                             if zero?(-(x,1)) then -1
                             else (((func func) -(x,2)) -(count,-1))
           in let div-caller = proc (x) (((div div) x) 0)
           in let f-helper = proc (func)
                               proc (x)
                                 if zero?(-(x,1)) then zero?(0)   
                                 else
                                   if zero?(-((div-caller x), -1)) then zero?(1)
                                   else ((func func) (div-caller x))
           in let f = proc (x) ((f-helper f-helper) x)
           in (f 1)" #t)
      
      (q3-test2 "let div = proc (func)
                       proc (x)
                         proc(count)
                           if zero?(x) then count
                           else
                             if zero?(-(x,1)) then -1
                             else (((func func) -(x,2)) -(count,-1))
           in let div-caller = proc (x) (((div div) x) 0)
           in let f-helper = proc (func)
                               proc (x)
                                 if zero?(-(x,1)) then zero?(0)   
                                 else
                                   if zero?(-((div-caller x), -1)) then zero?(1)
                                   else ((func func) (div-caller x))
           in let f = proc (x) ((f-helper f-helper) x)
           in (f 32)" #t)
      
      (q3-test3 "let div = proc (func)
                       proc (x)
                         proc(count)
                           if zero?(x) then count
                           else
                             if zero?(-(x,1)) then -1
                             else (((func func) -(x,2)) -(count,-1))
           in let div-caller = proc (x) (((div div) x) 0)
           in let f-helper = proc (func)
                               proc (x)
                                 if zero?(-(x,1)) then zero?(0)   
                                 else
                                   if zero?(-((div-caller x), -1)) then zero?(1)
                                   else ((func func) (div-caller x))
           in let f = proc (x) ((f-helper f-helper) x)
           in (f 34)" #f)
      
      ))
  )