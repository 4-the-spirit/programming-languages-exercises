(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      
      (q3-test1 "let integral-sum = proc (x) 
                                      if zero?(x) then 0 
                                      else -((integral-sum -(x,1)), -(0,x)) 
                                    in (integral-sum 5)" 15)
      
      (q3-test2 "let integral-sum = proc (x) 
                                      if zero?(x) then 0 
                                      else -((integral-sum -(x,1)), -(0,x)) 
                                    in (integral-sum 6)" 21)
      
      (q3-test3 "let integral-sum = proc (x) 
                                      if zero?(x) then 0 
                                      else -((integral-sum -(x,1)), -(0,x)) 
                                    in (integral-sum 9)" 45)

      ))
  )