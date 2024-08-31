(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      
      ; /////////////// MY TESTS ///////////////
  
      (my-test-1 "let calc = proc (x)
                                    {
                                      Int: proc (y)
                                           {
                                             Int: -(x, -(0,y));
                                           };
                                    } in let R = 20 in ((calc 100) R)" 120)
      
      (my-test-2 "let bool-func = proc (x) {
                                     Int: x;
                                     Bool: if x then proc (y) { Int: -(y,90); }
                                           else 100;
                                  } in ((bool-func zero?(0)) 990)" 900)
      
      ; ////////////////////////////////////////

      ; /////////////// TASK TESTS ///////////////
      
      (task-test-1 "let p = proc (x) 
                       { 
                          Int: zero?(-(x,8)); 
                          Bool: if x then 20 else 30; 
                          Proc: -((x 50), 30);
                       } in (p 7)" #f)
      
      (task-test-2 "let p = proc (x) 
                       { 
                          Int: zero?(-(x,8)); 
                          Bool: if x then 20 else 30; 
                          Proc: -((x 50), 30);
                       } in (p zero?(4))" 30)
      
      (task-test-3 "let p = proc (x) 
                       { 
                          Int: zero?(-(x,8)); 
                          Bool: if x then 20 else 30; 
                          Proc: -((x 50), 30);
                       } in (p proc (d) { Int: -(d,600); })" -580)
      
      (task-test-4 "let p = proc (x) 
                       { 
                          Int: zero?(-(x,8)); 
                          Bool: if x then 20 else 30; 
                          Int: -(5,x);
                          Proc: -((x 50), 30);
                       } in (p 7)" "proc-exp: Procedure definition error - duplicate types")
      
      (task-test-5 "let p = proc (x) {
                                         
                                     }
                    in (p 7)" "proc-exp: Procedure definition error - no body")
      
      (task-test-6 "let p = proc (x) 
                            {
                              Bool: if x then 20 else 30;
                              Proc: -((x 50), 30);
                            } 
                    in (p 7)" "apply-proc: There is no body for such parameter")
      
      ; //////////////////////////////////////////
      
      
      ))
  )