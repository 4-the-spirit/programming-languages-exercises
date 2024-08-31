(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      
      ; /////////////// MY TESTS ///////////////
      
      (type-changing-1 "let a=#[3]{55,66,77} in setref([a,0],zero?(0))" "value-of: Type of a variable can't be changed")
      (type-changing-2 "let a=#[3]{55,66,77} in setref([a,0],zero?(0))" "value-of: Type of a variable can't be changed")
      
      (out-of-bound-indexing-1 "let z=?[3]{zero?(0),zero?(1),zero?(0)} in [z,3]" "value-of: Bad array indexing")
      (out-of-bound-indexing-2 "let z=?[3]{zero?(0),zero?(1),zero?(0)} in [z,-1]" "value-of: Bad array indexing")
      
      (array-initialization-type "let v=#[4]{99,88,zero?(0),-(99,88)} in deref([v,0])" "value-of: Array initialization mismatch array type")
      
      (array-initialization-size-1 "let v=#[4]{99,88,77,66,55} in deref([v,0])" "value-of: Array initialization mismatch array size")
      (array-initialization-size-2 "let v=#[4]{99,88,77} in deref([v,0])" "value-of: Array initialization mismatch array size")
      
      ; ////////////////////////////////////////

      ; /////////////// TASK TESTS ///////////////
      
      (task-test-1 "let a = #[4]{10,20,30,40} 
                    in begin 
                         setref([a,2], 80);
                         deref([a,2])
                       end" 80)
      
      (task-test-2 "let a = ?[4]{10,20,30,40} 
                    in begin 
                         setref([a,2], 80);
                         deref([a,2])
                       end" "value-of: Array initialization mismatch array type")
      
      (task-test-3 "let a = #[4]{10,20,30} 
                    in begin 
                         setref([a,2], 80);
                         deref([a,2])
                       end" "value-of: Array initialization mismatch array size")
      
      (task-test-4 "let a = #[4]{10,20,30,40} 
                    in begin 
                         setref([a,7], 80);
                         deref([a,2])
                       end" "value-of: Bad array indexing")
      
      (task-test-5 "let p1 = proc (x) -(x,1)
                      in let p2 = proc (x) -(x,2)
                        in let p3 = proc (x) -(x,3)
                          in let a = @[3]{p1,p2,p3}
                    in begin
                      setref([a,2],p1);
                      (deref([a,2]) 9)
                    end" 8)
      
      ; //////////////////////////////////////////
      
      ))
  )