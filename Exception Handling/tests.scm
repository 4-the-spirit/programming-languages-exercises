(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
 
      ; ========================
      
      (diff-1 "-(99,zero?(0))" (excp-val (Exception "not a number")))
      (diff-2 "-(99,t)" (excp-val (Exception "environment")))
      (diff-3 "-(99,9)" 90)
      
      (zero-1 "zero?(zero?(9))" (excp-val (Exception "not a number")))
      (zero-2 "zero?(1)" #f)
      
      (if-1 "if 999 then 111 else 222" (excp-val (Exception "not a boolean")))
      (if-2 "if zero?(0) then 0 else 1" 0)
      
      (let-1 "let a = 2 b = -(t,1) c = 99 in c" (excp-val (Exception "environment")))
      (let-2 "let a = 2 in c" (excp-val (Exception "environment")))
      (let-3 "let a = if 1 then 1 else 0 in a" (excp-val (Exception "not a boolean")))
      
      (let*-1 "let* a = 1 b = -(a,-1) c = -(b,-1) in -(c,-(b,a))" 2)
      (let*-2 "let* a = zero?(0) b = zero?(if 1 then zero?(0) else zero?(0)) in 0" (excp-val (Exception "not a boolean")))

      ; ====== Additional Tests ======
      
      (test-1 "let t = -(6, zero?(9)) in 78" (excp-val (Exception "not a number")))
      (test-2 "-(t,8)" (excp-val (Exception "environment")))
      (test-3 "throw general" (excp-val (Exception "general")))
      (test-4 "try {-(if -(4,6) then 70 else 20 , 45)} 
               catch[general]: 11; 
               catch[not a number]: 12;
               catch[not a boolean]: 13;
               catch[environment]: 14;
               finally: 200;" 13)
      (test-5 "try {-(if -(4,6) then 70 else 20 , 45)} 
               catch[general]: 11; 
               catch[not a number]: 12;
               catch[not a boolean]: throw general;
               catch[environment]: 14;
               finally: 200;" (excp-val (Exception "general")))
      (test-6 "try {-(if -(4,6) then 70 else 20 , 45)} 
               catch[general]: 11; 
               catch[not a number]: 12;
               catch[environment]: 14;
               finally: 200;" (excp-val (Exception "not a boolean")))
    

      ))
  )