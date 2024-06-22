(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      ; /////////////// MY TESTS ///////////////
      
      (my-test-1 "let animal-1 = record { height: 90; weight: 140; legs: 4; hair: zero?(0); }
                  in begin @<animal-1>.weight = 130; <animal-1>.weight end" 130)
      
      (my-test-2 "let car-1 = record { cost: 145000; color: 987; id: 675; }
                  in
                    if zero?(-(675, <car-1>.id)) then 
                                                   begin 
                                                     @<car-1>.cost = 155000;
                                                     <car-1>.cost 
                                                   end
                    else -1" 155000)
      
      (my-test-3 "let tree-1 = record { height: 75; length: 6; fruits: 921; last-water: 551; }
                  in 
                    begin 
                      @<tree-1>.last-water = 552; 
                      <tree-1>.last-water 
                    end" 552)
      
      ; ////////////////////////////////////////
      
      
      ; /////////////// TASK TESTS ///////////////
      
      (task-test-1 "let r=record {
                                   x: 100;
                                   y: proc (f) zero?(f);
                                   z: 300;
                                 }
                    in
                      begin
                        @<r>.x = 800;
                        -(<r>.x,<r>.z)
                      end" 500)
      
      (task-test-2 "let w = 200 in -(300, <w>.x)" "get-field-exp: Not a record")
      
      (task-test-3 "let w = record { a: 200; b: 500; } in -(300, <w>.c)" "get-field-exp: There is no such field")
      
      
      ; //////////////////////////////////////////
      
      ))
  )