(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      (mult-1 "let x = 9 k = 1000 in *(x,k)" 9000)
      (mult-2 "let* pi = /(314,100) r = 10 in *(pi, *(r,r))" 314)
      (mult-3 "let* a = 3 b = 4 in +(*(a,a),*(b,b))" 25)
      
      (add-1 "let* e=5 b=+(e,+(e,e)) in +(b,b)" 30)
      (add-2 "let* g=90 h=*(10,g) in +(h,g)" 990)
      
      (div-1 "let n = 9 p = 90 in /(p,n)" 10)
      (div-2 "let* a=100 b=/(100,5) c=/(b,5) in c" 4)
      
      (equal-1 "equal? if zero?(0) then 100 else 90 -(1,0)" #f)
      (equal-2 "let* a=5 b=4 c=3 in equal? c -(b,1)" #t)
      
      (greater-1 "let a=10 b=8 v=6 in if greater? b -(a,a) then 111 else 0" 111)
      (greater-2 "greater? *(10,10) *(100,100)" #f)
      
      (less-1 "let a=10 b=8 v=6 in if less? b -(a,a) then 111 else 0" 0)
      (less-2 "less? *(10,10) *(100,100)" #t)

      ))
  )