(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      (empty-1 "emptylist" ())
      
      (extendedlist-1 "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))" (4 (3)))
      (extendedlist-2 "cons(1, cons(2, cons(3, emptylist)))" (1 2 3))
      
      (car-1 "car cons(1, cons(2, cons(3, emptylist)))" 1)
      (car-2 "let x = 99 v = 9 in car cons(-(x,v), cons(250, emptylist))" 90)
      
      (cdr-1 "cdr cons(1, cons(2, cons(3, emptylist)))" (2 3))
      (cdr-2 "let x = 99 v = 9 in cdr cons(-(x,v), cons(250, emptylist))" (250))
      
      (null?-1 "null? cons(if zero?(0) then 0 else 1, emptylist)" #f)
      (null?-2 "let q = cons(2, cons(4, emptylist)) in null? q" #f)
      (null?-3 "let q = emptylist in null? q" #t)
      
      (list-1 "let* a = 4 b = -(a,-4) c = -(b,-4) in list(a,b,c)" (4 8 12))
      (list-2 "list(if zero?(0) then zero?(0) else zero?(1))" (#t))

      ))
  )