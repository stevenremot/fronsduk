; Program for recursively telling if a number is even or not
Dum Nil

; Function "even"
Ldf (Ld (0 0) Sel
     (Nil
      Ldc 1
      Ld (0 0) Minus
      Cons
      Ld (1 0) Ap Not
      Join)
     (Ldc 1
      Join)
     Rtn)
Cons

; Function "odd"
Ldf (Ld (0 0) Sel
     (Nil
      Ldc 1
      Ld (0 0) Minus
      Cons
      Ld (1 1) Ap Not
      Join)
     (Ldc 0
      Join)
     Rtn)
Cons

; Recursive expression that tests if a number is even
Ldf (Nil
     Ldc 6
     Cons
     Ld (0 1) Ap
     Rtn)
Rap