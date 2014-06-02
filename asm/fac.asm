; Factorial example
Dum Nil

; Factorial definition
Ldf (Ld (0 0) Sel
     (Nil
      Ldc 1 Ld (0 0) Minus
      Cons
      Ld (1 0) Ap
      Ld (0 0) Times
      Join)
     (Ldc 1 Join)
     Rtn)
Cons

; Recursive expression that computes factorial 5
Ldf (Nil
     Ldc 5
     Cons
     Ld (0 0) Ap
     Rtn)
Rap
