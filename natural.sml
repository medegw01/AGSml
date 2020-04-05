(* convenient code *)
(*the nat datatype was addopted from my ml homework*)
structure ExposedNatural = struct               (* Look!  No ascription *)
  
  datatype nat = ZERO
               | TIMESPLUS of nat * int
  (* abstraction function:
    if d is a digit greater than 0
    A(0) = ZERO
    A(d) = (ZERO, d)
    A(10*m + d) = TIMES (A(m), d)
   *)

   fun invariant ZERO                  = true
     | invariant (TIMESPLUS (ZERO, d)) = 0 < d andalso  d < 10
     | invariant (TIMESPLUS (m, d))    = (invariant m)  
                                          andalso  (0 <= d andalso  d < 10)
  
  exception BadDivisor   (* divisor out of acceptable range *)
  exception Negative
  exception Match

  fun timesplus (ZERO, 0) = ZERO
    | timesplus (n, d)      = TIMESPLUS (n, d)
  fun times n = timesplus (n, 0)


  fun natOfDigit d = timesplus (ZERO, d)
  
  fun ofInt 0 = ZERO
    | ofInt n = timesplus (ofInt (n div 10), n mod 10)


  fun carryIntoNat (n,      0) = n
    | carryIntoNat (ZERO,   1) = natOfDigit 1
    | carryIntoNat (TIMESPLUS (m, d), 1) =
          timesplus (carryIntoNat (m, (d + 1) div 10), (d + 1) mod 10)
    | carryIntoNat _ = raise Match (* bad carry bit *)

    

 fun addWithCarry (  n1,     ZERO,       c) = carryIntoNat (n1, c)
   | addWithCarry (ZERO,       n2,       c) = carryIntoNat (n2, c)
   | addWithCarry (TIMESPLUS (m1, d1), TIMESPLUS (m2, d2), c) =
       let val d  = (d1 + d2 + c) mod 10
           val c' = (d1 + d2 + c) div 10  (* the "carry out" *)
       in  timesplus (addWithCarry (m1, m2, c'), d)
       end        
  
  fun /+/ (n1, n2) =  addWithCarry (n1, n2, 0)

  fun borrowFromNat (n, 0)                   = n
    | borrowFromNat (ZERO, c)                 = raise Negative
    | borrowFromNat ((TIMESPLUS (m, 0)), 1) = timesplus 
        (borrowFromNat (m, 1), 9)
    | borrowFromNat ((TIMESPLUS (m, d)), 1) = timesplus (m, d - 1)
    | borrowFromNat (_, s)                    = raise Negative

  
  fun subWithBorrow (n1, ZERO, b)   = borrowFromNat (n1, b)
    | subWithBorrow (ZERO,n2,_)     = raise Negative
    | subWithBorrow ((TIMESPLUS (m1, d1)), (TIMESPLUS (m2, d2)), b) =
          let val d = (d1 - d2 - b) mod 10
             val b' = if d1 - d2 - b < 0 then 1 else 0 (* the ”borrow out” *)
          in 
            timesplus (subWithBorrow (m1, m2, b'), d)
          end


   
  fun  /-/  (n1, n2)  = subWithBorrow (n1, n2, 0)
  
  
  fun /*/ (ZERO, _) = ZERO
    | /*/ (_, ZERO) = ZERO
    | /*/ (TIMESPLUS (m1, d1), TIMESPLUS (m2, d2)) =
         /+/ (ofInt (d1 * d2),
                /+/(times (/+/ (/*/(m1, natOfDigit d2),
                                        /*/(m2, natOfDigit d1))),
                         times (times (/*/ (m1, m2)))))
   

  fun flip f (x, y) = f (y, x)

  (* natOfDigits : int list -> nat *)
  fun natOfDigits ds = foldl (flip timesplus) ZERO ds
  

  fun listOfNat ZERO = []
    | listOfNat (TIMESPLUS (m, d)) = listOfNat m @ [d]
  

  fun decimal ZERO   = [0]
    | decimal n      = listOfNat n
  
  
  fun divWithCarry ([], n, c, result)    = (result, c)
     | divWithCarry (x::n1, n, c, result) = 
                  divWithCarry (n1, n,  (10*c + x) mod n, 
                    result @ [(10*c + x) div n])
   
  
  fun sdiv  (_, 0)    =  raise BadDivisor 
    | sdiv (ZERO, _)  =  {quotient = ZERO, remainder = 0}
    | sdiv (n1, n)    = if n < 0 then raise Negative else
                           let 
                            val (a, b) = divWithCarry (decimal n1, n, 0, [])
                           in 
                            {quotient = natOfDigits a, remainder = b}
                          end


  fun compare (n1, n2) = let
                           val name = /-/ (n1, n2)
                        in
                          case name
                            of ZERO => EQUAL
                             | _    => GREATER
                        end
                        handle Negative => LESS
 end 

structure Natural :> NATURAL = ExposedNatural   (* the module is sealed here *)

