functor BignumFn(structure N : NATURAL) :> BIGNUM
  =
   struct

   	datatype bigint = BIGZERO
   	                | POSITIVE of N.nat
   	                | NEGATIVE of N.nat
   (* abstraction function:
    A(0) = BIGZERO
    A(d) = POSITIVE (ZERO, d) d > 0
    A(d) = NEGATIVE (ZERO, d) d < 0
    A(10*m + d) = POSITIVE (TIMES (A(m), d)) m > 0
    A(10*m + d) = NEGATIVE (TIMES (A(m), d)) m < 0
   *)

   fun invariant BIGZERO = true
   	 | invariant (POSITIVE x) = N.invariant x
   	 | invariant (NEGATIVE x) = N.invariant x
   	


   exception BadDivision                (* contract violation for sdiv *)

   fun ofInt  0 = BIGZERO
   	 | ofInt  n = if  n > 0  then
   	              POSITIVE (N.ofInt n) else
   	              NEGATIVE (N.ofInt ((0 - 1)*n))

   fun negated BIGZERO = BIGZERO
   	 | negated (POSITIVE x) = NEGATIVE x
     | negated (NEGATIVE x) = POSITIVE x  (* "unary minus" *)
   
   infix 6 <+> <->
   infix 7 <*> sdiv
   
   val /+/ = N./+/
   val /-/ = N./-/
   val /*/ = N./*/

   infix 6 /+/ /-/
   infix 7 /*/ 
   
   fun sumBig ((POSITIVE x), (POSITIVE y))  = POSITIVE (x /+/ y)
   	 | sumBig ((POSITIVE x), (NEGATIVE y))  = let
   	 	                                       val boolV = N.compare (x,y) 
   	 	                                      in case boolV
   	 	                                       of LESS    => NEGATIVE (y /-/ x)
   	 	                                        | EQUAL   => BIGZERO
   	 	                                        | GREATER => POSITIVE (x /-/ y)
   	 	                                      end
   	 | sumBig ((NEGATIVE x), (POSITIVE y)) = let 
   	 	                                       val boolV = N.compare (x,y) 
   	 	                                     in case boolV
   	 	                                       of LESS    => POSITIVE (y /-/ x)
   	 	                                        | EQUAL   => BIGZERO
   	 	                                        | GREATER => NEGATIVE (x /-/ y)
   	 	                                    end
   	 | sumBig ((NEGATIVE x), (NEGATIVE y)) = NEGATIVE (x /+/ y)
   	 | sumBig (x ,BIGZERO)                 = x
   	 | sumBig (BIGZERO,y)                  = y

  
  fun n1 <+> n2  = sumBig (n1, n2)
   	 
  
  
  fun subBig ((POSITIVE x), (POSITIVE y)) = POSITIVE x <+> NEGATIVE y
  	| subBig ((POSITIVE x), (NEGATIVE y)) = POSITIVE x <+> POSITIVE y
  	| subBig ((NEGATIVE x), (POSITIVE y)) = NEGATIVE (x /+/ y)
  	| subBig ((NEGATIVE x), (NEGATIVE y)) = NEGATIVE x <+> POSITIVE y
  	| subBig (x ,BIGZERO)                 = x
  	| subBig (BIGZERO,y)                  = negated y

  fun n1  <-> n2 = subBig (n1, n2)
   

  fun mulBig ((POSITIVE x), (POSITIVE y)) = POSITIVE (x /*/ y)
  	| mulBig ((POSITIVE x), (NEGATIVE y)) = NEGATIVE (x /*/ y)
  	| mulBig ((NEGATIVE x), (POSITIVE y)) = NEGATIVE (x /*/ y)
  	| mulBig ((NEGATIVE x), (NEGATIVE y)) = POSITIVE (x /*/ y)
  	| mulBig (x ,BIGZERO)                 = BIGZERO
  	| mulBig (BIGZERO,y)                  = BIGZERO

  fun n1  <*> n2 = mulBig (n1, n2)
   
  
  fun sdivBig (_, 0)              = raise BadDivision
  	| sdivBig (NEGATIVE _, d)     = raise BadDivision
  	| sdivBig (BIGZERO, d)        = {quotient = BIGZERO, remainder = 0}
  	| sdivBig (POSITIVE n1, d)    = let
   	 	val {quotient = x, remainder = r} =  N.sdiv (n1, d)
   	 in
   	 	{quotient = POSITIVE x, remainder = r}
   	 end  


  fun n1 sdiv d      =   sdivBig (n1, d)         
 
     (* Contract for "short division" sdiv, which is defined only on
        *nonnegative* integers:

        Provided 0 < d <= K and n >= 0,
          sdiv (n, d) returns { quotient = q, remainder = r }
        such that
          n == q /*/ ofInt d /+/ ofInt r
          0 <= r < d

        Given a d out of range or a negative n, 
        sdiv (n, d) raises BadDivision
        
        The constant K depends on the number of bits in a machine
        integer, so it is not specified, but it is known to be at
        least 10.

     *)
        
   fun compare (BIGZERO, e)                 = let
   	                                            val j = e
   	                                          in case j
   	                                            of BIGZERO   => EQUAL
   	                                             | POSITIVE _=> LESS
   	                                             | NEGATIVE _=> GREATER
   	                                          end

   	 | compare (e, BIGZERO)                 = let 
   	 	                                        val j = e
   	 	                                       in case j
   	                                            of BIGZERO   => EQUAL
   	                                             | POSITIVE _=> GREATER
   	                                             | NEGATIVE _=> LESS 
   	                                            end
   	 | compare ((POSITIVE x), (POSITIVE y)) = N.compare (x, y)
   	 | compare ((NEGATIVE x), (NEGATIVE y)) = N.compare (y, x)
   	 | compare ((NEGATIVE _), (POSITIVE _)) = LESS
   	 | compare ((POSITIVE _), (NEGATIVE _))  = GREATER

 
fun intListStr []  = ""
  | intListStr (x::xs) = (Int.toString x) ^ intListStr xs
  
  fun toString BIGZERO  = "0"
   	| toString (POSITIVE natVal) = intListStr (N.decimal natVal)
    | toString (NEGATIVE natVal) = "-"^intListStr (N.decimal natVal)

     (* toString n returns a string giving the natural 
        representation of n in the decimal system.  If n is
        negative, toString should use the conventional minus sign "-".

        And when toString returns a string containing two or more digits,
        the first digit must not be zero.
     *)
  
   end