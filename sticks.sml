functor SticksFun(val N : int) :> GAME = struct

  datatype state = STICKS of int
                 | TURN of Player.player * state

  
  fun invariant (STICKS  _)   = true
    | invariant (TURN (_, _)) = true
    

  (* abstraction function: 
     A(d) =  TURN (p1, STICKS d)
     A(d) =  TURN (P2, STICKS d)
   *)
   

     structure Move = struct
     (* pick one of these two ways to define type `move` *)
     datatype move = ONE | TWO | THREE 
     
     fun invariant ONE   = true
       | invariant TWO   = true
       | invariant THREE = true
      
       (*abstraction function:
          A(1)  = ONE
          A(2)  = TWO
          A(3)  = THREE
       *)
      exception Move
      val s = " "

       (* creator *)
       
       (* 
         val parse : string -> move
           Converts the given string to a move; If the string 
           doesn't correspond to a valid move, parse raises Move 
       *)
        fun parse "1"     = ONE
          | parse "2"     = TWO
          | parse "3"     = THREE
          | parse "one"   = ONE
          | parse "two"   = TWO
          | parse "three" = THREE
          | parse _       = raise Move
      
      
      (* observers *)

      (*
        val prompt : Player.player -> string
           A request for a move from the given player 
          Example: "What square does player O choose?" 
      *)
      
       fun prompt p = "How many sticks does player"
                          ^s^Player.unparse p^s^"pick up? "

      
      (* 
         val visualize : Player.player -> move -> string
            A short string describing a move. 
            Example: "Player X picks up ...".
            The string may not contain a newline. 
      *)
       
     fun visualize p ONE   = "Player"^s^Player.unparse p^s^"takes 1 sticks"
       | visualize p TWO   = "Player"^s^Player.unparse p^s^"takes up 2 sticks"
       | visualize p THREE = "Player"^s^Player.unparse p^s^"takes up 3 sticks"

  end



   (* CREATORS *)

   (*
    val initial : Player.player -> state
        Initial state for a game in which 
        the given player moves first. 
    *)

   fun initial y = TURN (y, STICKS N)

   (* PRODUCERS *)

   (* 
     val makemove: state -> Move.move -> state
        Returns the state obtained by making the 
        given move in the given state.  Raises Move.Move
        if the state is final or if the given move is not
        legal in the given state. 
    *)
    fun makemove (TURN (p, STICKS y)) x = if y <= 0 then raise Move.Move
                          else let
                                 val name = x
                               in case name 
                                 of Move.ONE   =>  TURN (Player.otherplayer p, 
                                                           STICKS (y-1)) 
                                  | Move.TWO   =>  if y>1 then 
                                                   TURN (Player.otherplayer p, 
                                                                STICKS (y-2))
                                              else raise Move.Move

                                  | Move.THREE => if y>2 then 
                                                   TURN (Player.otherplayer p, 
                                                                STICKS (y-3))
                                              else raise Move.Move
                                end
     | makemove _ _                = raise Move.Move 
    

   (* OBSERVERS *)

   (*val visualize : state -> string
       A string representing the given state.  
       The string must show whose turn it is, 
       and it must end in a newline. 
    *)
    fun stickString 0 = "\n"
     | stickString n = "| "^stickString (n - 1)
    
    val eb = " faces an empty board\n"
    fun visualize (TURN (p, STICKS y)) = 
                           if y = 0 then "Player "^Player.unparse p^eb
                           else "Player "^Player.unparse p^" sees "^
                                           stickString y
      | visualize _                    = eb



   (*
     val whoseturn  : state -> Player.player
        Given a _non-final_ state, returns the player
        whose turn it is to move.    When given a
        final state, behavior is unspecified. 
   *)
   fun whoseturn (TURN (y, _))        = y
     | whoseturn _                    = raise Move.Move

  
  
  (* 
    val outcome : state -> Player.outcome option
       When given a final state, returns SOME applied to the outcome.
       When given a non-final state, returns NONE. 
  *)
   fun outcome (TURN (p, STICKS y)) = let
                                        val play_outcome = Player.WINS
                                               (Player.otherplayer p)
                                       in 
                                         if y = 0 then SOME play_outcome
                                         else  NONE
                                       end
     | outcome _                    = NONE
  
  (* 
     val isOver : state -> bool
       Tells if the given state is final. 
  *)
    fun isOver y  = let 
                     val rst = outcome y
                    in case rst
                      of NONE   => false
                       | SOME t => true 
                    end


  (* 
     val legalmoves : state -> Move.move list
       Lists all moves that are valid inputs to `makemove` in the
       given state.  The list is empty if and only if the given
       state is final. 
  *)
  fun legalmoves (TURN (p, STICKS n)) = if isOver (TURN (p, STICKS n)) then [] 
                                        else let 
                                              val mv1 = if n>0 then [Move.ONE] 
                                                        else []
                                              val mv2 = if n>1 then [Move.TWO] 
                                                        else []
                                               val mv3 = if n>2 then 
                                                      [Move.THREE] else []

                                             in (mv1 @ mv2) @ mv3 
                                             end

    | legalmoves _                     = raise Move.Move
  
end

structure TestSticks = SticksFun(val N = 14)