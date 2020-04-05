functor AgsFun (structure Game : GAME) :> 
   AGS 
     where type Game.Move.move = Game.Move.move
     and   type Game.state     = Game.state
  = struct
    structure Game = Game

  
    fun findBestMove ([], ts, state)   = (NONE, valOf (Game.outcome state))
      | findBestMove ([s], ts, state)  = 
                              let 
                                val outc_ = Game.outcome (Game.makemove state s)
                              in if outc_ = NONE then 
                                case ts 
                                   of [] => (SOME s, Player.WINS 
                                                  (Player.otherplayer 
                                                   (Game.whoseturn state)))
                                    | (n::ns) => (SOME n, Player.TIE)
                                else (SOME s, valOf (outc_))
                              end
      | findBestMove ((x::xs), ts, state) = 
                               if Game.isOver (Game.makemove state x)
                                then let
                                       val outc_ = Game.outcome 
                                                  (Game.makemove state x)
                                     in if outc_ = SOME Player.TIE then 
                                      findBestMove (xs, x::ts, state)
                                     else (SOME x, valOf (Game.outcome
                                               (Game.makemove state x)))
                                    end
                               else findBestMove (xs, ts, state)

  (*
       val advice : Game.state -> { recommendation : Game.Move.move option
                             ,expectedOutcome : Player.outcome
                             }

    *)

   fun advice state = 
                 let 
                   val move_list = Game.legalmoves state
                   exception ContractViolation
                 in if Game.isOver state then raise ContractViolation
                    else let 
                      val (rec_, outc_) = findBestMove (move_list, [], state)
                      in 
                       {recommendation = rec_, 
                         expectedOutcome = outc_} 
                      end
                end
end