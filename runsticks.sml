structure Sticks14 = SticksFun(val N = 14)
structure SAgs = AgsFun(structure Game = Sticks14)
structure P = PlayFun(structure Ags = SAgs)

structure InteractiveGames = struct
  val computero = P.getamove [Player.O] (* computer plays O *)
  fun play player = P.play computero (Sticks14.initial player)
  exception Args
  fun gameArgs [] = Player.X
    | gameArgs ["x"] = Player.X
    | gameArgs ["X"] = Player.X
    | gameArgs ["o"] = Player.O
    | gameArgs ["O"] = Player.O
    | gameArgs _ = raise Args

  fun run play () = 
    let val prog = CommandLine.name()
        val args = CommandLine.arguments()
    in
        (play (gameArgs args); OS.Process.success)
        handle Args =>
            (TextIO.output(TextIO.stdErr, "Usage: " ^ prog ^ " firstplayer\n"); 
             OS.Process.failure)
    end

  val _ = run play ()
end
