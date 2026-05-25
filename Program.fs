module Program

open System

let readDirection () : Direction option =
    let key = Console.ReadKey(true).Key
    match key with
    | ConsoleKey.W | ConsoleKey.UpArrow    -> Some Up
    | ConsoleKey.S | ConsoleKey.DownArrow  -> Some Down
    | ConsoleKey.A | ConsoleKey.LeftArrow  -> Some Left
    | ConsoleKey.D | ConsoleKey.RightArrow -> Some Right
    | _ -> None

let rec gameLoop (game: Game) =
    game.SpawnTile
    moveLoop game

and moveLoop (game: Game) =
    game.show ()
    match readDirection () with
    | None ->
        moveLoop game
    | Some dir ->
        match game.ApplyMove dir with
        | Win     -> Renderer.renderWin game.Score 
        | Lose    -> Renderer.renderLose game.Score
        | Playing -> gameLoop game
        | Error   -> moveLoop game

let game = Game(Renderer.renderStart)
game.SpawnTile
gameLoop game
