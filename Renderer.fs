module Renderer

open System

let private clear () = Console.Clear()

let renderBoard (board: Board) (score: int) =
    clear ()
    let tiles = board.getBoard
    let size = board.size
    let maxTile = tiles |> Seq.cast<int> |> Seq.reduce max
    let sep = "+" + String.replicate size "------+"

    printfn "  Score: %-5d  Best Tile: %-5d  " score maxTile 
    printfn ""
    for r in 0..size-1 do
        printfn "%s" sep
        for c in 0..size-1 do
            let v = tiles[r,c]
            if v = 0 then printf "|      "
            else printf "| %4d " v
        printfn "|"
    printfn "%s" sep
    printfn ""
    printfn "  W/↑  S/↓  A/←  D/→"

let renderWin (score: int) =
    printfn "  *** YOU WIN! ***"
    printfn "  Final Score: %d" score

let renderLose (score: int) =
    printfn "  *** GAME OVER ***"
    printfn "  Final Score: %d" score

let renderStart: IDifficulty =
    let rec loop () =
        clear ()
        printfn "  === 2048 ==="
        printfn ""
        printfn "  Select Difficulty:"
        printfn "  1. Easy   (target: 256)"
        printfn "  2. Medium (target: 1024)"
        printfn "  3. Hard   (target: 2048)"
        printfn ""
        printf "  Enter 1, 2, or 3: "
        match System.Console.ReadLine() with
        | "1" -> selectDifficulty Easy
        | "2" -> selectDifficulty Medium
        | "3" -> selectDifficulty Hard
        | _   -> loop ()
    loop ()
