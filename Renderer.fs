module Renderer

open System

let private clear () = Console.Clear()

let renderBoard (board: Board) (score: int) =
    clear ()
    let tiles = board.getBoard
    let size = board.size
    let maxTile = tiles |> Seq.cast<int> |> Seq.reduce max
    let sep = "+" + String.replicate size "------+"

    printfn "  Score: %-10d  Best Tile: %d" score maxTile
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
    clear ()
    printfn "  *** YOU WIN! ***"
    printfn "  Final Score: %d" score

let renderLose (score: int) =
    clear ()
    printfn "  *** GAME OVER ***"
    printfn "  Final Score: %d" score
