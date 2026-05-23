[<AutoOpen>]
module Game

type Game(difficulty: IDifficulty) =
    let board = Board(4)
    let mutable score = 0
    let targetTile = difficulty.targetTile

    let checkResult () =
        if board.isWin targetTile then Win
        elif board.isLose then Lose
        else Playing

    let rng = System.Random()

    member _.Score = score

    member _.SpawnTile =
        let value = if rng.NextDouble() < 0.9 then 2 else 4
        board.setRandom value

    member _.ApplyMove (dir: Direction) =
        let gain = board.move dir
        if gain = -1 then Error 
        else 
            score <- score + gain
            checkResult ()

    member _.show () =
        Renderer.renderBoard board score
