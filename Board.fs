[<AutoOpen>]
module Board

let private rng = System.Random()

type Board(size: int) =
    // let bindings must all come before member definitions
    let mutable tiles: Tiles = Array2D.zeroCreate size size

    let hasEmpty () =
        tiles |> Seq.cast<int> |> Seq.exists (fun x -> x = 0)

    let array2DEqual (a: 'T[,]) (b: 'T[,]) =
        Array2D.length1 a = Array2D.length1 b &&
        Array2D.length2 a = Array2D.length2 b &&
        seq {
            for r in 0..Array2D.length1 a - 1 do
                for c in 0..Array2D.length2 a - 1 do
                    yield a[r,c] = b[r,c]
        } |> Seq.forall id

    let mergeBoard (board: Tiles) (isTranspose: bool) (isReverse: bool) : int =
        let mutable gains = 0
        let pad (length: int) (input: int list) =
            input @ List.replicate (length - List.length input) 0
        let rec mergeOp (input: int list) : int list =
            match input with
            | A::B::tl ->
                if A = B then
                    gains <- gains + 2*A
                    2*A :: mergeOp tl
                else A :: mergeOp (B::tl)
            | input -> input
        let mergeLine (input: int list) : int list =
            input |> List.filter (fun x -> x > 0) |> mergeOp |> pad (Array2D.length1 board)
        for i in 0..Array2D.length1 board - 1 do
            if isTranspose then
                let line  = Array.toList board[*, i]
                let line  = if isReverse then List.rev line else line
                let line' = mergeLine line
                let line' = if isReverse then List.rev line' else line'
                board[*, i] <- List.toArray line'
            else
                let line  = Array.toList board[i, *]
                let line  = if isReverse then List.rev line else line
                let line' = mergeLine line
                let line' = if isReverse then List.rev line' else line'
                board[i, *] <- List.toArray line'
        gains

    let moveBoard (direction: Direction) (board: Tiles) =
        match direction with
        | Up    -> mergeBoard board true false
        | Down  -> mergeBoard board true true
        | Left  -> mergeBoard board false false
        | Right -> mergeBoard board false true

    let rec getZeros i j acc =
        if j = size then acc
        elif i = size then getZeros 0 (j+1) acc
        else getZeros (i+1) j (if tiles[i,j] = 0 then (i,j)::acc else acc)

    // member definitions come after all let bindings
    member _.size: int = size
    member _.getBoard = tiles

    member _.isWin (targetTile: int) : bool =
        (tiles |> Seq.cast<int> |> Seq.reduce max) >= targetTile

    member _.isLose: bool =
        if hasEmpty () then false
        else
            [Up; Down; Left; Right] |> List.forall (fun dir ->
                let before = Array2D.copy tiles
                ignore (moveBoard dir (Array2D.copy tiles))
                array2DEqual (Array2D.copy tiles) before
            )

    member _.move (direction: Direction) =
        let before = Array2D.copy tiles
        let gains = moveBoard direction tiles
        if array2DEqual tiles before then -1 else gains

    member _.setRandom (value: int) =
        if hasEmpty () then
            let zeros = getZeros 0 0 []
            let i, j = List.item (rng.Next(List.length zeros)) zeros
            tiles[i,j] <- value
