module Test

let mergeBoard (board: int[,]) (isTranspose: bool) (isReverse: bool): unit = 
    let rec mergeLine (input: int list): int list = 
        let pad (length: int) (input: int list)=
            let left = length - (List.length input)
            input @ (List.replicate left 0)

        let rec mergeOp (input: int list): int list = 
            match input with
            | A::B::tl -> 
                if A=B then 2*A :: mergeOp tl
                else A :: mergeOp (B::tl)
            | input -> input

        input
        |> List.filter (fun x -> x>0)
        |> mergeOp
        |> pad 4

    for i in 0..3 do
        if isTranspose then
            let line  = if isReverse then List.rev (Array.toList board[*, i]) else Array.toList board[*,i]
            let line' = if isReverse then List.rev (mergeLine line) else mergeLine line
            board[*, i] <- List.toArray line'
        else
            let line  = if isReverse then List.rev (Array.toList board[i,*]) else Array.toList board[i,*]
            let line' = if isReverse then List.rev (mergeLine line) else mergeLine line
            board[i, *] <- List.toArray line'

let data = [| [|2;2;2;0|]; [|2;2;4;1|]; [|2;2;4;4|]; [|2;2;4;4|] |]
let grid = Array2D.init 4 4 (fun r c -> data[r][c])
mergeBoard grid false true
printfn "%A" grid
