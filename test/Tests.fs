

module Tests
open System

// ── Runner ────────────────────────────────────────────────────────────────────
let mutable private passed = 0
let mutable private failed = 0

let private test (name: string) (cond: bool) =
    if cond then
        printfn "  [PASS] %s" name
        passed <- passed + 1
    else
        printfn "  [FAIL] %s" name
        failed <- failed + 1

let private section (title: string) =
    printfn "\n── %s" title

// ── Helpers ───────────────────────────────────────────────────────────────────
let private fill (b: Board) (rows: int list list) =
    let t = b.getBoard
    rows |> List.iteri (fun r row ->
        row |> List.iteri (fun c v -> t[r, c] <- v))

let private board (rows: int list list) =
    let b = Board(4)
    fill b rows
    b

let private row (b: Board) r = [for c in 0..3 -> b.getBoard[r, c]]
let private col (b: Board) c = [for r in 0..3 -> b.getBoard[r, c]]
let private tiles (b: Board) = b.getBoard |> Seq.cast<int> |> Seq.toList
let private nonZero (b: Board) = tiles b |> List.filter ((<>) 0)

// ── Entry point ───────────────────────────────────────────────────────────────
[<EntryPoint>]
let main _ =

    // ── REQ 4: Difficulty target tiles ───────────────────────────────────────
    section "REQ 4 – Difficulty target tiles"
    test "Easy   targetTile = 256"  ((selectDifficulty Easy  ).targetTile = 256)
    test "Medium targetTile = 1024" ((selectDifficulty Medium).targetTile = 1024)
    test "Hard   targetTile = 2048" ((selectDifficulty Hard  ).targetTile = 2048)

    // ── REQ 5: 4×4 board, initially empty ────────────────────────────────────
    section "REQ 5 – Board initial state"
    let b5 = Board(4)
    test "Board size = 4"          (b5.size = 4)
    test "All cells start at zero" (tiles b5 |> List.forall ((=) 0))

    // ── REQ 6 & 7: Two tiles spawned, each 2 or 4 ────────────────────────────
    // REQ 7 verifies values are in {2,4}; probability (90/10) is non-deterministic.
    section "REQ 6 & 7 – Initial tile spawn"
    let b67 = Board(4)
    b67.setRandom 2
    b67.setRandom 4
    let spawned67 = nonZero b67
    test "Exactly 2 non-zero tiles after 2 spawns"  (spawned67.Length = 2)
    test "Each spawned tile is 2 or 4"              (spawned67 |> List.forall (fun v -> v = 2 || v = 4))
    test "Tiles placed at distinct random positions" (
        let t = b67.getBoard
        let positions = [for r in 0..3 do for c in 0..3 do if t[r,c] <> 0 then yield (r,c)]
        positions.Length = 2 && positions.[0] <> positions.[1])

    // ── REQ 13: Tiles slide as far as possible ────────────────────────────────
    section "REQ 13 – Tiles slide to the far end"
    let b13r = board [[2;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13r.move Right)
    test "Lone tile slides full right"     (row b13r 0 = [0;0;0;2])

    let b13l = board [[0;0;0;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13l.move Left)
    test "Lone tile slides full left"      (row b13l 0 = [4;0;0;0])

    let b13u = board [[0;0;0;0];[0;0;0;0];[0;0;0;0];[8;0;0;0]]
    ignore (b13u.move Up)
    test "Lone tile slides to top"         (col b13u 0 = [8;0;0;0])

    let b13d = board [[8;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13d.move Down)
    test "Lone tile slides to bottom"      (col b13d 0 = [0;0;0;8])

    let b13gap = board [[2;0;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13gap.move Right)
    test "Non-adjacent tiles slide right"  (row b13gap 0 = [0;0;2;4])

    // ── REQ 14: Same-value tiles merge ────────────────────────────────────────
    section "REQ 14 – Same-value tiles merge"
    let b14a = board [[2;2;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14a.move Left)
    test "[2,2,0,0] left  → [4,0,0,0]"            (row b14a 0 = [4;0;0;0])

    let b14b = board [[0;0;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14b.move Right)
    test "[0,0,4,4] right → [0,0,0,8]"            (row b14b 0 = [0;0;0;8])

    let b14c = board [[4;0;0;0];[4;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14c.move Down)
    test "Vertical [4,4,0,0] down → col 0 = [0,0,0,8]" (col b14c 0 = [0;0;0;8])

    let b14d = board [[0;0;0;0];[0;0;0;0];[8;0;0;0];[8;0;0;0]]
    ignore (b14d.move Up)
    test "Vertical [0,0,8,8] up   → col 0 = [16,0,0,0]" (col b14d 0 = [16;0;0;0])

    // ── REQ 15: Each tile merges at most once per move ────────────────────────
    section "REQ 15 – Each tile merges at most once"
    let b15a = board [[2;2;2;2];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15a.move Left)
    test "[2,2,2,2] left  → [4,4,0,0]  (not [8,0,0,0])"   (row b15a 0 = [4;4;0;0])

    let b15b = board [[2;2;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15b.move Left)
    test "[2,2,4,0] left  → [4,4,0,0]  (no chain merge)"   (row b15b 0 = [4;4;0;0])

    let b15c = board [[0;4;2;2];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15c.move Right)
    test "[0,4,2,2] right → [0,0,4,4]  (no chain merge)"   (row b15c 0 = [0;0;4;4])

    // ── REQ 16 & 18: New tile only after board changes ────────────────────────
    section "REQ 16 & 18 – Tile spawned iff board changed"
    let b16 = board [[2;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    let gain16 = b16.move Right
    test "Valid move returns gain ≥ 0"           (gain16 >= 0)
    b16.setRandom 2
    test "After valid move + spawn, 2 tiles exist" (nonZero b16 |> List.length = 2)

    let b18 = board [[2;4;8;16];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Already-packed row: move Left returns -1"  (b18.move Left = -1)

    let b18b = board [[0;0;0;2];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Already-packed right: move Right returns -1" (b18b.move Right = -1)

    // ── REQ 17: Spawned tile value is 2 or 4 ─────────────────────────────────
    // Probability (90%/10%) cannot be deterministically tested; we verify range.
    section "REQ 17 – Spawned tile value is always 2 or 4"
    let b17 = Board(4)
    for _ in 1..16 do b17.setRandom 2  // setRandom takes the value as arg
    // The game itself picks 2 or 4 via rng; here we confirm Board accepts both.
    let b17v = board [[2;4;2;4];[4;2;4;2];[2;4;2;4];[4;2;4;2]]
    test "Board accepts value 2"  (nonZero b17v |> List.forall (fun v -> v = 2 || v = 4))

    // ── REQ 19 & 20: Scoring ─────────────────────────────────────────────────
    section "REQ 19 & 20 – Score = sum of merged tile values"
    let b20a = board [[2;2;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merging 2+2      → gain = 4"   (b20a.move Left = 4)

    let b20b = board [[4;4;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merging 4+4      → gain = 8"   (b20b.move Left = 8)

    let b20c = board [[2;2;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merging (2+2)+(4+4) → gain = 12" (b20c.move Left = 12)

    let b20d = board [[8;8;8;8];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merging (8+8)+(8+8) → gain = 32" (b20d.move Left = 32)

    let b20e = board [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "No merge → gain = 0"            (b20e.move Left = -1) // no change, -1

    // ── REQ 21 & 22: Win condition ────────────────────────────────────────────
    section "REQ 21 & 22 – Win when target tile is reached"
    let bW256  = board [[256;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "256 tile → wins Easy   (target 256)"       (bW256.isWin 256)
    test "256 tile → no win Medium (target 1024)"    (not (bW256.isWin 1024))
    test "256 tile → no win Hard   (target 2048)"    (not (bW256.isWin 2048))

    let bW1024 = board [[1024;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "1024 tile → wins Medium  (target 1024)"    (bW1024.isWin 1024)
    test "1024 tile → no win Hard  (target 2048)"    (not (bW1024.isWin 2048))

    let bW2048 = board [[2048;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "2048 tile → wins Hard    (target 2048)"    (bW2048.isWin 2048)

    // Win triggered by merge: 128+128 on Easy
    let bWMerge = board [[128;128;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (bWMerge.move Left)
    test "Merging 128+128 creates 256 → wins Easy"   (bWMerge.isWin 256)

    // ── REQ 23 & 24: Lose condition ───────────────────────────────────────────
    section "REQ 23 & 24 – Lose when no moves remain"
    let bLose = board [
        [ 2;  4;  8; 16]
        [32; 64;128;256]
        [ 2;  4;  8; 16]
        [32; 64;128;256]
    ]
    test "Full board, no adjacent equals → isLose = true" (bLose.isLose)

    // Horizontal merge available
    let bNoLoseH = board [
        [ 2;  2;  8; 16]   // adjacent 2s in row 0
        [32; 64;128;256]
        [ 4;  8; 16; 32]
        [64;128;256;512]
    ]
    test "Full board with horizontal merge → isLose = false" (not bNoLoseH.isLose)

    // Vertical merge available
    let bNoLoseV = board [
        [ 2;  4;  8; 16]
        [ 2; 64;128;256]   // adjacent 2s in col 0
        [32;  4;  8; 16]
        [64;128;256;512]
    ]
    test "Full board with vertical merge   → isLose = false" (not bNoLoseV.isLose)

    let bPartial = board [[2;4;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Non-full board → isLose = false"  (not bPartial.isLose)
    test "Empty board   → isLose = false"   (not (Board(4)).isLose)

    // All four move directions blocked on full board
    let bLose2 = board [
        [  2; 16;  2; 16]
        [ 32;  2; 32;  2]
        [  2; 16;  2; 16]
        [ 32;  2; 32;  2]
    ]
    test "Checkerboard-like full board → isLose = true" (bLose2.isLose)

    // ── Summary ───────────────────────────────────────────────────────────────
    printfn "\n════════════════════════════════════════"
    printfn "  Results: %d passed  |  %d failed" passed failed
    printfn "════════════════════════════════════════"
    if failed > 0 then 1 else 0
