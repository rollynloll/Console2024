module Tests

open System

// ── Mode & tracking ───────────────────────────────────────────────────────────
let mutable private brief       = false
let mutable private totalPassed = 0
let mutable private totalFailed = 0
let mutable private secPassed   = 0
let mutable private secFailed   = 0
let mutable private secActive   = false

let private flushSection () =
    if secActive && brief then
        printfn "%d / %d" secPassed (secPassed + secFailed)
    secPassed <- 0
    secFailed <- 0
    secActive <- false

let private section (title: string) =
    flushSection ()
    secActive <- true
    if brief then printf "  %-52s" title
    else          printfn "\n── %s" title

let private test (name: string) (cond: bool) =
    if cond then
        totalPassed <- totalPassed + 1
        secPassed   <- secPassed + 1
        if not brief then printfn "  [PASS] %s" name
    else
        totalFailed <- totalFailed + 1
        secFailed   <- secFailed + 1
        if not brief then printfn "  [FAIL] %s" name

// ── Helpers ───────────────────────────────────────────────────────────────────
let private board (rows: int list list) =
    let b = Board(4)
    let t = b.getBoard
    rows |> List.iteri (fun r row ->
        row |> List.iteri (fun c v -> t[r, c] <- v))
    b

let private row  (b: Board) r = [for c in 0..3 -> b.getBoard[r, c]]
let private col  (b: Board) c = [for r in 0..3 -> b.getBoard[r, c]]
let private tiles  (b: Board) = b.getBoard |> Seq.cast<int> |> Seq.toList
let private nonZero (b: Board) = tiles b |> List.filter ((<>) 0)

// ── Entry point ───────────────────────────────────────────────────────────────
[<EntryPoint>]
let main argv =
    brief <- argv |> Array.exists (fun a -> a = "--brief" || a = "-b")
    if not brief then
        printfn "2048 Test Engine  (--brief / -b for compact output)"

    // ── REQ 4: Difficulty target tiles ───────────────────────────────────────
    section "REQ 4  – Difficulty target tiles"
    test "Easy   targetTile = 256"  ((selectDifficulty Easy  ).targetTile = 256)
    test "Medium targetTile = 1024" ((selectDifficulty Medium).targetTile = 1024)
    test "Hard   targetTile = 2048" ((selectDifficulty Hard  ).targetTile = 2048)

    // ── REQ 5: 4×4 board, initially empty ────────────────────────────────────
    section "REQ 5  – Board initial state"
    let b5 = Board(4)
    test "Board size = 4"           (b5.size = 4)
    test "All 16 cells start at 0"  (tiles b5 |> List.forall ((=) 0))
    test "getBoard is 4×4 array"    (Array2D.length1 b5.getBoard = 4 &&
                                     Array2D.length2 b5.getBoard = 4)

    // ── REQ 6 & 7: Two tiles spawned, each 2 or 4 ────────────────────────────
    section "REQ 6&7 – Initial tile spawn"
    let b67 = Board(4)
    b67.setRandom 2
    b67.setRandom 4
    let sp = nonZero b67
    test "Exactly 2 non-zero tiles after 2 spawns"  (sp.Length = 2)
    test "Spawned tile values are in {2, 4}"         (sp |> List.forall (fun v -> v = 2 || v = 4))
    test "Two tiles land at distinct positions"      (
        let t   = b67.getBoard
        let pos = [for r in 0..3 do for c in 0..3 do if t[r,c] <> 0 then yield (r,c)]
        pos.Length = 2 && pos.[0] <> pos.[1])
    // setRandom on a full board must be a no-op
    let bFull = board [[2;4;8;16];[32;64;128;256];[2;4;8;16];[32;64;128;256]]
    let snap  = tiles bFull
    bFull.setRandom 2
    test "setRandom on full board does nothing"      (tiles bFull = snap)

    // ── REQ 13: Tiles slide as far as possible ────────────────────────────────
    section "REQ 13 – Tiles slide to the far end"
    let b13r = board [[2;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13r.move Right)
    test "Lone tile slides full right"                        (row b13r 0 = [0;0;0;2])

    let b13l = board [[0;0;0;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13l.move Left)
    test "Lone tile slides full left"                         (row b13l 0 = [4;0;0;0])

    let b13u = board [[0;0;0;0];[0;0;0;0];[0;0;0;0];[8;0;0;0]]
    ignore (b13u.move Up)
    test "Lone tile slides to top"                            (col b13u 0 = [8;0;0;0])

    let b13d = board [[8;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13d.move Down)
    test "Lone tile slides to bottom"                         (col b13d 0 = [0;0;0;8])

    // Gap between non-equal tiles: both compact, no merge
    let b13g1 = board [[2;0;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13g1.move Right)
    test "[2,0,4,0] right → [0,0,2,4]  (no merge)"           (row b13g1 0 = [0;0;2;4])

    let b13g2 = board [[0;2;0;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13g2.move Left)
    test "[0,2,0,4] left  → [2,4,0,0]  (no merge)"           (row b13g2 0 = [2;4;0;0])

    // Three tiles compact
    let b13tri = board [[2;4;8;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b13tri.move Right)
    test "[2,4,8,0] right → [0,2,4,8]  (three tiles)"        (row b13tri 0 = [0;2;4;8])

    // Already at destination: no board change → -1
    let b13nop = board [[0;0;2;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "[0,0,2,4] right already packed → -1"                (b13nop.move Right = -1)

    // ── REQ 14: Same-value tiles merge ────────────────────────────────────────
    section "REQ 14 – Same-value tiles merge"
    let b14a = board [[2;2;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14a.move Left)
    test "[2,2,0,0] left  → [4,0,0,0]"                (row b14a 0 = [4;0;0;0])

    let b14b = board [[0;0;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14b.move Right)
    test "[0,0,4,4] right → [0,0,0,8]"                (row b14b 0 = [0;0;0;8])

    // Merge across a gap
    let b14g1 = board [[2;0;2;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14g1.move Left)
    test "[2,0,2,0] left  → [4,0,0,0]  (merge across gap)"   (row b14g1 0 = [4;0;0;0])

    let b14g2 = board [[4;0;0;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14g2.move Right)
    test "[4,0,0,4] right → [0,0,0,8]  (far gap)"            (row b14g2 0 = [0;0;0;8])

    // Gap merge + trailing tile: [2,0,2,4] left → [4,4,0,0]
    let b14m = board [[2;0;2;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14m.move Left)
    test "[2,0,2,4] left  → [4,4,0,0]  (gap merge + slide)"  (row b14m 0 = [4;4;0;0])

    // Two merges in one row: [2,2,4,4] left → [4,8,0,0]
    let b14t = board [[2;2;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14t.move Left)
    test "[2,2,4,4] left  → [4,8,0,0]  (two merges)"         (row b14t 0 = [4;8;0;0])

    // Vertical merges
    let b14vd = board [[4;0;0;0];[4;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b14vd.move Down)
    test "Vertical [4,4,0,0] down → col 0 = [0,0,0,8]"       (col b14vd 0 = [0;0;0;8])

    let b14vu = board [[0;0;0;0];[0;0;0;0];[8;0;0;0];[8;0;0;0]]
    ignore (b14vu.move Up)
    test "Vertical [0,0,8,8] up   → col 0 = [16,0,0,0]"      (col b14vu 0 = [16;0;0;0])

    // ── REQ 15: Each tile merges at most once per move ────────────────────────
    section "REQ 15 – Each tile merges at most once"
    let b15a = board [[2;2;2;2];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15a.move Left)
    test "[2,2,2,2] left  → [4,4,0,0]     (not [8,0,0,0])"   (row b15a 0 = [4;4;0;0])

    // Merge result must NOT re-merge with the next tile
    let b15b = board [[2;2;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15b.move Left)
    test "[2,2,4,0] left  → [4,4,0,0]     (no chain)"         (row b15b 0 = [4;4;0;0])

    let b15c = board [[0;4;2;2];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15c.move Right)
    test "[0,4,2,2] right → [0,0,4,4]     (no chain)"         (row b15c 0 = [0;0;4;4])

    // Three of the same: only the leading pair merges
    let b15d = board [[4;4;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15d.move Left)
    test "[4,4,4,0] left  → [8,4,0,0]     (first pair only)"  (row b15d 0 = [8;4;0;0])

    let b15e = board [[0;4;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15e.move Right)
    test "[0,4,4,4] right → [0,0,4,8]     (last pair only)"   (row b15e 0 = [0;0;4;8])

    // Two independent pairs, each merges exactly once
    let b15f = board [[8;8;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (b15f.move Left)
    test "[8,8,4,4] left  → [16,8,0,0]    (two pairs)"        (row b15f 0 = [16;8;0;0])

    // Vertical: four same in a column
    let b15v = board [[2;0;0;0];[2;0;0;0];[2;0;0;0];[2;0;0;0]]
    ignore (b15v.move Up)
    test "Vertical [2,2,2,2] up  → col 0 = [4,4,0,0]"         (col b15v 0 = [4;4;0;0])

    // ── REQ 16 & 18: New tile only after board changes ────────────────────────
    section "REQ 16&18 – Tile spawned iff board changed"
    // Merge move: gain ≥ 0
    let b16a = board [[2;2;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merge move returns gain ≥ 0"                   (b16a.move Left >= 0)
    b16a.setRandom 2
    test "After merge-move + spawn, tile count = 2"      (nonZero b16a |> List.length = 2)

    // Slide-only (no merge) is still a valid move: returns 0, not -1
    let b16b = board [[2;0;0;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "[2,0,0,4] left → gain = 0 (slide, board changed)"   (b16b.move Left = 0)

    // Packed row: move in same direction → -1
    let b18a = board [[2;4;8;16];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Packed row: move Left   → -1"                  (b18a.move Left = -1)

    let b18b = board [[0;0;0;2];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Packed right: move Right → -1"                 (b18b.move Right = -1)

    // Bottom row packed: move Down → -1
    let b18c = board [[0;0;0;0];[0;0;0;0];[0;0;0;0];[2;4;8;16]]
    test "Bottom row: move Down    → -1"                 (b18c.move Down = -1)

    // setRandom places exactly one tile on empty board
    let b16sp = Board(4)
    b16sp.setRandom 2
    test "setRandom on empty board adds exactly 1 tile"  (nonZero b16sp |> List.length = 1)

    // ── REQ 17: Spawned tile value is 2 or 4 ─────────────────────────────────
    section "REQ 17 – Spawned tile is 2 or 4"
    let b17 = Board(4)
    b17.setRandom 2
    b17.setRandom 4
    test "setRandom places value 2"  (nonZero b17 |> List.exists ((=) 2))
    test "setRandom places value 4"  (nonZero b17 |> List.exists ((=) 4))

    // ── REQ 19 & 20: Scoring ─────────────────────────────────────────────────
    section "REQ 19&20 – Score = sum of merged tile values"
    let b20a = board [[2;2;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merge 2+2        → gain = 4"           (b20a.move Left = 4)

    let b20b = board [[4;4;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merge 4+4        → gain = 8"           (b20b.move Left = 8)

    let b20c = board [[16;16;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merge 16+16      → gain = 32"          (b20c.move Left = 32)

    // Two pairs in one row
    let b20d = board [[2;2;4;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merge (2+2)+(4+4) → gain = 12"         (b20d.move Left = 12)

    let b20e = board [[8;8;8;8];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Merge (8+8)+(8+8)  → gain = 32"        (b20e.move Left = 32)

    // Slide only: board changes, no merge → gain = 0
    let b20f = board [[2;0;0;4];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Slide only (no merge) → gain = 0"      (b20f.move Left = 0)

    // Multiple rows merge at once
    let b20g = board [[2;2;0;0];[4;4;0;0];[0;0;0;0];[0;0;0;0]]
    test "Multi-row: (2+2)+(4+4) → gain = 12"   (b20g.move Left = 12)

    // Vertical column merge
    let b20h = board [[8;0;0;0];[8;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "Column merge Up 8+8  → gain = 16"      (b20h.move Up = 16)

    // ── REQ 21 & 22: Win condition ────────────────────────────────────────────
    section "REQ 21&22 – Win when target tile is reached"
    let bW256  = board [[256;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "256 → wins Easy   (= 256)"             (bW256.isWin 256)
    test "256 → no win Medium  (256 < 1024)"     (not (bW256.isWin 1024))
    test "256 → no win Hard    (256 < 2048)"     (not (bW256.isWin 2048))

    let bW1024 = board [[1024;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "1024 → wins Medium (= 1024)"           (bW1024.isWin 1024)
    test "1024 → no win Hard   (1024 < 2048)"   (not (bW1024.isWin 2048))

    let bW2048 = board [[2048;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "2048 → wins Hard   (= 2048)"           (bW2048.isWin 2048)

    // isWin uses >=: larger tile also triggers win
    let b512 = board [[512;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    test "512 → wins Easy  (512 >= 256)"         (b512.isWin 256)
    test "512 → no win Medium (512 < 1024)"      (not (b512.isWin 1024))

    // 2048 satisfies all three thresholds
    test "2048 → wins Easy, Medium, Hard"        (
        bW2048.isWin 256 && bW2048.isWin 1024 && bW2048.isWin 2048)

    // Win triggered by a merge
    let bMW = board [[128;128;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]
    ignore (bMW.move Left)
    test "Merging 128+128 → 256 → wins Easy"     (bMW.isWin 256)

    // Empty board: nothing to win with
    test "Empty board → isWin 256 = false"       (not (Board(4).isWin 256))

    // ── REQ 23 & 24: Lose condition ───────────────────────────────────────────
    section "REQ 23&24 – Lose when no moves remain"

    let bL1 = board [[ 2; 4; 8;16];[32;64;128;256];[ 2; 4; 8;16];[32;64;128;256]]
    test "Full board, no adj equals → isLose = true"           (bL1.isLose)

    let bL2 = board [[ 2;16; 2;16];[32; 2;32; 2];[ 2;16; 2;16];[32; 2;32; 2]]
    test "Alternating pattern → isLose = true"                 (bL2.isLose)

    // One horizontal pair available
    let bNH = board [[ 2; 2; 8;16];[32;64;128;256];[ 4; 8;16;32];[64;128;256;512]]
    test "One horizontal pair → isLose = false"                (not bNH.isLose)

    // One vertical pair available (col 0, rows 0-1)
    let bNV = board [[ 2; 4; 8;16];[ 2;64;128;256];[32; 8;16;32];[64;128;256;512]]
    test "One vertical pair  → isLose = false"                 (not bNV.isLose)

    // Single pair in corner (col 3, rows 2-3)
    let bNC = board [[ 2; 4; 8;16];[32;64;128;256];[ 4; 8;16;32];[64;128;256;32]]
    test "Pair only in corner → isLose = false"                (not bNC.isLose)

    // Single pair in last two cells of last row
    let bNR = board [[ 2; 4; 8;16];[32;64;128;256];[ 4; 8;16;32];[64;128; 2; 2]]
    test "Pair only in last row  → isLose = false"             (not bNR.isLose)

    // All same value: countless merges
    let bSame = board [[2;2;2;2];[2;2;2;2];[2;2;2;2];[2;2;2;2]]
    test "Full board all same → isLose = false"                (not bSame.isLose)

    // Non-full: one empty cell
    let bOE = board [[ 2; 4; 8;16];[32;64;128;256];[ 2; 4; 8;16];[32;64;128; 0]]
    test "One empty cell → isLose = false"                     (not bOE.isLose)

    // Partial board
    test "Partial board → isLose = false"                      (
        not (board [[2;4;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]).isLose)

    // Completely empty
    test "Empty board → isLose = false"                        (not (Board(4).isLose))

    // ── Summary ───────────────────────────────────────────────────────────────
    flushSection ()
    printfn ""
    printfn "════════════════════════════════════════"
    printfn "  Results: %d passed  |  %d failed" totalPassed totalFailed
    printfn "════════════════════════════════════════"
    if totalFailed > 0 then 1 else 0
