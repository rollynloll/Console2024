[<AutoOpen>]
module Difficulty

type IDifficulty =
    abstract member diff: DiffEnum
    abstract member targetTile: int

type EasyDifficulty() =
    interface IDifficulty with
        member _.diff = Easy
        member _.targetTile = 256

type MediumDifficulty() =
    interface IDifficulty with
        member _.diff = Medium
        member _.targetTile = 1024

type HardDifficulty() =
    interface IDifficulty with
        member _.diff = Hard
        member _.targetTile = 2048

let selectDifficulty (input: DiffEnum) : IDifficulty =
    match input with
    | Easy   -> EasyDifficulty()   :> IDifficulty
    | Medium -> MediumDifficulty() :> IDifficulty
    | Hard   -> HardDifficulty()   :> IDifficulty
