type Stone = uint64

type Stones = Stone list

let parse (t: string) = 
    let stones = t.Split ' ' |> Array.filter (fun t -> t.Length > 0) |> Array.map uint64 |> Array.toList

    stones

let playStone stone =
    if stone <> 0UL // rule 1
    then 
        let stoneString = sprintf "%i" stone
        if (stoneString.Length % 2 = 0) // rule 2
        then
            let left = stoneString.Substring (0, (stoneString.Length / 2)) |> uint64
            let right = stoneString.Substring (stoneString.Length / 2) |> uint64
            [left; right]
        else
            [ stone * 2024UL ] // rule 3
    else [1UL]

let rec play stones rounds =
    printfn "Playing round. %i rounds left." rounds
    if rounds > 0
    then 
        let mutable newStones = []
        for stone in stones do
            let played = playStone stone
            newStones <- newStones @ played

        play newStones (rounds - 1)
    else stones

let rec playStoneRecursive stone round =
    let mutable sum = 0
    if (round > 0)
    then 
        let stones = playStone stone
        for nextStone in stones do
            sum <- sum + playStoneRecursive nextStone (round - 1)
        sum
    else
        1

let playStonesIterative stones rounds =
    let mutable sum = 0
    for stone in stones do
        sum <- sum + playStoneRecursive stone rounds
    sum

let example = "125 17"
let input = "572556 22 0 528 4679021 1 10725 2790"

let stones  = parse input

//let stonesAfterPlay = playStones stones 25

//let numberOfStones = stonesAfterPlay |> List.length

let numberOfStones = playStonesIterative stones 25

printfn "%i" numberOfStones
