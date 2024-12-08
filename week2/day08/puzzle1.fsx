type Point = int * int
type Frequency = char

type MeaningOfMapPoint =
    | Free
    | Antenna of Frequency
    | Antinode of Frequency
    | AntennaAndAntinode of (Frequency * Frequency)

type MapLayout = Map<int, Map<int, MeaningOfMapPoint>>

type MapDimension = int * int

type AntennaPositions = Map<Frequency, Point list>

type MapInstance = MapLayout * MapDimension * AntennaPositions

let printMap (map: MapInstance) =
    let layout, dim, _ = map
    let maxX, maxY = dim

    for i in [0..(maxX - 1)] do
        for j in [0..maxY - 1] do
            let point = layout[i][j]
            let char = match point with
                        | Free -> '.'
                        | Antenna a -> a
                        | Antinode _ -> '#'
                        | AntennaAndAntinode (a, _) -> a

            printf "%c" char
        printfn ""

let distance (p1: Point) (p2: Point) = 
    let i1, j1 = p1
    let i2, j2 = p2

    (i2 - i1, j2 - j1)

let addPoint p offset = 
    let i, j = p
    let a, b = offset

    (i + a, j + b)

let staysInsideMap dimensions node = 
    let a, b = node

    let maxA, maxB = dimensions

    let aOutOfBounds = a < 0 || a >= maxA
    let bOutOfBounds = b < 0 || b >= maxB

    let inBounds = not(aOutOfBounds) && not(bOutOfBounds)

    inBounds

let findAntinodeForAntenna dimensions current next = 
    let difference = distance current next
    let antinode = addPoint next difference

    if staysInsideMap dimensions antinode
    then Some antinode
    else None

let findAntinodes (map: MapInstance) = 
    let layout, dim, antennas = map
    let mutable newMapLayout = layout
    let mutable antinodes = Map.empty<Frequency, Point list>
    for frequency in antennas.Keys do
        let antennasForFrequency = antennas[frequency]
        antinodes <- antinodes |> Map.add frequency []
        let findAntiNodesForAntenna index antenna = 
            let antennasLeft = (antennasForFrequency |> Seq.except [antennasForFrequency.[index]]) 
            
            let antinodesForCurrentAntenna ant = findAntinodeForAntenna dim antenna ant

            let antennaNodesLeft = antennasLeft |> Seq.map antinodesForCurrentAntenna |> Seq.choose id |> Seq.toList

            antinodes <- antinodes |> Map.change frequency (fun q ->
                match q with 
                | Some s -> Some (s @ antennaNodesLeft)
                | None -> None
            )

        antennasForFrequency |> Seq.iteri findAntiNodesForAntenna 
        let currentAntinodes = antinodes[frequency]
        for antiNode in currentAntinodes do
            let i, j = antiNode
            let mapPoint = layout[i][j]
            newMapLayout <- newMapLayout |> Map.change i (
                fun line -> 
                match line with
                | Some line -> 
                    line 
                    |> Map.change j (fun entry ->
                        match entry with
                        | None -> None
                        | Some s -> 
                            match mapPoint with
                                | Free -> Antinode (frequency)
                                | Antinode u -> Antinode (u)
                                | Antenna v -> AntennaAndAntinode (v, frequency)
                                | _ -> s
                            |> Some
                    )  
                    |> Some 
                | None -> None
                )
         
    (newMapLayout, dim, antennas), antinodes

let parse (t: string) =
    let mutable map = Map.empty<int, Map<int, MeaningOfMapPoint>>
    let mutable maxY = 0
    let lines = t.Split System.Environment.NewLine
    let maxX = lines |> Array.length
    let mutable antennas = Map.empty<Frequency, Point list>

    let charToWayPoint c = 
        match c with
        | '.' -> Free
        | _ -> Antenna c

    for i in [0..lines.Length - 1] do
        let line = lines.[i]
        maxY <- max maxY line.Length
        let mutable lineMap = Map.empty<int, MeaningOfMapPoint>
        for j in [0..(line.Length - 1)] do
            let char = line.[j]
            let wp = charToWayPoint char

            lineMap <- lineMap |> Map.add j wp

            match wp with
            | Antenna f -> 
                if antennas.ContainsKey (f)
                then antennas <- antennas |> Map.change f (
                    fun v -> 
                        match v with
                        | Some v -> Some (v @ [(i, j)])
                        | None -> None
                )
                else antennas <- antennas |> Map.add f [(i, j)]
            | _ -> ()
        map <- map |> Map.add i lineMap

    (map, (maxX, maxY), antennas)


let example = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

let input = """................................y.................
............9.....Q................y..............
..................................................
..................................................
..........Q.......................x..N..1.........
.....9......6.e......................x.......j....
............e..x6Q9...............................
...........6..................................j...
...e....................................j.........
.............Q8.......................x..1........
.....w.......8...................y................
..n.......................y...................s...
.n................................................
.........n...............e........................
8..C..............r.....F......................j..
.......C......................................1..s
................n.u..................W...t........
......w..........r.........W..5J..................
.....p..............................J.............
.....T.................................d..........
......prw................uW.....Z.....t..6........
....p.r.....f............7........................
........C.f...q..................3.Y..............
.......w.f..........M.....C..5.......t............
....S..f.q.........................5..............
.............p......J........c................Z...
......................5...........................
....T...........u........D.....8........R.........
..0....T.............7M..........J....RZ..........
......t.Iu......................P.......W..Z......
...............D.......M.....i.......z..........s.
...........F..DM..q............R..................
........T....0............................c....s..
.E........0..............N........................
.......................................1........2X
..........Y....0.q....F..................X........
...............F.I..........................X.....
....U......z..............7i.........S..c.........
E.D..S...............................4.....2......
..S.........z..I......i.........m.............2...
.......E............I.....i..................R....
..................N...............................
....................................m.............
...Y...............P.............m...2............
................N...z................c............
.......................................4..........
........U.........P...............7..d..........4.
........................X....3....d...............
Y................P.U..........3...........d.......
...U..................................3..........."""

let map = parse input

let mapWithAntinodes, antinodes = findAntinodes map

printMap mapWithAntinodes

let getAntinodesCount (nodes: AntennaPositions) = 
    let mutable setOfNodes = []
    let addAntiNodesToSet (c: Point seq) = 
        setOfNodes <- setOfNodes @ (c |> Seq.toList)

    nodes.Values |> Seq.iter addAntiNodesToSet

    setOfNodes |> List.distinct |> List.length


printfn "%i" (getAntinodesCount antinodes)
