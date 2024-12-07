open System
type Line = char array
type Matrix = Line array

type SearchString = char * char * char * char
type FoundChar = (int * int)
type Finding = FoundChar * FoundChar * FoundChar * FoundChar

type FoundChars = FoundChar array
type Findings = Finding list

let parse (t: string) =
    let lines = t.Split System.Environment.NewLine
    let chars = lines |> Array.map (fun l -> l.ToCharArray()) 

    (chars)

let printmatrix (m: Matrix) = 
    for line in m do
        for c in line do
            printf "%c" c
        printfn ""

let dim m =
    let getSingleElement lst =
        if (lst |> List.length) = 1
        then lst.[0]
        else failwithf "List has more then one item: %A" lst
    let rowCount = m |> Array.length
    let columnCount = 
        if rowCount = 0
        then 0
        else [0..(rowCount-1)] |> List.map (fun ln -> m.[ln] |> Array.length) |> List.distinct |> getSingleElement
    
    (rowCount, columnCount)

let isInDim hayStack pos1 pos2 pos3 pos4 =
    let rowCount, colCount = dim hayStack

    let mutable result = true

    for pos in [pos1; pos2; pos3; pos4] do
        let r, c = pos
        result <- result && (r >= 0 && r < rowCount)
        result <- result && (c >= 0 && c < colCount)

    result

let fromHayStack (hs: Matrix) p1 p2 p3 p4 = 
    let r1, c1 = p1
    let r2, c2 = p2
    let r3, c3 = p3
    let r4, c4 = p4

    Some (hs.[r1].[c1], hs.[r2].[c2], hs.[r3].[c3], hs.[r4].[c4])

    

let getChars (hayStack: Matrix) pos1 pos2 pos3 pos4 =
    let notLeavingHayStack= isInDim hayStack pos1 pos2 pos3 pos4

    // printfn "%A %b" (pos1, pos2, pos3, pos4) notLeavingHayStack

    if notLeavingHayStack
    then fromHayStack hayStack pos1 pos2 pos3 pos4
    else None

let matches (straw: SearchString) (needle: SearchString) =
    let s1, s2, s3, s4 = straw
    let n1, n2, n3, n4 = needle

    ((s1 = n1) && (s2 = n2) && (s3 = n3) && (s4 = n4))

let searchHorizontalForward hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i + 1, j)
    let pos3 = (i + 2, j)
    let pos4 = (i + 3, j)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchHorizontalBackward hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i - 1, j)
    let pos3 = (i - 2, j)
    let pos4 = (i - 3, j)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchVerticalUpward hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i, j - 1)
    let pos3 = (i, j - 2)
    let pos4 = (i, j - 3)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchVerticalDownward hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i, j + 1)
    let pos3 = (i, j + 2)
    let pos4 = (i, j + 3)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchDiagonalForwardUp hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i + 1, j - 1)
    let pos3 = (i + 2, j - 2)
    let pos4 = (i + 3, j - 3)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchDiagonalForwardDown hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i + 1, j + 1)
    let pos3 = (i + 2, j + 2)
    let pos4 = (i + 3, j + 3)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchDiagonalBackwardUp hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i - 1, j - 1)
    let pos3 = (i - 2, j - 2)
    let pos4 = (i - 3, j - 3)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None

let searchDiagonalBackwardDown hayStack needle i j = 
    let pos1 = (i, j)
    let pos2 = (i - 1, j + 1)
    let pos3 = (i - 2, j + 2)
    let pos4 = (i - 3, j + 3)
    
    let straw = getChars hayStack pos1 pos2 pos3 pos4

    match straw with
    | Some s -> if matches s needle then Some (pos1, pos2, pos3, pos4) else None
    | None -> None


let involves (hayStack: Matrix) (needle: SearchString) (dimensions: int * int) (i: int) (j: int) : Findings =
    let mutable findings = []
    
    findings <- (searchHorizontalForward hayStack needle i j) :: findings
    findings <- (searchHorizontalBackward hayStack needle i j) :: findings
    findings <- (searchVerticalDownward hayStack needle i j) :: findings
    findings <- (searchVerticalUpward hayStack needle i j) :: findings
    findings <- (searchDiagonalForwardUp hayStack needle i j) :: findings
    findings <- (searchDiagonalForwardDown hayStack needle i j) :: findings
    findings <- (searchDiagonalBackwardUp hayStack needle i j) :: findings
    findings <- (searchDiagonalBackwardDown hayStack needle i j) :: findings

    findings |> List.choose id

let mapInvolve mapping rows columns =
    let mutable findings = []

    for i in rows do
        for j in columns do
            let localFindings: Findings = (mapping i j |> Seq.toList)
            findings <- findings |> List.append localFindings
            
    findings

let createDotsOnlyMatrix rows columns = 
    let mutable result = []
    for _ in [0..(rows - 1)] do
        result <- [| for _ in [0..(columns - 1)] do yield '.' |] :: result

    result |> List.toArray

let flattenFindings (findings: Findings): FoundChars =
    let mutable result = []

    for finding in findings do
        let a, b, c, d = finding
        result <- result @ [a; b; c; d]

    result |> List.toArray

let combineMatrices a b m =
    let d1, d2 = dim a
    let e1, e2 = dim b

    assert (d1 = e1)
    assert (e1 = e2)

    let r, c = e1, e2

    let mutable result = []

    for i in [0..(r-1)] do
        let mutable line = []
        for j in [0..(c-1)] do
            let pos = i, j
            let isInFindings = m |> Array.contains pos
            let content = 
                if isInFindings
                then a.[i].[j]
                else b.[i].[j]

            line <- line @ [content]
        result <- result @ [ line |> List.toArray ]
    
    result |> List.toArray

let applyFindingsToMap m (f: Findings) =
    let rows, columns = dim m

    let allFindings = (flattenFindings f)

    let dotsOnly = createDotsOnlyMatrix rows columns

    combineMatrices m dotsOnly allFindings

let transpose (m: Matrix) = 
    let search = ( 'X', 'M', 'A', 'S' )
    let rowCount, columnCount = dim m

    let rowIndices = [| 0..(rowCount - 1)|]
    let columnIndices = [| 0..(columnCount-1) |]

    let involvesXmas (i: int) (j: int) = involves m search (rowCount, columnCount) i j

    let involvedXmas = mapInvolve involvesXmas rowIndices columnIndices

    let countXmas = involvedXmas |> List.length

    countXmas, (applyFindingsToMap m involvedXmas)


let example = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

let input = """ASAMXSMXMAXSAMMAMXAXXAXXMSAMXASAMXSASMSMXAXMAMMSSSMAMSMMSASMMSSMSMSMSXMMSXMXMAXXMAASMMMXMXMMXMAMXSSSMSMSMSMMMMAMMSMSXXAXMMXSAXMMXXAMASMXMASM
MMXXAMXAXSMXMMSSMMSSMSMMXSSMXMAASASAAAAMSMSMSSXAMXXAMXAAAAMXASXAAAAAMASASASMSSMMMSXSAMXAMAMSSMASAMAAAAAAXXAAXMAMXSAMAMSSMAMXMMSMMXSXAMAAASAS
SAMMAXMSMXMAXMAMAAMXAAASAMXSXASAMAMMMXMMAXMAAXMXMMXSSSMMSXXMXMASMSMSMXMASAMAAXAAAMMSMMXMXAXAASAMAMMMSMSMSSSMSSXSAMSMMMAAMXXXAAAAAMAMSSSXMXAX
AAMSAMMAMASXSMSMMMSMSMSMASAMMMMSMXMASXSSMSMMMSMSXAMXAASAMMMXSAMAAXAXMXMAMXMXXSXMMMAXASAAMSSSMMXSAMXAAXMAMXMAXAXMXAAXMMSSMMSMMSSSMAMMAAAMMMSM
MMMSAXSMXMXAMAXXAXXAAXXMXMASMAAXXAXMSAAXSAXAAAASMSMMAMMXSAXAXAXMMMMMMXMASXMSMMXSAMMSAMMXMXAMXMAAXXMSMSMSMMMSMMXSMAMMSAAAXAASXXAMASXMMSMAAAMA
XAAMAMXXSMMMMAMSSMMSMSXMXSXMMMXXMMSXMMMMSXSMSMXMAMMXAMMMMMMXMMMMMAXMAAMXSXAAAXAMAXMAMMMSSMSMAMASAMXAAAAAMXAAAXAXAASAXMSXMSSSMMMMAMAMXXXSMSSM
SMSSXSXAAMMAMXXAMAMXXXAMMSAMXMXSMMAXMASXMAMAAAAAXXXSMSXASXMSXSAMSMSMMMSAXMASMMXSXMASXAXAXAMSXSAXMSSMSMSMSMSSSMMMSXMASMAXAXMXMAAMSSSSMSAMAAMX
XAMXAMMSMMSASMMASMMXMSSMAXAMASAAAXMMSASAMAMSMSMSAMXSAAMSMAAAAXXXAXAAMXMASXXMMAMXMSXMSXSMMMMXXMMSAAXXAXXASAAAXMSMMASXMXAAMMXXMMXMAAMAAMAMMMSS
XSSMAMAXAASXXXSAMXMAXAAMSSSMAMXSMMSAMXSXSXXAAXAXMXAMMMSMSMMMSSSSMSSSMMMAMMAMSAMAMXAXSXSXMSAXXMAMMMMMMXMASXSMMXSAMAMSAMXSSSXMSSSMMSMMSMXMAAXA
AXAMSMSMMMSAXMMMMAMMMMSAMXMMXSAXXAMXSAXMXXSMMMMMXMSSMXAAAXXXMAAAAAMAMAMAMMAMMAMXSMMMMAXMASASXXAXXMXSSXMAMMMAMASMMSSMXSXMAMMXAAXMAXMXMXSXMMMS
SSSMXAXXXASMSMAXXAAAAAXAXAMXAMASMSMMXMSMXAMXMAMAAAMAMSMSMSSMMSSMMMMAMSSMSSSSSSMSMXSAMSMXMSAMXSMSSXAAAXMASXSAMASXMMAXASAMMMAMMSMMSMSXMAMXMAAA
MAAXMAMSMMSAAMAMXMMSMXSSMASAMMMMMMASMXAAMSMMMASMSMSAMXXAAMMXAXAAXXMAXXAAMAAXMASXAMSAMAAXXMMMXAAAXMSSSMXSSMSAMMSAMXAMXMAMXXMAAAAMAASXMASASMSM
MSMMMSMSAAMMMSMSASMMXXAXSASAMXMXMSAMAMMSMMASXXSXAAMAMXMMSMAMXSSMMSSXSSMMMMSMSXMMXXSAMXSMMSASXSMMSMXAAAXAXASMMASMMMSMXSSMSSSSSSMMMMMASMSASAXX
XMAMSAAXMMMAMAMXMXAAMMMMMMSAMXXAXMAXMSAAASAMXMMMMMMSXSAXAMASXAASAAXMAXXXXAMXSMXMMMMAMSXMASMSAAAXAMMMMSMMMXMAMAXAXXXAAAXAAAAMAMAXSSSMMXMAMXMA
XSAMXMSMXMSMSASXSMMMMAAAXMSAMXSSSMSMXMASXMAXSAAAMAAXAMXSXSAMMMMMMXSASMSMMMSASMAXAAMAMAXMXSAMXMMSMSMSAXAXSXMXMSSSMMAMMXMMMSMMAMXMAMXAXXMSMSMS
MMMMXSAXAAAASAMXSAMMXSMSMMSAMMXAMAAXXXXMASXSXMASMSSMXMAMXMAMAAAAXAXXAAAXAXMXMSAMXSSSSXMXMMXMASXAXMXMMSMMMXMAMXAAAAXXMXSXMXXMASXMXMSMMXSAAAAA
XAAXXMAMMSMXMSXASAMMXMAAAXSAMXMMMSAMXXXMXXXSXSAMXMAMSMSSMSAXSMSSMMSSMSMSSXMAXMMSXAAAAMAMSAMXAMXMAXAMXAMAXSSMSMSMMMSMMMAXMAMSASXSAMAXAXSMXMSM
SSSSSMSMXAXSAMMMSAMXAMMXSMSXMASXMXMSSMMSASMSXMASMSXMXAMAMMMMMAMAXMAXXAASMAMMSAASMMMMMAXAMASMSSSXSXMAMXSMSAAXSAXXXXXMASMSAAXMASASASMSSMMSAAAX
XAMAAAXASAXMAXXXSXMMMSAAXXMASASMMMMAAMXAASASAMXMASMMMMMXMASAMMSAMMAMSMSMMAMMMMMXAXXAXMSXSAMXAAAAAMXSXMMXAMXXSAMSMMXSXSXMAMSMAMXMASAAMAASXSSM
MAMSMMMAASXMMMMXMMXSSMMXSASXMAMMXXMSSMXMSMMMMXSAMXAMAMSMSASXSXMAMMMMAMAMSSSMMSASXMXMSXAAMMSMMMMMMSAXSXSXXXMMMMMXAAXMMMMMAXMMMSXXXMMMMMMSXMAM
SXMXSMMSMAXMAXSAMXAXAASMSAMXMMMSMSMMXMMMAMXMAMMAXMMMASAAMMSXMASXMXMXASMXAAAAAXMMSAASMXMXMASXSAASXMASMMSAMXXAAXXMXMMAAAASXMXAXMASMXAXXSAMASAM
AASAXMAMXAASMASASMMSSMMXMMMXXMAXAMMAXMAXAMAXMASAMXMSXSMSMAXASAMXMASXMMMMMSMMXMXAMSMXAAXAMXMAXMMMAMMMXASAMSSSSSMSMMASMMMSMMSMXMAAMMMAAMASAMAS
SSMMSMASMMXAXXSAMAAAMASAMASAMXMMMMMSSSSSSSSMSXSASAMSAMXAMXSXMASAMAXMAASAXXAASXMMMMXMSMSSSSMSMSMXAMAAMMSAMXAXAMAAAMMMXSMXAXXXXMSMMSSSMSAMXSXM
XMAMXMASXMAMMXMASMMMSASASAMAAAMMSAMAAXAXMAMASMMASMMMAMMAMAMMSXSAMMSMXMSSXSMMMAAXAXAAAXMMAMAAAAAXSSMSSXSMMMSMMMSSSMAXAAASMMSMSMAMXAAAXMASXMAS
MMAMAMAXAMSXSXXAXAMAMMSMMASMSMAAMAMMMMSMMAMMMAMMMMASAMSSMMSAMAMAMSAXXAXMMSAMSSMMMSMSMSMMAMSMSSMXMAXAMXMASAXAAXMAMMXMXMAMXAAAXSMMMMSMMSAXAXAS
SSMSXSXSSMAAAXMMSAMXSASASXMAAMXMSSMSAAAAMXSXSSMAASMXAMXMASMXSASAMMASMAMXASAMAAMXXXMAMAMMXMXXAMXASMMMSAMAMMSSMSMSMMMAASMSMSMSMSXSAXAXXMASMMAS
XAAAXXAAMAMMMMXXAXXMMASMMAXSXSXAAAASMMMSMSMXMXSSMMMSSMASXMAXMAMASMAMXMMMMMAMSSMMXSSMSMXSXSAMXXSASMAASMSMSAXAAMMMAAASMSAAAAMAAXAAMSASXSAAXMAS
SMMSSMSMMXSSXMMSMMXAMXMXSSMMAMMXMMMMSXAXMAXAXXMMMSMAAXASAMMMMSMMSMSMAMMXSXMXXXXMAMAMXXAAMXMAXAXXSXMXSAAAMMSMXMAMSMMMAMMMSMSMSMSMMMAMAMSSXMAS
XMAMAXMASAAAAMXAAAXSSSMXSXXMAMMMXMSAXMMMSASMSMXAAAMXSMXSAMXAAXAXXAXSASMASAMXMAMMASMMAMMSMAMSMSMXMASAMMMSMXXAXMMXAMAMXMXMAXAMXAMXMXAMXMMMMMAS
MMSSMMXMMMSSMMSMMMSMAMMMMAMSMMAXSAMXSASAXAXXAXMMMMMAMXAXAMSMMSMMMMMSXSMASAMAMAMMAMAMXMMAXAXAAAAXMAMASXAXMXMMMSXAXSMSASASXSMSMXMASXXSXXXAXMXS
MMMAXMAXMAMXMXAXAMMMMAAAMAMAMXMSMXXXMXASMMMXAMXSASAMXMMSMMXXMAXAASMMMSMMXAXSSMSMMSMMSXSASXMMMMMAAMSMMMMSMXAAAXMMXAASXSASAXXSAMXAMXSMMMSMXAMX
SAXXMSSSMMSASXXMMXXXXXXXXASASAMMMSXMXXMMSMAMMAXMASAXXSMSAAMXAMMSMSAAAXMASMMAAAXXAAMMSAMXXMASXMSSSMAXAXMAMSSMMSAMMMXSAMMMMSMXAMSAMSAXAAAMMSMM
SAMSAMAAAASXMAMMSMMSSSXSMMMAXXSAAXAMMMXMAMSASMMMMMXAAMAMMASXAXAMMXMMMXXMASMSMMMSSMSAMSMXMMAMAAAAAAXMMMSAXAMXAXAXMXAMAMXAAAAMAMSAMAMMSMMMAAAX
MAMXAMSMMMSASXMAAAXAAXMSASMSMAMMSSSMAAAMXXXMSAMXSAMXXMAMSAMXMMMSASXSSMSMMXMMAMAMMXMASAMAXMASMMMSMMSMSAXMSMMMSMXMMMXMMMSMSSSMXXMAMMXMXASMSSSM
MAMXAMXAXXSAMAMXSSMMSMASAMAAAXMXMAMXSSMSSSMXSXMSMAMMMSXXMMSAMXMMASAAMMSAXMSSSMAXXAMXSASAXXAMAMXMAASXMASXAAXSXAMMMMSMSASAMXXXXXMAMXAXMAMAXAAA
SASXAXSXMXMASXMAMMAMAAMMXMXMSAMXMAMAMAMAAAMASASXSAAAAAXSAAMMSAAMAMMMMAMAMXAAASXSSXSMSMMMSMAMXMAXMMSAMMAMSSMSMSMAAMAAMASXSXMAMSMMXSMSMSMXMSMM
SAMMSMMAAXSAMMSXXMAMMSMMAXASXXSASAMMSAMMSMMASMMAMSMMXSXMMMAAXMMMSSSMMSSMSMMMMMMAAMAAXXAASXSMSSXSAMSXMXMAXMMMAAMXMSMSMXMAMMMAAAASMSXSAAAAXXXS
SASAMASXSXMAMAAASMMXAAASMXXMAASXSXSASAMXXXMASAMAMMXSMMMXSMMMXSXXMAXMAXAAXMSMSAAMMAMSMSMMSAXAMAXAAMMSMXXXMSAMSMSMXXMAMMMAMXMMSSSMAMAMSMSMMSAX
SAMASAMAMMSSMMMSMAAMSSMAMSMMMMXMSAMXSASMSAMAXAXSMSASAAXAXAXMAXMMMAMMSMMMMXMASXXXXXXAAAMMMXMSMMMSSMAAXSASASMMAMAXMAMXMSSSMXMAAXAMAMMMAMAAXMAM
MAMMMMMSMAAMAASAMMMMAAXMAAAXAMXSMXMASXMAXXMAXSMXAMAXSSMSSMMMMXAAMMSAMAMMMMMAMMSMMMSMSMSXAMXXAMAAAMSSXSAMASXSMSAXMAMSMAAAASMMSSSSSMXMASXMAMMA
SMMAAXXAAMSXSMSASMSSSXMXSXSMSSXMASMASAMSMSMSAAMMSMSMMMAXMASAMSSMSMMASMMAMAMASXAAAAXAMAMXMSASMMMSAMXMAMXMXMAMAMAMMAMAMMSMMMAAAAXAAXSSMSAMASAM
XAXMMMMXXMXAXMMMMMAAMAMXMAAXXMASXMMSSXMAAAAAMMXAAAMAAMMMSAMAXAMMMMMXMXSAXAXMMXXSMXSAMXSAAMAAXXXMASMMXMMMMMAMAMMASMSMSAXAMSMMMSMMMMMAMSAMASAM
SSMSSSSSSXXMXAAXAMMSMMXAMXMXXMAMAXMASAXMSMSMXXMSMSSSMSAAMMSXMASAXAMMXASMSMSAAMXMXMMAXXMMXMXMMSSMASAMMAMXASMSXSMMMXAXMASXMASMXXAAMAMAMXMMASAM
AAAXAXAAMAMXMMMSSMMAMMMXMAXAMMMMMMMAXXMXAMXXXMAXAMAAASXSXMAXSXMMSMSAMXSXAASMMMASASMXMXMASAMXAAAMXSAMSSMMXSAMASXXASAMSXSAMXSMMMSASXMAXAXMAXXM
MMMMMMMMSASAXAMXXAXXSAMMSSSMAAAASMSMSSXMASMMMAAMAXXMXMAXAXAMXMMMSXAMAMXMMMXMAMXSAMXMMAMASXSMMXSMAMAMAMAMASAMXMAMXAMXMASMMASASAMXAAASMSSSSMSS
MSMSAXXXSASASXSMSSMAXASAMMAMSMSMSAAAAAASASMMXMASMMMXMXAMXSXMASAAXXMXSAAAXXASXXAMAMAAXAMAMAMXXAXMMSSMXSAMXSAMXXSMAAXAMAMXMASAXXXMMMMXAAMAAAAA
XAAXASXMMAMAMAAMAMASXMMXXXAMXAXAMMMMMSXMAXASAMAAAXSMMMSSXMASXSMXSAMAMSSSSSMMSMXSXSXMSXSXMSXMMMSAXXAMASASAMAXAAMSXMXMMSAMXAMMMSXXAAAMMMMSMMMS
MMMMAMAMMSMSMSSMXSAMXSMSMSMSMMMMMSMMXXAXAMSMSMSSSMSASAMAMSAMXXAAMAMMXAXXAAMAMMMMAAMASAMXXMASAXXXMSMMASMMASMMMXAAXAMSAMASMSSXAXMAXMSSMXMXXAMX
XAASASXMAXMXXXMAMMMSXMAAAAMAXAAAAAAMSSMMMMXAMXAMXASXMASAXMXSSMMSSMMSMMXXSMMAMAAMSMMASAMSASAMMMXAXXXMASASAMXXXMMXSMMAASAMAAXMSXMSAAMMMAMMMSSM
MSXSASAMXSMSSMMAMMASMMMMAMMAMXSMSSSMAXMAXAMAMMMSMXMMSAMXSSMMAXAMAMAAASMAMXSXSSSXMAMXXAMSAMASASXXMAXSAMXMAMXMXMMAMAASMMMMMMXMAMAMMMXAMXSAAXAX
XMMMAMXMAAXMASXSSMAMAAAXSMMSSMMMMMAMAXMXMMSAMMMXXAAXMXSMXMASAMXMAMXSAAXAMMSXAAXAXXMAXSMMAMAMASAAMSMMMSASMMSSSMMASXMMXXXXAMXMSAMXXSSSMMMMMXAM
MSAMXMAMSMMXAMXMAMXSSMMMXAMAAAMXXSAMSMSAAXSASXMAXMSMMMMXAMXMMSSMASAMMMMASASMMMMSMAMMMXASAMXSXMMMMAAAASXSAAAAAXSASMSMMMSSMSXAXSXMXAAAXMMAXMXM
ASAMSMXXAASMSSMSAMXXMASMXAMSSMMAMAMXXASMSMSAMXMSXMAAXMAXMXMAXAXXMMXSAXSAMXSAMAAAXAMXASAMXMAXAXXSXSMMXSXSMMMSMMMXSAAAAAXAAMMMMMMMMMSMMSMASMSM
AMAMMAMSSSMAMAASMMMMXMASMSMXMAMSSMMAMXMXMAXMXMAXAMSXMMSSMASXMXSAXAXXXXMXXASAMMSSSMXXAMAXAMXMMMMSAXXSAMMXXAMMMMMMMXSSMSSMMMAAAAAAXXAXAAMMSAAA
MMMMSAMXMXMAMXXXMASXASAMXXMMXSMMAXMXSASAMMMMXMAMAMMXMAXAMASAMASMMSSMSSSSMASXMAXMAMSMSSMXMSMMSAMMAMAMAMAMSAMAAAAAMXMAXMAMXSSSMSSMXSASMMSAMXMM
SASMSASXSAXXSXMASXSXMMXXXAXSAMASMMXAXAMMXXAXXMSSSMXAXASMMMMAMASXAMAASAASMXMAMXSMAMXAAXMASAMAXASMXMMSMMSXMASMSSSSXSXSMMMMMMAXAXAMAMXAAMMMSMMM
SMSAMASASMSMMMSAMXMASMSMMMAMASXMMAMSMSMSMXSSXSAAMASXMAAXAMSSMASMMSMMMMMMMMMXMASMSSMSMMSXXAMASXMXAXXXASMAMAMMAMXMASAMXXAAMAAAMSAMXSMXSMAXAAAX
XXMAMXMAMSAAAAMASASMMAAAASXSXMMAMAXXAAAAAAXAMAMXMXXMASMSSMAMMMXAMMXXAMXAAASAMXSAAAAMAAAMSXMASAASMXXSMMMAMASMMMXMAMAMMXSSMMXSXAXSAXXAMMSSMSMS
MSSMMSMMMXXXMSSMSASAMSMXMXAMXXAXSMSMSMSMSMXASXXAXMAXMAXXAMXSXASMMMMMMXSSSSSSSXMMMXXSMMSAAAMXMMMMXSMXMASASXSASAXMMSSMAMMMMSAMXSSXXXMASAXMAMAS
AAAMAMASMSSMMAMAMAMXMXMXSMSMSMMMXMAAXXXAAXSXMMSXSXXMSMMSXMMXMXXMASAASMAAMAMAMMMXSXMAXMAMSSMSXAXSAAAXMASASASAMMXAAAMMXSXAAMASAMAMMXMSMXMMSMAM
SMMMXSMMMAAAXAMMMMMMMAMAXAXAXAMSASMSMSSSMMXAAXSMMMSMAAXMASASMSMSAMSSMAMAMAMAMSMAMXAXXXMAXAAMXAMMXMMXMMMXMXMXMASMMMSAASXMMSSMAMMAXAXXXAMXXXXA
AMAMXAMAMSSMMXMAAAAAXAMMMSMMMSMSAMAXAAXAXXSSMMMAAAAXMSASAMMAAAMMSMXXMSXASASAMAMASASMMMXSSMMASXXMMSMXMXMXAMXXXXAXSAMMXXAXAXXSXMASXMSMSMMMMMMA
MSASXMSAMMAXAMSSSSSSSXSXAAAMXXXMAMAMMSMXXXAAMSSSMSSSSXMMMSSMMMSAMXAXMASXSAMMSXSAXXXAMXAAXAMAMAAXAAAAMASMASXSMMSXMASMSSSMMSXMXSMAAAAAAMAAAAAS
XSASXXSXMSAMXXAMAAAAMMSMMSSMMMMSAMXXAAAASMSAMXAXMXMXMAXMMAAXXXMMMMMMSAMXMMMAAAMMSMSSMMSMSXMASXMMSSSXSAXSAMXMAMMMSAMXAAMAXSAMASXAMSMSMSSXSSSM
XMASMXMAMMASXMASMMMXMAXAXAAAAXAXXXSXSMSMXAAMXMMMSAMASXMSMSSSSSMXAMXAXAXXAAMSMXMAXMAMAXAASMSXSAMAXMAMMXXMXSXXAMAAMASMMMMSMXAMASAMXAAMMAMAAMAM
XMAXMAMSMSXMASXMXXAAMMSMMMSSMASMAMSMXXMAMMMSSXSASASAMAAAAAAAAAAASMMMSMSMSMXMXXAMXMSSSMMSMAMMMAMSMMSMSMMMMMMASMMXMMMMXSXXMSXMAXXSSMXMMASMMMAM
AMASXMXAAAAXMAMSAMXMXXAXAMAAMAXXMMSAMSMMSAAXMAMASMMASMSMMMMMMMMMMAAAXXAAAXXMASMXMMMAMXXAMAMMSMMAMSAAAAAAAAAMXASMSMAAAMASAAAMMXAMXMASMMSAXMAS
XMASASXMSMSSSMXSXMASMSMSSMXXMXSAMAXMAMXAMMSSMXMAMASXMXXXXXXMAASXSSMMSSMSMSSMAXMAMAMAMMSMSXMXASXXSXMSMSMSSSSXSAMAASMMXSAMXMMXXAMXXMASAMMMXSAM
XMMSMMAAAXXAAMXMASMSAAXAXXAMSMSXMAXSSMMMSAXAMMMSMMMMSMMMSMMSSSSMAAAAMAAMAMMMSSXSSXSXSAAMAMXMASXXMAMMAXAAAMXMAMMXMXXAXMMMMXMAMSXMSMAXASAMXMSS
SMXSXSMMMMMSMMASXMAMMMSSMMXSAAXMASMMAAAAMASAMMAMAXXMAAAAXAAAMXMAXSMSSMMMAMAMXAAXAMXMMMMMSASMAMASXSMMAMMMXMMMMMSMXMMMSMAAXAMAMAAXXMAMXXAXXAAS
SXAMMXAASXAXXSAXAMXMAMAXAMXXMXMMSMASMMMSSXSAXMASMMSXMMMXSMMSSSSSMMMAXMAMASASMMMMAMMXMASXMAMXAMXMXAMMSSXMXMASAAAAAAAMXXSMSMSXSSSMMMMMSSSMMXMS
MMMMAMSMMMXSAMXSMMMXXMASXMMMSMAXMXAMXXMAMAMXMMAXAAMMSSSXXASMXAAMAXSMXXMXMSAXAXXXMSMXSASAMAMSXSXAXASAAMAAXXAMMSMSMSSMAMMASMSXAAAMSAAAXAMXAAAX
SXMASMXASAAMAMMMAAXMSXMAXAAAAXMMSMSXSXMASMSXSMSSMMXAAAMAMSMMMMMMMMMXSASAMMMMSSMAMAAXMXXASXMMAXMAMXMMSXSMMMMSXMAXXMAMXSMAMASMMMMMSMSSMAMXSMMM
AXSAXASAXMXMSMASMXMAXAXAXXMSSXSAXAAASMMMSXMMMAXMASMMMMMSMXAMXMXSAMSASAXMMAMXMAXSMMSMMAXMMMAMAMMSSXSSXXMAXAAMASAMASAMXMMXMAMAXXXMXAAXXXSAXAAA
MMMAMXMXMMXXAMASXAASXMMMSMMXMASXMMMXMAAXMAXAMAMMAMXXAMXAASAMAMMSASMAMMMSSMSSSMMXASMMXMASAMAMMSAAAAXASXMSMSMSAMXSAMAAMSSMMXSMMMMMMMMSMXMASMMS
XXMAMASXSAASXMASMXXMAMAAAMMSMMMXSXMXSSMSASXSSSMSASMMSXSMAMMSAXXMXMMXMAAAAAAAAXASMMAXMXAXASXXAMMSMSMMMXAAAAAMXMAMAMAMXAAXSAMXXAAXXSMXMAMXMAMM
SMSMSMSAMXMSAMXSAMMSMXXMXSAAAAXMASXAXAXMXXXXAAXAASAAMAMXMAMSASAMSXXSSMSSMMMSMMMSXMAMMMMMXAXMASAXXMAAAMSMMMSMSMXSSMMXMSSMMASMMSXSAMXMSXMASAMS
AXAMXAMAMXAMAMXMMSAAMAMSMMMSSMMAMXMXSSMSAMMMSMSMMMMAMAMSMMXSAAMAAASAMXAMAMXAMXASAMAAMAMXMAAMXMASMSSMMXMAXAXAXXMAMAMXMAXASAMXAAXMAMAAAXMXXXSA
MXMSMMMAMXMXSMSAAMXMMASAAMAXAMXMXAAXMAAMXXAAAXAXXAXAMAMAAMASAMMXXXAXXMXMMMMXAMXMAMASXXSXMXMASMMSAAXXMASMMSMSMSAXMSMMMASMMASMSSMXAMXMASMAXXXM
XAXAMXXAXAXXXAMMMSSMXXSXSSXSAMSSSMSXSMMMXXXSAAMMSASMSSSMSMAXAXMSSSMXMSSMAASMSMSSMMMMAXSXSAXXAAAMAMSAMXMXAMXXASMSMXAAXXSAMXMXAAXSSSMXAAMMSMAA
SMSASMSMSASMMXMAMSXSAAMAMAAMAMAMAAXMASMSSXMAMAXAXAAAAMMMXMMXSMSAAAXAMAMSMMXAAAAASMMMMMXASMSMSMMMXAMXMAMXSXMMXMASAMSMSMXMMSSMMMMXMAMMSXXAAMMM
AASAMAAXXXMMAXSSXMASMSMSMSMSAMMSMMMAMAMAMMMSSSSMSAMMMSMMMASAAXMMXMXXMAXSMSMSMMMSMSAAMMMMMAXXAMASXXXAMASXMAXSXMSMMMXAMXXAAAAMASXMSAMXMMMSSMSX
MAMAMMMSMAMXMMXMAMAMXAAXMAXMAMXXXAAAMAMXMAAMAAAAXAXMAMAASAMSSMSXSMSMSMSMAXAXSSXMASXMAAMAMXMMASASAMSMSASASMMMAMXAXAMAMASMMSAMXSAAMMMAXMAMAASX
XASAMXAXXXMAMXASXMSSXMSMMMMXSMMSSMSXSMSMXMXSMSMMSMMMASMMMAMXXAXAXAAAAMAMXMXMASAMAMMMSASASAMMAMXSAMXMAXSMMMAMMMSMMMSAMXSXAXAMXSMMMMSASMSXMXMA
SASXSMXXMXSMSMMMMMAAXSAMXXAAMAMXAAMAXMAXAAMSAMXXAXAMAMAASXMMMMMMMSMSMSSSMAAMXMXMXMAAAXSASASAMXASAMASXMMXMXAMAAXXAXXAMMXMMSMMMMMMAMAAMAMASMMM
XAMXMXSAMAMAMAAAAMMSMSASMMMMSAMSMMMAMSASMSMSASMSMMMSSSSMMXMAMAAXMMMMAAAMAXXXSMMMMMMSXMMAMAMXXMASAMXMASAAMMSMMMSMSSSXMMXSAAAASAAXAMMSMXMASAAX
MSMSAASAMMSASXSSXSXMAXAMXAXASAMMASXXAMASXMAMAMAXSAMXAAAMXXSASMMSASXMMMSMSMSXAAMXAMXMAAMXMAMXMMAMMMASXMSXSAMAMMAMMMMAAXAMSSSMSSMSASXAMAMASMMS
XAAMMXMXMXMAXAAMMSMMMMSMMSMMSAMSAMMMAMXMASXMMMSMSSSMMMMSSXMASAASAMASXAXAAAASMMMSMSAXMMMSSXSAMMSSMSXSAMAXMAMAMSMSAAMXMMXXAMAAXAXMXMXSMMMAMAXM
SMXMXMSMMXMSMMMSAXXAAMAAAXAXSXMMXSASXMMSAMASXAAAMXMAMAXAXAMSMMMMAMXMMMSSMSMXMAMAMMMSMAAXMASASAMAAMMSXMASMXXAMAMSMXSASAXXMSXMMSMSXMAXAMMMSMMM
AMASAMAMXMAMXAAMASMSSSSSMMAMMMMMMMXSAAAMASAMMXMXMAXAMXMMSXMASXXXAMXSXXAMXXAASMMASAMSMMSSMAMMMMSSMSXSMSAMAASXMMMXAASAMSMMXMXMAAAAAMASMMAMAAAM
MMSSXSASXMSMSXSMAMMMAAAAXMMAMXAAAMASMMMSAMASAXSASXSMSMAXMASASMSSSMXSMMSSSMSXMXXXMAXXXXMMMSSXAXMASAMMASMMSMSAAXMMSMMXMAMMAMMMSSSSSMXMMSXSXSSS
XSAMXMXSXAAASXMMMSMSSMSMMXSAMSSXSMASASAMXSAMXAXMMXSAAMAMSAMXXAAMAMAXXAAMAAMXMSMSMSSMMMSAAAXMASMXMMAMAMAXMAXMMMMMMAXMSSMSAXSAAAXAXXAAMMMMMMMM
MAAMSMMMMSMXMAXXAAAXAAAXAXXAMAMAXMMSAMXSAMXSSMSXSAMSMMSAXXMMXSXSAMSSXSXSMMMAXAAAAAMAAASMSXXXMXXSMXSMMSMMMSMMXAASMXMASAASASMMSSMMMSSSMAAAAAAM
SSXMAAXMAXXASMMMSMSSMMMXSMSSMAXMMSXMASAMXSMMAXAXMAMMXMXMSMMSAAMSASAMXMMXXMXXXMSMSMSMMMSXMMSSMSAMAAMAXAAMAAAXMSXMASAMXMMMXMAXAMMXAMMAXSSSSSSS
AASXSSMMMSSMMAAAXXMAMXXXMMAMSMAAMXAMAMASMXASMMXMSSMMXMAXXAAXMMMMMMXMASASMMMSAAXXMASMMXMAMAAASMAMXSSMSSSMSSSMXMMSSMSAMXMXMMXMSSMSMMSAMMAMAAXX
MMMXAMASMMAMSSMSMXSAMXXXAMAXAXMMSXSMASAXMAXAMSSMMMASXSMSSMMSMSMSXSASASMMAAAXXMAXSASAXAMAMMMSAMAMXMAXAAMMXAMMMAAMAAXXAASAXXXAXAXXAAMXSMMMXMAM
MSMMXXAMMXAMMMAAAASAMMXSMSXSAXSASAXSMMMSXXSXMASAASMMASAAXMAAXAAAASAMXSMSMMXSAMXMMAMXXMMSXXXMMMMXXMAMMXMXMXMAXMMSMMMSSXSASMMMSMMMMMSASAMMAMXM
MAAMSMSSXSASAMXMMMSAMSMMAAXMASMAMMMXSAXMAMMXMASMMSAMXMMMMMSSSMSMMMXXMMXAAAASMMMMMMMSXMAXMAXXXASXSAXSMXAXSXSASXMSAMAMAAMMMAAAAAAAAMMMSAMSMMAM
SMSMAAAAASXMASAXXAMAXMAMMMMXXAMXMSMAMMXAMXAMMXMAXSMMSMAMSMAMAXXXXAXSXMSSSMXMMAXMAMAXAMASAMMXSXSASAMAXAMXMXMAMAAXSMASMAMASMMMSMMXMXAAXMMAMSAS
MAAXMMMSMMXSMSASMSMMMSSMSSMSXSAMXAMASMSMSXSAAMMSMMMAXSASAMASAMAMMSMMAMAAXMMMSMXSASMSSMASMMMXXXMMMMSXSXMAMMXMSMMMMMMXXMAMMAMAMAAASMXMXAXAXSAM
MSMSXAXXAMXSAMXXXXASXXXAMAASXMAXSASASAAAXAAXAAAAAXMSMMXSASXMMMSXAAASAMMSMAAXAMMSAMMAAMASAASXXMAXAXSXMASMSMAMAXSXMASXMASMXSMAMMMMSAAMSSMSMMAM
XAXMXMSSMMMMMMMMMSXMAMMXMMMMASAMMASMMXMMMMMMSMSSSMMXAMASMMMXSAXMXMMAMMAMXSMSASAMSSMMMMXSXMXXMASXMSMMSXAAAMSSXSAXSAMAMSMSAMMMSMMXXMMMAAXXASAM
XMSMAMAMXAAAAAAAMMMSAMAXSAMXMMMMSAMXAAMSSXSXXAAMAAASAMXSAXXAMASMMSXMSSSMXMASAMXSASASMMXMMSSMSAMXMAMAXMMSMMMAMMMMMASXMAAMXXAAAASMMMSMSXMMAMXM
MSAMAXASXSMSSSSXXAASXXXSAMXSAMXAMASMSMSAMXXMSMMXSMMMAMASXMMXMAMAXMAXAAAXMMMMAMXMASXMAMXMAMAXMXMAXAMMMAXAMAMAMMXMSAMASMSMSSMMSXMAMAAMXASMSSMM
XMAXMSMSAMXXAMXMMSXMXAMXAMAMAMMMXAMXMMMMSMSMXAAXXAMSSMMMAXXAMXSMMMMMMSMMXAAXAMXAMXMSAMAMSSMMMMSXSASXSSSMXAXASXAAAXSXMAAAAXMXXXSXMSSSXMMAAAAM
MXSXAAAXMXMSAMAMMXAMSSSMMMXXAMXSMSSMXAAXAAAMMMMXSAMAMASXMMSMSAMXSAAAXAXMSSSSXSXXMAXMAXAXMAXMAAAMXAAMXAAASXSMSMSSXXMAMXMMMSMXMASXAMAMAXMMMSSM
SAMMSMSMMAMXAMXMAMAMAMAAMSXSXSAAAAAASXSSSSXMAASXSMMXSAMASXMAMXSAMMXMMMXMAAAAASMASXSSMXSMSAMXMSSMMMAAMSMMMAAAXAAMMMSSMSXSMAMXMAMMAMASMMSAAMAA
MASAAAAMMASXSMSMXSAMASXMMSAAASAMXMMMAAMAAAASXMMMSAMAMMSMMAMSMAMMSSMMMXSAMMMMXMAXAAAMAAXXMXSXAAAXMAAMMAMAXSMMMSMMAAAXAAAMMAMXMASAXMASXAMMSSXM
MAMXMSMXXASAXAAMMMASASAXAMAMMMXXMMXMMMMSMMMMSAAASAMXMAAAMAMXMAXMAAAAAAAMAXXXAMSMMSMMMMSAMASMMSXMMSMXMASMMMXXAAMXSSMMSMAMSMSMSASMXMAMMMSMAMMM
SSSSXMXMASMXMSMSASAMASMMAMXMASMAMXAMXXAAXXXAXSMMSXMASXSSSSXXMAXMXMMMMMSXAXXSXSAAXAAAXMMAMAXAXXMMMAMASAMMAXSMSXSAAAXAMMXMAMAAMAXMXMASXMAMAMAX
SAAAAMSAMXAXMXXSXMMSMMMSMSMMASASMSSSMMMSSXMAMMSMXMXMAXAXAMXSMMSMSASXAXAMXMXAXSXXMSSMSSSMMSSSMMAXSASAMASXSMMXMAMMSMMXMSASASMSMASXMSASMSSSSSSS
MMMMMMXMAXMXAAMMMMAAXAAAASASAMXAAXAAXASAMXMMMMASXASMSMSMSMAMAMAASAMXXAMXAAXMMMXXMAMXAAXSAMXMASMMMXMAMAMXAAXAXMMAAXXAXXASXSAXMASMAMXSAMAAAAAA
MMXMASXSSMSMMMSAAMSMSMMSASAMASMMXMSMMSMASMMSASAMMMXAMAMAMMXSAMMXMAMASXXSXSMXAASMMAXMXMSMMMAXXMXAXMSSMAXASMMMSXXSASMMSMMMXMXMMAMMAMMSMMMMMMMM
MASAMSAAASMAAAMMSXXASXAXMMXMMAMXSAAMMMXAMAAMMMAXAXMXMAMAMAMSASXXSAMAMSAMXMASMMMASAMSAMAAMSSSMMXSAAAMSMMMXSASXMAXMMMAAAASXSXAAAXSASAMXXSASAXX
SASMXMMMMASXMSSMMAMAMXXXAMXSMAMAMMXSASMMMMSXMXSMSSSMSXMAMAMSAMXAXXMASXMASMXMSASMMMXSASMMMXMAMAAAMMMMAMXMASXMXMAMAAMMSSMSAMMAMXMXAMXSAASASMSA
MASXXMXAXMMXAAAMXAMMMMSSXMASMXMASMMMMSAXSAMXSAAMAMAAXMSMSMXMMMMMAMMMSMMASXAXSMMAAXAXAMXSAXSAMMMSXMMSMSXMAMASXXXXMMSAAAAMMMMASMSMSMSMXXMAMAXA
MMMMMMMMMXSMMSSMXXXAXAAAAMAXMAMXMAAAASXMMASAMSMMASMMMMAMAMAAAAAAAAAAMAMASMMMMXSSMMMSMSMAAMMMSMAMASXAXAAMXSAMMSMSMAMMSMMMAASXMASAMXXMMMMMMMMM
SMAXAAAXXAXAMAXASXSMSSMMXMASXXSAMSXMMMXXMAMAMAMSMMXAAMASXSSXSMSXMMMMSXMAMXMAXAAMAXXAAAMMSMXAAMMSAMXMMSXMXMMAMXAAMXMAMXXMSASAMSMMMSSMAMASAMAS
MXMXSXSXMXSXMASAMAAAAXXXAXAXAAXMMAXSAMMMMASMMMMAAMSXXXASAXMAMAMXSASXMAMMMSXMSSSSSMSMSMSXAXMXSSXMAMASAMMSMXSMSASMSXMXSMSXXAMMMAAMAAAXMSASASAS
SASAMMXASASAAXMAMSMMMSSSXSMMMMMSMSMXAAAXMASAAMSMSAMASMMMMMMXSAMASASMSAMXASAMXAAAMAXAXXAXXMMSMXXSXMMMASAAAAMASAMASMSMAAMAMXMASXSMMXSMAMASAMAS
SXMASASAMASAMXSXMMAXSAAXMXMASAXAAXASXSSXMMMXMMXMMAMAMAAAAAMAMAMXMAMAXSXMASAMMMMMMSMMMSMSSMSAAXMASXXMAMXMSXMXSAMXMAAXMMMAMASXSXMASXMAMMMMAMSM
XMASMAMAMXMAXASAMSSMMMSMSASXSMSSSMAMAAAMSSSSMXXXSXMAMMSMSXSMSMMMMMMSMMXSASAMAAXAAXAMASXAAXAMSMXXAMXMASXMAXMASMMXMSMSMMSASXXSXASMXASASXSMMMAM
SAMXMMMMMMSSMASAMAAXSXXXSXXAXMAMAMMMMMMMXAAAXXSAMXSSSMMXMXSXMASXMAAAAXMAASAMSXSMMXSMASMSSMXSAAMXMXMMAMXMAAMAMAAXXAXAAASAXXMAXAXMSMSAMXSAASXS
AXSXMAAAAXAXMASMMSAMXAXAMMMSMMSMSMSASMMSMMMMAMMSMXXMAXXASAMAXAMASMMSSMXMASXXMAMASAXMXSAAMAXMMSMSMASMMXXMSXMSSSMXSSSMSMMMAMXMMMAAXXMAMASXMMAM
MMMASMSSSMSSMASXXXXMAXMAMAAAAAAAMASASXASAMAMASAMXXASAMSMMASAMXSMMSXAXAMXMSMSXMSAMXSSSMXMXSAMXMAMMAMAAASXXXXMXAXMAAAAMXXAAMAXAXMSMMSMMASASMAM
XASXMXAMXAMXMSMMMMAMAMSXMMSSMMMSMAMMMMMSASXSMXAMMMMMAMAMMMMMSXAXAMMMMSXXXMAMAMMMMAXMAMAXMMMASMMMSMSXMMSAMXSMSMMSMSMMSSSSXSXSSXAAMXASMXSAMSSS
SASXXMSMMSMXMAAAXMAMAMMASMXMMSAAMAMXXAXXASAXXSMMASXSXSMSXMAAXMAMXMAAAXMMXMXMAMAAMMMXAMSMAAXMMAMAAXMASXMAMAMMMXAMXMAMXAAAAXMAXMSMMSMSXAMXMAMM
SAMXAAAAAXMASXSMSSXSXMSAMXSAASMMMAMMMSMMMMMMXAXSAMAMXSXMASXSMSMSASMSMSAMAMASMSXMAASMMMAASMSXSAMMMXXASASXMASASMMSAMSSMMMMXMASXMAMMMMXMASAMMSM
XAMMMSSMMMMAMMAAXAASAMMSSMMMMXAXSXSAAAXAAAAXXMXMASMMAMAMMMXAAAAMAAAMXSXSASXSXMAXMMMASXMMAMXMSMSMXAMXSAMXSASASAAMAMAAASXXAASMMMAXAAMAMXSXSAMX
SAMXAMAMSSMMSXMAMMMMAMAMMXAXMSSMMSSMSSSSSSSSMXMXAAAMASAMXXSMMMSMSMAMXMASASMXAXAASXSMMAMMAXMMSXAAMSMMMXMAMMSMMMMSMMSSMMSSXMXAMSXSSSMAXASAMXSX
MMXMASXMAAAAAAMAMXASMMXSASMSMAXAXMXXXMAMAXAXMASMMSXXAXAXAAAAAXXAXAMXAMAMMMMSMMSXMASASAMSMMMASMMXMAAXMAMXSASAXXAAAMAMXMXMSXMAXAXMAMXXMASXSAAM
XXXSXMXMSSMMSMSASXXXXXSMMMMMMASXMMMMXMAMXMMMSXSXXMMMSSSMAXXSMSMMMASMMSXSXAAAAAMAMAMMMAMASMMASXSASMSMSAMAMAMMMMSSSMMXAAAXAXSXMSMSAMXSSMMMMAXA
MSMMAAAXMXAAAASMMMSMXMASAAASAASAASAAMSASMASAMAMXAAMMMAMMMSAAXSAAMXMAMMAMMMXXMMSAMASXSXMMMMMMXASXSAXXSASXMSMSAXAXAAMSSSSSMXSXXXAMXMXXMASASMSS
AXASMMMMMSMMMXMMSAXMASMSSSSSMXSMSSMXXAAMXAMASAMSSXMAMSMAAMMMMSMMSSSMMSAMASAXSXMXXAMXSAMXASXAMMMMMAMAMMXAAAASMSMSMMMAAMAAXASMSMAMMMSASAMXAAAX
MSXMXMXAXAAAXMAMASXMAMXMAXAXAXMMAXXSAMMMMMSASMXAMASXXAMMMSASMXASAAAAASAXAXMXMAXASXXMXMXMMMXAXMAMMAMAMASMMMXMAAXAXXMMMMSMMMSAAMAMXXAAMMSAMMMM
MXMAASASMSSMXXMMAMMSSMAMXMMSXMAMXSMMAXAMAAMMXXMAMMMSSMSXASXSAMXMASMMMSAMSSSMSMMMXAASXMSSSMSSSSMMSXSXSMSSMSSMSMSSSMASXAXSMASXMSSSSMMSMXXMXMMM
AXASXMAXXXXMASMMMSAAASMMMAMAXMXMAXXSAMMSMSSMXSSSMSAAMXMMMMXXMSMSXMASAMAXAAAAAXAXMSMMAMAXAAMAAMMAMASAMXSAMAAMAMXAAXAMMSMAXAXSXXAAXXXAASMMASAM
MAXXSMMMMSMMASXAAMMSXMSAAXMAXMAMSSMMASXAXXAMAAAAMMMMSAMASXMSAXAAAAXMXSMMMSMMMSASMMMSAMASMMMMMMMAMXMSAMXMMSMMASMSMMMSAAMMMMMMXXMASMAASMASASAS
XMSAMXAAMAXMXSXMMSAMXASXSXMAXSAMXAASAMXAMSAMXMSMMAAMSASASAAXMMSMMAMSAAAXMAMSAMXAAAASASASAAXXXXSASAAXMSAXMAASASAMAXMMXSSXXMASAXXAMXAMXXXMXSMM
AXMXMSSSSMSMXMASAMXSMMMXAXMAMSASXSMMMSAXMSXXAXMASXSXSMMMSMSMXAMXSXAMSSXMSXSSXXSSMMMSAMXSXMSAAXSASMSMXSASMSXMASMSSMXMAMMAMXASASXMASAXSAMXXMAS"""

let parsed = parse input

let count, transposed  = transpose parsed

printmatrix transposed

printfn "%d" count
