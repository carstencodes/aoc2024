type Plant = char

type Point = int * int

type Area = Point list

type PlantAreaAndPerimeters = 
    Plant * Area * int * int

type MapLayout = Map<int, Map<int, Plant>>

type MapInstance = MapLayout * (int * int)

type Garden = MapInstance * Map<Plant, Point list>

type AreasByPlant = Map<Plant, Area list>

// type AreasAndPerimetersByPlant = Map<Plant, Area list>

type Direction = 
    | Top
    | Bottom
    | Left
    | Right

let surroundSlope slope = 
    let getNeighbors (point: Point): (Point * Direction) list =
        let i, j = point
        seq {
            yield ((i - 1, j), Left)
            yield ((i + 1, j), Right)
            yield ((i, j - 1), Top)
            yield ((i, j + 1), Bottom)
        } |> Seq.toList
    let isNotInSlope r =
        let p, _ = r
        
        not (slope |> List.contains p)

    let selectPoint r =
        let p, _ = r
        p

    let result = slope |> List.map getNeighbors |> List.concat |> List.filter isNotInSlope |> List.distinct

    result |> List.map selectPoint


let perimeterForArea area =
    surroundSlope area |> List.length

let surround (areasByPlant: AreasByPlant): PlantAreaAndPerimeters list = 
    let mutable areasAndPerimeters = []

    for plant in areasByPlant.Keys do
        for area in areasByPlant[plant] do
            let perimeter, size =
                perimeterForArea area, area |> List.length
            
            areasAndPerimeters <- areasAndPerimeters @ [plant, area, size, perimeter]
    areasAndPerimeters

let calculatePrice (areasAndPerimetersByPlant: PlantAreaAndPerimeters list) = 
    let priceOfSinglePlantArea p =
        let _, _, size, perimeter = p
        size * perimeter

    areasAndPerimetersByPlant |> List.map priceOfSinglePlantArea |> List.sum

let getNeighbors point =
    let i, j = point
    seq {
        yield ((i-1), j)
        yield ((i+1), j)
        yield (i, (j-1))
        yield (i, (j+1))
    }

let buildAdjacenceLists points =
    let isPoint p =
        points |> List.contains p
    let mutable result = Map.empty<Point, Point list>
    for point in points do
        let neighbors = getNeighbors point |> Seq.filter isPoint |> Seq.toList
        result <- result |> Map.add point neighbors

    result

let rec getAreas (adjacenceLists: Map<Point, Point list>): Area list =
    let rec traverseAdjacenceList point allPoints =
        let mutable points = allPoints |> Set.add point
        
        for nextPoint in adjacenceLists[point] do
            if (not (Set.contains nextPoint points))
            then 
                points <- points |> Set.union (traverseAdjacenceList nextPoint points)

        points

    let mutable areas = []

    let mutable pointsToVisit = adjacenceLists.Keys |> Seq.toList

    while not(pointsToVisit.IsEmpty) do
        let point = pointsToVisit.Head

        let pointsInGraph = traverseAdjacenceList point Set.empty<Point>

        pointsToVisit <- pointsToVisit |> List.except (pointsInGraph)

        areas <- areas @ [pointsInGraph |> Set.toList]

    areas
    
let divide (garden: Garden): AreasByPlant = 
    let _, plantList = garden
    let mutable result: AreasByPlant = Map.empty<Plant, Area list>

    for plant in plantList.Keys do
        let mutable areas = []
        let points = plantList[plant]
        let adjacenceLists = buildAdjacenceLists points

        let areasForPlant = getAreas adjacenceLists
        result <- result |> Map.add plant areasForPlant

    result

let parse (t: string): Garden =
    let mutable mapResult = Map.empty<int, Map<int, Plant>>
    let mutable plantPoints = Map.empty<Plant, Point List>

    let mutable i = 0
    for line in t.Split System.Environment.NewLine do
        let mutable j = 0
        let mutable mapLine = Map.empty<int, Plant>
        for plant in line.ToCharArray() do
            if (not (plantPoints.ContainsKey(plant)))
            then plantPoints <- plantPoints |> Map.add plant []

            let p = (i, j)
            plantPoints <- plantPoints |> Map.change plant (fun item -> 
                match item with
                | Some t -> Some (t @ [p])
                | None -> None
            )

            mapLine <- mapLine |> Map.add j plant
            j <- j + 1

        mapResult <- mapResult |> Map.add i mapLine
        i <- i + 1

    let dimI = mapResult.Keys |> Seq.length
    let dimJ = mapResult.Values |> Seq.map (fun f -> f.Keys.Count) |> Seq.distinct |> Seq.exactlyOne
        
    (mapResult, (dimI, dimJ)), plantPoints

let printAreas (garden: Garden) (areasByPlant: AreasByPlant) = 
    let mapInstance, _ = garden
    let map, dimensions = mapInstance

    let dimI, dimJ = dimensions

    if areasByPlant.IsEmpty
    then
        for i in [0..(dimI - 1)] do
            for j in [0..(dimJ - 1)] do
                printf "%c" (map[i][j])

            printfn ""
    else 
        printfn "~~~~~~~~~~~~~~~~~~~~~~"
        for plant in areasByPlant.Keys do
            for i in [0..(dimI - 1)] do
                for j in [0..(dimJ - 1)] do
                    let item = map[i][j]
                    if item = plant
                    then 
                        printf "%c" plant
                    else 
                        printf " "

                printfn ""
            printfn "~~~~~~~~~~~~~~~~~~~~~~"

let printGarden (g: Garden) =
    printAreas g Map.empty<Plant, Area list>

let example = """AAAA
BBCD
BBCC
EEEC"""

let secondExample = """OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"""

let largerExample = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

let input = """AAAAARRRRRRRRRRROOOOOOOOOOOOWWWWWWWWWWWWWMMMMXXQQQQQQQWWWXXXXXXXXQQQQQQQQQQQQQQQQQQQDDDDDDBBBBBBBBBBBBBBBBBRRRRRRRRRRREEEEEEEEEEEEEJJJJJJJJJ
AAAAARRRRRRRROOROOOOOOOOOOWWWWWWWWWWWWWWWWWMMMMQQQQQQQQWXXXXXXXXXQQQQXQQQQQQQQQQQQQQDDDDDDBBBBBBBBBBBBBBBBBRRRRRRRREEEEEEEEEEEEEZEJJJJJJJJJJ
OAAAARRRRRRRRROOOOOOOOOOOWWWWWWWWWWWWWWWWWWMMMMQQQQJQQQWXXXXXXXXXXXXXXXQQQQQQQQQQQQQDDDDDBBBBBBBBBBBBBBBBBBRZRRRRRRRREEEEEEEEEEEJJJJJJJJJJJJ
AAAAARRRRRRRRYHOOOOOOOOOOWWWWWWWWWWWWWWWWWWMMMMMMQQQQQQXXXXXXXXXXXXXXXXQQQQQQQQQQQQQDDDDDBBBBBBBBBBBBBBBBBBZZRRRRRRREEEEEEEEEEEEJJJJJJJJJJJJ
AAAARRRRRRRYYYOOOOOOOOOOOOWWWWWWWWWWWWWWWWLMMMMMQQQQQQQQXXXXXXXXXXXXXXQQQQQQQQQQQQQDDDDDDDDBBBBBBBBBBBBBBBZZZZRRRRREEEEEEEEEEEEEJJJJJJJJJJJJ
AAAARRRRRRYYYVOOOOOOOOOOWWWWWWWWWWWWWWWWWWWWGGQQQQQQQQQQQXXXXCCXXXXXIIQQQQQQQQQQQYYEEDDDDDDBBUBBBBNBBBBBBBZZZZRRRREEEEEEEEEEEEEEEEJJJJJJJJJJ
AAAARRRRRRYYYOOVOOOOOOOWWWWWWWWWWWWWWWWWWWWWGVQQQQQQQQQQQXXCXCCXIIIIIQQQQQQQQQQQYYEEDDYDDDHBBBBBHHBBBUBBBJZZZRRRRRREEEEEEEEEEEEEEEJJJJJJJJJJ
AAARRRRRYYYYYYYVPOOOOOOOOOOWYYWWWWWWWWWWWWWWGVVQQQQQQQQQHHHCCCCCIIIIIQQQQQQQQQQYYYYYYYYDHHHHHHHHHBBBBBBBYJZZZZZRRREEEEEEEEEGGEEEDDDJJJJJJJJJ
RAAARRRYYYYYYYPPPOPOOOOOOOOOYYYYWYWPWWWWSVVVVVVVQQQQQQQQHHHHHHNIIIIIIQQQIQQQQQJJJYYYYYYDHHHHHHHHBBBBBBBBBJJJJZRRRRRREEEEEEEGGEEEKKDKJJJJJJJJ
RRRRRMRYYYYYYUPPPPPOOOOOOOOXXXYYYYYPPWWWPPVVVVVVVVQQQHHHHHHHNHNIIIIIIIIIIQQJJJJJJYYYYYYYYHHYHHHHHBBBBBBJBJJJJZRRREEEEEEEEEEGGEEEKKKKJJJJJJJJ
RRRMMMYYYYYYYPPPPAPOOOOOOHXXXXYXYYYPPWPPPPVVVVVVVQHHHHCHHHHNNNNNNIIIIIIVJJJJJJJJJYYYYYYYYYYYYHHHHHHJJJJJJJJJJRRRRREXEEEEEEGGGGGGKKKKJJJJJJEG
RRRMMMMYYYYYYPPPPOOOOOOOOXXXXXXXXXPPPPPPPPPVVVVVVVVVHHHHHHBNNNNNNNIIIIIVJJJJJJJJJYYYYYYYYYYAYAAHHAAAJJJJJJJJJEERRREEEEGGGGGGGGGGRRKKKJJJEJEE
RRRMMMMMMYYYMMMPPOOOOOOBBXXXXXXNPPPPPPPPPPPVVVVVVDPPPHHHHHNNNNNNNNIIIIIVJJJJJJJJJYYYYYYYYYYAYAAHHAAJJJJJJJJEJXEERRRREGGGGGGGGGGGRRKKGGEJEEEE
RRMMMMMMYYMMMMMPMMOOOOWWXXXXXXXXKKKKKPGPPPPPPPPPPPPPPPPHHHENENNNNNIIIIIJJJJJJJJJJYYYYYYYYYAAAAAAAAAJJJJJJJEEEEEERRRRRRTGGGGGGGGGGRGGGEEEEEEE
RRMMMMMMMMMMMMMMMMMWWWWXXXXXXXXXKKKKKGGPPPPPPPPPPPPPPPPPHHEEEEENNNIIIIIJJJJJJJJJJJYYYYYYYYAAAAAAAAAJJJJJJEEEEEEEEERRRRTGZGGGGGGGGGGGGGEEEEEE
RRRMMMMMMMMMMMMMMMWWWWWXXXXXXGGGGKGUUUGGGYPPPPPPPPPPPPPPPPEEEEEEEEIIIIIJJJJJJJJJJJYYYYYYYYYAAAAAAAAJJJJJJJJEEEEEEERRRRTGGGGGGGGGGGGGGGEEEEEE
RRRMMMMMMMMMMMMMWWWWWWWWGGGGXGGGKKGGGGGYYYYPPPPPPPPPPPPPPPPPCCEEIIIIIJOJJJJJJJJJJJYYYYYYYYYAAAAAAJJJJJJJJJJEEEEEERRRRRTTGGGGGGGGGGGGGGEEEEEE
RRRRRMMMMMMMMMMMMMWWWWWWGGGGGGGKKKGGGGGYYYYPPPPPMPPPPPPPCPPCCXXXXXXJJJJJJJJJJJJJJJYYYYYYYYYAAAAAAJJJJJJJJJJJEEEEETTRRTTTTGGGGGGGGGGGGGEEEEEE
RRRRRMTMMMMMMLWWWWWWWWWWWWGGGGKKGGGGGGGGYPPPPPPPMPPPPPPPCCCCCCXXXXXXXXJJJJJJJJJJJQYYYYYYYYYAAAAAAAJJJJJJJJJJEEEETTTRTTTTTTTGGGGGGGGGGEEEEEEE
RRRYYMTTMMMMLLWWWWWWWWWWWWWAGGGGGGGGGGGGYGPPPPPPMMPPPPPPCCCCCCCXXXXXXXXJJJJJJJJJJJJYYYYYYYAAAAAAJAJJJJJQQQEEEETTTTTTTTTTTIIIGGNGGGGGGEEEEEEE
RRRYYMTTMMMMLLLLLWWWWWWWWWWAAAAGGGGGGGGGGGGPPPMMMAAAAPPCCCCCCXXXXXXXXXXXXXJJJJJJJYYYYYLLLLLAAAAAJJJJJJJJQQEEQETTTTTTTTTIIIIIINNGNGGGPEEEEEEE
YYYYYYJTMMJMLLLLLLLWWWWWWWAAAAAAAGGGGGGGGGLMMMMMMAMAAPCCCDCCCCCDDDXXXXXXXXXJXXXJJYYYYYLLLLLLLANNNJNNJJJQQQQQQTTTTTTTTIIIIIIIINNNNNGGEEEEEEEE
YYYYYYJTJJJLLLLLLLLWWWWWWZAAAAAAAGGGGGGGGLLLLMMMMMMAAPCCCDDCCCDDDDDDXXXXXXXXXXXWHHYHYLLLLLLLLLLLNNNNNJJQQQQQQTTTTTTCIIIIIIIIINNNNNNNEEEEEERE
YYYYYYJJJJJLJJLLLLWWWWWWWAAAAAAAAAGGGGGGGLLMMMMMMMMCCCCCDDDDDDDDDDDMDXXXXXXXXXXXHHHHLLLLLLLLLLLLNNNNNNNQQQQQQQTCCCCCIGIIIIIIICCCCNEEEEEERRRR
YYKKYYJJJJJJJJLLLLLWWWWJWAAAAAAAAGGGGGGGGLLMMMMMMMMXXXXDDDFDDDDDDDDDDXXXXXXXXXXHHHHHLLLLLSSLLLLLLLNNNNNQQQQQQQQQQCCCCCIIIIIIICCCCNNNEEEERRRR
YYKKKYJJJJJLLLLLLLLLWWWLQAAAAAAZZZZZZZZGLLMMMMMMMMMMXXXDDDDDDDDDDDDDXXXXXXXXXXXHHHHHLSSSSSSSSLLLLNNNNNQQQQQQQQQQQCCCCCIIIIIIIICCCNNNNERRCRRR
YYKKKJJJJJJLLLLLLLLLWWLLLAEAAAAZZZZZZZZMMMMMMMMMMMMMMMXDDDDDDDDDDDDDXXXXXZXXXXXTHHHLLLSSSSSSSSSSLLLLQQQQQQQQQQQQXXXCCCIIIIIIIICCCNNNNNRRRRRR
YYKKKJJJJJJJLLLLLLLLLLLLLEEAAAAZZZZZZZZZTMTMMMMMMMMXXXXXDDDDDDDDDDDZXXXEZZVXXXOTTHHHLLSSSSSSSSSLLLLLLQQQQQQQQQOQXXXXXCCCCCICCCCCCCNRRRRRRRRR
KKKKKJJJJJJJLLLLLLLLLLLLEEEEEAAZZZZZZZZZTTTTMMMMMMMMTTXXXDDDDDDDDDDZXXEEZZXXSSTTTTSHLLSSSSSSSSSSLLUULQQEEQEEQQQXXXXXCCCCCCCCCCCCCCNRRRRRRRRR
KKKKKKKJKJJJJLLLLLLLNMLLEEEECZZZZZZZZZZZTTTMMMMMTTTMTTXXXDDDDDDDDDZZZZZZZSSSSSSSTSSSSSSSSSSSSSLLLLUUUUUUEEEEEEEXXXXXXCCCCCCCCCCCCRRRRXRRRRRR
KKKKKKKKKJKKKKLLLLLLNNNNEEECCZZZZZZZZZZZTTTTTTTTTTTTTTTXXDDDDDDDDZZZZZZZZSSSSSSSSSSSMMSSISSSSLLKKKUUUUUUUUEEEXXXPPPPPCCCCCCCCCCZRRRRRRRRRRRR
KKKKKKKKKKKKKKLLLLLLLNNNNNNCCCZZZZZZZZZZZZZZZTTTTTTTTTGXXXDDDVVZZZZZZZZSSSSSSSSSSSSSSMSSZZZZZZZKKUUUUUUUUEEEEXXXPPPPPCCCCZCZZZZZDDDRRRRRRRRR
KKKKKKKKKKKKKKLLLLLNNNNNNNNNCCZZZZZZZZZZZZZZZXTTTGTTGGGGGXDDDVVZZZZZZZZSSSSZSSSSSSSSMMSMMZZZZZZKKDUUUUUUUPPPPPPPPPPPPCCCCZZZZZZTZZDRRRRRRRRR
KKMMMMMMMMMMKKKLLLNNNNNNNNNNCCZJJJZZZZZZZZZZZXXXXGGGGGGGGDDDDVVVZZZZZZZZZZZZZSSSSSSSMMMMMZZZZZZKUUUUUUUUEPPPPPPPPPPPPCCZZZZZZZZZZZDZRRRRRRRR
KKMMMMMMMMMMMMMMLNNNNNNNNNNNNCZJJJJJRRZZZZZZZXXXXFFGGGGGGGVVVVFVVFYZZZZZZZZZZSSSSSSSSSZZZZZZZZKKUUUUUUUUEPPPPPPPPPPPPXCCCZZZZZZZZPZZRRRYYRRR
KKMMMMMMMMMMMMMMNNNNNNNNNNNNNNNNNJJJJRZZZZZZZXXXXXGGGGGGGGVVVVFFFFZZZZZZZZZZZZSSSSSSSSKZZZZZZZZKKYUUUUPPPPPPPPPPPPPPPUKRKZZZZZZZZZZIZRMRRRRR
KKMMMMMMMMMMMMMMNNNNNNNNNNNNNNNNNJJJRRZZZZZZZXXXXGGGGGGGGGGVVVFFFFZZZZZZZZZZZSSSSSSSSSSZZZZZZZZYYYYYUUPPPPPPPPPUPPPPPUKKKZZZZZZZZZZZZQQORRRR
KMMMMMMMMMMMMMMMNNNNNNNNNNNNNNNNNNJJJRZZZZZZZXXXXGGGGGGGGGVVVVVFFFZZZZZZZZZHHHSSSSSSTSZZZZZZZZZZYYYUUEPPPPPPPPPUPPPPPKKKKKKZZZZZZZZQMQQORRRR
UMMMMMMMMMMMMMMMNLNNCNNNNNNNNNNNJJJVVRZZZZZZZZZZZZZGGGGGGVVVVVVFFTTJZZZZZZHHHHSSRRSTTZZZZZZZZZZZYYYYYEPPPPPPPPPUUUKKKKKKKKKZZZZZKKPPPPPPPPRR
UMMMMMMMMMMMMMMMLLLLCNNNNNNNNNNNRRRRRRZZZZZZZZZZZZZGGGGGGGGVVVVVVVTZZZZZZZZHHHSHRRRZZJZZZZZZZSZYYYYYYYPPPPPPPPPKUKKKKKKKKKKNNNNOOOPPPPPPPPRR
UMMMMMMMMMMMMMMMLLCCCCCCNNNNNNPPPRRRRRZZZZZZZZZZZZZGGGGGGGGGVVFVVVTTZZZZTTTEEHHHHZZZZZZZZZZZZSYYYYYYYYYPPPBBBUUKKKKKKKKKKKNNNNNNOOPPPPPPPPRL
UMMMMMMMMMMMMMMMMLCCCCCGCCCNNPPPPPPPPRRRRVRVZZZZZZZGGGGGGGGGGGFFFFFTTZZTTEEEEEEHSSZZZZZZZZZZZZZYYYYYYYBPPPBBBKKKKKKKKKKKKKNOONNOOOPPPPPPPPQQ
UMMMMMMMMMMMMMMMMMCCCCCCCCCCNPLLLPPPPPRRVVRVZZZZZZZGGGGGGGGLGFFFFFFTTTZTEYEEEEEHHSZZZZZZZZZZZZZZZYYYYYBPPPBKKKKKKKKKKKKKKOOOOOOOOOPPPPPPPPQQ
UUMMMMMMMMMMMMMMMMCCCCCCDDCCULLLPPPPPPRUVVVVZZZZZZZGGGGGGGGGGFFFFFTTTTTTEEEEEEEEEEZZZZZZZZZZZZZSYYYYYYYPPPBBBKKKKKKKKKKKKOOOOOOOOOPPPPPPPPPQ
UUUMMMMMMMMMMMMMMMMCCCCCCCCCUULLLPPPVVVVVVVVZZZZZZZGGGEEEEYYGFFFFFTTTTEEEEEEEEEEEEZZZZZZZZZZZZZYYYYYYYBPPPBBBKYKKKKKKKKKKOOOOOOWWWPPPPPPPPPQ
UUUUMMMMMMMMMMGGGGMCCCCCCCCUULLLLLLPVVVVVVVVZZZZZZZGGEEEEEEEEFFFFFTTTTTEEEEEEEEEEEEEZZZZZZZZZZZWYYYYYYYPPPBBBBKKKKKKKKKKKFOOOOOWWWPPPPPPPPPQ
GGUGMMMMZZMMMMGGGGTCCCCCCCCCUULLLLPPVVVVVVVVVFHHGGGTEEEEEEEEFFFFFFFFFTTTEEEEEEEEEEEEEZZZZZZZZZYYYMYYYYYPPPBBBBBBBKKKKFFFFFOOOOOWWWWWLLPPPPPF
GGGGGMMZZZZZZZGGGGTTCCCCCCUUUULLLLPPRVVVVVVVFFHHGGGEEEEEEEEEFFFFFFFFTTTTEEEEEEEEEEEEEZZZZZZZZZCYYYYYYYYYBBBBBBBBBKKKKKFFFKOOOOWWWWWWMMPPPPPF
UGGGGDDZZZZZZMGGGGTTEECCCCUUULLLLLLPPZVVVVVVFFFFEGGEEEEEEEEEFFFFFFUYUETTEUEEEEEEEEEEEEZZZZZZZZCCCCYYYYYYBBBBBBBFBBBKFFFFFKKOOKWWPPPPPPPPPPFF
GGGGGGGGGGGGGGGGGGTEEEEECCCUULLLLLZZZZZFFVFFFFFFEEEEEEEEEEEEFFFFFUUUUEEEEEEEEEEEEEEEEEEEZZZZCCCYYYYYYYYYBYYBBBFFFBBKFFFKKKPPPPPPPPPPPPPPPPFF
GGGGGGGGGGGGGGGGGGTPEPECCCUUUUUULZZZZZZFFFFFFFFFEFEEEEEEEEEEEFFFFFUUUEZEEEEEEEEEEEEEEEEEEZZCCCYYYYYYYYYYYYYYQFFFFKKKFFFFFKPPPPPPPPPPPPPPPPFF
GGGGGGGGGGGGGGGTTTTPPPPPUUUUUULLLLZZZZZFFFFFFFFFFFEEEEEEEEEEEFFFFFFUUUZEEELEEEEEEEEEEEEEEECCCCYYYYYYYYYYYYYYQQFFFFFKFFFKKKPPPPPPPPPPPPPPPSSF
ZGGGGGGGGGGGGGGTTTKPPPPPQUUULLLLLLLLLLLILFFFFFFFFFFFFEEEEEEEEFFFFFFFUUUUUUUTEEEEEEEEEEEEEEECVCCYYYYYYYYYYYYYQFFFFFFFFFFFFKPPPPPPPPWWWWWWWWSS
GGGGGGGGGGGGGGGTTKTPPPPQQQUQBBLLLLLLLLLLLFFFFFFFFFFFFEEEEEEEBHFFFFYYUUUUUUUTEEBBBBBBBBBEEEECVCCYYYYYYYYYYYYQQQFFFFFFFFFFFKPPPPPPPPWWWWWWWWSS
GGGGGGGGGGGGGGGTTTTQQQPQQQQQQQJJLLLLLLLLFFFFFFFFFFFFEEEEEEEEHHHFXFUUUUUUUUUTEEBBBBBBBBBEECCCVCCYYYYYYYYYYYYYQQQQFFFFFFFFFFPPPPPPPPWWWWWWWWSS
GGGGGGGGGGGGGGGTTTTQQQQQQQQFFLLLLLLLLLLLLFFFFFFFIFYYPPEEEHHHHHHHUUUUUUUUUUUEEEBBBBBBBBBEEEVVVCCYYYYYYYYYYYYYQQQFFFFFFFFFFFPPPPPPPPWWWWWWSSSS
GGGGGGGGGGGGGGGTTTTQQQQQQQQQFLGLLLLLLLLLLFFFFFFFCFYCPPEHHHHHHHHDUUUUUUUUUUUUUEBBBBBBBBBEEEVVVCYYYYYYYYYYYYYYYFFFFFFFFFFFFFPPPPPPPPWWWWWWSSSS
GGGGGGGGGGGGGGGTTTTQQQQQQQQFFLLLLLLLLLLHFFFFFCCCCCCCPPPPPPHHPZZDDUUUUBBBBBBBBBBBBBBBBBBEEETEVVVVVVVYVYYYYYKKKKKFFFFFFFFFFFPPPPPPPPWWWWWWSSSB
GGGGGGGGGTTTTTPTTTTTTQQQQQQQQELLLLLLLLLHHCFFFCCCCCCCPPPPPHHHPPZDDDDUUBBBBBBBBBBBBBBBBBBEEEEEEVVVVVVVVVYYKKKKYKKKFFFFFFFFFFFFMMWWWWWWWWWWDSSB
AAAAGGGGGSXTTTPTHHHHHQQQQQQQEEEELLLLLLLHCCCCCCCCCCCCPPPPPPPHPTDDDDDUABBBBBBBBBBBBBBBBBBEEEEEEVVVVVVVVVYYYKKKYYKKKFFFFFFFFFMMMMWWWWWWWWWWDDSB
AAAAGGGGGSXTDDSAHUHHHQQQQQEEEEEEELLLZZLHCCCCCCCCCHHPPPPPPPPPPDDDDDDDDBBBBBBBBBBBBBBBBBBBBBBVVVVVVVVVVVVYYKKKYOYKKFFFFFFMMMMMMMWWWWWWWWWWDDSB
AAAAAASSSSSSSSSHHHHHHHHQQQEEEEEEELLLZZZCCCCCCCCCCHHPPPPPPPPPDDDDDDDDDBBBBBBBBBBBBBBBBBBBBBBVVVVVVVVVVVVYYKKYYYYKFFFJQFFQNNNNMMWWWWMWWWWWMDSB
AAAAAASSSSSSSSSHHHHHHHQQQQEEEEEENNZZZZZCCCCCCCCCHHHHHPPPPPPPDDDDDDDDDBBBBBBBBBBBBBBBBBBBBBBNVVVVVVVVVVVYYYYYYYKKKKJJQQQQNNNNMMWWWWMMMMMMMMBB
AAAAAASSSSSSSSSSHHKKHKKQQEEEEEENNNNZZZZCCCCCCCCHHHHHPPPPPPPPDDDDDDDDDBBBBBBBBBBBBBBBBBBBBBBBVVVVVVVVVVVYYYYYYYYYYKJJJJJNNNNNNRWWWWMMMMMMMBBB
AAAAAASSSSSSSSSSHHKKKKKQQQEEERRNNNNZZZZZCCCCCCCCHPPPPPPPPPDDDDDDDDDDDBBBBBBBBBBBBBBBBBBBBBBBBVVVVVVVVVVYHYYYYYYYFJJJJJJNNNNRRRWWWWMMMMMMMBBB
AAAAASSSSSSSSSSSXHKKRRQQQQEEEERRNZZZZZLZZZKKKCCPPPPPPPPPPPPPOODDDDDDDDDDDGGGTTBBBBBBBBZZZBBBBBVVVVVVAAAAAAYYYYVYJJJJJJJNNJNRCCWWWWMMMMMMMBBB
AAAAASSSSSSSSSSSRRRRRRRBQEERRRRZZZZZLLLZZLKKKKPPPPPPPPPPAAAYAODDDDDDDDDDDGGGTTBBBBBBBBBBBBBBBBVVVVVVAAAAAAYYYYYJJJJJJJJJJJJCCTWWWWMMMMMMMBBB
ALAVVSSSSSSJSSSSRRRRRRRRARRRRRRRZZZZZZLLLLKKKKPPPPPPPPAAARAAADDDDDDDDDDDDDGGTTBBBJJBBBBBBBBBVVVVVVVAAAAAAAAYYYYJJJJJJJJJJCCCCCWWWWMMMMMMMMBB
VVAVVVVSSSVSSSSSZRRRRRRRRRRRRRZZZZZZZLLKLLKKKKKKPPKPPPAAAAAAAADDDDDDDDDDGGGGTTBBBAABBBBBBBBBVVVAAAAAAAAAAAAYJJJJJJJJJJJJJCCCCCWWWWMMMMMCMCCC
VVVVVVVVVVVVSVOSRRRRRRRRRRRRRZZZZZZZZEGKKKKKKKKKKKKPAAAAAAAAAADDDDDDDDDDGGGTTTGAAAABBBBBBBBBVXVZAAAAAAAAJJJJJJJJJJJJJJGJJVCCCCWWWWMMMMCCCCCC
ZZVVVVVVVVVVVVOOORRRRRRRRRREEEEEEEZZEEKKKKKKKKKKKPPPEAAAAAAAAACCDDDDDDDDGGTTTTAAAAAABBBBBBBIIAAAAAAAAAJJJJJJJJJJJJJJJJGGGGCCCCCCCCCMMCCCCCCC
ZZZVVVVVVVVVVVZZZRRRRRRRREREEWEEEEEEEEEKKKKKKKKKKPPPEAAAAAAAACCCAADDDDDGGGTTTTAAAAAABBBBBBBIIIITAAHHHAAJJJJJJJJJJJJJGJGGGGGCCCCCCCCCCCCCCCCC
ZZZVVVAAVVVVVZZZZRRRRRRRREEEEWEEEEEEEEEKKKKKKKKKFPPPEEEEEEAACCCCUUZZZDDQQQTTTAAQQQQQABDBBBBIITTTAHHHAAAJJJJJJJJJJLGGGGGGGGGGGGCCCCCCCCCCCCCC
ZZZZVVAAAVZVZZZZZZERRTRREEEEEEEEEEEEEEEKKKKKPPPFFPPPEBEEEECCCCCUUUZZZZQQQTTTTQQQQQQQABBBHBJTTTBTTHHHHHHHJJJJJJJJLLGFGGGGGGGGGGCCCCCCCCCCCCCC
ZZZZVZGGZZZZZZZZZZZZTTRRREEEEEEEEEEEEEEKKKKPPPPPFPPPPBKEEECCCUCCUUZZZZQQQQQQQQQQQQQQAUUUWWWTTTTTGHAHXXHJVCJVVJJLLLLGGGGGGGGGGGGGGGCCCCCCCCCC
ZZZZZZZGZZZZZZZZZZZQQTTTREEEEEEEEEEEEEEEKKPPPPPPPPPPPBKCECCCCUCCUUZZZZQQQQQQQQQQQQQQQQUUUJWWWTTTHHHHHHHHVCVVVVVLLVLVGGGGGGGGGGGGPGCCCCCCCCCC
ZZZZZZZGZZZZZZZZZZZZQTTTQEEEEEEEEEEEEEEEPPPPPPPPPPPPPBKCCCCCUUUUUUZZZZQQQQQQQQQQQQQQQQUUWWWWWTTTHHHHHHHHVVVVVVVVVVVVGGGGGGGGGGRRRCCCRCCRCCYC
ZZZZZZZZZZZZZZZZZZZQQTQQQQEEQEEEEEEEEEEEEPMPPPPPPOOPPKKCCUUUUUUUUUQQQQQQQQQQQQQQQQQQQQUUUWWWMMTTTMMHHHHHVVVVVVVVVVGGGGGGGGGGGRRRRRCCRCRRCCYF
ZZZZZZZZZZZZZZZZZZQQQQQQQQQQQEEEEEEEEEEEEPPPPPPPPOOKKKKCCURBRRUUUQQQQQQQQQQQQQQQQQQQQQUUWWWMMMMMMMMHHHHVVVVVVVVVVVGGGGGGGGGGGGGGRRRRRRRRYCYY
ZZZZZZZZZZZZZZZZQQQQQQQQQQQQEEEEEBDEEEEEEDPPPDDPKKKKKKKKCRRRRIRUUCQQQQQQQQQQQQQQQQQUUUUUWWMMMMMMMMMMHHHXXVVVVVVVVVGGGGGGGGGGGGGGRRRRRRRYYYYY
ZZZZZZZZZZZZZVVZQMMMQQMYQQQQEEEEEEDDEEEEEDPDDDDDKKKKKKCCCRRRRRRURCQQQQQQQQQQQQQQQQQUUUUUWWMMMMMMBBBBBHBXXVVXVUUKKKKGGGGGGGGGGGGGGRRRRRRRYYYY
ZZZZZZZZZZZZMMVMMMMMMMMYYQYYHHEEDDEEEEEEEDDDDDDDKKKKKKKKCCRRRRRRRCCQQBOOQQQQQQQQQQQUUUUUUUUUMMBBBBBBBBBXXXXXXUUUUKGGGGFFGGGGGGGRRRRRRYYYYYYY
ZZZZZZZZZZZWMMVMMMMMMMYYYYYYYHHHHHEEEEEDDDDDDDIDKKKKKKKKKCRRRRRRCCCQBBBBBBBBQQQQQQQQUUUBUUUMMBBBBBBBBBBXXXXXUUUUUULUFGFFGGGGGGGRFRYYYYYYYYYY
ZZZZZZZZZZZWMMMMMMMMMYYYYYYYYHHHHDEEEEEDDDDDDDKKKKKKKKKKKCCCCRCRCCCIVBBBBBBBQQQQQQUUUUUBBBMMMMBBBBBBBBBXXXXUUUUUUUUUUFFFFGGGFGFFFFFYYYYYYYYY
ZZZZZZZZMMMMMMMMMMMMMYYYYYYYHHHHHDDDDDDDDDDDDDKKKKKKKKKCCCCCCRRRCCCIBBBBBBQQQQQQQQUUVNNNBBBMMMBBBBBBBBBXXXXUUUUUUUUUUFFFFFFFFFFFFFFYYYYYYYYY
ZZZZZZZZZMMMMMMMMMMMYYYYYYYYHHHHYYKDDDKKDDDDDDKKKKKKGGKCCCCCCCCCCCCIIBBBBBBQQQQQOGUUVNNNBBBBNNBBBBBBBBBBXXXUUUUUUUUUUFFFFFHHHHFFFFYYYYYYYYYY
ZZZZZZZZZMMMMMMMMMYYYYYYYYYYYYYYYYKDKDKDDDDDKDKKKKKKGGGGCCCCCCCCCCIIBBBBBBBBQQQOOOOONNNNBNNBNNNNBBBBBBBXXXUUUUUUUUUUUFFFFHHHHFFHHFHYYYYYYYYY
ZZZZZZZZZMMMMMMMMYYYYYYYYYYYYKKKKKKKKKKDDDDKKKKKKGGKGGGGGCCCCCCIIIIIBBBBBBBBQQQQOOOENNNNNNNNNNNNBBBBTTBXXXUUUUUUUUUUUFFFFFFHHHFHHHHBYYYYYYYY
ZZZZZZZZZMMMMMMMMYCYYYYYYYYYYYYKKKKKKKKKKKKKKGKKGJGGGGGGGGGGCCCOIIIBBBBBBBBBBKQOOOOOONNNNNNNNNNZBBBBBTTTTTUUUUUUUUUUFFFFFFHHHHHHHHHYYYYYYYYY
ZZZZZZZZMMMMMMMMYYYYYYYYYYYBYBBWWWKKKKKKKKKKKGGGGGJGGGGGGGGGGCOOOBBBBBBBBBBBKKKOOOOOONNNNNNNNNNNBBTTTTTTTTAUUUUUUUUUFFFFFFHHHHHHHHHHHYYYYYYY
ZZZZZZZZOOOMZMMZZZYYYYYYYYBBBBBWWWWKKQKQKKKKGGGGGGGGGGGGGGGGGCOOBBBBBBBBBBBKKKOOOOOOONNNNNNNNNNNBBTTTTTTTAAUUUUUUUUFFFFFFFFFHHHHHHHHHYYYYYYY
ZZMZMZZOOOOOZMMZZZYYYYYYYYYBBBBWBWWKQQQQQKKKGGGGGGGGGGGGGGFGFFFOOOOBOOBBBBBBKOOOOOOOONNNNNNNNNNNNBBBTTTTTTTUUUUUUUFFFFFFFFFFFFHHHHHHHYYYYYYY
ZLMMMOOOOOOOZZZZZYYYYZYYYYYBBBBBBBWKKKQQQQKKKKGGGGGGGGGGGGFFFFFFOOOOOOOBKKKKKOOOOOOYYNNNNNNNNNNNNNNNTTTTTVTTOFFFFUFFFFFFFFFFFHHHHHZZZZYYYYYY
MMMMMOMMOOOOBZZZZZYYZZYZBBBBBBBJBHWHQQQQTTTKKKKGGGGGGGGGGFFFFFFFOOOOOOOBKKKKKOOOOGOXYYNNNNNNNNNNOEOOTTTTTTOOOOFFFFFFFFFFFVVFFFHZZZZZZZYZYYYY
MMMMMMMMOOOOBZZZZZZYZZZZZZZBBBJJJHWHQQQQTTTTTGGGGGFFFGGGFFFFFOOOOOOOOOOOHHHHHGGGOGGXYYYNNNNNNPPPOOOOOOTOOOOOOOFFWWWWFFFFFFVVVZZZZZZZZZYZYYYY
MMMMMMMMOOOOBZZZZZZZZZZZZZZBJJJJJHHHHQQTTTTTTTDGGFFFFFFFFFFFFOOOOOOOOOOHHHHHGGXGGGGXXYYNNNNNPPPPPPOOOOOOOOOOOFFWWWWFFFFFVVVVVZZZZZZZZZZZZYYY
MMMMMMMOOOOBBZZZZZZZZZZZZZZZJJJJJHHHHHTTTTTTTTDDDDFFFFFFFFFPPFOOOOOOOOOHHHKKXXXXXXXXXXNNNNNNPPPPPPOOOOOOOOOIOOOOWWWWWWFFFVVVVZZZZZZZZZZZZYYZ
MMMMMMOOOBBBBZZZZZZZZZZZZZJJJJJJJHHHHTTTDTTTTDDDDDFFFFFFHFFFFFOOOOOYOOHHHHHXXXXXXXXXCXXNGGNPPPPPPPOOOOOOOOIIIWWWWWWWWWWWFVVVVZZZZZZZZZZZZZZZ
MMMMMMOOBBBBBBZZZZZZZZZZZZZZJJJJJJJJHHHTDTTTTDDDDDFFFFFFFFFFFOOOOOOYYHHHHHXXXXXXXXXXXTUUUUNPPPNPPOOOOOOOOOOIWWWWWWWWWWWWWWWZZZZZZZZZZZZZZZZZ
MMMMMMMOOBBBBBBBZZZDDDZZZJJJJJJJJJJJHHHDDTTTTDDDDDFFFFFFFFFFFOOOOOYYYHHHHHHHXXXXXXXXXTUUUNNNNNNNOOOOOOOOFFRRWWWWWWWWWWWWWRRRZZZTZZZZZZZZPZZZ
MMMMMMMBBBBBBBBBZZZZDDDDJJJJJJJJHHJHHHHDDDDDDDDDDDFKFFFFFFLFFFOOOOYYYYHHHHHHXXXXXXXXXXUUUNNNNNNNNOOFFFFFFRRRRRWWWWWWWWWWWWWRRZZTZZZZZZZZPZPP
MMMMMMMMMBBBBBZZZZZDDDDDDDEJJJJJHHHHHHHDHDDDDDDDDFFKFFFFFFFFFOOOOOOYYYYHHHHHCXXXXXXXXUUNNNNNNNNNNNNFFFFFFRRRRRWWWWWWWWWWRWWRZZTTZZIPPZZPPPPP
MMMMMMMMBBBBBBZZZZDDDDDDDDDJJJJJHHHHHHHHHDDDDDDDDFFFFFFFFFFFOOOOOODDYYYYHHHHCCXXXXXXXUUUUNNNNNNNNNNFFFFFFSRRRRRWWWWWWWWRRRRRZZZZZPIPPPPPPPPP
NMMMNMNMBBBBBBBZZZZZDDDDDDDHJHHHHHHHHHHHRHDDDDDLFFFFFFFFFFFFOOOOODDDYYYYHYYYYXXXXXXXXXUUUNNNNNNNNNNNFFFFSSSSRRWWWWWWWWWRRRRRZZZZPPIPPPPPPPPP
NNNMNMNNBBBBBBBBZZZZPDDDDDDHHHHHHHHHHHHHHHLLLLLLLZFFFFFFFFFFOOOOODYYYYYYYYYYYYXXXXXXXXUUUUNNNNNNNNNFFFFFSSSSSSKKWWWWWWWRRRRRZZZZPPPPPPPPPPPP
NNNNNNNNNBBBBBBBZZZZZDDDDDDHHHHOHHHHHHHHHHHLLLLLLZZFFFFFFFFFFOOYYYYYYYYYYYYYYYYXXXXXXUUUUUNNTNNNNNFFFFFFFSSSSSSWWBWWWWWWZRZZZZZZZZPPPPPPPPPP
NNNNNNNNNBBBBBBBBZZZZZDDDDDHHHOOOOHHHHHHHHHHHLLLFFFFFFFFFFFFFOOYYYYYYYYYYYYYYYYXXUJXXUUUUUUNNNNWWFFFFFFSSSSSSSSSSWWWWWWWZZZZZZZZZZPPPPPPPPPP
NNNNNNNNBBBBBBBUUUZZZZDDDDHHHOOOOOHHHHHHHHHHHHHHHHFFFLFFOOOOOOOOOYYYYYYYBOYYYYYYYUUXUUUUUUUNNWWWFFFFFFFFFSSSSSSSSSSWWWZWZZZZZZZZZZZPPPPPPPPP
NNNNNNNNNBBBBBBBKUKRKKKDDHHHHHOOOOHHHHHHHHHHHHHHHHHFFFFFOOOOOOOOOOOZYYRYOOOYYYYYYUUUUUUUUUUUUUWWWFFFFFFFSSSSSSSSSSWWWZZZZZZZZZZZZZZZPPPPPPPP
NNNNNNNNYBBJBBXXKKKRKKPPPHHHHHHOOOOOHHHHHHHHHHHHHHIFFFFFOOOOOOOOZZOZYYLLOOOOOOYOUUUUUUUUUUUUUUWFFFFFFGGQQSSSSSSSSSSSWZZZZZZZZZZZZZZPPPPPPPPP
NNNNNNXBBBBBXXXKKKKRKKPPHHHHHHHOOOOOHHHHHHHHHHHFFFFFFFFFKOOOOOZOZZZZZZLLLLOOOOOOCUUUUUUUUUUUUUWWWVFFFFGGGSSSSSSSSSSSSSSZZZZZZZZNNNBPPPPNPPPP
NNNNNNBBBBBXXKXKKKKKKKKHHSHHHHOOOOOOHHHHHHHHHHHHHHFFFFZKKOKKZZZZZZZZZLLLLOOOOOOOCUUKUUUKUUUXVUWVVVVGGGGGGGGSSSSSSSSSSSSZZZZZZZZZNBBBPNNNNPPP
NNNNNTTTBBBXXKKKKKKKKKSHHSHHHHHOOOOOOOHHHHHHHHHHHHFFFFZKKKKKZZZZZZZZZZLLLLLOOOOOOOKKUKKKUUVVVVVYVVVYGGGGGGGGSSSSSSSSSSSSZZZZZZZZNNNBPNNNSPPP
NNNNNTTTTMBXKKKKKKKKKKSHSSSHHHHOOOOOOOHHHHHHHHHHHHHFZZZKKKKEEEZZZZZLLLLLLLLOOOOOKKKKUKKZUVVVVVVVVVVYGGGGGGSSSSSSSSSSSGGGGGGZZZZZNNNNGNNNNPNN
NTTNTTTTTTBXKKKKKKKKKKKKSSSHHHXOOOOOOPFFHHFHHFHHHHHZZXZZKKKEEEEEZPUULLLLLLLOOOOOZKKKKKZZCVVVVVVVVVVGGGGGGSSSSSSSSSSSSGGGGGGZGZZZNNNNNNNNNNNN
TTTNTTTTTTTTKNKKKKKKKKKKKKKVPHPFFOOOPPFFHHFFHFHHHHZZZXXXEEEEEEEEPPPUULLLLLOOOOOOZZKKZZZZZQVVVVVVVVVMGGGGGGMMMMSSSSSSSGGGGGGGGGZNNNNNNNNNNNNN
TTTTTTTTTTTITTTKKKKKKKKKKKKVPPPPPPPPPPFFFFFFHFFFFFXXXXEEEEEEEEPPPPPLLLLLLLLOPOCCZZKZPZZZZVVVVVVVVVVMMGGGGGMMMMSSSSGSSGGGGGGGGGZNNNNNNNNNNNNN
TTTTTTTTTTTTTEEEKKKKKKKKKKKPPPPPPPPPPPPFFFFFFFFFFFXXXXXXEEEEEPPJPPPLPPLLPPPPPPCZZZZZZZZZZVVVVVVVVVVMMMGGMMMMMSSGSSGGGGGGGGGGNZZZUNNNNNNNNNNN
TTTTTTTTTTTTTFEEEKKKKKKKKKKPPPPPPPPPPPPFFFFFFFFFFXXXXXXXXEEEXPPPPPPLPPPLPPPVPCCPZZZZZZZZZZVVVVVVVVVVMMGGMMMMMGGGGGGGGGGGGGGGNNNUUNNNNSNNNNNN
TTTTTTTTTTTTFFFEEFKKKKKHKKKPPPPPPPPPPPPFPPFFFFFFFXXXXXXXXEXXXOOPPPPPPPPPPPPPPPPPZZZZZZZZZZZVVVVVVMMVMMMMMMMGGGGGGGGGGGGGGGGGNNNNNNNSSSSNNNNN
TTTTTTTTTTTTTFFFFFZKKKWLLLKGPPPPPPPPPPPPPFFFFFFXXXXXXXXXXXKXXXPPPPPPPPPPPPPPPPPPZZZZZZZZZZZZVVVMMMMMMMMMMMMGGGGGGGGGGGGGGGGGGNNNNNNNSMSSNNNV
TTTTTTTTTTTFFFFFFFFFFFLLLLPPPPPPPPPPPPPFFFFFFFFFXXXXXXXXXXXXXXXPPPPPPPPPPPPPPPPPZZZZZZVZZZZZZZVMMMMMMMMFMMGGGGGGGGGGGGGGGGGGGNNNNNNNNMNSNVVV
TTTTTTTTKKKKFFFFFFFFFFLLLLLLZPPPPPPPPPPFFFFFFFFFXXXXXXXFXXSSSXPPPPPPPPPPPPPPPPPPZZZZZVVVZVZZZMMMMMMMMMFFFFFGGGGGGGGGGVGVVVVVGNNNNNNNMMNNNNNV
TTTTTTTKKKKKKFFFFFFFFFLLLLLLZZYYPPPPPPPPFFFFFRRFXXXXXXFFXXSSSSPPPPPPPPHPPPPPPPZZZZZZZQVVVVVVZMMMMMMMMMFFFFFGGGGGGGGVVVVVVVVNNYNNNNNYSSSSSSNS
TTTBBBBBKKKKFFFFFFFFFFFLLLLLLLYYYPQQPYPPFFFFRRRRRXXXXXFFFTTSSSPPPPPHHHHHPHPPJJJBLBBBVVVVVVVVMMMMMMMMMMFFFFFFGGGGGGNNVVVVVVVNYYYYYYYYSSSSSSSS
TTBBBBBBBBKFFFFFFFFFFFLLLLLLLYYYYYQYYYYFFFFFFRRRRXXXXXXXFTTTTDDDDHHHHHHHHHHHHJJBBBBBVVVVVVVVVMMMMMMMKFFFFFFFFGGGGGNNVZZVVVNNYYYYYYYSSSSSSSSS
BBBBBBBBBYKFFFFFFFFFFFLLLLLLYYYYYYYYYYFFFFRRRRRRRRRXXXXFFFTTDDDDDHHHHHHHHHHHHHBBDKBBVVVVVVVVMMMMMMKKKFFFFFFFFFFGFGNNNVVVVVNNYYYYYYSSSSSSSSSS
BSBSSSSUYYKFFFFFFFFFFLLVVVVYYYYYYYYYYYFFFFFRRRRRRRRRRGXTTTTTTDDDDIHHHHHHHHHHDDDDDKKKVVVVVVVVMMMMMKKKKKKTFFFFFFFFFNNNNVVVVVNTTRYPYYSSSSSSSSSS
SSSSSSSSYYYYYYFFFFFFFVLVVVVVVYYYYYYYYYYFFFFRRRRRRRRRRRTTTTTTTTDDDDDHHHHHHHHHHDDDKKKKVVVVVVVVVVMMMMKKKTTTFFFFFFFFFFNNNNNVVVNNTRYYYSSSSSSSSSSS
SSSSSSSUUYYYYYFFFFFHVVVVVVVVVVYYYYYYFFFFFFRRRRRRRRRRRRRTTTTTTTDDDDDDHHHHHHHHQHKKKKKKVVVVVVVSSMMMMMMEEETMFMVVYXFAAFNNNNNNVNNNTTTTYSSSSSSSSSSS
SSYSSSUUUYYYYYWFFFFVVVVVVVVVVYYYYYYYFFFRRRRRRRRRRRRRRRRTTTTTTTDDDDDDDDDDHHHHHHKKKKKKKVVVVVSSSMMMMMMEEEMMMMYYYYFAAAYNNNNNNNTNTTTTSSSSSSSSSSSS
SSSSSSSSUYYYYNSFFFFVVVVVVVYYYYYYYYYYKFFRRRRRRRRRRRRRRRTTTTTTTDDDDDDDDDDDHHHHHIKKKKKKIIIVVVSESMMMMMMEMMMMMMMYYYYYAAYNNNNNTTTTTTTTTSSSSSSSSSSS
SSSSSSCCYYYNNNSSSSSGGVVVVEEYYYYYYYYYKFFRRRRRRRRRRRRRRRTTTTTDDDDDDDDDDDDTHHHHHIKIKKIIIIIIIIEEMMMMMMEEEMMMMMMYYYYYYYYNYNNNTTTTTTTTYYSSSSSSSSSS
SSSSSSCCCCIIIIGGSSSGVVVVVVVYYYYYYYYYKFFIIRRRRRRRRRRRRRRRTTTTTDDDDDDDTTDTHHHAHIIIIIIIIIIIIIIEEMEEEEEEEMMMMLYYYYYYYYYYYYNNNTTTTTYTYXYMSSSSSSSS
SSSSTTCTCIIIIIIISSGGGVVVOOOOOYYYYYYYYIIIIIRRRRPRRRRRTRRRTTTTTDDDDDDDTTDTTHBAAIIIIIIIIIIIIIIEEEEEEEEEEEMMMMYYYYYYYYYYZZNNNTTTTTYYYXYMSSSSSSSS
TTTTTTTTTIIIIIIIIGGGGGVVVVYYYYYYYYYYYIIIIIRRRRRRRRRRRRRTTTTTTTDDDTDDDTTTTAAAAAIIIIIIIIIEEIEEEEEEEEEEEEMMMMMMMYYYYYYYZZZZZZZTTTYYYYYYYSSSQSSS
TTTTTTTTIIIIIIIIIGGGGGVVVVGGYYYYYYYYYIIIIHTRRRHRRRRRRRHTTTTTTTTTDTTTTTTTTTAAAAIIIIIIIIIEEEEEEEEEEEEEEEMMMMMYYYYYYYYYYZZZZZZTTTTYYYYYSSSSQSSS
TTTTTTTTIIIIIIIIIIIGGGGGGGGGGYYYYYYYIIIIIHRRRHHHRRRRRRHTTTTTTTTTTTTTTTTTTTAAIAIIIIIIIIIIEEEEEEEEEEEEEEMMMMMMYYYYYYYYYYTZZZTTTTTTTYYYYYSSSRSR
TTTTTTTTIIIIIIIGGGGGGGGGGGGGYYYYYYYYYIIIIHHHHHHHHHHHHHHHHHTTTTTTTTTTTTTTTAAAIIIIIIIIIIIEEEEEEEEEEEEEEEEEEEMYYYYYYYYYYYTXZZTTTTTTTTYBYRSRRRSR
TTTTTTTXIIRIRIIIGGGGGGGGGGGGYYYYYYEYIIIIIHHHHHHHHHHHHHHHHHHTTTTTTTTTTTTTAAAIIIIIIIIIIIEEEEEEEEEEEEEEEEEEEEMYYYYYYYYYTTTTTTTTTTTTTTBBYRRRRRRR"""

let garden = parse input

printGarden garden

let areasByPlant = divide garden

printAreas garden areasByPlant

let areasAndPerimetersByPlant = surround areasByPlant

printfn "%A" areasAndPerimetersByPlant

let price = calculatePrice areasAndPerimetersByPlant

printfn "%i" price
