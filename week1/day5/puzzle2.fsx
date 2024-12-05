
type ParsedPrintingPrecedenceRule = int * int

type ParsedPrintingPrecedenceRules = ParsedPrintingPrecedenceRule list

type PrintingPrecedenceRules = Map<int, int list>

type PrintJob = int list

type PrintJobs = PrintJob list

type PrintInput = PrintingPrecedenceRules * PrintJobs

let parseRules rules =
    let parseSingleRule (line: string) = 
        let tokens = line.Split '|'
        assert ((tokens |> Array.length) = 2)
        let firstPage = tokens.[0] |> int
        let secondPage = tokens.[1] |> int

        (firstPage, secondPage)
    let parsedRules = rules |> Array.map parseSingleRule

    parsedRules |> Array.toList

let parseJobs jobs = 
    let parseSingleJob (line: string) =
        let splitLine = line.Split ',' |> Array.map int |> Array.toList
        (splitLine)
    let parsedJobs = jobs |> Array.map parseSingleJob
    parsedJobs |> Array.toList

let toPrintingPrecedenceRules parsedRules =
    let mutable result = Map.empty<int, int list>

    for rule in parsedRules do
        let page, before = rule
        let items = 
            if not(result.ContainsKey(page))
            then []
            else result[page]

        let newItems = items @ [before] 
        result <- result |> Map.add page newItems

    result

let parse (t: string): PrintInput = 
    let lines = t.Split System.Environment.NewLine

    let emptyLineIndex = lines |> Array.findIndex (fun l ->  l = "")

    assert (emptyLineIndex >= 0)

    let rules = lines |> Array.take emptyLineIndex
    let jobs = lines |> Array.skip (emptyLineIndex + 1)

    (parseRules rules |> toPrintingPrecedenceRules, parseJobs jobs)

let remainingJobsFulfillRule (jobs: PrintJob) rules =
    let pageInRules p = rules |> List.contains p
    let jobFulFillsRule = jobs |> List.map pageInRules |> List.forall id
    
    (jobFulFillsRule)

let getRulesForPage page (rules: PrintingPrecedenceRules) =
    let isManagedByRule = rules.ContainsKey page
    let ruleForPage =
        if isManagedByRule
        then rules[page]
        else []

    (ruleForPage)

let fulfillsRules (job: PrintJob) (rules: PrintingPrecedenceRules) = 
    let jobFulFillsRule index page =
        let ruleForPage = getRulesForPage page rules

        let remainingJobs = List.skip (index + 1) job

        let pageFulfillsRule = remainingJobsFulfillRule remainingJobs ruleForPage

        (pageFulfillsRule)
        
    job |> List.mapi jobFulFillsRule |> List.forall id

let getValidJobs (p: PrintInput) =
    let rules, jobs = p

    let fulfillsSpecifiedRules job = fulfillsRules job rules
    let validJobs = jobs |> List.filter fulfillsSpecifiedRules

    (validJobs)

let reduceRulesForJobs (rules: PrintingPrecedenceRules) job =
    let mutable result = Map.empty<int, int list>

    for page in job do
        let rulesForPage = 
            if rules.ContainsKey page
            then rules[page]
            else []

        result <- result |> Map.add page rulesForPage

    result


let compareAccordingToRules (rules: PrintingPrecedenceRules) a b =
    let rulesForA = rules[a]
    let rulesForB = rules[b]

    let compareValue = 
        if (List.contains b rulesForA && List.contains a rulesForB)
        then 0
        else if List.contains b rulesForA
            then -1
            else if List.contains a rulesForB
                then 1
                else 0

    compareValue

let findIdealOrder (rules: PrintingPrecedenceRules) =
    let compareAccordingToRules a b = compareAccordingToRules rules a b
    let items = rules.Keys |> Seq.sortWith compareAccordingToRules |> Seq.toList

    items

let correctJob job rules = 
    let reducedRules = reduceRulesForJobs rules job

    let correctedJob = findIdealOrder reducedRules

    printfn "%A" correctedJob

    correctedJob

let correctJobs jobs rules =
    let correctJobByRules job = correctJob job rules
    let correctedJobs = jobs |> List.map correctJobByRules

    (correctedJobs)

let getCorrectedJobs (p: PrintInput) =
    let rules, jobs = p

    let fulfillsSpecifiedRules job = not (fulfillsRules job rules)
    let invalidJobs = jobs |> List.filter fulfillsSpecifiedRules

    let correctJobByRules jobList = correctJobs jobList rules
    let correctedJobs = correctJobByRules invalidJobs

    (correctedJobs)

let getMiddlePage job =
    let length = job |> List.length
    let middle = (length / 2)

    let page = job |> List.item middle

    (page)

let example = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

let input = """39|46
23|35
23|31
25|99
25|35
25|41
32|81
32|75
32|94
32|98
24|44
24|68
24|83
24|85
24|56
31|41
31|82
31|49
31|35
31|73
31|33
44|29
44|62
44|26
44|61
44|79
44|31
44|72
35|94
35|32
35|81
35|17
35|56
35|73
35|61
35|16
77|33
77|82
77|61
77|68
77|16
77|85
77|74
77|84
77|56
28|79
28|75
28|95
28|24
28|39
28|62
28|21
28|35
28|49
28|27
26|77
26|99
26|41
26|35
26|49
26|33
26|39
26|16
26|83
26|24
26|84
11|41
11|43
11|25
11|83
11|79
11|31
11|46
11|77
11|42
11|76
11|23
11|39
29|24
29|16
29|76
29|26
29|21
29|33
29|19
29|73
29|83
29|79
29|95
29|35
29|49
62|31
62|21
62|76
62|35
62|83
62|77
62|43
62|99
62|19
62|26
62|46
62|75
62|89
62|49
53|89
53|19
53|25
53|62
53|28
53|26
53|79
53|23
53|29
53|39
53|21
53|76
53|72
53|31
53|41
69|11
69|42
69|19
69|62
69|49
69|53
69|26
69|76
69|25
69|27
69|75
69|23
69|89
69|29
69|98
69|72
27|95
27|39
27|41
27|43
27|29
27|26
27|62
27|21
27|76
27|31
27|23
27|19
27|11
27|35
27|89
27|42
27|24
49|14
49|73
49|83
49|95
49|46
49|48
49|84
49|24
49|17
49|94
49|43
49|32
49|85
49|35
49|99
49|44
49|33
49|56
33|68
33|84
33|69
33|56
33|23
33|62
33|57
33|27
33|98
33|82
33|94
33|53
33|44
33|61
33|81
33|85
33|11
33|32
33|14
46|74
46|17
46|16
46|81
46|53
46|14
46|72
46|69
46|57
46|48
46|73
46|98
46|77
46|85
46|82
46|44
46|32
46|68
46|84
46|56
41|35
41|48
41|85
41|32
41|14
41|83
41|21
41|56
41|33
41|77
41|73
41|24
41|95
41|44
41|17
41|49
41|68
41|81
41|99
41|84
41|43
57|31
57|27
57|26
57|95
57|19
57|39
57|75
57|41
57|29
57|76
57|79
57|24
57|62
57|23
57|11
57|21
57|35
57|28
57|98
57|49
57|89
57|99
42|68
42|48
42|49
42|43
42|56
42|16
42|76
42|32
42|73
42|31
42|26
42|39
42|21
42|83
42|82
42|46
42|33
42|79
42|41
42|95
42|99
42|35
42|24
19|26
19|48
19|79
19|76
19|49
19|39
19|56
19|46
19|43
19|33
19|77
19|41
19|24
19|73
19|68
19|83
19|35
19|31
19|16
19|82
19|99
19|95
19|21
19|42
94|28
94|62
94|23
94|72
94|98
94|27
94|29
94|57
94|61
94|41
94|69
94|11
94|25
94|39
94|89
94|53
94|75
94|74
94|19
94|79
94|76
94|26
94|31
94|42
17|62
17|31
17|74
17|72
17|29
17|89
17|26
17|61
17|25
17|11
17|94
17|53
17|44
17|69
17|14
17|19
17|85
17|42
17|23
17|27
17|98
17|75
17|57
17|28
74|49
74|79
74|11
74|21
74|89
74|23
74|35
74|25
74|19
74|41
74|95
74|24
74|57
74|39
74|98
74|26
74|28
74|76
74|31
74|42
74|29
74|27
74|62
74|75
48|84
48|57
48|27
48|74
48|44
48|69
48|73
48|68
48|98
48|72
48|28
48|53
48|56
48|85
48|14
48|94
48|32
48|61
48|17
48|33
48|62
48|81
48|11
48|82
76|14
76|33
76|41
76|73
76|17
76|56
76|84
76|77
76|99
76|46
76|95
76|21
76|82
76|85
76|16
76|24
76|68
76|49
76|43
76|35
76|48
76|83
76|81
76|32
84|98
84|75
84|17
84|11
84|94
84|61
84|44
84|42
84|19
84|53
84|23
84|14
84|85
84|69
84|57
84|25
84|81
84|27
84|28
84|29
84|74
84|72
84|89
84|62
89|24
89|21
89|77
89|79
89|76
89|48
89|41
89|95
89|31
89|19
89|43
89|33
89|46
89|16
89|39
89|29
89|75
89|25
89|35
89|26
89|49
89|83
89|42
89|99
98|39
98|35
98|28
98|29
98|41
98|89
98|23
98|27
98|42
98|99
98|76
98|75
98|79
98|62
98|11
98|95
98|26
98|19
98|31
98|24
98|25
98|49
98|43
98|21
16|57
16|53
16|68
16|17
16|98
16|32
16|33
16|84
16|27
16|73
16|69
16|14
16|28
16|81
16|74
16|48
16|44
16|72
16|61
16|11
16|82
16|94
16|56
16|85
75|48
75|99
75|25
75|35
75|16
75|29
75|24
75|43
75|21
75|77
75|31
75|49
75|79
75|83
75|46
75|26
75|39
75|95
75|76
75|42
75|33
75|19
75|41
75|82
79|73
79|77
79|99
79|21
79|48
79|95
79|41
79|84
79|46
79|82
79|68
79|81
79|76
79|24
79|85
79|17
79|35
79|16
79|49
79|43
79|83
79|33
79|56
79|32
14|62
14|29
14|39
14|53
14|26
14|44
14|94
14|31
14|75
14|61
14|89
14|19
14|79
14|72
14|28
14|98
14|57
14|27
14|42
14|74
14|25
14|23
14|11
14|69
81|44
81|25
81|74
81|14
81|85
81|42
81|27
81|98
81|26
81|17
81|11
81|72
81|53
81|69
81|62
81|23
81|75
81|28
81|29
81|89
81|61
81|19
81|57
81|94
99|94
99|81
99|53
99|77
99|48
99|56
99|44
99|17
99|61
99|85
99|72
99|73
99|68
99|14
99|43
99|82
99|84
99|46
99|16
99|32
99|69
99|83
99|33
99|74
85|89
85|23
85|29
85|11
85|19
85|42
85|39
85|27
85|62
85|14
85|57
85|98
85|31
85|69
85|94
85|26
85|25
85|75
85|72
85|53
85|44
85|61
85|74
85|28
82|57
82|72
82|89
82|85
82|17
82|23
82|32
82|81
82|11
82|44
82|61
82|73
82|62
82|74
82|98
82|69
82|56
82|28
82|68
82|94
82|27
82|84
82|53
82|14
43|82
43|32
43|72
43|46
43|57
43|56
43|74
43|69
43|73
43|61
43|33
43|14
43|16
43|77
43|83
43|84
43|48
43|68
43|81
43|94
43|44
43|17
43|53
43|85
73|81
73|94
73|89
73|85
73|28
73|56
73|62
73|32
73|84
73|68
73|27
73|98
73|11
73|61
73|17
73|44
73|23
73|74
73|57
73|75
73|53
73|72
73|69
73|14
95|33
95|44
95|68
95|56
95|17
95|94
95|82
95|81
95|53
95|69
95|84
95|83
95|46
95|61
95|85
95|72
95|99
95|43
95|14
95|73
95|16
95|48
95|32
95|77
72|89
72|49
72|27
72|74
72|21
72|41
72|28
72|62
72|75
72|25
72|42
72|23
72|26
72|19
72|11
72|31
72|24
72|76
72|39
72|35
72|29
72|98
72|57
72|79
68|69
68|17
68|94
68|98
68|72
68|44
68|53
68|23
68|29
68|85
68|11
68|75
68|27
68|32
68|61
68|84
68|74
68|14
68|81
68|89
68|25
68|28
68|62
68|57
83|14
83|72
83|16
83|32
83|74
83|69
83|48
83|56
83|77
83|33
83|61
83|57
83|82
83|46
83|81
83|84
83|53
83|44
83|98
83|68
83|85
83|17
83|73
83|94
61|26
61|62
61|29
61|21
61|75
61|79
61|98
61|89
61|74
61|57
61|11
61|72
61|31
61|42
61|25
61|69
61|28
61|41
61|76
61|27
61|19
61|23
61|53
61|39
21|35
21|14
21|44
21|84
21|24
21|83
21|17
21|99
21|46
21|94
21|48
21|81
21|56
21|82
21|68
21|49
21|73
21|77
21|43
21|85
21|32
21|16
21|95
21|33
56|61
56|23
56|28
56|29
56|75
56|44
56|32
56|57
56|14
56|98
56|11
56|94
56|69
56|85
56|68
56|53
56|84
56|17
56|27
56|62
56|89
56|81
56|74
56|72
39|81
39|32
39|82
39|48
39|35
39|83
39|77
39|24
39|16
39|41
39|84
39|17
39|43
39|56
39|95
39|68
39|73
39|21
39|49
39|33
39|79
39|76
39|99
23|16
23|43
23|21
23|79
23|46
23|26
23|95
23|41
23|99
23|24
23|25
23|76
23|89
23|29
23|49
23|77
23|48
23|39
23|19
23|83
23|75
23|42
25|56
25|77
25|26
25|39
25|31
25|95
25|16
25|82
25|46
25|43
25|76
25|79
25|24
25|48
25|49
25|33
25|21
25|42
25|73
25|19
25|83
32|74
32|69
32|19
32|85
32|25
32|53
32|29
32|11
32|14
32|84
32|23
32|62
32|57
32|61
32|17
32|27
32|89
32|44
32|72
32|28
24|14
24|95
24|48
24|43
24|82
24|94
24|32
24|33
24|99
24|69
24|61
24|77
24|81
24|84
24|46
24|17
24|16
24|73
24|35
31|79
31|76
31|56
31|16
31|46
31|39
31|43
31|24
31|95
31|83
31|99
31|32
31|48
31|21
31|68
31|84
31|77
31|81
44|89
44|27
44|23
44|75
44|42
44|39
44|94
44|98
44|74
44|28
44|11
44|76
44|19
44|57
44|25
44|53
44|69
35|77
35|99
35|85
35|43
35|68
35|46
35|82
35|83
35|95
35|53
35|69
35|44
35|14
35|33
35|48
35|84
77|98
77|17
77|57
77|94
77|14
77|28
77|81
77|48
77|44
77|53
77|73
77|72
77|27
77|69
77|32
28|29
28|43
28|76
28|25
28|11
28|42
28|26
28|19
28|89
28|41
28|83
28|31
28|99
28|23
26|82
26|95
26|76
26|32
26|43
26|68
26|73
26|79
26|56
26|46
26|21
26|31
26|48
11|24
11|62
11|99
11|35
11|21
11|95
11|49
11|89
11|19
11|26
11|75
11|29
29|46
29|43
29|48
29|42
29|82
29|77
29|39
29|99
29|41
29|25
29|31
62|41
62|24
62|79
62|42
62|23
62|25
62|39
62|16
62|95
62|29
53|57
53|24
53|74
53|49
53|75
53|27
53|98
53|11
53|42
69|57
69|39
69|79
69|21
69|74
69|31
69|28
69|41
27|79
27|99
27|83
27|25
27|49
27|75
27|46
49|82
49|77
49|81
49|16
49|61
49|68
33|74
33|72
33|28
33|73
33|17
46|94
46|33
46|28
46|61
41|46
41|16
41|82
57|25
57|42
42|77

94,28,25,26,39
79,49,23,21,24,35,11,76,28,31,89,42,29,26,98,74,41,57,27
23,89,53,28,11,14,27,74,75,17,25,61,57,69,32,94,44,84,85,98,62,29,72
31,39,76,41,21,24,35,95,99,43,83,46,77,16,48,33,82,73,56,68,84
76,49,46,83,68
23,89,75,29,25,19,42,31,39,79,76,41,21,49,24,35,95,99,43,83,46,77,16
25,85,89,31,14,29,74,57,28,26,72,11,23
89,29,25,19,42,26,31,39,79,76,41,21,49,24,35,95,99,83,46,77,48
11,82,27,81,57,62,85,56,28,44,94,69,53,61,17,23,84,14,98
21,49,24,35,95,99,43,83,46,77,16,48,33,82,73,56,68,32,84,81,17,85,14
48,73,56,68,85,44,94,72,11
56,32,84,14,44,94,61,72,57,23,75
95,43,46,77,16,48,33,82,73,56,32,81,17,14,44,61,53
85,14,44,94,61,69,53,72,74,57,98,28,27,11,62,23,89,75,29,25,42,26,31
79,46,99,76,11,31,25,41,29
95,99,43,77,48,82,56,84,17,94,61
26,83,48,43,99,79,46,16,24,42,35,49,41,95,75,89,77
27,61,69,44,85
56,84,85,14,44,61,72,57,98,28,27,11,23,89,75
73,82,56,68,24,84,41,77,95,16,46,49,99,35,76,83,81,33,17,32,43,85,21
69,84,89,25,75,17,62,11,32,85,44,23,28,72,14
24,33,85,77,95,43,16,94,14,32,61
85,69,14,23,57,27,31,74,42,53,98,29,25,19,62,28,72,26,89,61,11
21,49,24,35,95,99,43,83,46,77,16,48,33,82,73,68,32,84,81,17,85,14,44
76,77,99,95,24,82,26,79,68,48,42,73,83
24,31,74,72,75,11,62,19,23,39,28,76,21,41,98,49,27,42,25,57,89,79,29
53,28,19,89,62,31,11,41,27,25,76
99,43,83,48,56,68,81,17,44,94,53
68,32,84,81,17,85,14,44,94,61,69,53,72,57,98,28,27,62,23,89,29
56,83,35,49,46,39,31,84,33
43,83,16,48,82,56,32,84,17,44,94,61,69,72,74
44,57,27,68,72,28,98,32,14,73,94,56,89,84,74,53,11
23,89,75,29,25,19,42,26,31,39,79,76,41,21,49,24,35,95,99,43,83,46,16
46,16,48,33,82,73,56,68,32,84,81,17,85,14,44,94,61,69,53,72,74,57,98
68,24,17,43,83,56,49,82,44,94,95,85,99,84,46,48,35,32,81,77,33,16,14
16,68,73,33,44,28,81,61,17,94,27,56,72,69,14
95,29,99,16,31,49,23
26,83,43,95,89
74,98,27,11,23,89,75,29,26
27,95,23,49,43,28,39
14,99,94,24,68,81,33,73,82,77,32,84,85,17,46,95,44,56,49,43,48,83,16
73,56,81,17,85,44,94,61,69,74,98,27,23
21,24,99,43,83,46,16,48,82,68,32,84,81,17,85,14,44
17,44,61,72,74,98,28,11,23,89,29,25,26
41,29,79,89,39,76,75,43,25,77,48,31,19,49,46,42,83
42,79,43,33,99,31,24,21,73,76,39,49,19,82,25,83,46,95,41
32,94,61,69,53,57,28,62,25
61,69,53,72,29,25,26,31,39,79,41
99,83,77,56,68,32,17,85,14,61,72
26,31,39,41,49,24,35,95,99,46,77,16,48,33,82,73,56,68,32
74,53,68,72,43,83,17,81,73,56,33,82,61
28,27,11,75,42,26,31,79,76,41,21,49,99
84,44,85,72,74,27,19,25,75,62,89
24,35,95,83,46,77,16,33,32,81,17
32,68,73,53,61,28,72,48,33,85,44,11,69,74,84,27,57,56,81,82,17
81,19,44,85,74,14,75,27,28,62,42,29,69,98,53,94,11,23,72
75,29,42,31,24,43,33
62,76,35,49,98,99,25,27,26,79,19,31,28,95,23
83,33,99,77,41,25,49,21,31
53,29,42,26,76
44,85,83,99,94,35,73,46,84,61,48,33,82,14,68,69,77,16,81,56,32,95,17
29,25,19,26,31,79,21,49,24,95,46,77,33
95,24,11,75,31,79,39,57,23
81,17,85,14,44,94,61,69,53,72,74,57,98,28,27,11,62,23,75,29,25,19,42
25,41,21,49,24,43,77,82,73
43,83,46,33,68,85,74
14,73,53,68,56,62,98,33,44,72,28,32,81,61,57,82,17,69,84,85,27,74,11
83,16,33,82,32,84,81,44,57
48,73,68,32,81,17,85,94,69,74,57,28,11
62,23,75,25,19,42,79,76,24,35,77
79,27,11,75,42,95,41,25,39,29,24,35,28,76,89,23,49,31,62,19,98,21,99
74,57,98,28,62,89,75,25,26,39,79,76,41,24,35
84,25,94,81,27,57,19,98,23
68,84,16,41,46,83,35,21,48,81,73,14,95,82,33
77,16,48,33,73,56,68,84,81,17,85,14,44,94,61,69,53,72,57,98,28
81,11,89,94,68,74,85
82,73,56,32,17,69,72,28,62
19,39,79,76,41,24,43,48,33,73,56
76,41,21,49,24,35,95,99,43,83,77,33,82,73,56,68,32,84,81,17,85
19,23,57,76,79,26,35,95,62,28,75,42,98,21,24
76,26,39,68,43,82,56,21,32
29,94,89,11,26,39,23,72,25,44,19,69,61,79,75
16,42,82,95,31,99,49
69,82,84,98,68,81,62,85,61,57,53,33,72,44,94
27,11,62,89,75,29,19,42,79,76,49,24,35,95,99,43,83
17,28,98,42,81,23,29,53,19
24,35,95,99,83,77,48,82,68,32,84,81,17,85,44,94,61
24,46,26,49,83,77,76,39,31
77,83,73,39,35,33,48,79,19,43,24,76,49,31,95,46,99,21,82,42,41,56,16
61,57,98,23,75,29,19,79,41
35,28,23,24,39,21,99,89,19,27,11,42,41,98,26,31,95,76,79
31,79,21,35,43,83,77,33,82,73,56,32,84
76,21,35,95,46,82,85
85,23,57,89,69,73,11,53,32,68,14,62,94,17,84,44,98,81,56,74,72
28,19,98,29,76,62,26,53,89,23,49,11,21
41,21,49,24,35,95,99,43,83,46,77,16,48,33,82,56,68,32,84,81,17,85,14
68,83,56,72,32,74,85,94,33,69,57,53,17,84,77,82,44,81,14,16,61,46,73
43,19,35,76,62,83,99,49,89,95,23,24,27,42,41,79,39
95,43,32,84,61
41,21,49,24,35,95,99,43,46,77,16,48,82,56,68,32,81,17,14
62,25,84,57,32,17,85
11,62,23,89,19,42,39,79,41,24,43,83,46
28,43,76,95,25,62,79,29,31,89,26
77,16,33,82,73,56,68,81,17,85,61,57,28
14,44,61,53,74,98,28,62,89,75,25,31,39
46,17,44,98,69,74,48,56,53,72,61,73,84,33,81,16,85,57,68,94,82,14,77
57,98,72,62,53,79,23,76,25,39,21,89,26,49,41,27,28,74,29,31,42
74,28,53,31,61,29,27,26,57
73,68,84,85,14,44,61,69,28,11,89
29,31,28,11,62,27,19,57,76,53,23,89,41,98,42,74,72,49,79,26,21,39,75
75,27,29,42,89,53,11,44,69,19,23,26,72,74,79,31,25,94,39,28,98,61,57
48,81,46,95,77,68,43,85,14,32,83,35,56,84,49,17,94,33,82
26,28,23,49,24,11,72,79,25
76,21,49,24,35,95,99,83,46,77,48,33,82,73,56,68,84,81,85
42,26,31,39,79,76,41,21,49,35,95,99,43,83,46,77,16,48,33,82,73,56,68
42,29,21,24,25,82,77,19,43,79,31,41,48
57,21,23,98,28,75,41,19,72,74,27
27,44,69,56,74,73,32,72,14,28,62,82,68,57,11,85,98,81,94,17,23
46,16,48,33,84,81,17,85,44,72,74,57,98
85,14,44,94,61,69,53,72,74,57,28,27,11,89,75,29,25,19,42
26,39,41,79,23,19,49,35,99,43,75,89,31,46,11
75,29,69,62,53,72,44,11,81,25,85,17,61,94,28,89,57
72,74,57,98,28,27,11,62,23,89,75,29,25,19,42,26,39,79,76,41,21,49,24
48,35,41,81,33,82,49,79,73,99,95,24,32,21,43,84,76,77,46,56,16
82,24,41,25,29,21,39,48,16,35,33,49,42,19,46
94,69,53,72,74,57,98,28,27,11,62,23,75,29,25,19,42,26,31,79,76
81,17,85,14,44,94,61,69,53,72,74,57,98,28,27,11,62,23,75,29,25,19,42
68,53,32,27,33,81,48,14,44,72,94,57,28,84,82,74,16,17,61,98,69,73,56
48,33,82,56,68,32,84,81,17,14,94,61,69,53,72,74,57,27,11
79,39,89,21,27,11,42,76,25,95,26,75,23,99,41
26,17,19,53,14,61,29,44,69,98,85,94,27,72,42,57,89,74,75,62,11,23,28
41,62,21,39,23,11,31,29,95
23,89,75,29,25,19,42,26,31,79,76,41,21,49,24,35,95,99,43,77,16
76,49,35,43,33,32,85
44,89,98,28,25,94,19,79,39,53,31
74,27,84,98,82,72,44,33,17,85,94,61,14,53,11,62,69,81,56,32,28,68,73
39,79,24,35,99,16,73,56,68,84,81
84,17,14,69,27,11,23
41,61,62,26,79,57,42,76,89,53,19
44,94,61,57,75,25,31,39,79
77,16,48,33,73,56,68,32,84,81,17,85,14,44,94,61,69,53,72,74,57,98,28
43,83,46,77,16,48,82,73,56,32,84,81,44,94,69,72,74
48,77,21,42,25,33,16
23,89,29,25,19,42,26,31,39,76,21,35,99,43,83,46,77
27,11,62,23,89,75,29,25,19,39,79,76,41,49,24,35,99,43,83
84,82,41,56,32,76,46,48,16,99,95,81,17,43,49,68,83,77,35
57,98,27,11,62,23,89,75,26,39,79,41,21
16,48,33,73,56,68,32,84,81,17,85,94,61,53,72,74,57,98,28
35,95,43,83,46,77,16,48,33,82,73,56,68,32,84,81,17,85,14,44,94,61,69
48,33,73,32,84,81,17,85,94,61,69,72,57,98,28,27,11
83,17,41,99,48,73,43,82,56,76,68,33,24
61,44,72,82,48,32,17
21,44,84,33,68,32,17
95,48,81,35,83,77,32,61,84
29,69,28,11,23,72,81,57,75,53,74,17,94,98,25,84,14,19,44,27,85
27,89,72,98,24,26,28,21,49,75,76,62,74
49,24,43,48,33,73,81,17,85,14,44
41,21,43,83,77,33,56
14,72,27,89,75,31,39
49,95,11,75,76,41,25,99,89,24,19,42,27,23,35,98,28
35,33,99,19,82,79,83,21,49,42,48,25,46,26,39,31,16
19,26,46,77,33,82,56
53,89,74,85,61,28,62,44,94,75,19,57,11,98,72,31,25,27,69,26,23
84,28,85,11,74,75,14,23,72,89,17
35,95,99,77,16,48,33,82,73,56,32,84,17,85,14,44,94,61,69
83,56,82,35,76,68,48,95,73,16,26,39,46,99,41
16,39,46,83,26,73,82,79,99,21,33
83,21,26,16,42,99,48,24,35,56,76,19,73,33,39,79,31
35,98,76,21,28,11,19,74,24,25,62
23,89,75,25,19,42,26,31,39,79,76,41,21,49,24,35,95,99,43,83,46,77,16
41,21,24,99,43,83,46,16,33,82,73,84,17,85,14
56,73,82,77,48,46,49,95,21,83,79,24,32,41,33,16,43,26,35,31,68,76,99
61,69,53,72,28,27,89,19,41
94,61,53,98,62,31,76
28,23,41,27,19,25,62,53,26,21,79,49,98,29,57,72,39
84,56,99,21,48,24,81,82,35,32,16,76,68,17,83,43,41
43,33,99,95,24,35,46,61,56
29,39,24,43,77,33,82
61,57,27,44,69,75,79,39,23,94,25,19,74,28,29,89,53
56,84,17,85,61,69,53,74,57,98,28,27,11,62,23,89,75
46,49,77,48,25,35,83,29,42,41,19,26,89,75,99,76,43,39,79,24,95,21,16
61,69,53,72,57,98,28,27,11,62,23,75,25,19,42,26,79,76,41
29,25,42,79,21,24,99,43,16,48,33
83,46,77,16,48,82,73,56,68,84,81,17,14,94,69,53,72,74,57
79,53,98,19,39,75,62,42,25,49,23,11,28,31,21
73,56,68,17,14,44,94,61,69,53,72,74,57,28,27,11,62,23,89
23,75,29,61,62,25,69,72,32,44,89,84,74,28,53
82,73,56,68,32,84,81,85,94,61,69,72,57,28,27,11,62
73,82,95,42,41,26,43,24,31,35,79,25,33,76,19,77,39,16,99
42,28,31,98,25,27,26,23,49,99,75,79,19,62,89,29,41,76,39,11,95
35,31,27,21,24,25,19,75,43,11,49,23,76,28,99
79,31,48,42,99,95,43,77,21,33,83,16,26,46,24,35,76,68,56
95,83,49,24,73,77,48,68,16,82,43,84,44,32,35
84,81,17,14,94,69,53,72,74,57,27,62,23,89,29,25,19
33,82,56,68,32,84,81,14,44,94,61,53,74,57,98,28,62
48,33,82,56,32,84,81,17,85,14,44,69,72,98,28
72,84,68,61,99,77,33,32,48
29,25,19,31,39,79,21,49,24,95,99,43,46,77,16,33,82
62,23,29,19,42,26,49,99,83
31,39,79,41,21,35,99,46,73
24,46,49,29,39,19,35,21,48,77,99,26,83,31,82,41,76,33,25,16,95,42,79
77,79,16,82,56,48,33,35,21,43,24,46,99,68,41,49,83,84,95
42,26,31,39,79,76,41,49,24,35,99,43,83,46,77,16,48,33,73,56,68
19,44,72,23,94,14,74,62,42,85,27,61,29,28,11,89,75,57,25,81,69,53,98"""

let parsed = parse input

let validJobs = getCorrectedJobs parsed

let middlePages = validJobs |> List.map getMiddlePage

let sumOfMiddlePages = middlePages |> List.sum

printfn "%i" sumOfMiddlePages
