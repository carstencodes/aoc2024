type SingleValue = int array
type TestValue = int * SingleValue
type TestValues = TestValue array

type Operator =
    | Add
    | Multiply

type OperatorArray = Operator array

type Solution = TestValue * OperatorArray

let add a b = a + b
let multiply a b = a * b

let evaluate operator a b =
    match operator with
    | Add -> add a b
    | Multiply -> multiply a b

let isValidSolution operators result values =
    let mutable operatorsLeft = operators
    let mutable valuesLeft = values |> Array.tail
    let mutable currentValue = values |> Array.head

    let operatorsCount = operatorsLeft |> Array.length
    let valuesCount = valuesLeft |> Array.length

    let mutable isSolution = (operatorsCount = valuesCount)

    while (isSolution && not(operatorsLeft |> Array.length > 0)) do
        let nextValue = valuesLeft |> Array.head
        let nextOperator = operatorsLeft |> Array.head

        valuesLeft <- valuesLeft |> Array.tail
        operatorsLeft <- operatorsLeft |> Array.tail

        currentValue <- evaluate nextOperator currentValue nextValue
        isSolution <- (currentValue <= result)

    isSolution <- isSolution && (result = currentValue)

    isSolution

let rec getPossibleSolutions length: OperatorArray array =
    let mutable result: OperatorArray array = [||]

    if length > 0
    then 
        let operations = getPossibleSolutions (length - 1)
        for operator in [Add; Multiply] do
            let operatorArray = [|operator|]
            for operation in operations do
                let newOperations = Array.concat [operatorArray; operation]
                result <- Array.append result [|newOperations|]

    printfn "%A %i" result length

    result
        

let findSolution (testValue: TestValue) : Option<Solution> =
    let mutable firstSolution = None
    let result, values = testValue

    let mutable solutions = getPossibleSolutions (values |> Array.length)

    printfn "%A" solutions

    let isValidSolutionForProblem solution = isValidSolution solution result values

    while (firstSolution = None && (solutions |> Array.length) > 0) do
        let solution = solutions |> Array.head
        solutions <- solutions |> Array.tail

        if (isValidSolutionForProblem solution)
        then
            firstSolution <- Some(testValue, solution)

    firstSolution

let findSolutions (testValues: TestValues) =
    testValues |> Array.map findSolution |> Array.choose id

let parse (t: string) = 

    let parseSingleValues (t: string) =
        let items = t.Split ' '

        let result = items |> Array.map int

        result

    let lineToTestValue (t: string) =
        let resultAndTestValue = t.Split ": "
        assert (resultAndTestValue |> Array.length > 0)
        let result = resultAndTestValue.[0] |> int
        let values = resultAndTestValue.[1] |> parseSingleValues

        result,  values

    let lines = t.Split System.Environment.NewLine
    let values = lines |> Array.map lineToTestValue

    values

let example = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

let input = """"""

let parsed = parse example

let solved: Solution array = findSolutions parsed

printfn "%i" (solved |> Array.length)
