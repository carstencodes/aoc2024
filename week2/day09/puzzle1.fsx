type FileId = int

type FileSystemEntry = 
    | Free
    | File of FileId

type FileSystem = FileSystemEntry seq

let printFileSystem (fs: FileSystem) =
    for f in fs do
        let printChar = 
            match f with
            | Free -> "."
            | File v -> 
                if v < 10
                then sprintf "%i" v
                else sprintf "F{%i}" v
        printf "%s" printChar
    
    printfn ""

let parse (t: string) =
    let parseChar (c: char) =
        let s = sprintf "%c" c
        s |> int

    let fileSystem = t.ToCharArray() |> Seq.map parseChar

    printfn "%A" (fileSystem |> Seq.toArray)

    let mutable fileId = 0
    let mutable fileEntry = File 0

    let parseFile length =
        let nextFileEntry = 
            match fileEntry with
            | Free -> 
                fileId <- fileId + 1
                File fileId
            | File _ -> Free

        let mutable ctr = length
        let mutable entries = []
        while (ctr > 0) do
            entries <- entries @ [fileEntry]
            ctr <- ctr - 1         
        fileEntry <- nextFileEntry

        printfn "%i %A" length entries

        entries

    fileSystem |> Seq.map parseFile |> Seq.concat

let example = "2333133121414131402"
let input = ""

let fileSystem = parse example |> Seq.toList

printFileSystem fileSystem
