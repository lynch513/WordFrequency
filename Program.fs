open System.IO

let readLines (filePath : string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitString (x: string) =
    x.Split([|','; ' '; ':'; '!'; '.'|])

let trimString (x: string) =
    x.Trim ()

let wordSeq (xs: string seq) =
    xs
    // |> Seq.collect (splitString >> Array.map trimString)
    |> Seq.collect (fun (x: string) -> x.Split([|','; ' '; ':'; '!'; '.'|])) 
    |> Seq.map (fun (x: string) -> x.Trim ())

let seq2FrequencyMap xs =
    Seq.fold (fun (acc : Map<_, int>) i ->
        if Map.containsKey i acc
        then Map.add i ((Map.find i acc) + 1) acc
        else Map.add i 1 acc)
        Map.empty
        xs

let map2SorterList xs =
    xs
    |> Map.toList
    |> List.sortWith (fun (k1, v1) (k2, v2) -> -compare v1 v2) 

let takeMostFrequencyWordsFromFile filePath wordCount =
        readLines filePath 
        |> wordSeq
        |> seq2FrequencyMap
        |> map2SorterList
        |> List.filter (fun (k, v) -> k.Length > 3)
        |> Seq.take wordCount


[<EntryPoint>]
let main argv =
    printfn "10 most common words in Gamlet:"
    for (word, freq) in takeMostFrequencyWordsFromFile @"./Gamlet.txt" 10 do
        printfn "%s: %i" word freq
    0 // return an integer exit code