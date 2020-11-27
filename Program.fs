open System.IO

module IO =
    let readLines (filePath : string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Words =
    let splitString (x: string) =
        x.Split([|','; ' '; ':'; '!'; '.'|])

    let trimString (x: string) =
        x.Trim ()

    let wordSeq (xs: string seq) =
        xs
        |> Seq.collect (splitString >> Array.map trimString)

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

    let takeMostFrequencyFromSeq (xs: string seq) =
            xs 
            |> wordSeq
            |> seq2FrequencyMap
            |> map2SorterList
            |> List.filter (fun (k, v) -> k.Length > 3)


[<EntryPoint>]
let main argv =
    printfn "10 most common words in Gamlet:"
    let mostFrequencyWordsList = 
        @"./Gamlet.txt"
        |> IO.readLines
        |> Words.takeMostFrequencyFromSeq 
        |> Seq.take 10
    for (word, freq) in mostFrequencyWordsList do
        printfn "%s: %i" word freq
    0 // return an integer exit code