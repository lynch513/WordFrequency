open System.IO
open System


module IO =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Words =
    let private splitString (x: string) =
        x.Split([|','; ' '; ':'; '!'; '.'|])

    let private trimString (x: string) =
        x.Trim ()

    let wordsFromSeq (xs: string seq) =
        xs
        |> Seq.collect (splitString >> Array.map trimString)

    let wordsFromString (x: string) =
        x
        |> splitString
        |> Array.map trimString
        |> Seq.ofArray

    let private seq2FrequencyMap xs =
        Seq.fold (fun (acc : Map<_, int>) i ->
            if Map.containsKey i acc
            then Map.add i ((Map.find i acc) + 1) acc
            else Map.add i 1 acc)
            Map.empty
            xs

    let private map2SorterList xs =
        xs
        |> Map.toList
        |> List.sortWith (fun (k1, v1) (k2, v2) -> -compare v1 v2) 

    let countFrequencyAndSort (xs: string seq) =
        xs
        |> seq2FrequencyMap
        |> map2SorterList
        |> List.filter (fun (k, v) -> k.Length > 3)

    let takeMostFrequencyWordsFromString (x: string) =
            x 
            |> wordsFromString
            |> countFrequencyAndSort

    let takeMostFrequencyWordsFromSeq (xs: string seq) =
            xs 
            |> wordsFromSeq
            |> countFrequencyAndSort


let application filePath wordCount =
    let wordsFreq = 
        filePath
        |> IO.readLines
        |> Words.takeMostFrequencyWordsFromSeq
        |> Seq.take wordCount
    printfn "%i most common words in %s:" wordCount filePath
    for (word, freq) in wordsFreq do
        printfn "%s: %i" word freq


[<EntryPoint>]
let main argv =
    let filePath = if argv.Length > 0 then Some(argv.[0]) else None 
    let wordCount = if argv.Length > 1 then Some(argv.[1]) else None
    let defaultWordCount = 10
    
    try
        match filePath, wordCount with
        | Some(filePath), None -> 
            application filePath defaultWordCount
        | Some(filePath), Some(wordCount) -> 
            match Int32.TryParse wordCount with
            | true, d -> 
                application filePath d
            | false, _ -> printfn "Argument wordCount must be integer"
        | _, _ -> printfn "Usage: dotnet run -- <filePath> [wordCount]"
    with 
    | _ -> printfn "Argument <filePath>: file not found or input illegal path" 
    0 // return an integer exit code