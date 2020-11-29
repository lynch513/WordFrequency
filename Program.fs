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


[<EntryPoint>]
let main argv =
    let defaultWordCount = 10

    let filePath = if argv.Length > 0 then Some(argv.[0]) else None 
    let wordCount = 
        match argv with
        | _ when argv.Length > 1 ->
            match Int32.TryParse argv.[1] with
            | true, d -> Some d 
            | false, _ -> None 
        | _ -> Some defaultWordCount

    try
        match filePath with
        | Some(filePath) -> 
            match wordCount with
            | Some d -> 
                printfn "%i most common words in %s:" d filePath
                filePath
                |> IO.readLines 
                |> Words.takeMostFrequencyWordsFromSeq 
                |> Seq.take d
                |> Seq.iter (fun (word, freq) -> printfn "%s: %i" word freq)
            | _ -> printfn "Argument [wordCount] must be integer"
        | _ -> printfn "Usage: dotnet run -- <filePath> [wordCount]"
    with 
    | _ -> printfn "Argument <filePath>: file not found or input illegal path" 
    0 // return an integer exit code