module InputHelper

open System

let inputToIntList() : int list =
    let list = Console.ReadLine()
    list.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> Array.toList

let inputLinesToIntList() : int list =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> not (String.IsNullOrWhiteSpace line))
    |> Seq.map int
    |> Seq.toList

let inputToDouble() : double =
    let rec readValue() =
        match Double.TryParse(Console.ReadLine()) with
        | true, value -> value
        | false, _ -> 
            printfn "Invalid input. Please try again."
            readValue()
    readValue()

let inputToInt() : int=
    let rec readValue() =
        match Int32.TryParse(Console.ReadLine()) with
        | true, value -> value
        | false, _ ->  
            printfn "Invalid input. Please try again."
            readValue()
    readValue()

