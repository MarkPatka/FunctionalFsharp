module IntroductionModule

open System

let repeatElements (N: int) (elements: int list) : int list =
    elements |> List.collect (fun element -> List.init N (fun _ -> element))

let filterArray (N: int) (elements: int list) : int list =
    elements |> List.filter (fun el -> N > el)

let removeOddIndexes (elements: int list) : int list =
    elements
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (i, _) -> i % 2 = 0)
    |> List.map snd

let createArrayOfLength (N: int) : int list =
    let rnd = Random()
    [for _ in 1..N -> rnd.Next() ]

let reverseList (elements: int list) : int list =
    [for i in (elements.Length - 1) .. -1 ..0 do
        yield elements.[i]]

let countOddElements (elements: int array) : int =
    elements
    |> Array.foldBack (fun x acc -> if x % 2 <> 0 then x + acc else acc) <| 0

let countLength (elements: int array) : int =
    Array.fold (fun acc _ -> acc + 1) 0 elements

let updateAbsValues (elements: int array) : int array =
    elements
    |> Array.map (fun x -> abs x)


