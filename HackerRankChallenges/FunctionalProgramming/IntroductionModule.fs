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


let calculateSerialExpansion (N: float) =
    let rec factorial = function
        | 0L | 1L -> 1L
        | k -> (int64 k) * factorial(k - 1L)

    let terms =
        [0..9]
        |> List.map (fun k ->
            let power = float N ** float k
            let fact = float (factorial (int64 k))
            power / fact)

    List.sum terms

let serialExpansion (elements: float list) =
    elements |> List.map calculateSerialExpansion

let defineIntegral (factors: int list) (powers: int list) (left: float) (right: float) : float * float =
    let calcSubintervalLength (L: float) (R: float) (dx: float) : int =
        int ((R - L) / dx) + 1

    let defineFunction (x: float) : float =
        List.zip factors powers
        |> List.sumBy (fun (ai, bi) -> float ai * (x ** float bi))

    let integrate (f: float -> float) (L: float) (R: float) (dx: float) : float=
        let N = calcSubintervalLength L R dx
        let xVals = [for i in 0..N - 1 -> L + float i * dx]

        let total =
            xVals 
            |> List.skip 1
            |> List.take (N - 2)
            |> List.sumBy f

        (dx / 2.0) * (f L + 2.0 * total + f R)

    let area = integrate defineFunction left right 0.001
    
    let squareFunction x = (defineFunction x) ** 2.0
    let volume = Math.PI * integrate squareFunction left right 0.001
    
    (area, volume)
