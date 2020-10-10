let split (str: string) = Array.toList (str.ToCharArray ())

let isIncreasing (password: int) = 

    let isBiggerOrSame (prev, isTrue) current = 
        (current, isTrue && prev <= current) 
    
    string password
    |> split
    |> List.map int
    |> List.fold isBiggerOrSame (0, true)
    |> snd

let hasPair (password: int) =

    let rec find adjasent pass =
        match pass with
        | [] -> adjasent
        | a :: b :: _ when a = b -> true
        | _ :: tail -> find adjasent tail

    string password
    |> split
    |> find false

let hasOnlyEvenRepeatings (password: int) =
    
    let rec find adjasent (pass: char list) =
        match pass with
        | [] -> adjasent
        | a :: b :: c :: d :: e :: f :: g :: _ when a = b && b = c && c = d && d = e && e = f && f = g -> true
        | a :: b :: c :: d :: e :: f :: _ when a = b && b = c && c = d && d = e && e <> f -> false
        | a :: b :: c :: d :: e :: _ when a = b && b = c && c = d && d <> e ->  true
        | a :: b :: c :: d :: _ when a = b && b = c && c <> d -> false
        | a :: b :: c :: _ when a = b && b <> c -> true
        | _ :: tail -> find adjasent tail

    string password
    |> split
    |> find true 

let passConditions pass =
    
    let apply f (pass, passes) = (pass, passes && f pass)
    
    (pass, true)
    |> apply hasPair
    |> apply isIncreasing
    |> apply hasOnlyEvenRepeatings
    |> snd


[<EntryPoint>]
let main _ = 
    printfn 
        "%d" ([172930 .. 683082] |> List.filter passConditions |> List.length) 
    0