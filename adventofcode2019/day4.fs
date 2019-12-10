module day4

open System

let check_pair state values =
    let fst = Seq.head values
    let snd = Seq.head (Seq.tail values)
    let (result: bool option) =
        match state with
            | None -> None
            | Some(st) -> 
                if fst > snd then None
                elif fst = snd then Some(true)
                else Some(st)
    result
    
let func (nums: Map<char, int>) (num: char) =
    Map.add num (match Map.tryFind num nums with
                     | None -> 1
                     | Some(el: int) -> el + 1)
                 nums
    
let has_two_adjacent_numbers (value: string) =
    Seq.fold func Map.empty<char, int> value |> Map.fold (fun state key value -> if value = 2 then true else state) false

let check_password (value: string) =
    let values = Seq.windowed 2 value
    let part1 =
            match Seq.fold check_pair (Some(false)) values with
                | Some(true) -> Some(value)
                | _ -> None
    part1
    
let check_password_part2 (value: string) =
    match check_password value with
        | None -> None
        | Some(_) ->
                match has_two_adjacent_numbers value with
                    | true -> Some(value)
                    | false -> None
    
[<EntryPoint>]
let main argv =
    let lines = IO.File.ReadAllLines @"..\..\inputs\4-small.in"
    let input = Seq.ofArray ((Seq.item 0 lines).Split([| '-' |]))
    let range_start = int (Seq.head input)
    let range_end = int (Seq.head (Seq.tail input))
    let range = [range_start .. range_end]
    let part1 = Seq.fold (fun correct num ->
                     match check_password (string num) with
                         | None -> correct
                         | Some(value) -> Seq.append correct [value])
                     Seq.empty<string> range
                     
    let part2 = Seq.fold (fun correct num ->
                     match check_password_part2 (string num) with
                         | None -> correct
                         | Some(value) -> Seq.append correct [value])
                     Seq.empty<string> range
     
    System.Console.WriteLine ("{0}", (Seq.length part1))
    printf "%A" part2
    System.Console.WriteLine ("{0}", (Seq.length part2))
    0
