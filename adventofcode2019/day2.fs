module day2

open System
open System.Numerics

let replace_el target new_val arr =
    Array.mapi (fun i old_val -> if i = target then new_val else old_val) arr

let rec run_commands intcode pos =
    let opcode = Array.get intcode pos
    let op = 
        match opcode with
            | 99 -> None
            | 1 -> Some (+)
            | 2 -> Some (*)
    match op with
        | None -> intcode
        | Some op ->
            let new_val = op (Array.get intcode (Array.get intcode (pos + 1))) (Array.get intcode (Array.get intcode (pos + 2)))
            let target = Array.get intcode (pos + 3)
            run_commands (replace_el target new_val intcode) (pos + 4)

let func intcode (final_verb, final_noun) (verb, noun) =
    let restored = replace_el 1 verb (replace_el 2 noun intcode)
    let initial = run_commands restored 0
    if (Array.get initial 0) = 19690720 then  (verb, noun)
    else (final_verb, final_noun)
    
let cross xs ys = seq { for x in xs do for y in ys -> x, y }

let find_verb_noun intcode =
    let result = Array.fold (func intcode) (0, 0) (Array.ofSeq (cross [0..99] [0..99]))
    result

// [<EntryPoint>]
let main argv = 
    let lines = IO.File.ReadAllLines @"..\..\inputs\2-small.in" |> List.ofSeq |> System.String.Concat
    System.Console.WriteLine ("{0}", lines)
    let initial = lines.Split([|','|], System.StringSplitOptions.None) |> Array.map int
    let restored = replace_el 1 12 (replace_el 2 2 initial)
    System.Console.WriteLine ("part 1: {0}", Array.get (run_commands restored 0) 0)
    System.Console.WriteLine ("part 2: {0}", (find_verb_noun initial))
    0
