// Learn more about F# at http://fsharp.org


// Learn more about F# at http://fsharp.org

open System
open System.IO

type Position = int

type OPT =
  | OPER of (int -> int -> int)
  | HALT

let collectResult (res1 : Result<'T list,'E>) (res2 : Result<'T,'E>) =
  match res1, res2 with
    | (Ok(ls),Ok(h)) -> Ok(h::ls)
    | (Error(s), _) -> Error(s)
    | (_ , Error(s)) -> Error(s)

let parseint (str : String) =
  match Int32.TryParse str with
    | (true, int64) -> int64 |> Ok
    | _ -> sprintf "Unable to parse %s" str |> Error


let opcode (pos : int) (arr : int[]) =  
  match arr.[pos] with
    | 1 when arr.Length > pos + 3   -> OPER (+) |> Ok
    | 2 when arr.Length > pos + 3 -> OPER (*) |> Ok
    | 99 -> HALT |> Ok
    | _ -> "Invalid op code provided" |> Error

let (|VALID|_|) (array : 't[]) pos =
  if pos >= 0 && pos < array.Length then
    Some(pos)
  else
    None

let applyOperator oper position (array : int[]) =
  let (x,y,z) = (array.[position+1], array.[position+2], array.[position+3])
  match (x,y,z) with
    | (VALID array x, VALID array y, VALID array z) ->
        array.[z] <- oper (array.[x]) (array.[y])
        true
    | _ -> false
  

let rec processCodes (position : int) (array : int[]) =  
  match (opcode position array) with
    | Ok(OPER(fn)) ->
      if applyOperator fn position array then
        processCodes (position + 4) array
      else
        Error("Invalid ops specified")
    | Ok(HALT) -> array |> Ok
    | Error(s) -> Error(s)


let runComputer array =
  processCodes 0 array


let find1And2 (array : int[]) =
  let len = array.Length
  let rec findIt i j =
    if j = len then findIt (i+1) 0
    elif i = len then None
    else
      let tempArr = Array.copy array
      tempArr.[1] <- i
      tempArr.[2] <- j
      match runComputer tempArr with
        | Ok(arr) when arr.[0] = 19690720 -> Some(i,j)
        | _ -> findIt i (j+1)      
  findIt 0 0



let runProgram array = 
  processCodes 0 array
  |> Result.map(fun a -> String.Join(',', a))

  
[<EntryPoint>]
let main argv =
    System.IO.File.ReadAllText("input.txt").Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseint
    |> List.ofArray
    |> List.fold collectResult (Ok([]))
    |> Result.map(List.rev >> Array.ofList)
    |> Result.map(find1And2)
    |> printfn "%A"

  

    0 // return an integer exit code


