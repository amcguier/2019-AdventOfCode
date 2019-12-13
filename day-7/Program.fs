open System
open System.IO

//let outputFile = StreamWriter("output.txt")


type Position = int

type ParamMode =
  | IMMEDIATE
  | POSITION

type OPT =
  | OUTPUT
  | INPUT
  | OPER of (int -> int -> int)
  | HALT
  | JTRUE
  | JFALSE
  | LESSTHAN
  | EQUALS

type Instruction = OPT * ParamMode list

let collectResult (res1: Result<'T list, 'E>) (res2: Result<'T, 'E>) =
  match res1, res2 with
  | (Ok(ls), Ok(h)) -> Ok(h :: ls)
  | (Error(s), _) -> Error(s)
  | (_, Error(s)) -> Error(s)

let parseint (str: string) =
  match Int32.TryParse str with
  | (true, int64) -> int64 |> Ok
  | _ -> sprintf "Unable to parse %s" str |> Error


let rec charsToParams accumed lst =
  match lst with
  | a :: b :: [] -> accumed
  | [] -> accumed
  | '1' :: rem -> charsToParams (IMMEDIATE :: accumed) rem
  | x :: rem -> charsToParams (POSITION :: accumed) rem


let splitOptNum x =
  let chars = x.ToString().ToCharArray()
  if chars.Length < 1 then
    Error("Invalid instruction specified")
  elif chars.Length < 2 then
    Ok(x, [||])
  else
    let code = (string chars.[chars.Length - 2] + string chars.[chars.Length - 1]) |> Int32.Parse

    let parms =
      chars
      |> List.ofArray
      |> charsToParams []
      |> Array.ofList
    Ok(code, parms)

let opcode (pos: int) (arr: int []) =
  let opMap =
    function
    | (1, prms) when arr.Length > pos + 3 -> (OPER (+), prms) |> Ok
    | (2, prms) when arr.Length > pos + 3 -> (OPER (*), prms) |> Ok
    | (3, prms) when arr.Length > pos + 1 -> (INPUT, prms) |> Ok
    | (4, prms) when arr.Length > pos + 1 -> (OUTPUT, prms) |> Ok
    | (5, prms) when arr.Length > pos + 2 -> (JTRUE, prms) |> Ok
    | (6, prms) when arr.Length > pos + 2 -> (JFALSE, prms) |> Ok
    | (7, prms) when arr.Length > pos + 2 -> (LESSTHAN, prms) |> Ok
    | (8, prms) when arr.Length > pos + 2 -> (EQUALS, prms) |> Ok
    | (99, prms) -> (HALT, prms) |> Ok
    | (code) -> sprintf "Invalid op code provided: code %A\toriginal: %i" code (arr.[pos]) |> Error

  arr.[pos]
  |> splitOptNum
  |> Result.bind opMap


let (|VALID|_|) (array: 't []) pos =
  if pos >= 0 && pos < array.Length then Some(pos)
  else None

let modeFrom (paramModes: ParamMode []) offset =
  let index = offset - 1
  if paramModes.Length > index then paramModes.[index]
  else POSITION

let readValue array pos mode =
  match (pos, mode) with
  | (VALID array x, IMMEDIATE) -> Ok(array.[pos])
  | (VALID array x, POSITION) ->
    match array.[pos] with
    | VALID array y -> Ok(array.[y])
    | _ -> Error("Invalid read parameter")
  | _ -> Error("Invalid read parameter")

let getWritePosition array pos mode =
  match (pos, mode) with
  | (VALID array x, POSITION) ->
    match array.[x] with
    | VALID array output -> Ok(output)
    | _ -> Error("Invalid write parameter")
  | _ -> Error("Invalid write parameter")

let applyOperator oper modes position (array: int []) =
  let r1 = readValue array (position + 1) (modeFrom modes 1)
  let r2 = readValue array (position + 2) (modeFrom modes 2)
  let writePos = getWritePosition array (position + 3) (modeFrom modes 3)
  match (r1, r2, writePos) with
  | (Ok(x), Ok(y), Ok(z)) ->
    array.[z] <- oper (x) (y)
    true
  | x -> printfn "invalid operator positions and values: %A oper: %A  modes: %A" x oper modes; false

// icky mutable stuff
let readInputFromConsole () =
  let mutable parseResult = (false, -99)
  while not (fst parseResult) do
    printfn "Please enter your integer intput"
    parseResult <- (Console.ReadLine() |> Int32.TryParse)
  snd parseResult

let applyInput inputFun array position modes =
  getWritePosition array (position + 1) (modeFrom modes 1)
  |> Result.map (fun x -> array.[x] <- inputFun())
  |> function
  | Ok(_) -> true
  | Error(_) -> false


let applyOutput outputFun array position modes =
  match readValue array (position + 1) (modeFrom modes 1) with
  | Ok(vl) ->
    outputFun vl
    true
  | _ -> false

let applyJump array position modes operator =
  let compare = readValue array (position + 1) (modeFrom modes 1)
  let newInstruction = readValue array (position + 2) (modeFrom modes 2)
  match (compare,newInstruction) with
    |(Ok(x),Ok(ptr)) ->
      if operator x then
        Ok(ptr)
      else
        Ok( position + 3 )
    | x -> sprintf "Invalid jump setup %A" x |> Error

let applyCompare array position modes operator =
  let first = readValue array (position + 1) (modeFrom modes 1)
  let second = readValue array (position + 2) (modeFrom modes 2)
  let write = getWritePosition array (position + 3) (modeFrom modes 3)
  match (first,second,write) with
    | (Ok(x),Ok(y),Ok(pos)) ->
      if operator first second then
        array.[pos] <- 1
      else
        array.[pos] <- 0
      true
    | x -> printfn "invalid compare %A" x; false
  
let rec processCodes inputFun outputFun  (position: int) (array: int []) =
  let loop = processCodes inputFun outputFun
  let instruction = (opcode position array)
//  sprintf "%A" instruction |> outputFile.WriteLineAsync |> ignore
  match instruction with
  | Ok((OPER(fn), prms)) ->
    if applyOperator fn prms position array then loop (position + 4) array
    else Error("Invalid ops specified")
  | Ok(INPUT, prms) ->
    if applyInput inputFun array position prms then loop (position + 2) array
    else Error("Invalid input instruction specified")
  | Ok(OUTPUT, prms) ->
    if applyOutput outputFun array position prms then loop (position + 2) array
    else Error("Invalid output instruction specified")
  | Ok(JTRUE,prms) ->
      match applyJump array position prms ( (<>) 0) with
        | Ok(vl) -> loop vl array
        | _ -> Error("Invalid jump instruction")
  | Ok(JFALSE,prms) ->
      match applyJump array position prms ( (=) 0) with
        | Ok(vl) -> loop vl array
        | _ -> Error("Invalid jump instruction")
  | Ok(LESSTHAN, prms) ->
      if applyCompare array position prms (<) then
        loop (position + 4) array
      else Error("Invalid less than provided")
  | Ok(EQUALS,prms) ->
      if applyCompare array position prms (=) then
        loop (position + 4) array
      else Error("Invalid equals provided")              
  | Ok(HALT, _) -> array |> Ok
  | Error(s) -> Error(s)

let printOutput a =
  printfn "%A" a

let runComputer inputFun outputFun array = processCodes inputFun outputFun 0 array


let find1And2 (array: int []) =
  let len = array.Length

  let rec findIt i j =
    if j = len then
      findIt (i + 1) 0
    elif i = len then
      None
    else
      let tempArr = Array.copy array
      tempArr.[1] <- i
      tempArr.[2] <- j
      match runComputer readInputFromConsole printOutput  tempArr with
      | Ok(arr) when arr.[0] = 19690720 -> Some(i, j)
      | _ -> findIt i (j + 1)
  findIt 0 0



let runProgram inputFun outputFun array = processCodes inputFun outputFun 0 array |> Result.map (fun a -> String.Join(',', a))


/////////////////////////////////////////
// List permutations from              //
// https://stackoverflow.com/a/3129136 //
/////////////////////////////////////////

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

///////////////////////////
// End list permtuations //
///////////////////////////

let generateInput lst =
  let mutable x =
    seq {
      for i in lst do
      yield i
      }
  (fun () -> let item = Seq.head x
             x <- Seq.tail x
             item)


let mutable output = 0

let writeAccumulator x =
  output <- x

let signalInputGenerator phase signal =
  let mutable toggle = false;  
  fun  () -> toggle <- not toggle; if toggle then phase else signal
  

let rec signalProgram prog (lst : int list) (input : int) =
  match lst with
    | [] -> output
    | (a::rem) -> runComputer (signalInputGenerator a input) writeAccumulator prog
                  |> ignore
                  signalProgram prog rem output
                  
[<EntryPoint>]
let main argv =
  let inputs = [0;1;2;3;4;] |> permute
  
  let prog =
    System.IO.File.ReadAllText("input.txt").Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseint
    |> List.ofArray
    |> List.fold collectResult (Ok([]))
    |> Result.map (List.rev >> Array.ofList)
    |> function | Ok(prog) -> prog
                | Error(e) -> failwith e

  let outputs = 
    inputs
    |> List.map (fun x -> signalProgram prog x 0)
 

  let maxValue =
    outputs
    |> Seq.max

  let maxIndex = outputs |> Seq.findIndex ((=) maxValue)

  inputs.Item maxIndex  
  |> fun x -> printfn "max output is %A for max signal value of %i" x maxValue 
  
  0
 

