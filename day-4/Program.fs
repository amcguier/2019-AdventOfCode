// Learn more about F# at http://fsharp.org

open System


let (|INCREASING|DECREASING|) num =
  let rec strIncr lst =
    match lst with
      | a::b::rem ->
          if (a <= b) then strIncr (b::rem)
          else DECREASING
      | _ -> INCREASING

  num
  |> strIncr

let (|DOUBLE|NOTDOUBLE|) num =
  let rec dbleUp hasDble curr  =
    match curr with
    | a::b::c::d::e::f::rem when  a = b && b = c && c = d && d = e && e = f -> dbleUp hasDble rem
    | (a :: b :: c :: d::  e :: rem) when a = b && b = c && c = d && d = e -> dbleUp hasDble rem
    | (a :: b :: c :: d :: rem) when a = b && b = c && c = d  -> dbleUp hasDble rem
    | (a :: b :: c  :: rem) when a = b && b = c -> dbleUp hasDble rem
    | (a :: b :: rem ) when a = b -> DOUBLE
    | (a :: rem) -> dbleUp hasDble rem
    | [] -> if hasDble then DOUBLE else NOTDOUBLE
    
  dbleUp false num
  

let listify (x : int) =
  x.ToString().ToCharArray()
  |> List.ofArray

[<EntryPoint>]
let main argv =
  let startNum = 178416
  let endNum = 676461

  seq { for i in [startNum..endNum] do yield i}
  |> Seq.map listify
  |> Seq.sumBy (function | INCREASING & DOUBLE -> 1 | _ -> 0)
  |> printfn "%i"
  0 // return an integer exit code
