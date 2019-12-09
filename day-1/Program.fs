// Learn more about F# at http://fsharp.org

open System
open System.IO




let parse64 (str : String) =
  match Int64.TryParse str with
    | (true, int64) -> Result.Ok(int64)
    | _ -> sprintf "Unable to parse %s" str |> Result.Error

let rec fuelCalculationRec agg num =
  // note integer division will handle the rounding here
  let res = (num / 3L) - 2L
            |> function | x when x > 0L -> x | _ -> 0L

  if res > 0L then
    fuelCalculationRec (agg+res) res
  else
    agg
    
let fuelCalculation num =
  // note integer division will handle the rounding here
  (num / 3L) - 2L
  |> function | x when x > 0L -> x | _ -> 0L
  

let binder (result : Result<int64,string>) (result2 : Result<int64, string>) =
  result |> Result.bind (fun x -> Result.map ((+) x) result2)



[<EntryPoint>]
let main argv =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map parse64
    |> Array.map (Result.map (fuelCalculationRec 0L)) 
    |> Array.fold binder (Ok(0L))
    |> function | Ok(n) -> printfn "we need %i fuel" n | Error(s) -> printfn "c%s" s
    0 // return an integer exit code
