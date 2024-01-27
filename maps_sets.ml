(* define a string -> 'a map *)
module StrMap = Map.Make(
  struct
    let compare = String.compare
    type t = string
  end )

(* define a string set *)
module StrSet = Set.Make(
  struct
    let compare = String.compare
    type t = string
  end )

(* adding things to a map, where adding an existing key overwrites an existing mapping for the key *)
let maptest1 () =
  let m = StrMap.empty in           (* m = {} *)
  let m = StrMap.add "a" 1 m in     (* m = {"a":1} *)
  let m = StrMap.add "b" 2 m in     (* m = {"a":1, "b":2} *)
  let m = StrMap.add "a" 3 m in     (* m = {"a":3, "b":2} *)
  m

(* looking stuff up in the map : call with map from maptest1 *)
let maptest2 m =
  let b = StrMap.exists (fun k _ -> k = "a") m in  (* is "a" in the domain? *)
  let x = if b then StrMap.find "a" m else -1 in   (* if it is, do an unsafe find that may raise exception *)
  let y = StrMap.find_opt "b" m in                 (* safe find for b : should be Some 2 *)
  let z = StrMap.find_opt "c" m in                 (* safe find for c : should be None *)
  (x,y,z)

(* iterating over a map, assumes int StrMap.t *)
let maptest3 m =
  (* walk the map and accumulate the values that the keys map to *)
  StrMap.fold (fun _ v acc -> acc + v) m 0
