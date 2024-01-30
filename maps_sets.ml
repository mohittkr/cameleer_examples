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

(* list-based map: list functions for the map things we need. *)
let rec list_map_add k v = function
  | []                     -> [(k,v)]
  | (a,b)::rest when k = a -> (k,v)::rest
  | (a,b)::rest            -> (a,b)::(list_map_add k v rest)

let list_map_fold f m x =
  List.fold_left (fun acc (k,v) -> f k v acc) x m

let list_map_exists f m =
  List.fold_left (fun acc (k,v) -> acc || (f k v)) false m

let list_map_empty = []

let rec list_map_find_opt k = function
  | []                     -> None
  | (a,b)::rest when a = k -> Some b
  | (a,b)::rest            -> list_map_find_opt k rest

exception Not_found

let list_map_find k m =
  match list_map_find_opt k m with
  | Some v -> v
  | None   -> raise Not_found

(* wrappers for map/set functions *)

let map_add = StrMap.add
let map_find = StrMap.find
let map_find_opt = StrMap.find_opt
let map_fold = StrMap.fold
let map_empty = StrMap.empty
let map_exists = StrMap.exists

(* adding things to a map, where adding an existing key overwrites an existing mapping for the key *)
let maptest1 () =
  let m = map_empty in           (* m = {} *)
  let m = map_add "a" 1 m in     (* m = {"a":1} *)
  let m = map_add "b" 2 m in     (* m = {"a":1, "b":2} *)
  let m = map_add "a" 3 m in     (* m = {"a":3, "b":2} *)
  m

(* looking stuff up in the map : call with map from maptest1 *)
let maptest2 m =
  let b = map_exists (fun k _ -> k = "a") m in  (* is "a" in the domain? *)
  let x = if b then map_find "a" m else -1 in   (* if it is, do an unsafe find that may raise exception *)
  let y = map_find_opt "b" m in                 (* safe find for b : should be Some 2 *)
  let z = map_find_opt "c" m in                 (* safe find for c : should be None *)
  (x,y,z)

(* iterating over a map, assumes int map_t *)
let maptest3 m =
  (* walk the map and accumulate the values that the keys map to *)
  map_fold (fun _ v acc -> acc + v) m 0


(*** Examples for sets **)  
let rec string_set_add (x: string) (l: (string) list) : (string) list =
  match l with
  | [] -> x :: [] 
  | h :: t -> if h = x then h :: t else h :: string_set_add x t

let rec string_set_union (x: (string) list) (l: (string) list) :
  (string) list =
  match l with
  | [] -> x
  | h :: t -> string_set_union (string_set_add h x) t 

let rec string_set_mem (x: string) (l: (string) list) : bool =
  match l with
  | [] -> false
  | y :: r -> x = y || string_set_mem x r

(* wrappers for set functions *)
let set_add = StrSet.add
let set_union = StrSet.union
let set_mem = StrSet.mem

let set_test1 s = 
  let s1 = set_add "1" s in 
  set_mem "1" s1

let set_test2 =
    let s1 = set_mem "1" (set_add "1" StrSet.empty) in 
    let s2 = string_set_mem "1" (string_set_add "1" []) in
    s1 = s2

let set_test3 = 
    let s1 = set_union StrSet.empty (StrSet.singleton "1") in 
    let s2 = string_set_union [] ["2"] in
    set_mem "1" s1 = string_set_mem "1" s2


let () = Printf.printf "%b \n" set_test3
