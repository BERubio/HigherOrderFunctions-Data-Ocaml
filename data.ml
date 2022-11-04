open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
match t with
| IntLeaf -> IntNode(x, None, IntLeaf,IntLeaf,IntLeaf)
| IntNode(num, snum, left, mid, right) ->
if x = num then t
else if x < num && snum = None then IntNode(x, Some num, left, mid, right)
else if x < num && snum <> None && Some x < snum then IntNode(num, snum, (int_insert x left), mid, right)
else if x > num && snum <> None && Some x < snum then IntNode(num, snum, left, (int_insert x mid), right)
else IntNode(num, snum, left, mid, (int_insert x right))


let rec int_mem x t =
match t with
| IntLeaf -> false
| IntNode(num, snum, left, mid, right) ->
if num = x then true
else if Some x = snum then true
else if x < num && snum <> None && Some x < snum then int_mem x left
else if x > num && snum <> None && Some x < snum then int_mem x mid
else int_mem x right


let rec int_size t =
match t with
|IntLeaf -> 0
|IntNode(num, snum, left, mid, right) ->
if num >= 0 && snum <> None then
2 + (int_size left) + (int_size mid) + (int_size right)
else 1 + (int_size left) + (int_size mid) + (int_size right)


let rec int_max t =
match t with
| IntLeaf -> raise (Invalid_argument("int_max"))
| IntNode(num, None, _, _, IntLeaf) -> num
| IntNode(num, Some value, _, _, IntLeaf) -> value
| IntNode(_, _, _, _, right) -> int_max right  

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map


let empty_tree_map = MapLeaf


let rec map_put k v t = 
match t with
| MapLeaf -> MapNode((k, v), None, MapLeaf, MapLeaf, MapLeaf)
| MapNode((key,value), None, left, mid, right) ->
if k = key then raise (Invalid_argument("map_put"))
else if k > key then MapNode((key, value), Some(k, v), left, mid, right)
else MapNode((k, v), Some(key, value), left, mid, right)
| MapNode((key, value), Some(skey,sval), left, mid, right) ->
if k = key then raise (Invalid_argument("map_put"))
else if Some k = Some skey then raise (Invalid_argument("map_put"))
else if k > key && Some k > Some skey then MapNode((key, value), Some(skey,sval), left, mid, (map_put k v right))
else if k < key && Some k < Some skey then MapNode((key, value), Some(skey,sval), (map_put k v left), mid, right)
else MapNode((key, value), Some(skey, sval), left, (map_put k v mid), right) 


let rec map_contains k t = 
match t with
| MapLeaf -> false
| MapNode ((key, value), None, left, mid, right) ->
if k = key then true
else if k < key then map_contains k left
else if k > key then map_contains k right
else map_contains k mid
| MapNode((key, value), Some(skey, sval), left, mid, right) ->
if k = key then true
else if Some k = Some skey then true
else if k < key then map_contains k left
else if k > key then map_contains k right
else map_contains k mid


let rec map_get k t =
match t with
| MapLeaf -> raise (Invalid_argument "map_get")
|MapNode((key, value), None, left, mid, right) ->
if k = key then value
else if k < key then map_get k left
else if k > key then map_get k right
else map_get k mid
| MapNode((key, value), Some(skey, sval), left, mid, right) ->
if k = key then value
else if Some k = Some skey then sval
else if k < key then map_get k left
else if k > key then map_get k right
else map_get k mid

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string * int) list list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = ["end", 100]::table

let pop_scope (table : lookup_table) : lookup_table =
match table with
  | [] -> failwith "No scopes remain!"
  | _::t -> t

let add_var name value (table : lookup_table) : lookup_table = 
match table with
| [] -> failwith "There are no scopes to add a variable to!"
| h::t ->
  match h with
   | [] -> failwith "There are no scopes to add a variable to!"
   | hd::tl -> ([(name, value)]@(hd::tl))::t

let rec lookup name (table : lookup_table) =
match table with
  | [] -> failwith "Variable not found!"
  | h::t ->
    let rec lookup_aux lst word = match lst with
      | [] -> failwith "Variable not found!"
      | h::t -> let (word, var) = h in
         if word = name then var else lookup_aux t name
    in lookup_aux h name
