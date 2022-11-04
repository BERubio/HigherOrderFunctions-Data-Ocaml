open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e =
fold (fun a elem -> if elem = e then true else a) false lst
	  
let is_present lst x =
fold (fun newlst elem -> if elem = x then (newlst@[1]) else (newlst@[0])) [] lst   

let count_occ lst target =
fold (fun count x -> if x = target then (count + 1) else count) 0 lst

let uniq lst =
fold (fun newlst elem -> if (contains_elem newlst elem) <> true then newlst@[elem] else newlst) [] lst

let rec count_instances lst elem count  =
match lst with
| [] -> 0
| h::t -> if h = elem then count_instances t elem (count + 1) else count_instances t elem count 

let assoc_list lst =
let newlst = (uniq lst) in 
fold (fun a elem -> a@[(elem, count_occ lst elem)]) [] newlst 

let ap fns args =
fold (fun newlst fns -> newlst@(map fns args)) [] fns  
