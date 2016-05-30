
type id = unit ref


(*module NodeSet = Set.Make(struct*)
    (*type t = node*)
    (*let compare = compare *)
(*end) *)

type node =
  | None 
  (* id, predecessor * sucessor *)
  | Node of id * node list * node list

type graph = node list 

let get_nodes graph = 
  List.map (fun (node, _) -> node) graph

let equal (n1, id1) (n2, id2) = id1 = id2

let rec successor graph node = 
  match graph with
  | [] -> failwith "node not found in graph" 
  | (nd, _) :: [] -> 
      (match equal nd node with
      | true -> None
      | false -> failwith "node not found in the graph")
  | (nd, _) :: ((nxt, _) :: tl as rest) ->
      match equal nd node with
      | true -> Some nxt
      | false -> successor

