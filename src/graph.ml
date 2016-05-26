type node = int

module NodeSet = Set.Make(struct
    let t = node
    let compare = compare 
end) 

type graph = (int *  NodeSet.t) * list 

