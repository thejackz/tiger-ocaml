open Core.Std

type inode = {
    id:             int;
    connects:       inode list;
}

type igraph = inode list 

let make_igraph cfg : igraph = 
  let rec helper cfg col = 
    match cfg with
    | [] -> List.rev col
    | tl :: [] -> 
        let n = {id = tl.id; connects = [];} in
        helper [] (n :: col)
    | node :: (next :: _ as rest) ->
        match node.stmt with
        | LABEL ()
        let n = { 
          id = node.id; 
          connects = Temp.TempSet.diff 
        }
