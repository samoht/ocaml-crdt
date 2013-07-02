(* XXX: WIP *)

module Make (A: Dclock.OrderedType) (Vertex: Dclock.OrderedType) = struct

  module VertexSet = Dset.Make(A)(Vertex)

  module Edge = struct
    type t = Vertex.t * Vertex.t
    let compare (a1, a2) (b1, b2) =
      match Vertex.compare a1 b1 with
      | 0 -> Vertex.compare a2 b2
      | x -> x
    let to_string (a1, a2) =
      Printf.sprintf "(%s->%s)" (Vertex.to_string a1) (Vertex.to_string a2)
  end

  module EdgeSet = Dset.Make(A)(Edge)

  type t = {
    vertices: VertexSet.t;
    edges   : EdgeSet.t;
  }

end
