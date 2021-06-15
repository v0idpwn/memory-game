open! Base;;

type status = Open | Closed [@@deriving sexp, equal]

type t =
  {
    status: status;
    content: int;
  } [@@deriving sexp, equal]
 

let new_card content = {status = Closed; content;}

let compare ca cb = 
  ca.content = cb.content
