type status = Open | Closed

type t =
  {
    status: status;
    content: int;
  }

let new_card content = {status = Closed; content;}

let compare ca cb = 
  ca.content = cb.content
