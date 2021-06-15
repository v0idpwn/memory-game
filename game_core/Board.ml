open! Base;;

type state =
  | Clear
  | CardOpen
  | Won
[@@deriving sexp, equal]

type t = {
  cards: Card.t Array.t;
  state: state;
  size: int;
} [@@deriving sexp, equal]

(* Returns a new board *)
let new_board number_of_pairs =
  let base_list = Array.init number_of_pairs ~f:Card.new_card in
  let with_pairs = Array.concat [base_list; base_list;] in
  let () = Array.permute with_pairs in
  {cards = with_pairs; state = Clear; size = number_of_pairs * 2}

(* Calculates next state for given board *)
let next_state board =
  match Array.count board.cards ~f:(fun card -> phys_equal card.status Open) with
  | open_cards when open_cards = board.size -> Won
  | _ -> CardOpen

(* MUTATES board cards changing a card state to open 
   Returns board with updated cards and state *)
let open_card board index =
  let card = Array.get board.cards index in
  let () = Array.set board.cards index {card with status = Open} in
  {board with state = next_state board}

(* MUTATES board cards, closing cards open without match
   Returns board with updated cards and state 
   TODO: optimize it, current complexity over O(n^2) *)
let clear_board board =
  let opened_cards = Array.filter_mapi board.cards ~f:(fun idx card ->
    match card.status with
    | Open -> Some(idx, card)
    | _ -> None)
  in
  let unmatched_cards = Array.filter opened_cards ~f:(fun (_idx, card) -> 
    Array.count opened_cards ~f:(fun (_idx, c) -> Card.compare c card) = 1)
  in
  let _ : unit array = Array.map unmatched_cards ~f:(fun (idx, (card : Card.t)) -> 
    Array.set board.cards idx {card with status = Closed}
  )
  in
  {board with state = next_state board}

