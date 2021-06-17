open! Base
open! Bonsai_web
module Board = Game_core.Board
module Card = Game_core.Card
module Input = Unit

let vertical_size = 8

let horizontal_size = 6

module Model = struct
  type t = { board : Board.t; play_count : Int.t } [@@deriving sexp, equal]

  let default =
    let board = Board.new_board (vertical_size * horizontal_size / 2) in
    { board; play_count = 0 }
end

module Result = struct
  type t = Vdom.Node.t
end

module Action = struct
  type t = OpenCard of Int.t [@@deriving sexp_of]
end

let apply_action ~inject:_ ~schedule_event:_ _input (model : Model.t)
    (action : Action.t) =
  match action with
  | OpenCard card_idx -> (
      match (Array.get model.board.cards card_idx).status with
      | Closed ->
          let model = { model with play_count = model.play_count + 1 } in
          if model.play_count % 2 = 1 then
            let board = Board.clear_board model.board in
            { model with board = Board.open_card board card_idx }
          else { model with board = Board.open_card model.board card_idx }
      | Open ->
          if model.play_count % 2 = 1 then model
          else
            let board = Board.clear_board model.board in
            {
              board = Board.open_card board card_idx;
              play_count = model.play_count + 1;
            })

(* Rendering helpers *)
let render_card inject idx (card : Card.t) =
  let text =
    match card.status with Open -> Int.to_string card.content | Closed -> ""
  in
  Vdom.Node.button
    [
      Vdom.Attr.classes [ "button"; "is-large" ];
      Vdom.Attr.on_click (fun _ -> inject (Action.OpenCard idx));
    ]
    [ Vdom.Node.text text ]

let render_play_count play_count =
  let play_count_humanized = Int.to_string (play_count / 2) in
  Vdom.Node.div [Vdom.Attr.classes ["has-text-primary"]]
    [
      Vdom.Node.text
        (String.concat ~sep:" " [ "Play count:"; play_count_humanized ]);
    ]

let render_status (state : Board.state) =
  match state with
  | Clear -> Vdom.Node.div [] []
  | CardOpen -> Vdom.Node.div [] []
  | Won -> Vdom.Node.div [Vdom.Attr.classes ["has-text-primary"]] [ Vdom.Node.text "Won" ]

let compute ~inject _input (model : Model.t) =
  let card_renders =
    Array.to_list (Array.mapi model.board.cards ~f:(render_card inject))
  in
  let card_render_lines = List.chunks_of card_renders ~length:vertical_size in
  let wrapped_lines = List.map card_render_lines ~f:(Vdom.Node.div []) in
  Vdom.Node.div []
    (List.append
       [ render_play_count model.play_count; render_status model.board.state ]
       wrapped_lines)

let name = Source_code_position.to_string [%here]
