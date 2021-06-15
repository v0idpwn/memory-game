open! Base;;
open! Bonsai_web;;

module Board = Game_core.Board
module Card = Game_core.Card

module Input = Unit

module Model = struct
  type t = {board: Board.t; play_count: Int.t} [@@deriving sexp, equal]
  let default = {board = Board.new_board 10; play_count = 0}
end

module Result = struct
  type t = Vdom.Node.t
end

module Action = struct
  type t = OpenCard of Int.t [@@deriving sexp_of]
end

let apply_action ~inject:_ ~schedule_event:_ _input (model : Model.t) (action : Action.t) =
  match action with
  | OpenCard card_idx -> {model with board = Board.open_card model.board card_idx}
;;

let compute ~inject:_ _input _model =
  Vdom.Node.div [] []

let name = Source_code_position.to_string[%here]
