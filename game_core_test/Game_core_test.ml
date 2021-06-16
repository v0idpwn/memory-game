(* Test suite *)
open OUnit2
open Base
module Card = Game_core.Card
module Board = Game_core.Board

(* New board is clear *)
let new_board_is_clear _ctx =
  let board = Board.new_board 10 in
  assert_equal board.state Clear

(* Two consecutively generated boards are different *)
let new_boards_are_different _ctx =
  let board1 = Board.new_board 100 in
  let board2 = Board.new_board 100 in
  let are_equal =
    Array.for_alli board1.cards ~f:(fun idx c ->
        Card.compare c (Array.get board2.cards idx))
  in
  assert_bool "Arrays are not different" (not are_equal)

(* Cards are correctly duplicated on board *)
let card_duplication_on_board _ctx =
  let board = Board.new_board 10 in
  let () = assert_equal (Array.length board.cards) 20 in
  let () = assert_equal board.size 20 in
  let () =
    assert_bool "Unique card count not 2"
      (Array.count board.cards ~f:(fun c -> c.content = 1) = 2)
  in
  ()

(* If all cards are open, board is won *)
let win_condition _ctx =
  let board = Board.new_board 10 in
  let () =
    Array.map_inplace board.cards ~f:(fun card -> { card with status = Open })
  in
  assert_equal (Board.next_state board) Won

(* Open card returns array with card open *)
let open_card_normal_flow _ctx =
  let board1 = Board.new_board 10 in
  let board1 = Board.open_card board1 1 in
  let opened_card =
    match Array.find board1.cards ~f:(fun c -> phys_equal c.status Open) with
    | Some _ -> true
    | None -> false
  in
  let () = assert_bool "No open card found" opened_card in
  let () = assert_equal board1.state CardOpen in
  ()

(* Opening last card triggers win 
   We open first 19 cards inplace and open the 20th one through `open_card` *)
let open_card_win_flow _ctx =
  let board = Board.new_board 10 in
  let _ =
    Array.mapi board.cards ~f:(fun idx card ->
        let opened_card = { card with status = Open } in
        if idx < 19 then Array.set board.cards idx opened_card else ())
  in
  let board = Board.open_card board 19 in
  assert_equal board.state Won

(* Clearing board removes unpaired open cards 
   We open a single card, clear board and check if it is still open. It should
   be closed.
 *)
let clear_board_removes_unpaired _ctx =
  let board = Board.new_board 10 in
  let board = Board.open_card board 19 in
  let board = Board.clear_board board in
  assert_equal ~cmp:phys_equal (Array.get board.cards 19).status Closed

let clear_board_keeps_pairs _ctx =
  let board = Board.new_board 10 in
  let board = Board.open_card board 19 in
  let open_card = Array.get board.cards 19 in
  let match_card =
    Array.findi board.cards ~f:(fun _idx card ->
        card.content = open_card.content)
  in
  let match_card_idx, _ = Option.value_exn match_card in
  let board = Board.open_card board match_card_idx in
  let board = Board.clear_board board in
  let () =
    assert_equal ~cmp:phys_equal (Array.get board.cards 19).status Open
  in
  let () =
    assert_equal ~cmp:phys_equal (Array.get board.cards match_card_idx).status
      Open
  in
  ()

(* Running *)
let tests =
  "core game test"
  >::: [
         "new board is clear" >:: new_board_is_clear;
         "new boards are different" >:: new_boards_are_different;
         "card duplication on board" >:: card_duplication_on_board;
         "win condition" >:: win_condition;
         "open card normal flow" >:: open_card_normal_flow;
         "open card win flow" >:: open_card_win_flow;
         "clear board removes unpaired" >:: clear_board_removes_unpaired;
         "clear board keeps pairs" >:: clear_board_keeps_pairs;
       ]

let () = run_test_tt_main tests
