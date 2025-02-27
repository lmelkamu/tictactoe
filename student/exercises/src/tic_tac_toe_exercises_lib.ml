open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

let case_one =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let map = Map.keys pieces in
  let all_positions game_kind =
    List.concat_map
      (List.range 0 (Game_kind.board_length game_kind))
      ~f:(fun row ->
        List.map
          (List.range 0 (Game_kind.board_length game_kind))
          ~f:(fun column -> { Position.row; Position.column }))
  in
  List.filter (all_positions game_kind) ~f:(fun position ->
    not (List.mem map position ~equal:Position.equal))
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let x_map = Map.filter pieces ~f:(fun v -> Piece.equal v Piece.X) in
  let o_map = Map.filter pieces ~f:(fun v -> Piece.equal v Piece.O) in
  match game_kind with
  | Tic_tac_toe ->
    let horizontal map key data =
      match
        Map.find map (Position.left key), Map.find map (Position.right key)
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2 ->
        if Piece.equal piece_1 data && Piece.equal piece_2 data
        then true
        else false
      | _, _ -> false
    in
    let vertical map key data =
      match
        Map.find map (Position.up key), Map.find map (Position.down key)
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2 ->
        if Piece.equal piece_1 data && Piece.equal piece_2 data
        then true
        else false
      | _, _ -> false
    in
    let diagonal1 map key data =
      (*let up_left = Map.find map (Position.up (Position.left key)) in let
        down_right = Map.find map (Position.down (Position.right key)) in
        print_s [%message "" (up_left : Piece.t option) (down_right : Piece.t
        option)]; *)
      match
        ( Map.find map (Position.up (Position.left key))
        , Map.find map (Position.down (Position.right key)) )
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2 ->
        if Piece.equal piece_1 data && Piece.equal piece_2 data
        then true
        else false
      | _, _ -> false
    in
    let diagonal2 map key data =
      match
        ( Map.find map (Position.up (Position.right key))
        , Map.find map (Position.down (Position.left key)) )
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2 ->
        if Piece.equal piece_1 data && Piece.equal piece_2 data
        then true
        else false
      | _, _ -> false
    in
    let is_Win map =
      Map.existsi map ~f:(fun ~key ~data ->
        if Map.mem map key
        then (
          let horizontal = horizontal map key data in
          let vertical = vertical map key data in
          let diagonal1 = diagonal1 map key data in
          let diagonal2 = diagonal2 map key data in
          (* print_s [%message "" (key : Position.t) (data : Piece.t)
             (horizontal : bool) (vertical : bool) (diagonal1 : bool)
             (diagonal2 : bool)]; *)
          horizontal || vertical || diagonal1 || diagonal2)
        else false)
    in
    if is_Win x_map
    then Evaluation.Game_over { winner = Some Piece.X }
    else if is_Win o_map
    then Evaluation.Game_over { winner = Some Piece.O }
    else Game_continues
  | Omok ->
    let horizontal map key data =
      match
        ( Map.find map (Position.left key)
        , Map.find map (Position.left (Position.left key))
        , Map.find map (Position.right key)
        , Map.find map (Position.right (Position.right key)) )
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2, Some piece_3, Some piece_4 ->
        if Piece.equal piece_1 data
           && Piece.equal piece_2 data
           && Piece.equal piece_3 data
           && Piece.equal piece_4 data
        then true
        else false
      | _, _, _, _ -> false
    in
    let vertical map key data =
      match
        ( Map.find map (Position.up key)
        , Map.find map (Position.up (Position.up key))
        , Map.find map (Position.down key)
        , Map.find map (Position.down (Position.down key)) )
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2, Some piece_3, Some piece_4 ->
        if Piece.equal piece_1 data
           && Piece.equal piece_2 data
           && Piece.equal piece_3 data
           && Piece.equal piece_4 data
        then true
        else false
      | _, _, _, _ -> false
    in
    let diagonal1 map key data =
      match
        ( Map.find map (Position.up (Position.left key))
        , Map.find
            map
            (Position.up (Position.up (Position.left (Position.left key))))
        , Map.find map (Position.down (Position.right key))
        , Map.find
            map
            (Position.down
               (Position.down (Position.right (Position.right key)))) )
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2, Some piece_3, Some piece_4 ->
        if Piece.equal piece_1 data
           && Piece.equal piece_2 data
           && Piece.equal piece_3 data
           && Piece.equal piece_4 data
        then true
        else false
      | _, _, _, _ -> false
    in
    let diagonal2 map key data =
      match
        ( Map.find map (Position.up (Position.right key))
        , Map.find
            map
            (Position.up (Position.up (Position.right (Position.right key))))
        , Map.find map (Position.down (Position.left key))
        , Map.find
            map
            (Position.down
               (Position.down (Position.left (Position.left key)))) )
      with
      (*If both exists, check if equal, else continue checking*)
      | Some piece_1, Some piece_2, Some piece_3, Some piece_4 ->
        if Piece.equal piece_1 data
           && Piece.equal piece_2 data
           && Piece.equal piece_3 data
           && Piece.equal piece_4 data
        then true
        else false
      | _, _, _, _ -> false
    in
    let is_Win map =
      Map.existsi map ~f:(fun ~key ~data ->
        if Map.mem map key
        then
          horizontal map key data
          || vertical map key data
          || diagonal1 map key data
          || diagonal2 map key data
        else false)
    in
    if is_Win x_map
    then Evaluation.Game_over { winner = Some Piece.X }
    else if is_Win o_map
    then Evaluation.Game_over { winner = Some Piece.O }
    else Game_continues
;;

(* | _ -> Illegal_state *)

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let moves = available_moves ~game_kind ~pieces in
  (*Iterate through list and modify map and see if anyone would win*)
  List.filter moves ~f:(fun move ->
    (* print_endline (Position.to_string move); *)
    let state =
      evaluate ~game_kind ~pieces:(Map.set pieces ~key:move ~data:me)
    in
    (* print_endline (Evaluation.to_string state); *)
    match state with
    | Evaluation.Game_over { winner = Some x } ->
      if Piece.equal x me then true else false
    | _ -> false)
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let moves = available_moves ~game_kind ~pieces in
  if List.is_empty (winning_moves ~me:(Piece.flip me) ~game_kind ~pieces)
  then []
  else
    List.filter moves ~f:(fun move ->
      not
        (List.mem
           (winning_moves ~me:(Piece.flip me) ~game_kind ~pieces)
           move
           ~equal:Position.equal))
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* (*After you've implemented [available_moves], uncomment these tests! *)
   let%expect_test "yes available_moves" = let (moves : Position.t list) =
   available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   List.sort ~compare:Position.compare in print_s [%sexp (moves : Position.t
   list)]; [%expect {| (((row 0) (column 1)) ((row 0) (column 2)) ((row 1)
   (column 1)) ((row 1) (column 2)) ((row 2) (column 1))) |}] ;;

   let%expect_test "no available_moves" = let (moves : Position.t list) =
   available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   List.sort ~compare:Position.compare in print_s [%sexp (moves : Position.t
   list)]; [%expect {| () |}] ;;

   (* When you've implemented the [evaluate] function, uncomment the next two
   tests! *) let%expect_test "evalulate_win_for_x" = print_endline (evaluate
   ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   Evaluation.to_string); [%expect {| (Game_over(winner(X))) |}] ;;

   let%expect_test "evalulate_non_win" = print_endline (evaluate
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   Evaluation.to_string); [%expect {| Game_continues |}] ;; *)

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:case_one.game_kind
      ~pieces:case_one.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:case_one.game_kind
      ~pieces:case_one.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect
    {|
  (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2))
   ((row 2) (column 1))) |}]
;;
