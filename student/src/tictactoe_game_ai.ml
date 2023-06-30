open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  match
    List.is_empty
      (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces)
  with
  | true -> failwith "No moves"
  | false ->
    List.random_element_exn
      (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces)
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if List.is_empty winning_moves
  then random_move_strategy ~game_kind ~pieces
  else List.random_element_exn winning_moves
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let moves = Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces in
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  (* print_endline (List.to_string ~f:Position.to_string winning_moves); *)
  if not (List.is_empty winning_moves)
  then List.random_element_exn winning_moves
  else (
    let losing_moves =
      Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
    in
    if List.is_empty losing_moves
    then List.random_element_exn moves
    else
      List.random_element_exn
        (List.filter moves ~f:(fun move ->
           not (List.mem losing_moves move ~equal:Position.equal))))
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

(* let score ~(me : Piece.t) ~(game_kind : Game_kind.t) ~(pieces : Piece.t
   Position.Map.t) : float = match Tic_tac_toe_exercises_lib.evaluate
   ~game_kind ~pieces with | Tic_tac_toe_exercises_lib.Evaluation.Game_over {
   winner = Some x } -> if Piece.equal x me then Float.infinity else
   Float.neg_infinity | _ -> 0.0 ;;

   let rec minimax ~(me : Piece.t) ~(game_kind : Game_kind.t) ~(pieces :
   Piece.t Position.Map.t) ~(game_status : Game_status.t) ~(scores : float
   list) ~(depth : int) : float = match Tic_tac_toe_exercises_lib.evaluate
   ~game_kind ~pieces with | Tic_tac_toe_exercises_lib.Evaluation.Game_over {
   winner = Some _x } -> let curr_score = score ~me ~game_kind ~pieces in
   print_s [%message "" (curr_score : float)]; curr_score | _ -> if depth = 0
   then score ~me ~game_kind ~pieces else ( let avail_moves =
   Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces in let scores
   = List.map avail_moves ~f:(fun position -> let piece = match game_status
   with | Turn_of piece -> piece | _ -> failwith "" in let new_pieces =
   Map.set pieces ~key:position ~data:piece in minimax ~me ~game_kind
   ~pieces:new_pieces ~game_status:(Game_status.Turn_of (Piece.flip me))
   ~scores ~depth:(depth - 1)) in if Game_status.equal game_status
   (Game_status.Turn_of me) then ( match List.max_elt scores
   ~compare:Float.compare with | Some v -> v | None -> failwith "fail") else
   ( match List.min_elt scores ~compare:Float.compare with | Some v -> v |
   None -> failwith "fail")) ;; *)

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  pick_winning_move_or_block_if_possible_strategy
    ~me
    ~game_kind:game_state.game_kind
    ~pieces:game_state.pieces
;;
(* let moves = Tic_tac_toe_exercises_lib.available_moves
   ~game_kind:game_state.game_kind ~pieces:game_state.pieces in let
   minimax_map = List.map moves ~f:(fun move -> let new_pieces = Map.set
   game_state.pieces ~key:move ~data:me in let score = minimax ~me
   ~game_kind:game_state.game_kind ~pieces:new_pieces
   ~game_status:game_state.game_status ~scores:[] ~depth:6 in print_s
   [%message "" (score : float)]; move, score) in match List.max_elt
   minimax_map ~compare:(fun (_move1, minimax1) (_move2, minimax2) ->
   Float.compare minimax1 minimax2) with | Some (final_move, _final_minimax)
   -> final_move | None -> failwith "error" *)
