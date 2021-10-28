open! Core
open State_estimators
open Src

let model =
  Est.Applicable.create_model
    [ Est.Applicable.create (module Update_v) (Update_v.create ())
    ; Est.Applicable.create (module Update_x) (Update_x.create ())
    ]
;;

let tick state_history = Est.Applicable.apply (Rsh.add_state state_history) model

let print_data state_history =
  printf
    "v: %f, x: %f\n"
    (Rsh.find_exn state_history Sds.v)
    (Rsh.find_exn state_history Sds.x)
;;

let num_ticks = 100
let max_length = 2
let state_history = Rsh.create ~max_length

let () =
  let final =
    List.fold_left
      (List.range 0 num_ticks)
      ~init:state_history
      ~f:(fun state_history _i ->
        let updated = tick state_history in
        print_data updated;
        updated)
  in
  ignore (final : Rsh.t)
;;
