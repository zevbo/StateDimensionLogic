open! Core

(* cnode = connected_node *)
module Cnode = struct
  type t =
    | P :
        { node : 'a Sd_node.t
        ; next : 'a
        }
        -> t
end

module Mutex = Error_checking_mutex

(* node_on should become a list representing for each thread, or something like that *)
(* locks must be held when writing, no requirement to hold while reading 
  although in practice necessary in some places *)

type t =
  { rsh : Rsh.t ref
  ; continuations : ctxt list ref
  ; rsh_lock : Mutex.t
  ; continuations_lock : Mutex.t
  ; cnodes : Cnode.t Map.M(Int).t
  }

and ctxt =
  { t : t
  ; node : Sd_node.child_t
  ; children : Thread.t list
  }

(* zTODO: add ability to increase lengths *)

let to_cnode cnodes (C node : Sd_node.child_t) : Cnode.t = Map.find_exn cnodes node.id

exception Unsafe_curr_requirement of Sd.Packed.t [@@deriving sexp]

(* when inconsitent estimates are introduced, will need a new error *)
exception Possible_overwrite of Sd.Packed.t [@@deriving sexp]

let rec current_checks
    ?(explored = Set.empty (module Int))
    ?((* passed down *)
      current_estimated = Set.empty (module Sd.Packed))
    ?((* passed down *)
      timer_estimations = Map.empty (module Int))
    (* passed back *)
      cnodes
    on
  =
  let verify_dep lang =
    let dep = Sd_lang.dependencies lang in
    Map.iteri dep ~f:(fun ~key ~data ->
        if data = 0 && not (Set.mem current_estimated key)
        then raise (Unsafe_curr_requirement key))
  in
  let merge_timer_data ~key:_key x =
    match x with
    | `Both (set1, set2) -> Some (Set.union set1 set2)
    | `Left v | `Right v -> Some v
  in
  match to_cnode cnodes on with
  | P { node; next } ->
    if Set.mem explored node.id
    then (
      match node.info with
      | Tick ->
        let curr_val = Map.find timer_estimations node.id in
        let new_val =
          Set.union
            current_estimated
            (Option.value curr_val ~default:(Set.empty (module Sd.Packed)))
        in
        Map.set timer_estimations ~key:node.id ~data:new_val
      | _ -> timer_estimations)
    else (
      let explored =
        Set.add
          explored
          (match on with
          | C { id; _ } -> id)
      in
      let recur
          ?(explored = explored)
          ?(current_estimated = current_estimated)
          ?(timer_estimations = timer_estimations)
          on
        =
        current_checks ~explored ~current_estimated ~timer_estimations cnodes on
      in
      match node.info, next with
      | Exit, () -> timer_estimations
      | Tick, node -> recur node
      | Fork, (n1, n2) -> Map.merge (recur n1) (recur n2) ~f:merge_timer_data
      | Est est, n ->
        verify_dep est.logic;
        Set.iter est.sds_estimating ~f:(fun sd ->
            if Set.mem current_estimated sd then raise (Possible_overwrite sd));
        let current_estimated = Set.union current_estimated est.sds_estimating in
        recur ~current_estimated n
      | Desc f, (n1, n2) ->
        verify_dep f;
        Map.merge (recur n1) (recur n2) ~f:merge_timer_data)
;;

let rec dependencies cnodes ?(explored = Set.empty (module Int)) (on : Sd_node.child_t) =
  match to_cnode cnodes on with
  | P { node; next } ->
    if Set.mem explored node.id
    then Map.empty (module Sd.Packed)
    else (
      let explored = Set.add explored node.id in
      let dependencies node = dependencies cnodes ~explored node in
      match node.info, next with
      | Exit, () -> Map.empty (module Sd.Packed)
      | Tick, t -> dependencies t
      | Fork, (t1, t2) -> Sd_lang.dependency_union (dependencies t1) (dependencies t2)
      | Est est, t ->
        Sd_lang.dependency_union (Sd_lang.dependencies est.logic) (dependencies t)
      | Desc logic, (t_true, t_false) ->
        Sd_lang.dependency_union
          (Sd_lang.dependencies logic)
          (Sd_lang.dependency_union (dependencies t_true) (dependencies t_false)))
;;

let assert_all_nodes_known (connections : Sd_node.conn list) cnodes =
  let assert_in (from : _ Sd_node.t) (Sd_node.C node) =
    if not (Map.mem cnodes node.id)
    then
      raise
        (Invalid_argument
           (Printf.sprintf
              "Invalid connections list for General_model.create: Node %n links to node \
               %n, but there is no connection entry for node %n"
              from.id
              node.id
              node.id))
  in
  List.iter connections ~f:(fun (Conn (node, next)) ->
      let (nodes : Sd_node.child_t list) =
        match node.info, next with
        | Exit, () -> []
        | Tick, node -> [ node ]
        | Fork, (n1, n2) -> [ n1; n2 ]
        | Est _, node -> [ node ]
        | Desc _, (n1, n2) -> [ n1; n2 ]
      in
      List.iter nodes ~f:(assert_in node))
;;

let create (connections : Sd_node.conn list) (start : 'a Sd_node.t) =
  let cnodes_l =
    List.map connections ~f:(fun (Conn (node, next)) -> node.id, Cnode.(P { node; next }))
  in
  let cnodes_l =
    (Sd_node.exit.id, Cnode.(P { node = Sd_node.exit; next = () })) :: cnodes_l
  in
  let cnodes_op = Map.of_alist (module Int) cnodes_l in
  let cnodes =
    match cnodes_op with
    | `Duplicate_key id ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Attempted to create general_model with the same node twice [id: %n]"
              id))
    | `Ok cnodes -> cnodes
  in
  assert_all_nodes_known connections cnodes;
  let _timer = current_checks cnodes (C start) in
  let sd_lengths = Map.map (dependencies cnodes (C start)) ~f:(fun n -> n + 1) in
  let t =
    { rsh = ref (Rsh.create ~sd_lengths ())
    ; continuations = ref []
    ; rsh_lock = Mutex.create ()
    ; continuations_lock = Mutex.create ()
    ; cnodes
    }
  in
  let node_ctxt = { t; node = C start; children = [] } in
  t.continuations := [ node_ctxt ];
  t
;;

type step_result =
  | On of Sd_node.child_t
  | Tick_block of Sd_node.child_t
  | Exited
  | Forked of Sd_node.child_t * Sd_node.child_t

let thread_exit ctxt =
  List.iter ctxt.children ~f:Thread.join;
  Thread.exit ();
  raise (Failure "General_model thread failed to exit")
;;

(* currently no safety checks *)
(* maybe should be worried about expontential thread increase? *)

let step ctxt ~safety =
  (* must do this only once in any execution to stop inconsistency *)
  let (P { node; next }) = to_cnode ctxt.t.cnodes ctxt.node in
  match node.info, next with
  | Exit, () -> Exited
  | Tick, node ->
    (* *)
    Tick_block node
  | Fork, (node, node_forked) -> Forked (node, node_forked)
  | Est est, node ->
    let new_rs = Sd_est.execute est ~safety !(ctxt.t.rsh) in
    Mutex.lock ctxt.t.rsh_lock;
    ctxt.t.rsh := Rsh.use !(ctxt.t.rsh) new_rs;
    Mutex.unlock ctxt.t.rsh_lock;
    On node
  | Desc logic, (t_true, t_false) ->
    On (if Sd_lang.execute logic !(ctxt.t.rsh) then t_true else t_false)
;;

let rec run_thread_tick ctxt ~safety =
  let run_thread_tick = run_thread_tick ~safety in
  match step ctxt ~safety with
  | On node -> run_thread_tick { ctxt with node }
  | Tick_block node ->
    Mutex.lock ctxt.t.continuations_lock;
    ctxt.t.continuations := { ctxt with node } :: !(ctxt.t.continuations);
    Mutex.unlock ctxt.t.continuations_lock;
    thread_exit ctxt
  | Exited -> thread_exit ctxt
  | Forked (node, forked_node) ->
    let t =
      (* zTODO: new_function *)
      Thread.create
        ~on_uncaught_exn:`Kill_whole_process
        run_thread_tick
        { ctxt with node = forked_node }
    in
    run_thread_tick { ctxt with node; children = t :: ctxt.children }
;;

type safety = Sd_est.safety

let run_tick t ~safety =
  let continuations = !(t.continuations) in
  t.continuations := [];
  let threads =
    List.map continuations ~f:(fun ctxt ->
        Thread.create ~on_uncaught_exn:`Print_to_stderr (run_thread_tick ~safety) ctxt)
  in
  List.iter threads ~f:Thread.join;
  t.rsh := Rsh.add_empty_state !(t.rsh);
  t
;;

let rec run t ~safety ~num_ticks =
  match num_ticks, !(t.continuations) with
  | 0, _ | _, [] -> t
  | _ -> run (run_tick t ~safety) ~safety ~num_ticks:(num_ticks - 1)
;;
