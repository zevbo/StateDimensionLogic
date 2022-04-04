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
let id_to_cnode : Cnode.t Map.M(Int).t -> int -> Cnode.t = Map.find_exn
let to_cnode cnodes (C node : Sd_node.child_t) : Cnode.t = id_to_cnode cnodes node.id

exception Unsafe_curr_requirement of Sd.Packed.t [@@deriving sexp]

(* when inconsitent estimates are introduced, will need a new error *)
exception Expected_overwrite of Sd.Packed.t [@@deriving sexp]
exception Possible_overwrite of Sd.Packed.t [@@deriving sexp]

module Graph = Graph.Graph (Int)

let to_graph cnodes start =
  let rec explorer on g =
    let e = explorer in
    match to_cnode cnodes on with
    | P { node; next } ->
      if not (Set.is_empty (Graph.next g node.id))
      then g
      else (
        match node.info, next with
        | Exit, () -> g
        | Tick, C { id; _ } -> e next (Graph.add_edge g node.id id)
        | Fork, (C { id = id1; _ }, C { id = id2; _ }) ->
          e (fst next) (e (snd next) (Graph.add_edges g node.id [ id1; id2 ]))
        | Est _, C { id; _ } -> e next (Graph.add_edge g node.id id)
        | Desc _, (C { id = id1; _ }, C { id = id2; _ }) ->
          e (fst next) (e (snd next) (Graph.add_edges g node.id [ id1; id2 ])))
  in
  explorer start Graph.empty
;;

type flow =
  { guaranteed : Set.M(Sd.Packed).t
  ; possibility : Set.M(Sd.Packed).t
  ; guaranteed_nodes : Set.M(Int).t
  }
[@@deriving sexp_of, equal]

let create_flow guaranteed possibility guaranteed_nodes =
  { guaranteed; possibility; guaranteed_nodes }
;;

let empty_flow =
  create_flow
    (Set.empty (module Sd.Packed))
    (Set.empty (module Sd.Packed))
    (Set.empty (module Int))
;;

let graph_flow g cnodes (start : Sd_node.child_t) =
  let rev_g = Graph.rev g in
  let start_id =
    match start with
    | C { id; _ } -> id
  in
  let id_to_estimating id =
    match id_to_cnode cnodes id with
    | P { node; next = _ } ->
      (match node.info with
      | Est est -> est.sds_estimating
      | _ -> Set.empty (module Sd.Packed))
  in
  let is_tick id =
    match id_to_cnode cnodes id with
    | P { node = { info = Tick; _ }; _ } -> true
    | _ -> false
  in
  let node_flow (flow : (int, flow, 'a) Map.t) node =
    if is_tick node
    then empty_flow
    else (
      let flows =
        Set.fold (Graph.next rev_g node) ~init:[] ~f:(fun l id ->
            let prev = Map.find_exn flow id in
            let guaranteed_nodes = Set.add prev.guaranteed_nodes id in
            let guaranteed = Set.union prev.guaranteed (id_to_estimating id) in
            { prev with guaranteed; guaranteed_nodes } :: l)
      in
      (* it cannot be a first entry to the node if it is guaranteed to already have been passed from the path *)
      let possible_entries =
        List.filter flows ~f:(fun flow -> not (Set.mem flow.guaranteed_nodes node))
      in
      let guaranteed, guaranteed_nodes =
        match node = start_id, possible_entries with
        | false, hd :: tl ->
          List.fold_left
            tl
            ~init:(hd.guaranteed, hd.guaranteed_nodes)
            ~f:(fun
                 (old_guaranteed, old_guaranteed_nodes)
                 { guaranteed; guaranteed_nodes; _ }
               ->
              ( Set.inter old_guaranteed guaranteed
              , Set.inter old_guaranteed_nodes guaranteed_nodes ))
        | _ -> Set.empty (module Sd.Packed), Set.empty (module Int)
      in
      let flow_possiblities =
        List.map flows ~f:(fun { guaranteed = g; possibility = p; _ } ->
            let all = Set.union g p in
            Set.filter all ~f:(fun sd -> not (Set.mem guaranteed sd)))
      in
      let possibility =
        List.fold_left flow_possiblities ~init:(Set.empty (module Sd.Packed)) ~f:Set.union
      in
      let flow = create_flow guaranteed possibility guaranteed_nodes in
      flow)
  in
  let rec update_flow curr_flow tick =
    let new_flow =
      Map.mapi curr_flow ~f:(fun ~key ~data:_data -> node_flow curr_flow key)
    in
    let any_change =
      Map.existsi new_flow ~f:(fun ~key ~data ->
          not (equal_flow data (Map.find_exn curr_flow key)))
    in
    if any_change then update_flow new_flow (tick + 1) else new_flow
  in
  let og_flow = Map.map (Graph.as_map rev_g) ~f:(fun _ -> empty_flow) in
  update_flow og_flow 0
;;

let new_current_checks cnodes start =
  let g = to_graph cnodes start in
  let flows = graph_flow g cnodes start in
  let verify_dep flow lang =
    let dep = Sd_lang.dependencies lang in
    Map.iteri dep ~f:(fun ~key ~data ->
        if data = 0 && not (Set.mem flow.guaranteed key)
        then raise (Unsafe_curr_requirement key))
  in
  Map.iteri flows ~f:(fun ~key:id ~data:flow ->
      match id_to_cnode cnodes id with
      | P { node; _ } ->
        (match node.info with
        | Exit | Tick | Fork -> ()
        | Est est ->
          verify_dep flow est.logic;
          Set.iter est.sds_estimating ~f:(fun sd ->
              if Set.mem flow.guaranteed sd
              then raise (Expected_overwrite sd)
              else if Set.mem flow.possibility sd
              then raise (Possible_overwrite sd))
        | Desc desc -> verify_dep flow desc))
;;

let _current_checks cnodes start =
  let explored = Hash_set.create (module Int) in
  let rec explorer on current_estimated =
    (* verifies dependencies on current tick state dimensions *)
    let verify_dep lang =
      let dep = Sd_lang.dependencies lang in
      Map.iteri dep ~f:(fun ~key ~data ->
          if data = 0 && not (Set.mem current_estimated key)
          then raise (Unsafe_curr_requirement key))
    in
    match to_cnode cnodes on with
    | P { node; next } ->
      if Hash_set.mem explored node.id
      then ()
      else (
        Hash_set.add
          explored
          (match on with
          | C { id; _ } -> id);
        match node.info, next with
        | Exit, () -> ()
        | Tick, node -> explorer node (Set.empty (module Sd.Packed))
        | Fork, (n1, n2) ->
          explorer n1 current_estimated;
          explorer n2 current_estimated
        | Est est, n ->
          verify_dep est.logic;
          Set.iter est.sds_estimating ~f:(fun sd ->
              if Set.mem current_estimated sd then raise (Possible_overwrite sd));
          let current_estimated = Set.union current_estimated est.sds_estimating in
          explorer n current_estimated
        | Desc f, (n1, n2) ->
          verify_dep f;
          explorer n1 current_estimated;
          explorer n2 current_estimated)
  in
  explorer start (Set.empty (module Sd.Packed))
;;

exception Possible_exponential_threading of Sd_node.child_t

(* this doesn't work I'm pretty sure *)
let _all_ticks cnodes start =
  let explored = Hash_set.create (module Int) in
  let rec explore on =
    match to_cnode cnodes on with
    | P { node; next } ->
      if Hash_set.mem explored node.id
      then Set.empty (module Int)
      else (
        Hash_set.add explored node.id;
        match node.info, next with
        | Exit, () -> Set.empty (module Int)
        | Tick, next_node -> Set.add (explore next_node) node.id
        | Fork, (n1, n2) | Desc _, (n1, n2) -> Set.union (explore n1) (explore n2)
        | Est _est, n -> explore n)
  in
  explore start
;;

let exponential_threads_check cnodes start =
  let rec next_ticks ~explored on =
    match to_cnode cnodes on with
    | P { node; next } ->
      if Set.mem explored node.id
      then (
        match node.info with
        | Tick -> Set.add (Set.empty (module Int)) node.id
        | _ -> Set.empty (module Int))
      else (
        let explored = Set.add explored node.id in
        let recur = next_ticks ~explored in
        match node.info, next with
        | Exit, () -> Set.empty (module Int)
        | Tick, next_node -> Set.add (recur next_node) node.id
        | Fork, (n1, n2) ->
          let n1_set = recur n1 in
          let n2_set = recur n2 in
          let comb = Set.union n1_set n2_set in
          if not (Set.length comb = Set.length n1_set + Set.length n2_set)
          then (
            Set.iter n1_set ~f:(fun id ->
                if Set.mem n2_set id then raise (Possible_exponential_threading (C node)));
            assert false);
          comb
        | Est _, node -> recur node
        | Desc _, (n1, n2) -> Set.union (recur n1) (recur n2))
  in
  ignore (next_ticks ~explored:(Set.empty (module Int)) start : Set.M(Int).t)
;;

let safety_checks cnodes start =
  new_current_checks cnodes start;
  exponential_threads_check cnodes start
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
  safety_checks cnodes (C start);
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

let rsh t = !(t.rsh)