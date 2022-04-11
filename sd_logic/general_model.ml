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

let to_graph cnodes start ~use_ticks =
  let rec explorer on g =
    let e = explorer in
    match to_cnode cnodes on with
    | P { node; next } ->
      if not (Set.is_empty (Graph.next g node.id))
      then g
      else (
        match node.info, next with
        | Exit, () -> g
        | Tick, C { id; _ } ->
          e next (if use_ticks then Graph.add_edge g node.id id else g)
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
  }
[@@deriving sexp_of, equal]

let create_flow guaranteed possibility = { guaranteed; possibility }
let empty_flow = create_flow (Set.empty (module Sd.Packed)) (Set.empty (module Sd.Packed))

(*
let set_union_skewed s1 s2 = Set.fold s1 ~init:s2 ~f:Set.add
*)
let acyclic_graph_flow g cnodes (start : Sd_node.child_t) =
  let id_to_scc, scc_graph = Graph.scc_graph (Graph.rev g) in
  let start_id =
    match start with
    | C { id; _ } -> id
  in
  let id_to_estimating id =
    let scc = Map.find_exn id_to_scc id in
    if Set.length scc = 1
    then (
      match id_to_cnode cnodes id with
      | P { node; next = _ } ->
        (match node.info with
        | Est est -> est.sds_estimating
        | _ -> Set.empty (module Sd.Packed)))
    else Set.empty (module Sd.Packed)
  in
  let flows = Hashtbl.create (module Int) in
  let rec explore_from on =
    match id_to_cnode cnodes on with
    | P { node; _ } ->
      (match node.info with
      | Tick -> Some empty_flow
      | _ ->
        (match Hashtbl.find flows on with
        | Some v -> v
        | None ->
          Hashtbl.set flows ~key:on ~data:None;
          let value_ops =
            Set.fold (Graph.next scc_graph on) ~init:[] ~f:(fun l i ->
                (match explore_from i with
                | None -> None
                | Some flow ->
                  let guaranteed = Set.union (id_to_estimating i) flow.guaranteed in
                  Some { flow with guaranteed })
                :: l)
          in
          let values = List.filter_map value_ops ~f:(fun a -> a) in
          let all_guaranteed = List.map values ~f:(fun f -> f.guaranteed) in
          let guaranteed =
            match all_guaranteed, on = start_id with
            | _, true -> Set.empty (module Sd.Packed)
            | hd :: tl, false -> List.fold_right tl ~init:hd ~f:Set.union
            | [], false -> Set.empty (module Sd.Packed)
          in
          let possibility =
            List.fold_left
              values
              ~init:(Set.empty (module Sd.Packed))
              ~f:(fun p { possibility; guaranteed } ->
                let all = Set.union possibility guaranteed in
                Set.union (Set.filter all ~f:(fun s -> not (Set.mem guaranteed s))) p)
          in
          let f = Some (create_flow guaranteed possibility) in
          Hashtbl.set flows ~key:on ~data:f;
          f))
  in
  let scc_flows =
    Map.mapi (Graph.as_map scc_graph) ~f:(fun ~key ~data:_ ->
        Option.value_exn (explore_from key))
  in
  let flows =
    Map.fold
      scc_flows
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data m ->
        Set.fold (Map.find_exn id_to_scc key) ~init:m ~f:(fun m id ->
            Map.set m ~key:id ~data))
  in
  flows
;;

let check_scc cnodes scc =
  if Set.length scc > 1
  then
    Set.iter scc ~f:(fun id ->
        match id_to_cnode cnodes id with
        | P { node = { info = Est est; _ }; _ } ->
          Set.iter est.sds_estimating ~f:(fun sd -> raise (Possible_overwrite sd))
        | _ -> ())
;;

let current_checks cnodes start =
  let g = to_graph cnodes start ~use_ticks:false in
  let scc_list = Graph.scc_list g in
  List.iter scc_list ~f:(check_scc cnodes);
  let flows = acyclic_graph_flow g cnodes start in
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

let next_nodes : type a. a Sd_node.t -> a -> Sd_node.child_t list =
 fun node next ->
  match node.info, next with
  | Tick, n -> [ n ]
  | Exit, () -> []
  | Fork, (n1, n2) -> [ n1; n2 ]
  | Desc _, (n1, n2) -> [ n1; n2 ]
  | Est _, n -> [ n ]
;;

exception Infinite_loop

let check_ends cnodes start =
  let explored = Hash_set.create (module Int) in
  let rec explore on =
    match to_cnode cnodes on with
    | P { node; next } ->
      if Hash_set.mem explored node.id
      then (
        match node.info with
        | Tick -> true
        | _ -> false)
      else (
        Hash_set.add explored node.id;
        match next_nodes node next with
        | [] -> true
        | l -> List.exists l ~f:explore)
  in
  if not (explore start) then raise Infinite_loop
;;

exception Possible_exponential_threading of Sd_node.child_t

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
  let g = to_graph cnodes start ~use_ticks:true in
  let check_tick tick_node next =
    let g = Graph.remove_edge g tick_node next in
    let id_to_scc, scc_g = Graph.scc_graph g in
    let is_large id =
      Set.length (Map.find_exn id_to_scc id) > 1 || Set.mem (Graph.next g id) id
    in
    let has_fork id =
      let scc = Map.find_exn id_to_scc id in
      Set.exists scc ~f:(fun id ->
          match id_to_cnode cnodes id with
          | P { node = { info = Fork; _ }; _ } -> true
          | _ -> false)
    in
    let explored = Hashtbl.create (module Int) in
    Hashtbl.set explored ~key:tick_node ~data:true;
    let rec explore on =
      match Hashtbl.find explored on with
      | Some v -> v
      | None ->
        let next = Graph.next scc_g on in
        (* acyclic b/c we're using the scc graph, hence only need to add to hashtbl once explore is finished *)
        let values = Set.map (module Bool) next ~f:explore in
        let count = Set.count values ~f:ident in
        if has_fork on && (count > 1 || (count > 0 && is_large on))
        then
          raise
            (Possible_exponential_threading
               (match id_to_cnode cnodes on with
               | P { node; _ } -> C node));
        count > 0
    in
    ignore (explore next : bool)
  in
  Set.iter
    (Map.key_set (Graph.as_map g))
    ~f:(fun id ->
      match id_to_cnode cnodes id with
      | P { node = { info = Tick; _ }; next = C next } -> check_tick id next.id
      | _ -> ())
;;

let safety_checks cnodes start =
  check_ends cnodes start;
  current_checks cnodes start;
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