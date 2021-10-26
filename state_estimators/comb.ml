open! Core
open Src

module F (Sd : Robot_state.Sd) = struct
  module Sd_set = Set.Make (Sd)

  let create (est_l : Est.F(Sd).t list) ~f =
    assert (not (List.is_empty est_l));
    (object (this)
       val est_l = est_l

       method get_est =
         let i = f () in
         match List.nth est_l i with
         | Some est -> est
         | None ->
           raise
             (Invalid_argument
                (sprintf
                   "Attempted to use out of range estimator of Comb estimator. Index: \
                    %d, Length: %d"
                   i
                   (List.length est_l)))

       method union_sets ~f =
         List.fold est_l ~init:Sd_set.empty ~f:(fun acc est -> Set.union acc (f est))

       method current_sds_required =
         this#union_sets ~f:(fun est -> est#current_sds_required)

       method past_sds_required = this#union_sets ~f:(fun est -> est#past_sds_required)
       method sds_estimating = this#union_sets ~f:(fun est -> est#sds_estimating)

       method uses_measrumeants =
         List.fold
           (List.map est_l ~f:(fun est -> est#uses_measrumeants))
           ~init:true
           ~f:( || )

       method estimate = this#get_est#estimate
       method get_uncertianty = this#get_est#get_uncertianty
     end
      :> Est.F(Sd).t)
  ;;
end
