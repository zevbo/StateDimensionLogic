open Simple_example
open Sd_logic

let%test "missing_sd" =
  try
    ignore
      (Seq_model.create
         [ Update_x.est; Update_v.est; Light_on.est; Print.est ]
         ~end_cond:End_cond.end_cond
        : Seq_model.t);
    false
  with
  | e ->
    (match e with
    | Seq_model.Premature_sd_req sd -> Sd.Packed.equal sd (Sd.pack Sds.v)
    | _ -> false)
;;

let%expect_test _ =
  Main.run ();
  [%expect
    {|
  executing light_on logic
  In light on check logic...
  v: 0.036306, x: 0.036306, light on?: false
  executing light_on logic
  v: 0.596659, x: 0.632964, light on?: false
  executing light_on logic
  v: 0.765495, x: 1.398459, light on?: false
  executing light_on logic
  v: 0.816120, x: 2.214579, light on?: false
  executing light_on logic
  v: 1.056428, x: 3.271007, light on?: false
  executing light_on logic
  v: 0.798507, x: 4.069514, light on?: false
  executing light_on logic
  v: 0.664160, x: 4.733674, light on?: false
  executing light_on logic
  v: 0.377758, x: 5.111432, light on?: false
  executing light_on logic
  v: 0.392957, x: 5.504389, light on?: false
  executing light_on logic
  v: 0.313838, x: 5.818227, light on?: false
  executing light_on logic
  v: 0.719608, x: 6.537835, light on?: false
  executing light_on logic
  v: 0.628346, x: 7.166180, light on?: false
  executing light_on logic
  v: 0.657649, x: 7.823829, light on?: false
  executing light_on logic
  v: 1.033643, x: 8.857472, light on?: false
  executing light_on logic
  v: 1.137863, x: 9.995335, light on?: false
  executing light_on logic
  v: 0.794383, x: 10.789719, light on?: false
  executing light_on logic
  v: 1.056714, x: 11.846432, light on?: false
  executing light_on logic
  v: 1.006372, x: 12.852804, light on?: false
  executing light_on logic
  v: 1.308122, x: 14.160926, light on?: false
  executing light_on logic
  v: 1.263732, x: 15.424658, light on?: false
  executing light_on logic
  v: 0.895749, x: 16.320406, light on?: false
  executing light_on logic
  v: 0.888956, x: 17.209362, light on?: false
  executing light_on logic
  v: 1.163781, x: 18.373143, light on?: false
  executing light_on logic
  v: 1.095746, x: 19.468889, light on?: false
  executing light_on logic
  v: 1.498910, x: 20.967799, light on?: false
  executing light_on logic
  v: 1.612935, x: 22.580734, light on?: false
  executing light_on logic
  v: 1.341965, x: 23.922699, light on?: false
  executing light_on logic
  v: 1.190465, x: 25.113165, light on?: false
  executing light_on logic
  v: 1.104885, x: 26.218049, light on?: false
  executing light_on logic
  v: 1.449151, x: 27.667201, light on?: false
  executing light_on logic
  v: 1.455539, x: 29.122740, light on?: false
  executing light_on logic
  v: 1.121447, x: 30.244187, light on?: false
  executing light_on logic
  v: 1.087077, x: 31.331264, light on?: false
  executing light_on logic
  v: 1.261040, x: 32.592304, light on?: false
  executing light_on logic
  v: 1.206902, x: 33.799206, light on?: false
  executing light_on logic
  v: 1.801043, x: 35.600249, light on?: false
  executing light_on logic
  v: 1.831284, x: 37.431533, light on?: false
  executing light_on logic
  v: 1.870266, x: 39.301799, light on?: false
  executing light_on logic
  v: 2.433033, x: 41.734832, light on?: false
  executing light_on logic
  v: 2.622257, x: 44.357089, light on?: false
  executing light_on logic
  v: 2.780704, x: 47.137793, light on?: false
  executing light_on logic
  v: 2.439229, x: 49.577022, light on?: false
  executing light_on logic
  In light on check logic... Light is now on!
  v: 2.284548, x: 51.861570, light on?: true
  executing light_on logic
  v: 2.255672, x: 54.117242, light on?: true
  executing light_on logic
  v: 1.857590, x: 55.974831, light on?: true
  executing light_on logic
  v: 2.106565, x: 58.081397, light on?: true
  executing light_on logic
  v: 2.115248, x: 60.196645, light on?: true
  executing light_on logic
  v: 2.470436, x: 62.667081, light on?: true
  executing light_on logic
  v: 2.513197, x: 65.180279, light on?: true
  executing light_on logic
  v: 2.646726, x: 67.827005, light on?: true
  executing light_on logic
  v: 3.166457, x: 70.993462, light on?: true
  executing light_on logic
  v: 3.652212, x: 74.645673, light on?: true
  executing light_on logic
  v: 3.491979, x: 78.137652, light on?: true
  executing light_on logic
  v: 3.209376, x: 81.347028, light on?: true
  executing light_on logic
  v: 2.815349, x: 84.162378, light on?: true
  executing light_on logic
  v: 2.513425, x: 86.675803, light on?: true
  executing light_on logic
  v: 2.139792, x: 88.815596, light on?: true
  executing light_on logic
  v: 2.446097, x: 91.261693, light on?: true
  executing light_on logic
  v: 2.858264, x: 94.119957, light on?: true
  executing light_on logic
  v: 2.737652, x: 96.857609, light on?: true
  executing light_on logic
  v: 2.923314, x: 99.780923, light on?: true
  executing light_on logic
  v: 2.598854, x: 102.379778, light on?: true |}]
;;
