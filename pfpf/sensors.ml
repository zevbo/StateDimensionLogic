open! Core
open! Sd_logic

let lencoder = Encoder.create 0.1 State_sds.lpos
let rencoder = Encoder.create 0.1 State_sds.rpos
let gps = Gps.create State_sds.pos 0.4 20. 1.0
let imu = Imu.create 0.01 State_sds.angle
