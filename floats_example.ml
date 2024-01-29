
(* A vector with floating point values *)
type vec3 = { x : float; y : float; z : float}

(* Vector addition *)
let vec3_add (a : vec3) (b : vec3) = 
  { x = a.x +. b.x;
    y = a.y +. b.y;
    z = a.z +. b.z }

(* Constant vector*)
let x_axis_unit : vec3 = {x = 1.0; y = 0.0; z = 0.0}
let y_axis_unit : vec3 = {x = 0.0; y = 1.0; z = 0.0}

let principal_axis_add =
    vec3_add x_axis_unit y_axis_unit

(* vector dot product *)
let dot_prod (a : vec3) (b : vec3) =
  a.x *. b.x +. a.y *. b.y +. a.z *. b.z

(* Vector length *)  
let vec_length (a: vec3) : float =
  sqrt (a.x ** 2. +. a.y ** 2. +. a.z ** 2.) 

(* Angle between two vectors*)
let theta (a : vec3) (b: vec3) = 
    acos ((dot_prod a b) /. ((vec_length a) *. (vec_length b)))    


let principal_axis_theta =
    theta x_axis_unit y_axis_unit    

let () = 
Printf.printf "Sum of x- and y-principal axis: %f \t %f \t %f \n" principal_axis_add.x principal_axis_add.y principal_axis_add.z;
Printf.printf "Angle between principal axis %f \n" principal_axis_theta    