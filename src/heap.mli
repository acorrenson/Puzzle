(** Module implementing (imperative) Binary Heaps *)

(** Abstract type for binary heaps *)
type 'a t

(** Array to binary heap conversion *)
val of_array : (int * 'a) array -> 'a t

(** Binary heap to array conversion *)
val to_array : 'a t -> (int * 'a) array

(** Insert a value *)
val insert : 'a t -> 'a -> int -> unit

(** Extract the root value *)
val extract : 'a t -> int * 'a

(** Returns true if the heap is empty *)
val is_empty : 'a t -> bool