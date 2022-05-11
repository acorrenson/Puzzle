type 'a t = {
  mutable size : int;
  values : (int * 'a) Vector.t;
}

(* Get the parent of a given index *)
let parent i = (i - 1)/2

(* Get the right son of a given index *)
let sonl i = 2*i + 1

(* Get the left son of a given index *)
let sonr i = 2*i + 2

(* Restore the heap in [i]
   assuming that subtrees *)
let rec heapify h i : unit =
  let imax = List.fold_left (fun acc j ->
      if Vector.get h.values j <= Vector.get h.values acc
      then j else acc
    ) i ([sonl i; sonr i] |> List.filter ((>) (h.size)))
  in
  if imax <> i then begin
    Vector.swap h.values i imax;
    heapify h imax
  end

let of_array a =
  let ra = Vector.of_array a in
  let l = Vector.length ra in
  let h = { size = l; values = ra } in
  for i = (l/2) - 1 downto 0 do
    heapify h i
  done;
  h

let sift_up h i =
  let p = ref (parent i) in
  let c = ref i in
  let v = Vector.get h.values i in
  while (!c > 0 && v <= Vector.get h.values !p) do
    Vector.swap h.values !p !c;
    p := parent !p;
    c := parent !c;
  done

let insert h x px =
  Vector.set h.values h.size (px, x);
  sift_up h h.size;
  h.size <- h.size + 1

let extract h =
  if h.size = 0 then failwith "Heap is of size 0 : cannot extract root"
  else begin
    let root = Vector.get h.values 0 in
    Vector.swap h.values 0 (h.size -1);
    h.size <- h.size -1;
    heapify h 0;
    root
  end

let to_array h =
  Array.init h.size (Vector.get h.values)

let is_empty h =
  h.size = 0