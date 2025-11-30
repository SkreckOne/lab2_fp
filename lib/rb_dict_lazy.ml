module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

module Make (Key : OrderedType) : S with type key = Key.t = struct
  type key = Key.t
  type color = Red | Black
  type 'a tree = Leaf | Node of { color : color; left : 'a tree; key : key; value : 'a; is_deleted : bool; right : 'a tree }
  type 'a t = { root : 'a tree; active : int; total : int }

  let empty = { root = Leaf; active = 0; total = 0 }
  let is_empty t = t.active = 0

  [@@@ocamlformat "disable"]

  let balance color left key value is_deleted right =
    let make_balanced a kx vx dx b ky vy dy c kz vz dz d =
      Node
        {
          color = Red;
          left = Node { color = Black; left = a; key = kx; value = vx; is_deleted = dx; right = b };
          key = ky;
          value = vy;
          is_deleted = dy;
          right = Node { color = Black; left = c; key = kz; value = vz; is_deleted = dz; right = d };
        }
    in

    match (color, left, key, value, is_deleted, right) with
    | Black, Node { color=Red; left=Node {color=Red; left=a; key=kx; value=vx; is_deleted=dx; right=b}; key=ky; value=vy; is_deleted=dy; right=c }, kz, vz, dz, d
    | Black, Node { color=Red; left=a; key=kx; value=vx; is_deleted=dx; right=Node {color=Red; left=b; key=ky; value=vy; is_deleted=dy; right=c} }, kz, vz, dz, d
    | Black, a, kx, vx, dx, Node { color=Red; left=Node {color=Red; left=b; key=ky; value=vy; is_deleted=dy; right=c}; key=kz; value=vz; is_deleted=dz; right=d }
    | Black, a, kx, vx, dx, Node { color=Red; left=b; key=ky; value=vy; is_deleted=dy; right=Node {color=Red; left=c; key=kz; value=vz; is_deleted=dz; right=d} }
      ->
        make_balanced a kx vx dx b ky vy dy c kz vz dz d

    | c, l, k, v, del, r -> Node { color = c; left = l; key = k; value = v; is_deleted = del; right = r }

  [@@@ocamlformat "enable"]

  let internal_add key value tree =
    let rec ins t =
      match t with
      | Leaf -> Node { color = Red; left = Leaf; key; value; is_deleted = false; right = Leaf }
      | Node { color = c; left = l; key = k; value = v; is_deleted = del; right = r } ->
          let cmp = Key.compare key k in
          if cmp < 0 then balance c (ins l) k v del r
          else if cmp > 0 then balance c l k v del (ins r)
          else Node { color = c; left = l; key = k; value; is_deleted = false; right = r }
    in
    match ins tree with Node r -> Node { r with color = Black } | Leaf -> Leaf

  let rec internal_find_opt key t =
    match t with
    | Leaf -> None
    | Node { left = l; key = k; value = v; is_deleted = del; right = r; _ } ->
        let cmp = Key.compare key k in
        if cmp < 0 then internal_find_opt key l else if cmp > 0 then internal_find_opt key r else if del then None else Some v

  let internal_remove key t =
    let rec loop t =
      match t with
      | Leaf -> Leaf
      | Node { color = c; left = l; key = k; value = v; is_deleted = del; right = r } ->
          let cmp = Key.compare key k in
          if cmp < 0 then Node { color = c; left = loop l; key = k; value = v; is_deleted = del; right = r }
          else if cmp > 0 then Node { color = c; left = l; key = k; value = v; is_deleted = del; right = loop r }
          else Node { color = c; left = l; key = k; value = v; is_deleted = true; right = r }
    in
    loop t

  let rec internal_fold f t acc =
    match t with
    | Leaf -> acc
    | Node { left = l; key = k; value = v; is_deleted = del; right = r; _ } ->
        let acc_l = internal_fold f l acc in
        let acc_curr = if del then acc_l else f k v acc_l in
        internal_fold f r acc_curr

  let rec mem_physical key t =
    match t with
    | Leaf -> false
    | Node { left = l; key = k; right = r; _ } ->
        let cmp = Key.compare key k in
        if cmp < 0 then mem_physical key l else if cmp > 0 then mem_physical key r else true

  let rebuild t =
    let new_root = internal_fold (fun k v acc -> internal_add k v acc) t.root Leaf in
    { root = new_root; active = t.active; total = t.active }

  let find_opt key t = internal_find_opt key t.root

  let add key value t =
    let root = t.root in
    let new_root = internal_add key value root in

    match internal_find_opt key root with
    | Some _ -> { t with root = new_root }
    | None ->
        let is_resurrection = mem_physical key root in
        let new_active = t.active + 1 in
        let new_total = if is_resurrection then t.total else t.total + 1 in
        { root = new_root; active = new_active; total = new_total }

  let remove key t =
    match internal_find_opt key t.root with
    | None -> t
    | Some _ ->
        let new_root = internal_remove key t.root in
        let new_active = t.active - 1 in
        let new_t = { root = new_root; total = t.total; active = new_active } in

        if new_t.total > 2 * new_t.active && new_t.active > 0 then rebuild new_t else new_t

  let fold f t acc = internal_fold f t.root acc
  let union t1 t2 = fold (fun k v acc -> add k v acc) t1 t2

  let map f t =
    let rec internal_map t =
      match t with
      | Leaf -> Leaf
      | Node ({ left = l; value = v; right = r; _ } as node_record) -> Node { node_record with left = internal_map l; value = f v; right = internal_map r }
    in
    { t with root = internal_map t.root }

  let filter predicate t =
    let rec internal_filter t =
      match t with
      | Leaf -> Leaf
      | Node ({ left = l; key = k; value = v; is_deleted = del; right = r; _ } as node) ->
          let should_keep = predicate k v in
          let new_del = del || not should_keep in
          Node { node with left = internal_filter l; is_deleted = new_del; right = internal_filter r }
    in
    let filtered_root = internal_filter t.root in
    let temp_t = { root = filtered_root; active = 0; total = t.total } in
    rebuild temp_t

  let rec internal_to_seq t =
    let open Seq in
    match t with
    | Leaf -> empty
    | Node { left = l; key = k; value = v; is_deleted = del; right = r; _ } ->
        let left_seq = internal_to_seq l in
        let right_seq = internal_to_seq r in
        let node_seq = if del then empty else return (k, v) in
        append left_seq (append node_seq right_seq)

  let to_seq t = internal_to_seq t.root

  let compare cmp_val t1 t2 =
    let seq1 = to_seq t1 in
    let seq2 = to_seq t2 in
    let rec loop s1 s2 =
      match (s1 (), s2 ()) with
      | Seq.Nil, Seq.Nil -> 0
      | Seq.Nil, _ -> -1
      | _, Seq.Nil -> 1
      | Seq.Cons ((k1, v1), next1), Seq.Cons ((k2, v2), next2) ->
          let c_key = Key.compare k1 k2 in
          if c_key <> 0 then c_key
          else
            let c_val = cmp_val v1 v2 in
            if c_val <> 0 then c_val else loop next1 next2
    in
    loop seq1 seq2
end
