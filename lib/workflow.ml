(** Identifier of a workflow node.

    A node id is just a name, but the signature keeps its representation
    abstract, so node identifiers cannot be confused with arbitrary strings, and
    graphs can key on them through {!module:Node_id.Set} and
    {!module:Node_id.Map}. *)
module Node_id : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end = struct
  type t = string

  let of_string id = id
  let to_string id = id
  let equal = String.equal
  let compare = String.compare

  module Set = Set.Make (String)
  module Map = Map.Make (String)
end

(** Workflow graphs parsed from YAML into a fully validated representation.

    Every value of {!type:Graph.t} satisfies the spec invariants
    {e by construction}:

    - node identifiers are unique;
    - every dependency refers to a declared node;
    - every edge endpoint refers to a declared node.

    Those facts are established once, in {!val:Graph.of_yaml}, and the [private]
    record types let the rest of the program read graphs without re-checking
    what parsing already guaranteed. A value of {!type:Graph.t} cannot be built
    any other way, so it cannot encode an invalid graph. *)
module Graph : sig
  type node = private {
    id : Node_id.t;
    prompt : string;
    result : string;
    deps : Node_id.t list;
        (** Dependencies, each guaranteed to be the id of a declared node. *)
  }

  type edge = private {
    source : Node_id.t;  (** Guaranteed to be a declared node. *)
    target : Node_id.t;  (** Guaranteed to be a declared node. *)
  }

  type t = private {
    name : string;
    nodes : node list;
    edges : edge list;
  }

  (** Why a YAML value failed to describe a valid workflow graph. Each case
      records what was expected, what was seen, and where. *)
  type error =
    | Unexpected_type of {
        path : string;
        expected : string;
        found : string;
      }
    | Missing_field of {
        path : string;
        field : string;
      }
    | Duplicate_node of Node_id.t
    | Unknown_dependency of {
        node : Node_id.t;
        dependency : Node_id.t;
      }
    | Unknown_edge_endpoint of {
        edge_index : int;
        endpoint : [ `Source | `Target ];
        id : Node_id.t;
      }

  val string_of_error : error -> string

  val of_yaml : Yaml.value -> (t, error) result
  (** Parse and validate a workflow graph from an already-decoded YAML value.

      Decoding YAML source text into a {!type:Yaml.value} is the caller's
      responsibility (e.g. via [Yaml.of_string] / [Yaml.of_file]). *)

  type topological_order = private Node_id.t list
  (** A topological ordering of a graph's nodes: every node appears after all of
      its dependencies. Existence of this value is a witness that the graph is
      acyclic. *)

  type cycle = private Node_id.t list
  (** A non-empty sequence of node ids [[n0; n1; ...; nk]] where each [n(i+1)]
      is a dependency of [n(i)], and [n0] is a dependency of [nk]: a witness
      that the graph is cyclic. *)

  type cycle_status =
    | Acyclic of topological_order
    | Cyclic of cycle

  val cycle_status : t -> cycle_status
  (** Classify a graph, returning the evidence for the verdict: a topological
      order when acyclic, or a concrete cycle when cyclic. *)

  val is_cyclic : t -> bool
  (** [is_cyclic g] is [true] iff {!val:cycle_status} returns {!constr:Cyclic}.
  *)
end = struct
  let ( let* ) = Result.bind

  type node = {
    id : Node_id.t;
    prompt : string;
    result : string;
    deps : Node_id.t list;
  }

  type edge = {
    source : Node_id.t;
    target : Node_id.t;
  }

  type t = {
    name : string;
    nodes : node list;
    edges : edge list;
  }

  type error =
    | Unexpected_type of {
        path : string;
        expected : string;
        found : string;
      }
    | Missing_field of {
        path : string;
        field : string;
      }
    | Duplicate_node of Node_id.t
    | Unknown_dependency of {
        node : Node_id.t;
        dependency : Node_id.t;
      }
    | Unknown_edge_endpoint of {
        edge_index : int;
        endpoint : [ `Source | `Target ];
        id : Node_id.t;
      }

  let string_of_error error =
    match error with
    | Unexpected_type { path; expected; found } ->
        Printf.sprintf "%s: expected %s but found %s" path expected found
    | Missing_field { path; field } ->
        Printf.sprintf "%s: missing required field %S" path field
    | Duplicate_node id ->
        Printf.sprintf "duplicate node id %S" (Node_id.to_string id)
    | Unknown_dependency { node; dependency } ->
        Printf.sprintf "node %S depends on unknown node %S"
          (Node_id.to_string node)
          (Node_id.to_string dependency)
    | Unknown_edge_endpoint { edge_index; endpoint; id } ->
        let which = match endpoint with `Source -> "from" | `Target -> "to" in
        Printf.sprintf "edges[%d]: %s refers to unknown node %S" edge_index
          which (Node_id.to_string id)

  (* Stage one: project the loosely-typed YAML tree onto the spec's shape, while
     it is still expressed in terms of strings. This stage owns all the "is this
     the right kind of value?" reasoning, with precise paths. *)

  type raw_node = {
    raw_id : string;
    raw_prompt : string;
    raw_result : string;
    raw_deps : string list;
  }

  type raw_edge = {
    raw_source : string;
    raw_target : string;
  }

  let kind (value : Yaml.value) : string =
    match value with
    | `Null -> "null"
    | `Bool _ -> "boolean"
    | `Float _ -> "number"
    | `String _ -> "string"
    | `A _ -> "list"
    | `O _ -> "object"

  let expect_object ~path value : ((string * Yaml.value) list, error) result =
    match value with
    | `O fields -> Ok fields
    | other ->
        Error
          (Unexpected_type { path; expected = "object"; found = kind other })

  let expect_string ~path value : (string, error) result =
    match value with
    | `String s -> Ok s
    | other ->
        Error
          (Unexpected_type { path; expected = "string"; found = kind other })

  let expect_list ~path value : (Yaml.value list, error) result =
    match value with
    | `A values -> Ok values
    | other ->
        Error (Unexpected_type { path; expected = "list"; found = kind other })

  let field ~path name fields : (Yaml.value, error) result =
    match List.assoc_opt name fields with
    | Some value -> Ok value
    | None -> Error (Missing_field { path; field = name })

  let string_field ~path name fields : (string, error) result =
    let* value = field ~path name fields in
    expect_string ~path:(path ^ "." ^ name) value

  (* Apply [f] across a list, threading errors and preserving order. *)
  let map_result f items : ('b list, 'e) result =
    let rec loop acc items : ('b list, 'e) result =
      match items with
      | [] -> Ok (List.rev acc)
      | item :: rest ->
          let* mapped = f item in
          loop (mapped :: acc) rest
    in
    loop [] items

  (* Apply [f] across a list together with each element's index. *)
  let map_indexed f items : ('b list, 'e) result =
    map_result (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) items)

  let parse_string_list ~path value : (string list, error) result =
    let* items = expect_list ~path value in
    map_indexed
      (fun index item ->
        expect_string ~path:(Printf.sprintf "%s[%d]" path index) item)
      items

  let parse_raw_node ~path value : (raw_node, error) result =
    let* fields = expect_object ~path value in
    let* raw_id = string_field ~path "id" fields in
    let* raw_prompt = string_field ~path "prompt" fields in
    let* raw_result = string_field ~path "result" fields in
    let* raw_deps =
      match List.assoc_opt "deps" fields with
      | None -> Ok []
      | Some value -> parse_string_list ~path:(path ^ ".deps") value
    in
    Ok { raw_id; raw_prompt; raw_result; raw_deps }

  let parse_raw_edge ~path value : (raw_edge, error) result =
    let* fields = expect_object ~path value in
    let* raw_source = string_field ~path "from" fields in
    let* raw_target = string_field ~path "to" fields in
    Ok { raw_source; raw_target }

  let parse_indexed_list ~path parse_item value : ('a list, error) result =
    let* items = expect_list ~path value in
    map_indexed
      (fun index item ->
        parse_item ~path:(Printf.sprintf "%s[%d]" path index) item)
      items

  let parse_raw value : (string * raw_node list * raw_edge list, error) result =
    let* fields = expect_object ~path:"$" value in
    let* name = string_field ~path:"$" "name" fields in
    let* nodes =
      let* value = field ~path:"$" "nodes" fields in
      parse_indexed_list ~path:"nodes" parse_raw_node value
    in
    let* edges =
      match List.assoc_opt "edges" fields with
      | None -> Ok []
      | Some value -> parse_indexed_list ~path:"edges" parse_raw_edge value
    in
    Ok (name, nodes, edges)

  (* Stage two: turn the string-shaped tree into the validated graph. This is
     where references become facts: ids are proven unique, and every dependency
     and edge endpoint is resolved against the declared nodes. *)

  let declared_ids raw_nodes : (Node_id.Set.t, error) result =
    let rec loop seen nodes : (Node_id.Set.t, error) result =
      match nodes with
      | [] -> Ok seen
      | raw :: rest ->
          let id = Node_id.of_string raw.raw_id in
          if Node_id.Set.mem id seen then Error (Duplicate_node id)
          else loop (Node_id.Set.add id seen) rest
    in
    loop Node_id.Set.empty raw_nodes

  let resolve_node ~declared raw : (node, error) result =
    let id = Node_id.of_string raw.raw_id in
    let resolve_dep dep_string : (Node_id.t, error) result =
      let dependency = Node_id.of_string dep_string in
      if Node_id.Set.mem dependency declared then Ok dependency
      else Error (Unknown_dependency { node = id; dependency })
    in
    let* deps = map_result resolve_dep raw.raw_deps in
    Ok { id; prompt = raw.raw_prompt; result = raw.raw_result; deps }

  let resolve_edge ~declared ~index raw : (edge, error) result =
    let source = Node_id.of_string raw.raw_source in
    let target = Node_id.of_string raw.raw_target in
    let check endpoint id : (unit, error) result =
      if Node_id.Set.mem id declared then Ok ()
      else Error (Unknown_edge_endpoint { edge_index = index; endpoint; id })
    in
    let* () = check `Source source in
    let* () = check `Target target in
    Ok { source; target }

  let of_yaml value =
    let* name, raw_nodes, raw_edges = parse_raw value in
    let* declared = declared_ids raw_nodes in
    let* nodes = map_result (resolve_node ~declared) raw_nodes in
    let* edges =
      map_indexed (fun index raw -> resolve_edge ~declared ~index raw) raw_edges
    in
    Ok { name; nodes; edges }

  type topological_order = Node_id.t list
  type cycle = Node_id.t list

  type cycle_status =
    | Acyclic of topological_order
    | Cyclic of cycle

  (* Classify [graph] with a depth-first search.

     Determinism: the traversal order is fixed entirely by list order -- [drive]
     over [graph.nodes], [walk] over each node's deps -- and [visited] / [deps_of]
     are only queried ([mem] / [find_opt]), never iterated. With no randomness
     and no set/map enumeration, the same graph value always yields the same
     verdict and, when cyclic, the same witnessing cycle.

     That witness is the cycle closed by the *first* back-edge the search hits,
     not a canonical or minimal one: reordering otherwise-equivalent [nodes] /
     [deps] can surface a different (still valid) cycle. *)
  let cycle_status graph =
    let deps_of =
      List.fold_left
        (fun acc node -> Node_id.Map.add node.id node.deps acc)
        Node_id.Map.empty graph.nodes
    in
    let rec drive ~visited ~order nodes : (Node_id.t list, cycle) result =
      match nodes with
      | [] -> Ok order
      | node :: rest ->
          let* visited, order = visit ~stack:[] ~visited ~order node.id in
          drive ~visited ~order rest
    and visit ~stack ~visited ~order id :
        (Node_id.Set.t * Node_id.t list, cycle) result =
      if Node_id.Set.mem id visited then Ok (visited, order)
      else if List.exists (Node_id.equal id) stack then
        (* [id] is an ancestor still being expanded: a back-edge, hence a cycle.
           This check fires before [id] would be pushed, so [id] occurs at most
           once on [stack]. *)
        Error (cycle_through ~stack id)
      else
        let rec walk ~visited ~order deps :
            (Node_id.Set.t * Node_id.t list, cycle) result =
          match deps with
          | [] -> Ok (Node_id.Set.add id visited, id :: order)
          | dep :: rest ->
              let* visited, order =
                visit ~stack:(id :: stack) ~visited ~order dep
              in
              walk ~visited ~order rest
        in
        walk ~visited ~order (dependencies id)
    (* The current DFS [stack] (most recent first) is the chain of nodes being
       expanded; revisiting one of them closes a cycle. Recover it by reading
       the stack back to that node.

       This is a pure function of [stack] and [id], so equal inputs give equal
       output. And because [id] occurs exactly once on [stack] (see [visit]),
       [upto] has a single, unambiguous stopping point. *)
    and cycle_through ~stack id : cycle =
      let rec upto path : Node_id.t list =
        match path with
        | [] -> []
        | x :: rest -> if Node_id.equal x id then [ x ] else x :: upto rest
      in
      List.rev (upto stack)
    and dependencies id : Node_id.t list =
      Option.value ~default:[] (Node_id.Map.find_opt id deps_of)
    in
    match drive ~visited:Node_id.Set.empty ~order:[] graph.nodes with
    | Ok order -> Acyclic (List.rev order)
    | Error cycle -> Cyclic cycle

  let is_cyclic graph =
    match cycle_status graph with Cyclic _ -> true | Acyclic _ -> false
end
