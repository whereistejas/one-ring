type node = {
  id : string;
  prompt : string;
  result : string;
  mutable deps : node list;
}

type edge = {
  from : string;
  to_ : string; [@key "to"]
}
[@@deriving yojson]

type t = {
  name : string;
  nodes : node list;
  edges : edge list;
}

type parse_error = string

type cycle_status = Cyclic | Acyclic

module Raw = struct
  type node = {
    id : string;
    prompt : string;
    result : string;
    deps : string list; [@default []]
  }
  [@@deriving yojson]

  type t = {
    name : string;
    nodes : node list;
    edges : edge list;
  }
  [@@deriving yojson]
end

let ( let* ) = Result.bind

let rec yaml_to_yojson = function
  | `Null -> `Null
  | `Bool value -> `Bool value
  | `Float value -> `Float value
  | `String value -> `String value
  | `A values -> `List (List.map yaml_to_yojson values)
  | `O fields -> `Assoc (List.map (fun (k, v) -> (k, yaml_to_yojson v)) fields)

let map_result f values =
  List.fold_right
    (fun value acc ->
      let* values = acc in
      let* value = f value in
      Ok (value :: values))
    values (Ok [])

let resolve_graph (raw : Raw.t) =
  let table = Hashtbl.create (List.length raw.nodes) in
  let nodes =
    List.map
      (fun (raw_node : Raw.node) ->
        let node =
          { id = raw_node.id; prompt = raw_node.prompt; result = raw_node.result; deps = [] }
        in
        Hashtbl.add table raw_node.id node;
        (raw_node, node))
      raw.nodes
  in
  let* _ =
    map_result
      (fun ((raw_node, node) : Raw.node * node) ->
        let* deps =
          map_result
            (fun id ->
              match Hashtbl.find_opt table id with
              | Some node -> Ok node
              | None -> Error ("unknown node dependency: " ^ id))
            raw_node.deps
        in
        node.deps <- deps;
        Ok ())
      nodes
  in
  Ok { name = raw.name; nodes = List.map snd nodes; edges = raw.edges }

let cycle_status (graph : t) =
  let seen = Hashtbl.create (List.length graph.nodes) in
  let rec visit (node : node) =
    match Hashtbl.find_opt seen node.id with
    | Some `Visiting -> true
    | Some `Visited -> false
    | None ->
        Hashtbl.add seen node.id `Visiting;
        let cyclic = List.exists visit node.deps in
        Hashtbl.replace seen node.id `Visited;
        cyclic
  in
  if List.exists visit graph.nodes then Cyclic else Acyclic

let is_cyclic graph = cycle_status graph = Cyclic

let of_yaml_value value =
  match Raw.of_yojson (yaml_to_yojson value) with
  | Ok raw -> resolve_graph raw
  | Error message -> Error message

let of_string yaml =
  match Yaml.of_string yaml with
  | Ok value -> of_yaml_value value
  | Error (`Msg message) -> Error message
