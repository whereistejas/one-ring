type node = {
  id : string;
  prompt : string;
  result : string;
  deps : string list; [@default []]
}
[@@deriving yojson]

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
[@@deriving yojson]

type parse_error = string

type cycle_status = Cyclic | Acyclic

let rec yaml_to_yojson = function
  | `Null -> `Null
  | `Bool value -> `Bool value
  | `Float value -> `Float value
  | `String value -> `String value
  | `A values -> `List (List.map yaml_to_yojson values)
  | `O fields -> `Assoc (List.map (fun (k, v) -> (k, yaml_to_yojson v)) fields)

let node_exists nodes id = List.exists (fun node -> String.equal node.id id) nodes

let validate_deps graph =
  let validate_node node =
    match List.find_opt (fun dep -> not (node_exists graph.nodes dep)) node.deps with
    | Some dep -> Error ("unknown node dependency: " ^ dep)
    | None -> Ok ()
  in
  match List.find_map (fun node -> Result.fold ~ok:(fun () -> None) ~error:Option.some (validate_node node)) graph.nodes with
  | Some error -> Error error
  | None -> Ok graph

let cycle_status graph =
  let rec visit path node =
    List.mem node.id path
    || List.exists
         (fun dep ->
           match List.find_opt (fun node -> String.equal node.id dep) graph.nodes with
           | Some dep_node -> visit (node.id :: path) dep_node
           | None -> false)
         node.deps
  in
  if List.exists (visit []) graph.nodes then Cyclic else Acyclic

let is_cyclic graph = cycle_status graph = Cyclic

let of_yaml_value value =
  match of_yojson (yaml_to_yojson value) with
  | Ok graph -> validate_deps graph
  | Error message -> Error message

let of_string yaml =
  match Yaml.of_string yaml with
  | Ok value -> of_yaml_value value
  | Error (`Msg message) -> Error message
