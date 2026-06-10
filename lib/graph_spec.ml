type node = {
  id : string;
  prompt : string;
  result : string;
  deps : string list;
}

type edge = {
  from : string;
  to_ : string;
}

type t = {
  name : string;
  nodes : node list;
  edges : edge list;
}

type parse_error = string

let ( let* ) = Result.bind
let ( >>= ) = Result.bind

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing required field: " ^ name)

let optional_field name fields = List.assoc_opt name fields

let as_object field_name = function
  | `O fields -> Ok fields
  | _ -> Error (field_name ^ " must be an object")

let as_string field_name = function
  | `String value -> Ok value
  | _ -> Error (field_name ^ " must be a string")

let as_list field_name = function
  | `A values -> Ok values
  | _ -> Error (field_name ^ " must be a list")

let parse_string_list field_name value =
  let* values = as_list field_name value in
  List.fold_right
    (fun value acc ->
      let* values = acc in
      let* value = as_string field_name value in
      Ok (value :: values))
    values (Ok [])

let parse_node value =
  let* fields = as_object "node" value in
  let* id = field "id" fields >>= as_string "node.id" in
  let* prompt = field "prompt" fields >>= as_string "node.prompt" in
  let* result = field "result" fields >>= as_string "node.result" in
  let* deps =
    match optional_field "deps" fields with
    | Some deps -> parse_string_list "node.deps" deps
    | None -> Ok []
  in
  Ok { id; prompt; result; deps }

let parse_edge value =
  let* fields = as_object "edge" value in
  let* from = field "from" fields >>= as_string "edge.from" in
  let* to_ = field "to" fields >>= as_string "edge.to" in
  Ok { from; to_ }

let parse_list field_name parse_item fields =
  let* value = field field_name fields in
  let* values = as_list field_name value in
  List.fold_right
    (fun value acc ->
      let* values = acc in
      let* value = parse_item value in
      Ok (value :: values))
    values (Ok [])

let of_yaml_value value =
  let* fields = as_object "graph" value in
  let* name = field "name" fields >>= as_string "name" in
  let* nodes = parse_list "nodes" parse_node fields in
  let* edges = parse_list "edges" parse_edge fields in
  Ok { name; nodes; edges }

let of_string yaml =
  match Yaml.of_string yaml with
  | Ok value -> of_yaml_value value
  | Error (`Msg message) -> Error message
