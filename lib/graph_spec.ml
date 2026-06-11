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

let rec yaml_to_yojson = function
  | `Null -> `Null
  | `Bool value -> `Bool value
  | `Float value -> `Float value
  | `String value -> `String value
  | `A values -> `List (List.map yaml_to_yojson values)
  | `O fields ->
      `Assoc
        (List.map
           (fun (key, value) -> (key, yaml_to_yojson value))
           fields)

let of_yaml_value value =
  match of_yojson (yaml_to_yojson value) with
  | Ok graph -> Ok graph
  | Error message -> Error message

let of_string yaml =
  match Yaml.of_string yaml with
  | Ok value -> of_yaml_value value
  | Error (`Msg message) -> Error message
