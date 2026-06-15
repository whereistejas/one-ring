module G = One_ring.Workflow.Graph
module Node_id = One_ring.Workflow.Node_id

let ( let* ) = Result.bind

(* Decoding YAML text is the caller's job, so the test harness does it before
   handing a [Yaml.value] to {!G.of_yaml}. Malformed test YAML is a bug in the
   test, not a graph error, so we abort loudly. *)
let parse_text text =
  match Yaml.of_string text with
  | Ok value -> G.of_yaml value
  | Error (`Msg m) -> failwith ("test YAML is malformed: " ^ m)

(* ------------------------------------------------------------------ *)
(* A tiny property-test harness over the stdlib PRNG. Each property is a
   function from a generated input to [(unit, string) result]; on failure we
   print the offending input so the case is reproducible from the fixed seed. *)

let iterations = 2000

let forall name ~gen ~show property =
  let rec loop i =
    if i >= iterations then
      Printf.printf "ok   %s (%d cases)\n%!" name iterations
    else begin
      let input = gen () in
      (match property input with
      | Ok () -> ()
      | Error reason ->
          Printf.eprintf "FAIL %s [case %d]\n  reason: %s\n  input:\n%s\n%!"
            name i reason (show input);
          exit 1);
      loop (i + 1)
    end
  in
  loop 0

let check name condition reason =
  if condition then Ok () else Error (reason ^ " (" ^ name ^ ")")

(* ------------------------------------------------------------------ *)
(* Generators. A "spec" here is the abstract description of a graph we feed in;
   we generate it, render it to YAML, and check what the parser makes of it. *)

type spec = {
  nodes : (string * string list) list; (* id, dependency ids *)
  edges : (string * string) list;
}

let node_ids spec = List.map fst spec.nodes

let show_spec spec =
  let node (id, deps) =
    Printf.sprintf "    %s -> [%s]" id (String.concat "; " deps)
  in
  let edge (from, to_) = Printf.sprintf "    %s => %s" from to_ in
  String.concat "\n"
    (("  nodes:" :: List.map node spec.nodes)
    @ ("  edges:" :: List.map edge spec.edges))

let yaml_of_spec spec : Yaml.value =
  let node (id, deps) =
    `O
      [
        ("id", `String id);
        ("prompt", `String ("prompt for " ^ id));
        ("result", `String ("result for " ^ id));
        ("deps", `A (List.map (fun d -> `String d) deps));
      ]
  in
  let edge (from, to_) = `O [ ("from", `String from); ("to", `String to_) ] in
  `O
    [
      ("name", `String "generated");
      ("nodes", `A (List.map node spec.nodes));
      ("edges", `A (List.map edge spec.edges));
    ]

let ids_upto n = List.init n (fun i -> Printf.sprintf "n%d" i)

(* An acyclic spec: node i may only depend on earlier nodes, so the declaration
   order is itself a valid topological order. *)
let gen_dag () =
  let n = 1 + Random.int 6 in
  let ids = ids_upto n in
  let nodes =
    List.mapi
      (fun i id ->
        let deps = List.filteri (fun j _ -> j < i && Random.bool ()) ids in
        (id, deps))
      ids
  in
  { nodes; edges = [] }

(* A definitely-cyclic spec: a ring n0 -> n1 -> ... -> n(k) -> n0 (a self-loop
   when there is a single node). *)
let gen_ring () =
  let n = 1 + Random.int 6 in
  let ids = ids_upto n in
  let nodes =
    List.mapi (fun i id -> (id, [ Printf.sprintf "n%d" ((i + 1) mod n) ])) ids
  in
  { nodes; edges = [] }

let random_choice list = List.nth list (Random.int (List.length list))

(* ------------------------------------------------------------------ *)
(* Helpers shared by the properties. *)

let parse_ok spec =
  match G.of_yaml (yaml_of_spec spec) with
  | Ok graph -> Ok graph
  | Error e ->
      Error ("expected a valid graph but parsing failed: " ^ G.string_of_error e)

let dep_strings (node : G.node) = List.map Node_id.to_string node.deps

let index_in order id =
  let rec aux i lst =
    match lst with
    | [] -> None
    | x :: _ when String.equal x id -> Some i
    | _ :: rest -> aux (i + 1) rest
  in
  aux 0 order

let sort = List.sort String.compare

(* ------------------------------------------------------------------ *)
(* Properties. *)

(* Parsing preserves the declared structure: names, node count, and each node's
   dependency set survive the round-trip into the typed representation. *)
let prop_structure_preserved spec =
  let* graph = parse_ok spec in
  let* () =
    check "name" (String.equal graph.G.name "generated") "name not preserved"
  in
  let* () =
    check "node count"
      (List.length graph.G.nodes = List.length spec.nodes)
      "node count changed"
  in
  let deps_match (parsed, (_, declared)) =
    sort (dep_strings parsed) = sort declared
  in
  check "deps"
    (List.for_all deps_match (List.combine graph.G.nodes spec.nodes))
    "dependency set not preserved"

(* An acyclic graph is classified [Acyclic] with a topological order that is a
   permutation of all nodes in which every dependency precedes its dependent. *)
let prop_dag_acyclic spec =
  let* graph = parse_ok spec in
  match G.cycle_status graph with
  | G.Cyclic _ -> Error "DAG misclassified as cyclic"
  | G.Acyclic order ->
      let order = (order :> Node_id.t list) |> List.map Node_id.to_string in
      let all_present =
        sort order = sort (node_ids spec)
        && List.length order = List.length spec.nodes
      in
      let deps_before_node =
        List.for_all
          (fun (id, deps) ->
            match index_in order id with
            | None -> false
            | Some node_pos ->
                List.for_all
                  (fun dep ->
                    match index_in order dep with
                    | Some dep_pos -> dep_pos < node_pos
                    | None -> false)
                  deps)
          spec.nodes
      in
      if not all_present then
        Error "topological order is not a permutation of nodes"
      else if not deps_before_node then
        Error "a dependency does not precede its node"
      else Ok ()

(* A cyclic graph is classified [Cyclic] with a genuine cycle: consecutive ids
   (with wrap-around) are real dependency edges. *)
let prop_ring_cyclic spec =
  let* graph = parse_ok spec in
  let deps_of =
    List.map
      (fun (n : G.node) -> (Node_id.to_string n.id, dep_strings n))
      graph.G.nodes
  in
  match G.cycle_status graph with
  | G.Acyclic _ -> Error "ring misclassified as acyclic"
  | G.Cyclic cycle -> (
      match (cycle :> Node_id.t list) |> List.map Node_id.to_string with
      | [] -> Error "cycle witness is empty"
      | first :: _ as path ->
          let edge_holds from to_ =
            match List.assoc_opt from deps_of with
            | Some deps -> List.mem to_ deps
            | None -> false
          in
          let rec consecutive nodes =
            match nodes with
            | [ last ] -> edge_holds last first
            | a :: (b :: _ as rest) -> edge_holds a b && consecutive rest
            | [] -> true
          in
          if consecutive path then Ok ()
          else Error "cycle witness is not a real cycle")

(* A dependency on an undeclared node is reported precisely. *)
let prop_unknown_dependency spec =
  let target = random_choice (node_ids spec) in
  let nodes =
    List.map
      (fun (id, deps) ->
        if String.equal id target then (id, "ghost" :: deps) else (id, deps))
      spec.nodes
  in
  match G.of_yaml (yaml_of_spec { spec with nodes }) with
  | Error (G.Unknown_dependency { node; dependency }) ->
      check "unknown dep"
        (String.equal (Node_id.to_string node) target
        && String.equal (Node_id.to_string dependency) "ghost")
        "wrong node/dependency reported"
  | Error other ->
      Error ("expected Unknown_dependency, got: " ^ G.string_of_error other)
  | Ok _ -> Error "expected Unknown_dependency, but parsing succeeded"

(* A duplicated node id is reported as a duplicate. *)
let prop_duplicate_node spec =
  let dup = random_choice (node_ids spec) in
  let nodes = spec.nodes @ [ (dup, []) ] in
  match G.of_yaml (yaml_of_spec { spec with nodes }) with
  | Error (G.Duplicate_node id) ->
      check "duplicate"
        (String.equal (Node_id.to_string id) dup)
        "wrong duplicate id reported"
  | Error other ->
      Error ("expected Duplicate_node, got: " ^ G.string_of_error other)
  | Ok _ -> Error "expected Duplicate_node, but parsing succeeded"

(* An edge to an undeclared node is reported precisely. *)
let prop_unknown_edge_endpoint spec =
  let from = random_choice (node_ids spec) in
  let edges = [ (from, "ghost") ] in
  match G.of_yaml (yaml_of_spec { spec with edges }) with
  | Error (G.Unknown_edge_endpoint { endpoint; id; _ }) ->
      check "unknown edge"
        (endpoint = `Target && String.equal (Node_id.to_string id) "ghost")
        "wrong endpoint reported"
  | Error other ->
      Error ("expected Unknown_edge_endpoint, got: " ^ G.string_of_error other)
  | Ok _ -> Error "expected Unknown_edge_endpoint, but parsing succeeded"

(* Valid edges round-trip through the full YAML text pipeline. *)
let prop_yaml_text_roundtrip spec =
  let ids = node_ids spec in
  let edges =
    List.map
      (fun _ -> (random_choice ids, random_choice ids))
      (ids_upto (Random.int 4))
  in
  let spec = { spec with edges } in
  match Yaml.to_string (yaml_of_spec spec) with
  | Error (`Msg m) -> Error ("could not serialise YAML: " ^ m)
  | Ok text -> (
      match parse_text text with
      | Error e -> Error ("parsing failed: " ^ G.string_of_error e)
      | Ok graph ->
          check "edge count"
            (List.length graph.G.edges = List.length edges)
            "edge count changed through YAML text")

(* ------------------------------------------------------------------ *)
(* Concrete regressions: the worked examples from the spec. *)

let regression_examples () =
  let research =
    {|
name: research-workflow
nodes:
  - id: gather-context
    prompt: Gather context about the topic.
    result: Context summary
  - id: draft-answer
    prompt: Draft an answer using the gathered context.
    result: Draft response
    deps:
      - gather-context
edges:
  - from: gather-context
    to: draft-answer
|}
  in
  (match parse_text research with
  | Error e ->
      failwith ("research-workflow should parse: " ^ G.string_of_error e)
  | Ok graph -> (
      assert (String.equal graph.G.name "research-workflow");
      assert (List.length graph.G.nodes = 2);
      assert (List.length graph.G.edges = 1);
      let draft = List.nth graph.G.nodes 1 in
      assert (dep_strings draft = [ "gather-context" ]);
      let edge = List.hd graph.G.edges in
      assert (String.equal (Node_id.to_string edge.G.source) "gather-context");
      assert (String.equal (Node_id.to_string edge.G.target) "draft-answer");
      assert (not (G.is_cyclic graph));
      match G.cycle_status graph with
      | G.Acyclic order ->
          let order = (order :> Node_id.t list) |> List.map Node_id.to_string in
          assert (order = [ "gather-context"; "draft-answer" ])
      | G.Cyclic _ -> assert false));

  let cyclic =
    {|
name: cyclic-workflow
nodes:
  - id: a
    prompt: A
    result: A
    deps:
      - b
  - id: b
    prompt: B
    result: B
    deps:
      - a
|}
  in
  (match parse_text cyclic with
  | Error e -> failwith ("cyclic-workflow should parse: " ^ G.string_of_error e)
  | Ok graph -> (
      assert (G.is_cyclic graph);
      match G.cycle_status graph with
      | G.Cyclic _ -> ()
      | G.Acyclic _ -> assert false));

  Printf.printf "ok   regression examples\n%!"

(* ------------------------------------------------------------------ *)

let () =
  Random.init 0x5EED;
  forall "structure preserved" ~gen:gen_dag ~show:show_spec
    prop_structure_preserved;
  forall "DAG is acyclic" ~gen:gen_dag ~show:show_spec prop_dag_acyclic;
  forall "ring is cyclic" ~gen:gen_ring ~show:show_spec prop_ring_cyclic;
  forall "unknown dependency" ~gen:gen_dag ~show:show_spec
    prop_unknown_dependency;
  forall "duplicate node" ~gen:gen_dag ~show:show_spec prop_duplicate_node;
  forall "unknown edge endpoint" ~gen:gen_dag ~show:show_spec
    prop_unknown_edge_endpoint;
  forall "YAML text round-trip" ~gen:gen_dag ~show:show_spec
    prop_yaml_text_roundtrip;
  regression_examples ()
