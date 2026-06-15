let yaml =
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

let cyclic_yaml =
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
edges:
  - from: a
    to: b
  - from: b
    to: a
|}

let () =
  match One_ring.Graph_spec.of_string yaml with
  | Error message -> failwith message
  | Ok graph ->
      let open One_ring.Graph_spec in
      assert (String.equal graph.name "research-workflow");
      assert (List.length graph.nodes = 2);
      assert (List.length graph.edges = 1);
      let draft : node = List.nth graph.nodes 1 in
      assert (draft.deps = [ "gather-context" ]);
      let edge = List.hd graph.edges in
      assert (String.equal edge.from "gather-context");
      assert (String.equal edge.to_ "draft-answer");
      assert (cycle_status graph = Acyclic);
      assert (not (is_cyclic graph))

let () =
  match One_ring.Graph_spec.of_string cyclic_yaml with
  | Error message -> failwith message
  | Ok graph ->
      let open One_ring.Graph_spec in
      assert (cycle_status graph = Cyclic);
      assert (is_cyclic graph)
