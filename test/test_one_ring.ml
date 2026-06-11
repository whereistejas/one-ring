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

let () =
  match One_ring.Graph_spec.of_string yaml with
  | Error message -> failwith message
  | Ok graph ->
      assert (String.equal graph.name "research-workflow");
      assert (List.length graph.nodes = 2);
      assert (List.length graph.edges = 1);
      let draft = List.nth graph.nodes 1 in
      assert (draft.deps = [ "gather-context" ]);
      let edge = List.hd graph.edges in
      assert (String.equal edge.from "gather-context");
      assert (String.equal edge.to_ "draft-answer")
