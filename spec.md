# one-ring workflow graph specification

A workflow graph is defined as YAML with three top-level fields:

- `name`: workflow graph name.
- `nodes`: collection of workflow nodes.
- `edges`: collection of directed edges between nodes.

## Schema

```yaml
name: string
nodes:
  - id: string
    prompt: string
    result: string
    deps:
      - string
edges:
  - from: string
    to: string
```

## Nodes

Each entry in `nodes` defines a node in the workflow graph.

- `id`: unique node identifier.
- `prompt`: prompt or instruction associated with the node.
- `result`: expected or computed node result.
- `deps`: optional list of node IDs this node depends on.

Node IDs must be unique across the graph, and every ID listed in `deps` must
refer to a node declared in `nodes`. Parsing fails with a specific error
otherwise.

## Edges

Each entry in `edges` defines a directed relationship between two nodes.

- `from`: source node ID.
- `to`: destination node ID.

Both `from` and `to` must refer to nodes declared in `nodes`.

## Cycles

Workflow graphs may be cyclic. Classifying a parsed graph reports either:

- `Acyclic`, with a topological ordering of the nodes — every node ordered
  after all of its dependencies; or
- `Cyclic`, with a concrete cycle — a sequence of node IDs that forms a
  dependency loop.

`is_cyclic` is a boolean shortcut for callers that only need the verdict.

## Example

```yaml
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
```
