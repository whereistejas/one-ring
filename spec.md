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

## Edges

Each entry in `edges` defines a directed relationship between two nodes.

- `from`: source node ID.
- `to`: destination node ID.

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
