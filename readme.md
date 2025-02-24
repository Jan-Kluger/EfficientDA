## Working on effeicient algoritms and datastructures in functional OCaml

Hobby project working on efficient algorithms and data structures in purley functional (except b+ trees) OCaml. 

## Finished projects

- #### Max flow graph solver
Under ``` graph.ml ``` the function ``` maxflow ``` computes the max flow on the graph which has been build. The algorithm being used is the Ford Fulkerson augmenting path algorithm for max flow computation. Removing dead end nodes is *not* implemented, despite improving runtime, since im too lazy.