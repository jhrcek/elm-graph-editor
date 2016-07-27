# Elm Graph editor

A simple browser-based graph editor written in [Elm](http://elm-lang.org/). It uses [vis.js](http://visjs.org/) library for graph visualization and layout.

## How to build it

1. [Install Elm](http://elm-lang.org/install)
2. Clone this repository: `git clone git@github.com:jhrcek/elm-graph-editor.git`
3. `cd elm-graph-editor`
4. `./build.sh`
5. Open the file `index.html` in your browser

## TODOs

- [x] Generate simple graph source code in several formats, in particular
    - [x] [dot](http://www.graphviz.org/content/dot-language) format that can be used by [graphviz](http://graphviz.org/)
    - [x] [tgf](https://en.wikipedia.org/wiki/Trivial_Graph_Format) format that can be imported by tools like [gephi](https://gephi.org/) or [yed](https://www.yworks.com/products/yed)
    - [x] String based representation suitable for [fromNodesAndEdges](http://package.elm-lang.org/packages/elm-community/graph/1.0.0/Graph#fromNodesAndEdges) function of elm-community/elm-graph library
- [ ] Implement update functionality for nodes and edges
- [ ] Simpler way to create edges using drag & drop interactions via vis' canvas
