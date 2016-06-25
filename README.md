# Elm Graph editor

A simple browser-based graph editor written in [Elm](http://elm-lang.org/). It is optimized for speedy keyboard-based graph data entry, uses [vis.js](http://visjs.org/) library for visualizing graphs being edited. It contains explicit visual representation of editor's state using [Finite State Atomaton](https://en.wikipedia.org/wiki/Finite-state_machine).

## How to build it

1. [Install Elm](http://elm-lang.org/install)
2. Clone this repository: `git clone git@github.com:jhrcek/elm-graph-editor.git`
3. `cd elm-graph-editor`
4. `elm make src/Main.elm --warn --output=js/elm-graph-editor.js`
5. Open the file `elm-graph-editor.html` in your browser

## Upcoming improvements

Generate simple graph source code in several formats, in particular

- [dot](http://www.graphviz.org/content/dot-language) format that can be used by [graphviz](http://graphviz.org/)
- Simple [tgf](https://en.wikipedia.org/wiki/Trivial_Graph_Format) format that can be imported by tools like [gephi](https://gephi.org/) or [yed](https://www.yworks.com/products/yed)
- String based representation suitable for [fromNodesAndEdges](http://package.elm-lang.org/packages/sgraf812/elm-graph/1.1.2/Graph#fromNodesAndEdges) function of sgraf812/elm-graph library
