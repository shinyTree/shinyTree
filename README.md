[![cran checks](https://badges.cranchecks.info/worst/shinyTree.svg)](https://cran.r-project.org/web/checks/check_results_shinyTree.html)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/shinyTree?color=brightgreen)](https://www.r-pkg.org/pkg/shinyTree)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/shinyTree)](https://www.rpackages.io/package/shinyTree)
[![codecov](https://app.codecov.io/gh/shinyTree/shinyTree/branch/master/graph/badge.svg)](https://app.codecov.io/gh/shinyTree/shinyTree)

shinyTree
==========

The `shinyTree` package enables Shiny application developers to use the 
[jsTree](http://jstree.com) library in their applications.

![shiny tree screenshot](https://shinyTree.github.io/shinyTree/images/st.png)

Installation
------------

You can install the latest development version of the code using the devtools R package.

```
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("shinyTree/shinyTree")
```


Getting Started
---------------

#### 01-simple ([Live Demo](https://trestletech.shinyapps.io/st-01-simple/))

```
library(shiny)
runApp(system.file("examples/01-simple", package = "shinyTree"))
```

A simple example to demonstrate the usage of the shinyTree package.

#### 02-attributes ([Live Demo](https://trestletech.shinyapps.io/st-02-attributes/))

```
library(shiny)
runApp(system.file("examples/02-attributes", package = "shinyTree"))
```

Manage properties of your tree by adding attributes to your list when rendering.

#### 03-checkbox ([Live Demo](https://trestletech.shinyapps.io/st-03-checkbox/))

```
library(shiny)
runApp(system.file("examples/03-checkbox", package = "shinyTree"))
```

Use checkboxes to allow users to more easily manage which nodes are selected.

#### 04-selected ([Live Demo](https://shinyTree.shinyapps.io/st-04-selected/))

```
library(shiny)
runApp(system.file("examples/04-selected", package = "shinyTree"))
```

An example demonstrating how to set an `input` to the value of the currently selected node in the tree.

#### 05-structure ([Live Demo](https://trestletech.shinyapps.io/st-05-structure/))

```
library(shiny)
runApp(system.file("examples/05-structure", package = "shinyTree"))
```

Demonstrates the low-level usage of a shinyTree as an input in which all attributes describing the state of the tree can be read.


#### 06-search ([Live Demo](https://trestletech.shinyapps.io/st-06-search/))

```
library(shiny)
runApp(system.file("examples/06-search", package = "shinyTree"))
```

An example showing the use of the search plugin to allow users to more easily navigate the nodes in your tree.

#### 07-drag-and-drop ([Live Demo](https://trestletech.shinyapps.io/st-07-drag-and-drop/))

```
library(shiny)
runApp(system.file("examples/07-drag-and-drop", package = "shinyTree"))
```

An example demonstrating the use of the drag-and-drop feature which allows the user to reorder the nodes in the tree.

#### 08-class ([Live Demo](https://trestletech.shinyapps.io/st-08-class/))

```
library(shiny)
runApp(system.file("examples/08-class", package = "shinyTree"))
```

An example demonstrating the use of the ability to style nodes using custom classes.

#### 09-themes

```
library(shiny)
runApp(system.file("examples/09-themes", package = "shinyTree"))
```

An example demonstrating the use of built-in tree themes.

#### 10-node-ids

```
library(shiny)
runApp(system.file("examples/10-node-ids", package = "shinyTree"))
```

An example demonstrating the ability to label and return node identifiers and classes.

#### 11-tree-update

```
library(shiny)
runApp(system.file("examples/11-tree-update", package = "shinyTree"))
```

An example demonstrating the ability to update a tree with a new tree model.  This was broken in the original version as the tree was destroyed upon initialization.

#### 12-types

```
library(shiny)
runApp(system.file("examples/12-types", package = "shinyTree"))
```

An example demonstrating node types with custom icons.

#### 13-icons

```
library(shiny)
runApp(system.file("examples/13-icons", package = "shinyTree"))
```

An example demonstrating various ways to use icons on nodes.

#### 14-files

```
library(shiny)
runApp(system.file("examples/14-files", package = "shinyTree"))
```

Demonstrates how to create a file browser tree.

#### 15-files

```
library(shiny)
runApp(system.file("examples/15-data", package = "shinyTree"))
```

Demonstrates how to attach and retreive metadata from a node.

#### 16-async

```
library(shiny)
runApp(system.file("examples/16-async", package = "shinyTree"))
```

Demonstrates how to render a tree asynchronously.

#### 17-contextmenu

```
library(shiny)
runApp(system.file("examples/17-contextmenu", package = "shinyTree"))
```

Demonstrates how to enable the contextmenu.

#### 18-modules

```
library(shiny)
runApp(system.file("examples/18-modules/app.R", package="shinyTree"))
runApp(system.file("examples/18-modules/app_types.R", package="shinyTree"))
```

Demonstrates how to use shinyTree with shiny modules.

#### 19-data.tree
```
library(shiny)
runApp(system.file("examples/19-data.tree", package = "shinyTree"))
```

Demonstrates how to pass a data.tree to shinyTree.

#### 20-api

```
library(shiny)
runApp(system.file("examples/20-api", package = "shinyTree"))
```

An example demonstrating how to extend the operations on the tree to the rest of
the [jsTree's core functionality](https://www.jstree.com/api/).

#### 21-options

```
library(shiny)
runApp(system.file("examples/21-options/app_setState_refresh.R", package="shinyTree"))
```

Demonstrates how to fine-tune shinyTree's behaviour with options. Specifically:
When internal `jstree` code calls `set_state` or `refresh`, a callback is made so that the shiny
server is notified and `observe` and `observeEvents` for the tree are fired.
This can be useful if the developer would like `observe` and `observeEvents` to run after
using `updateTree`. (By default, `updateTree` does not run `observe` or `observeEvent` because it
is assumed that the shiny application knows that the tree is being changed already.)

#### 23-file-icons

```
library(shiny)
library(shinyTree)
runApp(system.file("examples/23-file-icons", package = "shinyTree"))
```

An example demonstrating how to create a file tree with individual icons.


Known Bugs
----------

See the [Issues page](https://github.com/shinyTree/shinyTree/issues) for information on outstanding issues. 

License
-------

The development of this project was generously sponsored by the [Institut de 
and performed by [Jeff Allen](https://trestletech.com). The code is
licensed under The MIT License (MIT).

Copyright (c) 2014 Institut de Radioprotection et de Sûreté Nucléaire

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
