shinyTree
==========

The `shinyTree` package enables Shiny application developers to use the 
[jsTree](http://jstree.com) library in their applications.

![shiny tree screenshot](https://trestletech.github.io/shinyTree/images/st.png)

Installation
------------

You can install the latest development version of the code using the devtools R package.

```
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("trestletech/shinyTree")
```


Getting Started
---------------

#### 01-simple ([Live Demo](https://trestletech.shinyapps.io/st-01-simple/))

```
library(shiny)
runApp(system.file("examples/01-simple", package="shinyTree"))
```

A simple example to demonstrate the usage of the shinyTree package.

#### 02-attributes ([Live Demo](https://trestletech.shinyapps.io/st-02-attributes/))

```
library(shiny)
runApp(system.file("examples/02-attributes", package="shinyTree"))
```

Manage properties of your tree by adding attributes to your list when rendering.

#### 03-checkbox ([Live Demo](https://trestletech.shinyapps.io/st-03-checkbox/))

```
library(shiny)
runApp(system.file("examples/03-checkbox", package="shinyTree"))
```

Use checkboxes to allow users to more easily manage which nodes are selected.

#### 04-selected ([Live Demo](https://trestletech.shinyapps.io/st-04-selected/))

```
library(shiny)
runApp(system.file("examples/04-selected", package="shinyTree"))
```

An example demonstrating how to set an `input` to the value of the currently selected node in the tree.

#### 05-structure ([Live Demo](https://trestletech.shinyapps.io/st-05-structure/))

```
library(shiny)
runApp(system.file("examples/05-structure", package="shinyTree"))
```

Demonstrates the low-level usage of a shinyTree as an input in which all attributes describing the state of the tree can be read.


#### 06-search ([Live Demo](https://trestletech.shinyapps.io/st-06-search/))

```
library(shiny)
runApp(system.file("examples/06-search", package="shinyTree"))
```

An example showing the use of the search plugin to allow users to more easily navigate the nodes in your tree.

#### 07-drag-and-drop ([Live Demo](https://trestletech.shinyapps.io/st-07-drag-and-drop/))

```
library(shiny)
runApp(system.file("examples/07-drag-and-drop", package="shinyTree"))
```

An example demonstrating the use of the drag-and-drop feature which allows the user to reorder the nodes in the tree.

#### 08-class ([Live Demo](https://trestletech.shinyapps.io/st-08-class/))

```
library(shiny)
runApp(system.file("examples/08-class", package="shinyTree"))
```

An example demonstrating the use of the ability to style nodes using custom classes.

#### 09-api

```
library(shiny)
runApp(system.file("examples/09-api", package="shinyTree"))
```

An example demonstrating how to extend the operations on the tree to the rest of
the [jsTree's core functionality](https://www.jstree.com/api/).

Known Bugs
----------

See the [Issues page](https://github.com/trestletech/shinyTree/issues) for information on outstanding issues. 

License
-------

The development of this project was generously sponsored by the [Institut de 
Radioprotection et de Sûreté Nucléaire](http://www.irsn.fr/EN/Pages/home.aspx) 
and performed by [Jeff Allen](http://trestletech.com). The code is
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
