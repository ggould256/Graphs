# The _naegling.net_ Graph Package

A set of classes for defining and coloring graphs written in scala.

## Directories

- /
    + `src/`
        * `main/scala` -- _Scala source files for deployment_
            - `net/naegling/graph/` -- _The graph-related classes_
                + `coloring/` -- _Graph coloring library_
            - `net/naegling/apps/` -- _Things that can be invoked from the command line_
        * `test/scala` -- _Scala source files for testing_
            - `net/naegling/graph/` -- _Tests for graph-related classes_
                + `coloring/` -- _Tests for graph coloring library_
    + `lib/` -- _Third-party libraries required by this project_
    + `build.sbt` -- _The sbt build definition file for this file_
    + `project/`
        * `build.sbt` -- _sbt metaconfiguration (just adds the start script plugin_
        * `build.properties` -- _sbt metaconfiguration to specify the sbt version_
    + `lib/` -- _Third-party libraries required by this project_

Note that we are using unmanaged dependencies in the `lib` directory rather than pulling
from sbt.  This is to simplify setup for eclipse users, as eclipse sbt support is still
rather fiddly to get right.

## Getting Started

To build the classes:
    sbt clean compile stage

To build the documents:
    sbt doc
(documents will go in target/scala-2.9.2/api/net/naegling/graph/package.html)

To test that the classes are working properly:
    sbt test

