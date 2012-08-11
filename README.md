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

## Getting Started

To build the classes:
    sbt clean compile stage

To build the documents:
    sbt doc
(documents will go in target/scala-2.9.2/api/net/naegling/graph/package.html)

To test that the classes are working properly:
    sbt test

## Integrating with Eclipse

sbt uses a library-caching mechanism called "ivy" to manage its libraries; by default
eclipse knows nothing of ivy.  Therefore you must install an Ivy plugin and cause sbt
to deliver an ivy dependencies file pointing to local copies of the relevant libraries.
To do this:

+ Install IvyDE into Eclipse.  This is done via:
    - Help >> Install new Software...
    - add the site `http://www.apache.org/dist/ant/ivyde/updatesite`
    - select all of the packages
+ Cause a local delivery.  This is done by running `sbt deliver-local`
+ Tell the package to reload the settings
    - Find the reference to the ivy-0.1.xml file in your project library references
    - right-click it
    - select "Reload Settings"
