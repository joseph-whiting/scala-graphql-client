[![Build Status](https://travis-ci.org/joseph-whiting/scala-graphql-client.svg?branch=master)](https://travis-ci.org/joseph-whiting/scala-graphql-client)
# scala-graphql-client
sbt plugin for typesafe consumption of GraphQL APIs in Scala

This is an attempt to make a way to consume GraphQL APIs easily and safely in Scala. The excellent Sangria library exists for the server side, but the use cases for this are in web/native Scala/ScalaJS frontends and backends that need to interface with GraphQL APIs.

I'm currently working on the basic framework of it and trying to get an end-to-end example going for a simple use case.

See [here](https://joseph-whiting.github.io/2018/04/10/Idea-For-A-Scala-GraphQL-Client-DSL-Code-Generator/) for the motivation and goals behind it

## Workflow

Unit tests: `sbt test` or `sbt ~test` for continuous testing

It uses scripted to run e2e tests for the plugin, to run these tests run `sbt scripted`.

The projects for the e2e tests are in `src/sbt-test/`, and are grouped into folders. To debug the e2e tests it can be helpful to run them manually. Run `sbt compile publishLocal` from the root of this project, and then `sbt -Dplugin.version="x.x.x"` and `compile`, taking the version number from the `build.sbt` for this project.


## Todo list

### For version 0.2.0
- forming queries
- sending queries
- deserialising responses
- parameters
- package names, file names, class names housekeeping

### Later
- converting queries written in graphQL to use the DSL
- add optional macro for { } in queries
- website
- example project in seperate repo & tutorial
- license & contributing
