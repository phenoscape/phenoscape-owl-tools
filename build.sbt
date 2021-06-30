enablePlugins(JavaAppPackaging)

organization := "org.phenoscape"

name := "kb-owl-tools"

version := "1.12"

mainClass in Compile := Some("org.phenoscape.owl.build.Command")

scalaVersion := "2.13.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

resolvers += "Phenoscape Maven repository" at "https://svn.code.sf.net/p/phenoscape/code/trunk/maven/repository"

javaOptions += "-Xmx100G"

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies ++= {
  Seq(
    "org.apache.commons" % "commons-lang3" % "3.10",
    "commons-io" % "commons-io" % "2.6",
    "org.jdom" % "jdom" % "2.0.2",
    "net.sourceforge.owlapi" % "owlapi-distribution" % "4.5.16",
    "org.semanticweb.elk" % "elk-owlapi" % "0.4.3",
    "com.blazegraph" % "bigdata-core" % "2.1.4" exclude ("org.slf4j", "slf4j-log4j12"),
    "org.openrdf.sesame" % "sesame-rio" % "2.7.12",
    "org.phenoscape" %% "scowl" % "1.3.4",
    "org.phenoscape" %% "owlet" % "1.8.1",
    "org.phenoscape" %% "phenoscape-kb-ingest" % "1.6.2",
    "com.outr" %% "scribe-slf4j" % "3.5.4",
    "org.apache.directory.studio" % "org.apache.commons.codec" % "1.8",
    "com.github.pathikrit" %% "better-files" % "3.8.0",
    "com.lihaoyi" %% "utest" % "0.7.4" % Test,
    "org.phenoscape" %% "sparql-utils" % "1.3.1",
    "org.phenoscape" %% "sparql-utils-owlapi" % "1.3.1",
    "io.monix" %% "monix" % "3.2.1",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"
  )
}
