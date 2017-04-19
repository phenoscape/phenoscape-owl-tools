import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

organization  := "org.phenoscape"

name          := "kb-owl-tools"

version       := "1.5-SNAPSHOT"

packageArchetype.java_application

mainClass in Compile := Some("org.phenoscape.owl.build.Command")

scalaVersion  := "2.11.11"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

resolvers += "Phenoscape Maven repository" at "http://phenoscape.svn.sourceforge.net/svnroot/phenoscape/trunk/maven/repository"

resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"

resolvers += "apache-repo-releases" at "http://repository.apache.org/content/repositories/releases/"

resolvers += "BBOP repository" at "http://code.berkeleybop.org/maven/repository"

javaOptions += "-Xmx100G"

libraryDependencies ++= {
  Seq(
      "junit"                  %   "junit"                         % "4.10" % "test",
      "org.apache.commons"     %   "commons-lang3"                 % "3.1",
      "commons-io"             %   "commons-io"                    % "2.4",
      "org.jdom"               %   "jdom"                          % "2.0.2",
      "com.github.scala-blitz" %%  "scala-blitz"                   % "1.2",
      "net.sourceforge.owlapi" %   "owlapi-distribution"           % "3.5.0",
      "org.semanticweb.elk"    %   "elk-owlapi"                    % "0.4.2",
      "com.blazegraph"         %   "bigdata-core"                  % "2.1.2",
      "org.openrdf.sesame"     %   "sesame-rio"                    % "2.7.12",
      "org.phenoscape"         %%  "scowl"                         % "1.0.2",
      "org.phenoscape"         %   "owlet"                         % "1.3",
      "org.phenoscape"         %%  "phenoscape-kb-ingest"          % "1.4.7",
      "log4j"                  %   "log4j"                         % "1.2.17",
      "org.apache.directory.studio" % "org.apache.commons.codec" % "1.8"
  )
}
