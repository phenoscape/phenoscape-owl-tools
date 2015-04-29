import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

organization  := "org.phenoscape"

name          := "kb-owl-tools"

version       := "1.3"

packageArchetype.java_application

mainClass in Compile := Some("org.phenoscape.owl.build.PhenoscapeKB")

scalaVersion  := "2.11.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

resolvers += "Phenoscape Maven repository" at "http://phenoscape.svn.sourceforge.net/svnroot/phenoscape/trunk/maven/repository"

resolvers += "nxparser-repo" at "http://nxparser.googlecode.com/svn/repository/"

resolvers += "Bigdata releases" at "http://systap.com/maven/releases/"

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
      "com.bigdata"            %   "bigdata"                       % "1.3.4",
      "org.openrdf.sesame"     %   "sesame-rio"                    % "2.6.10",
      "org.phenoscape"         %   "scowl"                         % "0.9",
      "org.phenoscape"         %   "owlet"                         % "1.3"
  )
}
