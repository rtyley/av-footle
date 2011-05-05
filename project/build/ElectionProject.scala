import sbt.FileFilter
import sbt._

class PAElectionProject(info: ProjectInfo) extends DefaultProject(info){

  val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test" withSources()

  override def compileOptions = super.compileOptions ++ Seq(Unchecked, Optimise)
}

