import sbt._

class MainBuildProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}
