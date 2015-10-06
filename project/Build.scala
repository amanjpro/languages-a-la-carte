import sbt._
import Keys._
import com.simplytyped.Antlr4Plugin._
import sbtunidoc.Plugin._
import UnidocKeys._
import sbtassembly.AssemblyPlugin.autoImport._
// import com.typesafe.sbt.SbtGit.{GitKeys => git}


object SharedSettings {
  // def sourceURL(proj: String, branch: String = "master"): String =
  //   s"https://github.com/amanjpro/sana/blob/$branch/$proj€{FILE_PATH}.scala#L1"

  def antlr(proj: String): Option[String] =
    Some(s"ch.usi.inf.l3.sana.$proj.antlr")



  def antlrSetting(name: String): Setting[Option[String]] =
    antlr4PackageName in Antlr4 := antlr(name)

  val buildSettings  = antlr4Settings ++ Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    organization := "ch.usi.inf.l3",
    scalaVersion := "2.11.7",
    apiURL := Some(url("http://sana.github.io")),
    exportJars := true,
    javaSource in Antlr4 := (javaSource in Compile).value,
    antlr4GenListener in Antlr4 := false,
    antlr4GenVisitor in Antlr4 := true,
    assemblyMergeStrategy in assembly := {
      case PathList("org", "antlr4", xs @ _*)              => MergeStrategy.first
      case PathList("org", "antlr", xs @ _*)               => MergeStrategy.first
      case PathList("org", "stringtemplate", xs @ _*)      => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    },
    scalacOptions ++= Seq("-unchecked", "-deprecation",
      "-feature", "-Xlint", "-Xfatal-warnings"),
    scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits",
         "-diagrams", "-author", "-explaintypes",
         "-doc-title", "Sana Compiler Framework",
         "-language:implicitConversions,higherKinds"),
    // Help SBT to find Api docs for libraries that don't provide any in
    // their artifacts
    // apiMappings ++= {
    //   val cp: Seq[Attributed[File]] = (fullClasspath in Compile).value
    //   def findManagedDependency(organization: String, name: String): File = {
    //     ( for {
    //         entry <- cp
    //         module <- entry.get(moduleID.key)
    //         if module.organization == organization
    //         if module.name.startsWith(name)
    //         jarFile = entry.data
    //       } yield jarFile
    //     ).head
    //   }
    //   Map(
    //       findManagedDependency("org.scalaz", "scalaz-core") ->
    //            url("http://docs.typelevel.org/api/scalaz/stable/7.0.2/doc/")
    //   )
    // },
    autoAPIMappings := true,
    libraryDependencies ++=
      List("org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
            "org.ow2.asm" % "asm-all" % "5.0.3",
            "com.github.scopt" %% "scopt" % "3.3.0")
            // "org.scalaz" %% "scalaz-core" % "7.1.1")
  )
}

object build extends Build {

  import SharedSettings._

  def project(name: String, deps: Seq[Project] = Nil,
        moreSettings: Seq[Setting[_]] = Seq()): Project = {
    val proj = Project(
      id   = name,
      base = file(name),
      settings = buildSettings ++ moreSettings
        // ++ Seq(
        // scalacOptions in (Compile, doc) <++= (baseDirectory in
        //   LocalProject(name)).map {
        //     bd => Seq("-sourcepath", bd.getAbsolutePath,
        //       "-doc-source-url", sourceURL(name))
        //   })
    )
    deps match {
      case Seq()      => proj
      case Seq(d)     => proj dependsOn d
      case _          => proj dependsOn (deps.flatMap(_.dependencies): _*)
    }
  }

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings,
      // ++
      // site.settings ++ ghpages.settings: _*) ++ settings ),
    aggregate = Seq(framework, macros, testLang, tiny, calcj, primj,
                   brokenj)
      // arrayj)
  ) settings (unidocSettings: _*)


  lazy val framework = project("framework")
  lazy val macros = project("macros", Seq(framework), Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      resolvers += Resolver.sonatypeRepo("snapshots"),
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0-M5" cross CrossVersion.full)
    ))

  lazy val testLang = project("testLang", Seq(macros))
  lazy val tiny = project("tiny", Seq(macros))
  lazy val calcj = project("calcj", Seq(tiny))
  lazy val primj = project("primj", Seq(calcj), Seq(antlrSetting("primj")))
  lazy val brokenj = project("brokenj", Seq(primj))
}
