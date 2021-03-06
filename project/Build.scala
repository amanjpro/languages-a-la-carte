import sbt._
import Keys._
import com.simplytyped.Antlr4Plugin._
import sbtunidoc.Plugin._
import bintray.BintrayPlugin._
import bintray.BintrayKeys._
import UnidocKeys._
import sbtassembly.AssemblyPlugin.autoImport._
import de.heikoseeberger.sbtheader.HeaderPattern
import de.heikoseeberger.sbtheader.HeaderKey._
// import com.typesafe.sbt.SbtGit.{GitKeys => git}


object SharedSettings {
  def sourceURL(proj: String, branch: String = "master"): String =
    s"https://github.com/amanjpro/languages-a-la-carte/blob/$branch/$proj€{FILE_PATH}.scala#L1"

  def antlr(proj: String): Option[String] =
    Some(s"ch.usi.inf.l3.sana.$proj.antlr")



  def antlrSetting(name: String): Setting[Option[String]] =
    antlr4PackageName in Antlr4 := antlr(name)


  val buildSettings  = antlr4Settings ++ Defaults.defaultSettings ++
      bintraySettings ++ Seq(
    version := "1.0.1",
    organization := "ch.usi.inf.l3.sana",
    bintrayOrganization := Some("sana"),
    crossPaths := false,
    publishMavenStyle := true,
    // pomExtra := pomXml,
    pomExtra := (
      <scm>
        <url>git@github.com:amanjpro/languages-a-la-carte.git</url>
        <connection>scm:git:git@github.com:amanjpro/languages-a-la-carte.git</connection>
      </scm>
      <developers>
        <developer>
          <id>amanjpro</id>
          <name>Amanj Sherwany</name>
          <url>https://github.com/amanjpro</url>
        </developer>
        <developer>
          <id>nosheenzaza</id>
          <name>Nosheen Zaza</name>
          <url>https://github.com/nosheenzaza</url>
        </developer>
      </developers>
    ),
    scalaVersion := "2.11.7",
    licenses += ("BSD New",
      url("http://opensource.org/licenses/https://opensource.org/licenses/BSD-3-Clause")),
    headers := Map(
      "scala" -> ((
        HeaderPattern.cStyleBlockComment,
        """|/*
           | * Copyright (c) <2015-2016>, see CONTRIBUTORS
           | * All rights reserved.
           | *
           | * Redistribution and use in source and binary forms, with or without
           | * modification, are permitted provided that the following conditions are met:
           | *   * Redistributions of source code must retain the above copyright
           | *     notice, this list of conditions and the following disclaimer.
           | *   * Redistributions in binary form must reproduce the above copyright
           | *     notice, this list of conditions and the following disclaimer in the
           | *     documentation and/or other materials provided with the distribution.
           | *   * Neither the name of the <organization> nor the
           | *     names of its contributors may be used to endorse or promote products
           | *     derived from this software without specific prior written permission.
           | *
           | * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
           | * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
           | * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
           | * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
           | * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
           | * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
           | * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
           | * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
           | * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
           | * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
           | */
           |
           |""".stripMargin
      ))
    ),
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
            "org.apache.commons" % "commons-lang3" % "3.0",
            "com.github.scopt" %% "scopt" % "3.3.0"),

    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
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
      settings = buildSettings ++ moreSettings ++ Seq(
        // publishTo := Some("Sana repository" at "https://api.bintray.com/maven/amanjpro"),
        apiURL := Some(url(s"http://amanjpro.github.io/languages-a-la-carte/web/api/$name")),
        scalacOptions in (Compile, doc) <++= (baseDirectory in
          LocalProject(name)).map {
            bd => Seq("-sourcepath", bd.getAbsolutePath,
              "-doc-source-url", sourceURL(name))
      })
    )
    deps match {
      case Seq()      => proj
      case _          =>
      deps.foldLeft(proj)((z, y) => {
        z dependsOn y
      })
    }
  }

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ Seq(publish := { }),
      // ++
      // site.settings ++ ghpages.settings: _*) ++ settings ),
    aggregate = Seq(testLang, tiny, calcj, primj,
                   brokenj, ooj, dcct, arrayj, arrooj, robustj, dynj,
                   ppj, modulej, oberon0, guod)
      // arrayj)
  ) settings (unidocSettings: _*)


  lazy val tiny        = project("tiny")
  lazy val calcj       = project("calcj", Seq(tiny))
  lazy val testLang    = project("testLang", Seq(tiny), Seq(publish := { } ))
  lazy val primj       = project("primj", Seq(calcj), Seq(antlrSetting("primj")))
  lazy val brokenj     = project("brokenj", Seq(primj))
  lazy val ooj         = project("ooj", Seq(brokenj), Seq(antlrSetting("ooj")))
  lazy val arrayj      = project("arrayj", Seq(brokenj))
  lazy val arrooj      = project("arrooj", Seq(ooj, arrayj))
  lazy val robustj     = project("robustj", Seq(arrooj))
  lazy val dynj        = project("dynj", Seq(robustj))
  lazy val ppj         = project("ppj", Seq(dynj))
  lazy val modulej     = project("modulej", Seq(ppj)) // this implements java 1.0
  lazy val guod        = project("guod", Seq(modulej))





  // Consistency language example
  lazy val dcct        = project("dcct", Seq(ooj), Seq(antlrSetting("dcct")))

  // Oberon-0
  lazy val oberon0     = project("oberon0", Seq(arrooj), Seq(antlrSetting("oberon0")))


}
