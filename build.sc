import mill._
import mill.scalalib._
import mill.contrib.scalapblib._

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.1.0`
import io.github.davidgregory084.TpolecatModule

trait ScalaSettingsModule extends TpolecatModule {
  def scalaVersion = "2.12.8"
  def scalacPluginIvyDeps = Agg(
    ivy"com.olegpy::better-monadic-for:0.3.0-M4",
    ivy"org.scalameta:semanticdb-scalac_${scalaVersion()}:4.1.3"
  )
  def scalacOptions = T {
    super.scalacOptions() ++ Seq(
      "-Ypartial-unification",
      "-Yno-imports",
      "-Yrangepos"
    )
  }
  trait Test extends Tests {
    def ivyDeps = Agg(
      ivy"org.typelevel::cats-core:1.3.0",
      ivy"io.chrisdavenport::cats-scalacheck:0.1.0",
      ivy"com.lihaoyi::pprint:0.5.3",
      ivy"com.github.pathikrit::better-files:3.6.0",
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"org.scalacheck::scalacheck:1.14.0"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
    def scalacOptions = T { super.scalacOptions().filterNot(Set("-Yno-imports")) }
  }
}

object decompiled extends JavaModule

object proto extends ScalaPBModule with ScalaSettingsModule {
  def scalaPBVersion = "0.8.0"
  def scalaPBFlatPackage = true
  def scalaPBGrpc = false
  def scalacOptions = T { super.scalacOptions().filterNot(Set("-Yno-imports")) }
}

object common extends ScalaSettingsModule {
  def moduleDeps = Seq(proto)
  def ivyDeps = T {
    super.ivyDeps() ++ Agg(
      ivy"org.typelevel::cats-core:1.3.0",
      ivy"org.typelevel::paiges-core:0.2.1",
      ivy"com.lihaoyi::fansi:0.2.5",
      ivy"com.outr::scribe:2.6.0"
    )
  }
  object test extends super.Test
}

object rts extends ScalaSettingsModule

object parser extends ScalaSettingsModule {
  def moduleDeps = Seq(common)
  def ivyDeps = Agg(ivy"com.lihaoyi::fastparse:2.0.4")
  object test extends super.Test
}

object resolver extends ScalaSettingsModule {
  def moduleDeps = Seq(common)
  object test extends super.Test
}

object typechecker extends ScalaSettingsModule {
  def moduleDeps = Seq(common)
  object test extends super.Test
}

object codegen extends ScalaSettingsModule {
  def moduleDeps = Seq(common, rts)
  def asmVersion = T { "6.2" }
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:${asmVersion()}",
    ivy"org.ow2.asm:asm-commons:${asmVersion()}",
    ivy"org.ow2.asm:asm-util:${asmVersion()}"
  )
  object test extends super.Test {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}

object main extends ScalaSettingsModule {
  def mainClass = Some("inc.main.Main")
  def moduleDeps = Seq(common, rts, parser, resolver, typechecker, codegen)
  def ivyDeps = Agg(
    ivy"com.github.scopt::scopt:3.7.0",
    ivy"com.lihaoyi::pprint:0.5.3",
    // PPrint definitely requires scala-reflect
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
  )
  object test extends super.Test {
    override def moduleDeps =
      super.moduleDeps :+ common.test
  }
}
