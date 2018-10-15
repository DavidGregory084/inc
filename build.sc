import mill._
import mill.scalalib._
import mill.contrib.scalapblib._

trait ScalaSettingsModule extends ScalaModule {
  def scalaVersion = "2.12.7"
  def scalacOptions = Seq(
    // Common options
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
    // scalaVersion >= 2.12
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    // scalaVersion >= 2.11
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Ywarn-infer-any",                   // Warn when a type argument is inferred to be `Any`.
    // Partial unification
    "-Ypartial-unification"
  )
  trait Test extends Tests {
    def ivyDeps = Agg(
      ivy"org.typelevel::cats-core:1.3.0",
      ivy"io.chrisdavenport::cats-scalacheck:0.1.0",
      ivy"com.lihaoyi::pprint:0.5.3",
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"org.scalacheck::scalacheck:1.14.0"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object decompiled extends JavaModule

object common extends ScalaPBModule with ScalaSettingsModule {
  def scalaPBVersion = "0.8.0"
  def scalaPBFlatPackage = true
  def scalaPBGrpc = false
  def ivyDeps = T {
    super.ivyDeps() ++ Agg(
      ivy"org.typelevel::cats-core:1.3.0",
      ivy"org.typelevel::paiges-core:0.2.1",
      ivy"com.github.pathikrit::better-files:3.6.0",
      ivy"com.lihaoyi::fansi:0.2.5",
      ivy"com.outr::scribe:2.6.0"
    )
  }
  object test extends super.Test
}

object rts extends ScalaSettingsModule

object parser extends ScalaSettingsModule {
  def moduleDeps = Seq(common)
  def ivyDeps = Agg(ivy"com.lihaoyi::fastparse:1.0.0")
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
