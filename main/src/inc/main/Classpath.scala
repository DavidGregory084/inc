package inc.main

import cats.data.Chain
import cats.data.OptionT
import cats.data.Validated
import cats.syntax.either._
import cats.syntax.traverse._
import inc.codegen.Codegen
import inc.common._

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.lang.ClassLoader
import java.lang.String
import java.net.URL
import java.nio.file.Paths
import scala.Array
import scala.Byte
import scala.Either
import scala.Option
import scala.Predef.ArrowAssoc
import scala.Predef.wrapRefArray
import scala.Unit
import scala.collection.immutable.List

object Classpath {
  def parseUrls(classpath: String): Compile[Array[URL]] = {
    val urlStrings = classpath.split(File.pathSeparator)

    val urls = Chain
      .fromSeq(urlStrings.toIndexedSeq)
      .traverse { p =>
        val path = Validated.catchNonFatal(Paths.get(p))
        val url  = path.map(_.toUri.toURL)
        url.leftMap(_ => ConfigError.invalidClasspathEntry(p))
      }
      .map(_.iterator.toArray)
      .toEither

    OptionT.liftF(urls)
  }

  def classNotFound(pos: Pos, className: String): Either[List[ConfigError], Array[Byte]] =
    ConfigError.missingClassData(pos, className).asLeft[Array[Byte]]

  def readClassBytes(
    classloader: ClassLoader,
    className: String,
    pos: Pos
  ): Either[List[ConfigError], Array[Byte]] = {
    val classStream  = Option(classloader.getResourceAsStream(className))
    val outputStream = new ByteArrayOutputStream()

    val bufSize = 8192
    val buf     = new Array[Byte](bufSize)

    def pipe(is: InputStream, os: OutputStream, buf: Array[Byte]): Unit = {
      val num = is.read(buf)
      if (num > 0) {
        os.write(buf)
        pipe(is, os, buf)
      }
    }

    classStream.fold(classNotFound(pos, className)) { inputStream =>
      try pipe(inputStream, outputStream, buf)
      finally {
        inputStream.close()
        outputStream.close()
      }

      Either.right(buf)
    }
  }

  def readEnvironment(
    imports: List[Import],
    classloader: ClassLoader
  ): Compile[Environment[Meta.Typed]] = {
    val distinctPrefixes = imports.map {
      case ImportModule(pkg, nm, pos) =>
        (pkg, nm, pos)
      case ImportSymbols(pkg, nm, _, pos) =>
        (pkg, nm, pos)
    }.distinct

    val classpathModules = distinctPrefixes.traverse { case (pkg, nm, pos) =>
      val className =
        pkg.mkString("/") + "/" + nm + ".class"

      for {
        classBytes <- readClassBytes(classloader, className, pos)
        mod        <- Codegen.readInterface(classBytes)
      } yield (mod.fullName -> mod)
    }

    val environment = classpathModules.map(_.toMap).map { modules =>
      val environments = imports.map {
        case i @ ImportModule(_, _, _) =>
          val mod        = modules(i.moduleName)
          val env        = mod.typeEnvironment
          val fqn        = if (mod.pkg.isEmpty) mod.name else mod.pkg.mkString("/") + "/" + mod.name
          val modPrefix  = env.prefixed(mod.name)
          val fullPrefix = env.prefixed(fqn)
          modPrefix ++ fullPrefix
        case i @ ImportSymbols(_, _, syms, _) =>
          val mod   = modules(i.moduleName)
          val fqn   = if (mod.pkg.isEmpty) mod.name else mod.pkg.mkString("/") + "/" + mod.name
          val decls = mod.typeEnvironment.filter(d => syms.contains(d))
          decls ++ decls.prefixed(fqn)
      }

      environments.foldLeft(Environment.empty[Meta.Typed])(_ ++ _)
    }

    OptionT.liftF(environment)
  }
}
