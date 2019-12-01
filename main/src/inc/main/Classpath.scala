package inc.main

import inc.common._
import inc.codegen.Codegen
import cats.data.{ Chain, Validated }
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import java.lang.{ ClassLoader, String }
import java.io.{ ByteArrayOutputStream, File, InputStream, OutputStream }
import java.nio.file.Paths
import java.net.URL
import scala.{ Array, Byte, Either, Option, Unit }
import scala.collection.immutable.{ List, Map }
import scala.Predef.{ ArrowAssoc, wrapRefArray }

object Classpath {
  def parseUrls(classpath: String): Either[List[Error], Array[URL]] = {
    val urlStrings = classpath.split(File.pathSeparator)
    Chain.fromSeq(urlStrings.toIndexedSeq).traverse { p =>
      val path = Validated.catchNonFatal(Paths.get(p))
      val url = path.map(_.toUri.toURL)
      url.leftMap(_ => ConfigError.invalidClasspathEntry(p))
    }.map(_.iterator.toArray).toEither
  }

  def classNotFound(pos: Pos, className: String): Either[List[ConfigError], Array[Byte]] =
    ConfigError.missingClassData(pos, className).asLeft[Array[Byte]]

  def readClassBytes(classloader: ClassLoader, className: String, pos: Pos): Either[List[ConfigError], Array[Byte]] = {
    val classStream = Option(classloader.getResourceAsStream(className))
    val outputStream = new ByteArrayOutputStream()

    val bufSize = 8192
    val buf = new Array[Byte](bufSize)

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

  def readEnvironment(imports: List[Import], classloader: ClassLoader): Either[List[Error], Map[String, TopLevelDeclaration[NameWithType]]] = {
    val distinctPrefixes = imports.map {
      case ImportModule(pkg, nm, pos) =>
        (pkg, nm, pos)
      case ImportSymbols(pkg, nm, _, pos) =>
        (pkg, nm, pos)
    }.distinct

    val classpathModules = distinctPrefixes.traverse {
      case (pkg, nm, pos) =>
        val className =
          pkg.mkString("/") + "/" + nm + ".class"

        for {
          classBytes <- readClassBytes(classloader, className, pos)
          mod <- Codegen.readInterface(classBytes)
        } yield (mod.fullName -> mod)
    }

    val declarations = classpathModules.map(_.toMap).map { modules =>
      imports.flatMap {
        case i @ ImportModule(_, _, _) =>
          val mod = modules(i.moduleName)
          mod.declarations.map { d =>
            (mod.name + "." + d.name) -> d
          }
        case i @ ImportSymbols(_, _, syms, _) =>
          val mod = modules(i.moduleName)
          mod.declarations
            .filter(d => syms.contains(d.name))
            .map(d => d.name -> d)
      }
    }

    declarations.map(_.toMap)
  }

}
