package inc.common

import better.files._
import java.io.{File => JavaFile}

sealed abstract class ClasspathEntry {
  def file: File
}
case class Archive(file: File) extends ClasspathEntry
case class Directory(file: File) extends ClasspathEntry
case class ClassFile(file: File) extends ClasspathEntry

object ClasspathParser {
  val ArchiveExtensions = Set(".jar", ".zip")
  val JarExtension = Set(".jar")

  def hasExtension(file: File, extensions: Set[String]) =
    file
      .extension(toLowerCase = true)
      .exists(extensions.contains)

  def entryFor(segment: String): Option[ClasspathEntry] = {
    val file = segment.toFile

    // For a JAR or zip file that contains class files, the class path ends in the name of the zip or JAR file
    val archive =
      if (hasExtension(file, ArchiveExtensions))
        Some(Archive(file))
      else
        None

    // For class files in an unnamed package, the class path ends with the directory that contains the class files
    // For class files in a named package, the class path ends with the directory that contains the root package
    val directory =
      if (file.isDirectory)
        Some(Directory(file))
      else
        None

    // Classpath entries which are neither directories nor archives should be ignored
    archive orElse directory
  }

  def expandWildcard(segment: String): Iterator[ClasspathEntry] = {
    // Remove the wildcard character
    val jarDirectory = segment
      .substring(0, segment.length - 1)
      .toFile

    // The order in which files are enumerated is not specified for wildcards
    jarDirectory
      .collectChildren(hasExtension(_, JarExtension))
      .map(Archive)
  }

  def parse(classpathString: String): List[ClasspathEntry] = {
    val separated = classpathString.split(JavaFile.pathSeparator)

    var classpathEntries = List.empty[ClasspathEntry]

    // We're building a list, so start at the end of the array
    var i = separated.length - 1

    while (i >= 0) {
      val segment = separated(i)

      // Expand wildcard entries
      if (segment.endsWith("*"))
        expandWildcard(segment).foreach { entry =>
          classpathEntries = entry :: classpathEntries
        }
      else
        entryFor(segment).foreach { entry =>
          classpathEntries = entry :: classpathEntries
        }

      i -= 1
    }

    classpathEntries
  }

}
