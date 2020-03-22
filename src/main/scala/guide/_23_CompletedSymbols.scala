package guide

import scala.reflect.io.{ NoAbstractFile, ZipArchive }

object _23_CompletedSymbols extends App {
  val g1 = newGlobal()
  import g1._

  def safeInfo(sym: Symbol): Type = if (sym.hasCompleteInfo) sym.rawInfo else NoType

  def packageClassOrSelf(sym: Symbol): Symbol =
    if (sym.hasPackageFlag && !sym.isModuleClass) sym.moduleClass else sym

  def walkTopLevels(root: Symbol): List[Symbol] =
    if (root.hasPackageFlag) safeInfo(packageClassOrSelf(root)).decls.filter(root != _).flatMap(walkTopLevels(_)).toList else List(root)

  def symFilePath(sym: Symbol) = sym.associatedFile match {
    case x: ZipArchive#Entry => s"${x.underlyingSource.value.name}(${x.path})" // avoid absolute jar path
    case x                   => x.name
  }

  compile("class C { println(0.toString) }", g1)

  val completedTopLevels = walkTopLevels(RootClass).filterNot(_.hasPackageFlag).filter(x => x.associatedFile != NoAbstractFile && x.hasRawInfo && x.rawInfo.isComplete)

  println(completedTopLevels.filter(_.sourceFile == null).map(sym => (sym.fullName, symFilePath(sym))).mkString("\n"))
}
