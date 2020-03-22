package guide

import scala.reflect.ensureAccessible
import scala.reflect.internal.Trees
import scala.tools.nsc.Global

object _24_RscComparison extends App {
  val defDefRhsField = ensureAccessible(classOf[Trees#DefDef].getDeclaredField("rhs"))
  val valDefRhsField = ensureAccessible(classOf[Trees#ValDef].getDeclaredField("rhs"))

  def run(g: Global)(unit: g.CompilationUnit): Unit = {
    import g._
    val dummy = Ident("droppped").setType(ErrorType).setSymbol(NoSymbol)
    object RhsDropper extends Traverser {
      override def traverse(tree: g.Tree): Unit = tree match {
        case dd: DefDef => defDefRhsField.set(dd, dummy); super.traverse(dd)
        case vd: ValDef => valDefRhsField.set(vd, dummy); super.traverse(vd)
        case _          => super.traverse(tree)
      }
    }
    RhsDropper.traverse(unit.body)
  }

  val g1 = newGlobal( extraPhases = g2 => newSubComponent(g2, "parser")((g3, unit) => run(g3)(unit)) :: Nil, options = "-Ystop-after:typer")

  println(compile("class C { val foo: Int = 42; def bar: String = asdfasd }", g1).tree)
}
