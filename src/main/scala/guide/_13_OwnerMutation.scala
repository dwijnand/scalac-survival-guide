package guide

import scala.tools.nsc.Global

object _13_OwnerMutation extends App {
  def run(g: Global)(unit: g.CompilationUnit): Unit = {
    import g._
    println(showRaw(unit.body))
    val sym = unit.body.collect {
      case dd: ClassDef if dd.name.string_==("Local") =>
        dd.symbol
    }.head
    println(sym.owner) // class C
    println(sym.originalOwner) // method foo
  }

  val g1 = newGlobal("-Ystop-after:test", extraPhases = g2 => newSubComponent(g2, "lambdalift")((g3, unit) => run(g3)(unit)) :: Nil)
  val tree = compile("class C { def foo = { class Local } }", g1).assertNoErrors().tree
}
