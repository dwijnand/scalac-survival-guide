package guide

import scala.tools.nsc.transform.{InfoTransform, TypingTransformers}
import scala.tools.nsc.Global

object _22_InfoTransform extends App {
  def mkPhase(g: Global) = new AddClassName { val global: g.type = g }

  val g1 = newGlobal("-Xprint:addClassName", extraPhases = g2 => (mkPhase(g2), "addClassName") :: Nil)

  compile("class C; class D extends C; class E extends D", g1)
  compile("class D extends C; class E extends D; class C", g1)
}

abstract class AddClassName extends InfoTransform with TypingTransformers {
  import global._

  val phaseName      = "addClassName"
  val runsAfter      = List("typer")
  val runsRightAfter = None

  private lazy val nme_ClassName = TermName("className")

  def transformInfo(sym: Symbol, tpe: Type) = {
    if (currentRun.compiles(sym) && sym.isClass)
      tpe match {
        case ClassInfoType(parents, decls, clazz) =>
          val newDecls = decls.cloneScope
          val methSym = newDecls.enter(sym.newMethodSymbol(nme_ClassName, sym.pos))
          methSym.setInfo(NullaryMethodType(definitions.StringTpe))
          exitingPhase(currentRun.phaseNamed(phaseName)) {
            if (tpe.baseClasses.drop(1).exists(bcSym => methSym.overriddenSymbol(bcSym) != NoSymbol))
              methSym.setFlag(Flag.OVERRIDE)
          }
          ClassInfoType(parents, newDecls, clazz)
        case _ => tpe
      }
    else tpe
  }

  protected def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case Template(parents, self, body) =>
        val classSym = tree.symbol.enclClass
        val methSym = exitingPhase(currentRun.phaseNamed(phaseName))(classSym.info.decl(nme_ClassName))
        if (methSym != NoSymbol) {
          val ddef = localTyper.typedPos(tree.pos)(DefDef(methSym, Literal(Constant(classSym.name.decoded))))
          treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformTrees(body) :+ ddef)
        } else super.transform(tree)
      case _ => super.transform(tree)
    }
  }
}
