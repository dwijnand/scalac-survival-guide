package guide

import java.io.PrintStream
import java.nio.file.{ Files, Paths }

import scala.tools.nsc.util._

sealed abstract class AppTest(app: App) {
  def name = app.getClass.getSimpleName.stripSuffix("$")

  def main(args: Array[String]): Unit = if (!run(args.toList)) sys.error(s"$name failed")

  def run(argv: List[String]) = {
    var updateCheck = false
    argv match {
      case Nil                     => ()
      case "--update-check" :: Nil => updateCheck = true
      case _                       => sys.error(s"Unknown options $argv")
    }

    val checkFile = Paths.get(s"src/test/resources/$name.check")
    if (!Files.exists(checkFile)) {
      Files.createDirectories(checkFile.getParent)
      Files.createFile(checkFile)
    }

    val incoming = stringFromStream { o =>
      val err = System.err
      val p = new PrintStream(o, /* autoFlush = */ true)
      try {
        System.setErr(p)
        Console.withOut(p)(Console.withErr(p)(app.main(Array())))
      } catch {
        case t: Throwable if !t.isInstanceOf[VirtualMachineError] => p.println(stackTraceString(t))
      } finally {
        System.setErr(err)
        p.close()
      }
    }

    val expected = new String(Files.readAllBytes(checkFile))

    if (incoming == expected) {
      println(s"${Console.GREEN}$name: SUCCESS${Console.RESET}")
      true
    } else if (updateCheck) {
      Files.write(checkFile, incoming.getBytes)
      println(s"${Console.YELLOW}$name: UPDATED${Console.RESET}")
      true
    } else {
      println(s"${Console.RED}$name: FAILED${Console.RESET}")
      false
    }
  }
}

object Tests {
  def main(args: Array[String]): Unit = if (apps.map(_.run(args.toList)).exists(!_)) sys.error("Tests failed")

  val apps = List(
    _01_Intro_Test,
    _02_HelloWorld_Test,
    _03_Trees_Test,
    _04_TypesAndSymbols_Test,
    _05_Unshrouding_Test,
    _06_Unshrouding_Test,
    _07_Typer_Test,
    _08_OwnGoals_Test,
    _09_JarLister_Test,
    _10_Mixin_Test,
    _12_InfoTransformerHell_Test,
    _13_OwnerMutation_Test,
    _14_SymbolTableMutation_Test,
    _15_DefaultGetterAnnotation_Test,
    _16_FunctionInline_Test,
    _17_MatchSplitter_Test,
    _18_DefaultMethodAdder_Test,
    _19_ScalaSig_Test,
    _20_RelatedMethodAdder_Test,
    _21_ParamAlias_Test,
    _22_InfoTransform_Test,
    _23_CompletedSymbols_Test,
    _24_RscComparison_Test
  )
}

object _01_Intro_Test                   extends AppTest(_01_Intro)
object _02_HelloWorld_Test              extends AppTest(_02_HelloWorld)
object _03_Trees_Test                   extends AppTest(_03_Trees)
object _04_TypesAndSymbols_Test         extends AppTest(_04_TypesAndSymbols)
object _05_Unshrouding_Test             extends AppTest(_05_Unshrouding)
object _06_Unshrouding_Test             extends AppTest(_06_Unshrouding)
object _07_Typer_Test                   extends AppTest(_07_Typer)
object _08_OwnGoals_Test                extends AppTest(_08_OwnGoals)
object _09_JarLister_Test               extends AppTest(_09_JarLister)
object _10_Mixin_Test                   extends AppTest(_10_Mixin)
object _12_InfoTransformerHell_Test     extends AppTest(_12_InfoTransformerHell)
object _13_OwnerMutation_Test           extends AppTest(_13_OwnerMutation)
object _14_SymbolTableMutation_Test     extends AppTest(_14_SymbolTableMutation)
object _15_DefaultGetterAnnotation_Test extends AppTest(_15_DefaultGetterAnnotation)
object _16_FunctionInline_Test          extends AppTest(_16_FunctionInline)
object _17_MatchSplitter_Test           extends AppTest(_17_MatchSplitter)
object _18_DefaultMethodAdder_Test      extends AppTest(_18_DefaultMethodAdder)
object _19_ScalaSig_Test                extends AppTest(_19_ScalaSig)
object _20_RelatedMethodAdder_Test      extends AppTest(_20_RelatedMethodAdder)
object _21_ParamAlias_Test              extends AppTest(_21_ParamAlias)
object _22_InfoTransform_Test           extends AppTest(_22_InfoTransform)
object _23_CompletedSymbols_Test        extends AppTest(_23_CompletedSymbols)
object _24_RscComparison_Test           extends AppTest(_24_RscComparison)
