import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

package object guide {
  val Width = 80
  def --------------- = println("\n" + ("-" * Width) + "\n")

  def ===(s: String) = p(title('=', s))
  def ---(s: String) = p(title('-', s))

  def title(filler: Char, s: String) = {
    val diff = Width - s.length - 1
    val l = diff / 2
    val r = diff - l
    "\n" + ("=" * l) + " " + s + " " + ("=" * r) + "\n"
  }

  var lastWasComment = false
  def p(a: Any) = {
    val msg = a.toString
    val isComment = msg.startsWith("// ")
    if (lastWasComment) println(msg)
    else println("\n" + msg)
    lastWasComment = isComment
  }

  def newGlobal(options: String = ""): Global = {
    val reporter = new StoreReporter
    val settings = new Settings()
    settings.processArgumentString("-usejavacp " + options)
    val g = new Global(settings, reporter)
    new g.Run
    g
  }
  def compile(code: String, global: Global = newGlobal()): CompileResult[global.type] = {
    val run = new global.Run
    global.reporter.reset()
    val source = global.newSourceFile(code)
    run.compileSources(source :: Nil)
    val tree = run.units.toList.head.body
    val infos = global.reporter match {
      case sr: StoreReporter => sr.infos
      case _ => Nil
    }
    new global.Run
    new CompileResult[global.type](global, global.reporter.hasErrors, tree, infos.toList)
  }

  def lookupIdent(g: Global)(name: g.Name): g.Symbol = {
    import g._
    val context = analyzer.newTyper(analyzer.rootContext(NoCompilationUnit)).context
    val lookup = context.lookupSymbol(name, _ => true)
    lookup.symbol
  }

  case class CompileResult[G <: Global](global: G, error: Boolean, tree: G#Tree, infos: List[StoreReporter#Info])

  private var indent = 0
  var debug = true
  def trace[A](msg: String)(body: => A): A = {
    def oneLine(s: String) = s.replaceAll("""\{\n"""", "{ ").replaceAll("\n", "; ")
    def p(msg: String) = if (debug) println((" " * 4 * indent) + oneLine(msg))
    p(msg)
    indent += 1
    val result = try body finally indent -= 1
    p("=> " + result)
    result
  }
}
