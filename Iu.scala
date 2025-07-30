import scala.io.StdIn
import scala.collection.mutable

@main def Iu(): Unit =
  val global = Env.standard()
  println("iu (c) tttiw (l) mit | :q")
  repl(global)

/// DATA
sealed trait LVal

final case class LNum(v: BigDecimal) extends LVal
final case class LBool(v: Boolean) extends LVal
final case class LSymbol(name: String) extends LVal
final case class LList(items: List[LVal]) extends LVal
final case class LStr(s: String) extends LVal
final case class LClosure(
    params: List[String],
    body: List[LVal],
    env: Env
) extends LVal
final case class LBuiltin(
    name: String,
    fn: List[LVal] => Either[String, LVal]
) extends LVal
final case class LSpecial(
    name: String,
    fn: (List[LVal], Env) => Either[String, LVal]
) extends LVal

object LVal:
  val True: LBool = LBool(true)
  val False: LList = LList(Nil)

/// ENV
final class Env(
    private val table: mutable.Map[String, LVal],
    val parent: Option[Env]
):
  def get(sym: String): Option[LVal] =
    table.get(sym).orElse(parent.flatMap(_.get(sym)))

  def define(sym: String, v: LVal): Unit =
    table.update(sym, v)

object Env:
  def empty(parent: Option[Env] = None): Env =
    new Env(mutable.LinkedHashMap.empty, parent)

  def standard(): Env =
    val env = empty(None)
    // Special forms
    env.define("list", LBuiltin("list", Builtins.list))
    env.define("sqrt", LBuiltin("sqrt", Builtins.sqrt))
    env.define("lambda", LSpecial("lambda", SpecialForms.lambda))
    env.define("load", LSpecial("load", SpecialForms.load))

    env.define("quote", LSpecial("quote", SpecialForms.quote))
    env.define("if", LSpecial("if", SpecialForms.`if`))
    env.define("define", LSpecial("define", SpecialForms.define))
    env.define("cond", LSpecial("cond", SpecialForms.cond))
    env.define("and", LSpecial("and", SpecialForms.and))
    env.define("or", LSpecial("or", SpecialForms.or))

    // Predicates, list ops
    env.define("atom", LBuiltin("atom", Builtins.`atom`))
    env.define("eq", LBuiltin("eq", Builtins.`eq`))
    env.define("car", LBuiltin("car", Builtins.car))
    env.define("cdr", LBuiltin("cdr", Builtins.cdr))
    env.define("cons", LBuiltin("cons", Builtins.cons))

    // Logic
    env.define("not", LBuiltin("not", Builtins.not))
    env.define("xor", LBuiltin("xor", Builtins.xor))

    // Arithmetic
    env.define("+", LBuiltin("+", Builtins.plus))
    env.define("-", LBuiltin("-", Builtins.minus))
    env.define("*", LBuiltin("*", Builtins.times))
    env.define("/", LBuiltin("/", Builtins.div))
    env.define("%", LBuiltin("%", Builtins.mod))
    env.define("^", LBuiltin("^", Builtins.pow))

    // Equality
    env.define("=", LBuiltin("=", Builtins.numEq))
    env.define("<", LBuiltin("<", Builtins.lt))
    env.define(">", LBuiltin(">", Builtins.gt))
    env.define("<=", LBuiltin("<=", Builtins.le))
    env.define(">=", LBuiltin(">=", Builtins.ge))

    // Constants
    env.define("#t", LVal.True)
    env.define("#nil", LVal.False)

    // String ops
    env.define("str-append", LBuiltin("str-append", Builtins.strAppend))
    env.define("join", LBuiltin("join", Builtins.join))
    env.define("strlen", LBuiltin("strlen", Builtins.strlen))
    env.define("display", LBuiltin("display", Builtins.display))
    env.define("println", LBuiltin("println", Builtins.println))

    env

/// EVAL
def eval(expr: LVal, env: Env): Either[String, LVal] =
  expr match
    case s: LNum      => Right(s)
    case b: LBool     => Right(b)
    case s: LStr      => Right(s)
    case f: LBuiltin  => Right(f)
    case sf: LSpecial => Right(sf)
    case c: LClosure  => Right(c)
    case LSymbol(name) =>
      env.get(name).toRight(s"Unbound symbol: $name")
    case LList(Nil) =>
      Right(LList(Nil))
    case LList(op :: args) =>
      op match
        case LSymbol(name) =>
          env.get(name) match
            case Some(LSpecial(_, sf)) => sf(args, env)
            case Some(LBuiltin(_, fn)) => evalList(args, env).flatMap(fn)
            case Some(c: LClosure) =>
              evalList(args, env).flatMap(applyClosure(c, _))
            case Some(v) => Left(s"Not a function: ${Printer.show(v)}")
            case None    => Left(s"Unbound operator: $name")

        case _ =>
          // Operator is an expression producing a function/closure
          eval(op, env).flatMap {
            case LBuiltin(_, fn) => evalList(args, env).flatMap(fn)
            case c: LClosure => evalList(args, env).flatMap(applyClosure(c, _))
            case v           => Left(s"Not a function: ${Printer.show(v)}")
          }

private def evalList(
    args: List[LVal],
    env: Env
): Either[String, List[LVal]] =
  args.foldLeft[Either[String, List[LVal]]](Right(Nil)) { (acc, a) =>
    for
      xs <- acc
      v <- eval(a, env)
    yield xs :+ v
  }

private def applyClosure(
    c: LClosure,
    argVals: List[LVal]
): Either[String, LVal] =
  if c.params.length != argVals.length then
    Left(s"arity mismatch: expected ${c.params.length}, got ${argVals.length}")
  else
    val callEnv = Env.empty(Some(c.env))
    c.params.zip(argVals).foreach { case (p, v) =>
      callEnv.define(p, v)
    }
    // Evaluate body sequentially; return last or #nil
    c.body match
      case Nil => Right(LVal.False)
      case _ =>
        c.body.foldLeft[Either[String, LVal]](Right(LVal.False)) {
          (acc, form) =>
            acc.flatMap(_ => eval(form, callEnv))
        }

/// SPECIAL FORMS
object SpecialForms:
  import scala.util.Using
  import scala.io.Source

  val load: (List[LVal], Env) => Either[String, LVal] = { (args, env) =>
    args match
      case one :: Nil =>
        eval(one, env).flatMap {
          case LStr(path)   => doLoad(path, env)
          case LSymbol(sym) => doLoad(sym, env)
          case other =>
            Left(s"load expects a string or symbol, got ${Printer.show(other)}")
        }
      case _ =>
        Left("load expects exactly 1 argument")
  }

  private def doLoad(path: String, env: Env): Either[String, LVal] =
    Using(Source.fromFile(path, "UTF-8")) { src =>
      src.mkString
    }.toEither.left
      .map(_.getMessage)
      .flatMap { content =>
        Reader.readMany(content).flatMap { forms =>
          // Evaluate each form; return last or #nil
          forms.foldLeft[Either[String, LVal]](Right(LVal.False)) { (acc, f) =>
            acc.flatMap(_ => eval(f, env))
          }
        }
      }

  // helper to reuse in lambda above
  private def sequence[A](
      xs: List[Either[String, A]]
  ): Either[String, List[A]] =
    xs.foldLeft[Either[String, List[A]]](Right(Nil)) { (acc, e) =>
      for
        as <- acc
        a <- e
      yield as :+ a
    }

  val lambda: (List[LVal], Env) => Either[String, LVal] = { (args, env) =>
    args match
      case LList(paramsRaw) :: body if body.nonEmpty =>
        val params = paramsRaw.map {
          case LSymbol(name) => Right(name)
          case other =>
            Left(
              s"lambda parameter must be a symbol, got ${Printer.show(other)}"
            )
        }
        sequence(params).map(names => LClosure(names, body, env))
      case _ =>
        Left(
          "lambda expects (lambda (<params>) <body>...), at least one body form"
        )
  }

  val quote: (List[LVal], Env) => Either[String, LVal] = { (args, _) =>
    args match
      case v :: Nil => Right(v)
      case _        => Left("quote expects exactly 1 argument")
  }

  val `if`: (List[LVal], Env) => Either[String, LVal] = { (args, env) =>
    args match
      case cond :: conseq :: alt :: Nil =>
        eval(cond, env).flatMap { c =>
          if Truth.isTruthy(c) then eval(conseq, env)
          else eval(alt, env)
        }
      case _ =>
        Left("if expects exactly 3 arguments: (if test then else)")
  }

  val define: (List[LVal], Env) => Either[String, LVal] = { (args, env) =>
    args match
      case LSymbol(name) :: valueExpr :: Nil =>
        eval(valueExpr, env).map { v =>
          env.define(name, v)
          v
        }
      case _ => Left("define expects (define <symbol> <expr>)")
  }

  val cond: (List[LVal], Env) => Either[String, LVal] = { (clauses, env) =>
    def evalClause(cl: LVal): Either[String, Option[LVal]] =
      cl match
        case LList(LSymbol("else") :: expr :: Nil) =>
          eval(expr, env).map(Some(_))
        case LList(test :: expr :: Nil) =>
          eval(test, env).flatMap { t =>
            if Truth.isTruthy(t) then eval(expr, env).map(Some(_))
            else Right(None)
          }
        case _ => Left("cond clauses must be (test expr) or (else expr)")

    clauses
      .foldLeft[Either[String, Option[LVal]]](Right(None)) { (acc, cl) =>
        acc.flatMap {
          case some @ Some(_) => Right(some)
          case None           => evalClause(cl)
        }
      }
      .map(_.getOrElse(LVal.False))
  }

  val and: (List[LVal], Env) => Either[String, LVal] = { (args, env) =>
    def loop(rest: List[LVal], last: LVal): Either[String, LVal] = rest match
      case Nil => Right(last)
      case h :: t =>
        eval(h, env).flatMap { v =>
          if Truth.isTruthy(v) then loop(t, v)
          else Right(LVal.False)
        }

    args match
      case Nil => Right(LVal.True)
      case _   => loop(args, LVal.True)
  }

  val or: (List[LVal], Env) => Either[String, LVal] = { (args, env) =>
    def loop(rest: List[LVal]): Either[String, LVal] = rest match
      case Nil => Right(LVal.False)
      case h :: t =>
        eval(h, env).flatMap { v =>
          if Truth.isTruthy(v) then Right(v)
          else loop(t)
        }

    loop(args)
  }

/// Builtins
object Builtins:

  val display: List[LVal] => Either[String, LVal] = {
    case v :: Nil =>
      v match
        case LStr(s) => print(s) // raw, no escaping
        case other   => print(Printer.show(other))
      Right(LVal.False)
    case _ => Left("display expects exactly 1 argument")
  }

  val println: List[LVal] => Either[String, LVal] = {
    case v :: Nil => display(List(v)).map(_ => { print("\n"); LVal.False })
    case _        => Left("println expects exactly 1 argument")
  }

  val strAppend: List[LVal] => Either[String, LVal] = { args =>
    args
      .foldLeft[Either[String, String]](Right("")) {
        case (Right(acc), LStr(s)) => Right(acc + s)
        case (Right(_), other) =>
          Left(s"str-append expects strings, got ${Printer.show(other)}")
        case (l @ Left(_), _) => l
      }
      .map(LStr.apply)
  }

  val join: List[LVal] => Either[String, LVal] = {
    case LStr(sep) :: LList(items) :: Nil =>
      val parts = items.map {
        case LStr(s) => Right(s)
        case other =>
          Left(s"join expects a list of strings, got ${Printer.show(other)}")
      }
      sequence(parts).map(ss => LStr(ss.mkString(sep)))
    case _ => Left("join usage: (join <separator:string> <list-of-strings>)")
  }

  val strlen: List[LVal] => Either[String, LVal] = {
    case LStr(s) :: Nil => Right(LNum(BigDecimal(s.length)))
    case v :: Nil => Left(s"strlen expects a string, got ${Printer.show(v)}")
    case _        => Left("strlen expects exactly 1 argument")
  }

  private def expectNum(v: LVal): Either[String, BigDecimal] = v match
    case LNum(n) => Right(n)
    case other   => Left(s"Expected number, got ${Printer.show(other)}")

  private def toNum(n: BigDecimal): LNum = LNum(n)

  // numeric equality and order
  private def expect2Nums(
      args: List[LVal],
      opname: String
  ): Either[String, (BigDecimal, BigDecimal)] = args match
    case a :: b :: Nil =>
      for
        x <- expectNum(a)
        y <- expectNum(b)
      yield (x, y)
    case _ => Left(s"$opname expects exactly 2 numeric arguments")

  // Map a numeric function over a scalar or (nested) list
  private def broadcast1(a: LVal)(
      f: BigDecimal => Either[String, BigDecimal]
  ): Either[String, LVal] = a match
    case LNum(x)   => f(x).map(LNum.apply)
    case LList(xs) => sequence(xs.map(broadcast1(_)(f))).map(LList.apply)
    case other => Left(s"Expected number or list, got ${Printer.show(other)}")

  // Combine two lists/scalars with elementwise op and cycling
  private def broadcast2(a: LVal, b: LVal)(
      f: (BigDecimal, BigDecimal) => Either[String, BigDecimal]
  ): Either[String, LVal] = (a, b) match
    case (LNum(x), LNum(y)) =>
      f(x, y).map(LNum.apply)

    // two lists -> elementwise with cycling
    case (LList(xs), LList(ys)) =>
      if xs.isEmpty || ys.isEmpty then Right(LList(Nil))
      else
        val n = math.max(xs.length, ys.length)
        val res = (0 until n).toList.map { i =>
          val xi = xs(i % xs.length)
          val yi = ys(i % ys.length)
          broadcast2(xi, yi)(f)
        }
        sequence(res).map(LList.apply)

    // one list, one scalar
    case (LList(xs), r) =>
      sequence(xs.map(x => broadcast2(x, r)(f))).map(LList.apply)
    case (l, LList(ys)) =>
      sequence(ys.map(y => broadcast2(l, y)(f))).map(LList.apply)

    case (other1, other2) =>
      Left(
        s"Expected numbers or lists, got ${Printer.show(other1)} and ${Printer.show(other2)}"
      )

  // Pure numeric helpers for use with broadcast*
  private inline def pure1(
      g: BigDecimal => BigDecimal
  ): LVal => Either[String, LVal] =
    a => broadcast1(a)(x => Right(g(x)))
  private inline def pure2(
      g: (BigDecimal, BigDecimal) => BigDecimal
  ): (LVal, LVal) => Either[String, LVal] =
    (a, b) => broadcast2(a, b)((x, y) => Right(g(x, y)))

  // Numeric pow for scalars
  private def numPow(base: BigDecimal, exp: BigDecimal): BigDecimal =
    if exp.scale == 0 && exp.isValidInt then base.pow(exp.toInt)
    else BigDecimal(Math.pow(base.toDouble, exp.toDouble))

  val numEq: List[LVal] => Either[String, LVal] = args =>
    expect2Nums(args, "=").map { case (x, y) =>
      if x == y then LVal.True else LVal.False
    }

  val lt: List[LVal] => Either[String, LVal] = args =>
    expect2Nums(args, "<").map { case (x, y) =>
      if x < y then LVal.True else LVal.False
    }

  val gt: List[LVal] => Either[String, LVal] = args =>
    expect2Nums(args, ">").map { case (x, y) =>
      if x > y then LVal.True else LVal.False
    }

  val le: List[LVal] => Either[String, LVal] = args =>
    expect2Nums(args, "<=").map { case (x, y) =>
      if x <= y then LVal.True else LVal.False
    }

  val ge: List[LVal] => Either[String, LVal] = args =>
    expect2Nums(args, ">=").map { case (x, y) =>
      if x >= y then LVal.True else LVal.False
    }

  val list: List[LVal] => Either[String, LVal] = args => Right(LList(args))

  val sqrt: List[LVal] => Either[String, LVal] = {
    case v :: Nil =>
      broadcast1(v) { n =>
        if n < 0 then Left("sqrt of negative number")
        else Right(BigDecimal(Math.sqrt(n.toDouble)))
      }
    case _ => Left("sqrt expects exactly 1 argument")
  }

  // broadcasted reduction with identity 0
  val plus: List[LVal] => Either[String, LVal] = args =>
    args.foldLeft[Either[String, LVal]](Right(LNum(0))) { (acc, a) =>
      for
        x <- acc
        y <- Right(a)
        r <- pure2(_ + _)(x, y)
      yield r
    }

  val minus: List[LVal] => Either[String, LVal] = {
    case Nil =>
      Left("- expects at least 1 argument")
    case x :: Nil =>
      broadcast1(x)(n => Right(-n))
    case x :: rest =>
      rest.foldLeft[Either[String, LVal]](Right(x)) { (acc, a) =>
        for
          left <- acc
          right <- Right(a)
          r <- pure2(_ - _)(left, right)
        yield r
      }
  }

  // broadcasted reduction with identity 1
  val times: List[LVal] => Either[String, LVal] = args =>
    args.foldLeft[Either[String, LVal]](Right(LNum(1))) { (acc, a) =>
      for
        x <- acc
        y <- Right(a)
        r <- pure2(_ * _)(x, y)
      yield r
    }

  // (/ a b c ...) => ((a / b) / c) ...
  val div: List[LVal] => Either[String, LVal] = {
    case Nil      => Left("/ expects at least 1 argument")
    case x :: Nil =>
      // reciprocal
      broadcast2(LNum(1), x) { (_, d) =>
        if d == 0 then Left("Division by zero")
        else Right(BigDecimal(1) / d)
      }
    case x :: rest =>
      def step(lhs: LVal, rhs: LVal): Either[String, LVal] =
        broadcast2(lhs, rhs) { (n, d) =>
          if d == 0 then Left("Division by zero") else Right(n / d)
        }
      rest.foldLeft[Either[String, LVal]](Right(x)) { (acc, a) =>
        for left <- acc; r <- step(left, a) yield r
      }
  }

  // arity 2
  val mod: List[LVal] => Either[String, LVal] = {
    case a :: b :: Nil =>
      broadcast2(a, b) { (x, y) =>
        if y == 0 then Left("mod by zero")
        else Right(x.remainder(y))
      }
    case _ => Left("% expects exactly 2 arguments")
  }

  // arity 2
  val pow: List[LVal] => Either[String, LVal] = {
    case a :: b :: Nil =>
      broadcast2(a, b)((x, y) => Right(numPow(x, y)))
    case _ => Left("^ expects exactly 2 arguments")
  }

  val `atom`: List[LVal] => Either[String, LVal] = {
    case v :: Nil =>
      val isAtom = v match
        case LList(items)                    => items.isEmpty
        case LBuiltin(_, _) | LSpecial(_, _) => false
        case _                               => true
      Right(if isAtom then LVal.True else LVal.False)
    case _ => Left("atom? expects exactly 1 argument")
  }

  val `eq`: List[LVal] => Either[String, LVal] = {
    case a :: b :: Nil =>
      Right(if equalDeep(a, b) then LVal.True else LVal.False)
    case _ => Left("eq? expects exactly 2 arguments")
  }

  val car: List[LVal] => Either[String, LVal] = {
    case LList(h :: _) :: Nil => Right(h)
    case LList(Nil) :: Nil    => Left("car of empty list")
    case other :: Nil =>
      Left(s"car expects a non-empty list, got ${Printer.show(other)}")
    case _ => Left("car expects exactly 1 argument")
  }

  val cdr: List[LVal] => Either[String, LVal] = {
    case LList(_ :: t) :: Nil => Right(LList(t))
    case LList(Nil) :: Nil    => Left("cdr of empty list")
    case other :: Nil =>
      Left(s"cdr expects a non-empty list, got ${Printer.show(other)}")
    case _ => Left("cdr expects exactly 1 argument")
  }

  val cons: List[LVal] => Either[String, LVal] = {
    case x :: LList(xs) :: Nil => Right(LList(x :: xs))
    case _ :: y :: Nil => Left(s"cons expects second argument to be a list")
    case _             => Left("cons expects exactly 2 arguments")
  }

  val not: List[LVal] => Either[String, LVal] = {
    case v :: Nil =>
      Right(if Truth.isTruthy(v) then LVal.False else LVal.True)
    case _ => Left("not expects exactly 1 argument")
  }

  val xor: List[LVal] => Either[String, LVal] = args =>
    val truths = args.count(Truth.isTruthy)
    Right(if truths % 2 == 1 then LVal.True else LVal.False)

  // helpers
  private def equalDeep(a: LVal, b: LVal): Boolean = (a, b) match
    case (LNum(x), LNum(y))       => x == y
    case (LBool(x), LBool(y))     => x == y
    case (LSymbol(x), LSymbol(y)) => x == y
    case (LStr(x), LStr(y))       => x == y
    case (LList(xs), LList(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall { case (x, y) =>
        equalDeep(x, y)
      }
    case _ => false

  private def sequence[A](
      xs: List[Either[String, A]]
  ): Either[String, List[A]] =
    xs.foldLeft[Either[String, List[A]]](Right(Nil)) { (acc, e) =>
      for
        as <- acc
        a <- e
      yield as :+ a
    }

// TRUITHINESS
object Truth:
  def isTruthy(v: LVal): Boolean = v match
    case LList(Nil)   => false
    case LBool(false) => false
    case _            => true

/// PRINTER
object Printer:
  def show(v: LVal): String = v match
    case LNum(n)           => formatNumber(n)
    case LBool(b)          => if b then "#t" else "#nil"
    case LSymbol(s)        => s
    case LStr(s)           => "\"" + escape(s) + "\""
    case LList(xs)         => xs.map(show).mkString("(", " ", ")")
    case LBuiltin(name, _) => s"#<builtin ${name}>"
    case LSpecial(name, _) => s"#<special ${name}>"
    case _: LClosure       => "#<closure>"

  private def escape(s: String): String =
    s.flatMap {
      case '\\' => "\\\\"
      case '\"' => "\\\""
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case c    => c.toString
    }

  private def formatNumber(n: BigDecimal): String =
    val bd = n.underlying().stripTrailingZeros()
    if bd.scale() <= 0 then bd.toPlainString else bd.toPlainString

/// Reader, Parser
object Reader:
  def readMany(input: String): Either[String, List[LVal]] =
    val toks = tokenize(input)
    val ps = Parser(toks)
    ps.parseAll()

  private def tokenize(s: String): List[String] =
    val b = new StringBuilder
    val out = scala.collection.mutable.ListBuffer.empty[String]
    var i = 0
    var inString = false

    def flushToken(): Unit =
      if b.nonEmpty then { out += b.toString; b.clear() }

    while i < s.length do
      val c = s.charAt(i)
      if inString then
        if c == '\\' && i + 1 < s.length then
          b += c; i += 1; b += s.charAt(i); i += 1
        else if c == '"' then
          b += c; i += 1; flushToken(); inString = false
        else
          b += c; i += 1
      else
        c match
          case ';' =>
            while i < s.length && s.charAt(i) != '\n' do i += 1
          case '(' | ')' | '\'' =>
            flushToken()
            out += c.toString
            i += 1
          case '"' =>
            flushToken()
            inString = true
            b += '"'
            i += 1
          case ws if ws.isWhitespace =>
            flushToken(); i += 1
          case _ =>
            b += c; i += 1
    flushToken()
    out.toList

  final case class Parser(tokens: List[String]):
    private var i = 0

    def parseAll(): Either[String, List[LVal]] =
      val buf = mutable.ListBuffer.empty[LVal]
      while !eof do
        parseForm() match
          case Right(v) => buf += v
          case Left(e)  => return Left(e)
      Right(buf.toList)

    private def eof: Boolean = i >= tokens.length
    private def peek: String = tokens(i)
    private def next(): String = { val t = tokens(i); i += 1; t }

    private def parseForm(): Either[String, LVal] =
      if eof then Left("Unexpected end of input")
      else
        next() match
          case "(" => parseList()
          case ")" => Left("Unexpected )")
          case "'" => parseForm().map(v => LList(List(LSymbol("quote"), v)))
          case tok => atom(tok)

    private def parseList(): Either[String, LVal] =
      val items = mutable.ListBuffer.empty[LVal]
      while !eof && peek != ")" do
        parseForm() match
          case Right(v) => items += v
          case Left(e)  => return Left(e)
      if eof then Left("Unclosed (")
      else
        next() // consume ")"
        Right(LList(items.toList))

    private def atom(tok: String): Either[String, LVal] =
      tok match
        case "#t"   => Right(LVal.True)
        case "#nil" => Right(LVal.False)
        case _
            if tok.startsWith("\"") && tok.endsWith("\"") && tok.length >= 2 =>
          unescape(tok.substring(1, tok.length - 1)).map(LStr.apply)
        case _ =>
          if tok.matches("""[-+]?\d+(\.\d+)?""") then
            Right(LNum(BigDecimal(tok)))
          else Right(LSymbol(tok))

    private def unescape(in: String): Either[String, String] =
      val sb = new StringBuilder
      var i = 0
      while i < in.length do
        in.charAt(i) match
          case '\\' =>
            if i + 1 >= in.length then return Left("bad string escape at end")
            in.charAt(i + 1) match
              case '\\' => sb += '\\'
              case '"'  => sb += '"'
              case 'n'  => sb += '\n'
              case 't'  => sb += '\t'
              case 'r'  => sb += '\r'
              case x    => sb += x
            i += 2
          case c =>
            sb += c; i += 1
      Right(sb.toString)

/// REPL
def repl(env: Env): Unit =
  var buffer = new StringBuilder()
  var depth = 0
  var running = true
  var timeMode = false

  def handleCommand(cmd: String): Boolean =
    // returns true if the command was handled
    cmd.trim match
      case ":time on" =>
        timeMode = true; println("time mode is now ON"); true
      case ":time off" =>
        timeMode = false; println("time mode is now OFF"); true
      case ":time" =>
        timeMode = !timeMode;
        println(s"time mode is now ${if timeMode then "ON" else "OFF"}"); true
      case ":time?" =>
        println(s"time mode is ${if timeMode then "ON" else "OFF"}"); true
      case ":quit" | ":q" =>
        running = false; true
      case _ =>
        false

  while running do
    val prompt = if depth == 0 then "iu |> " else "..... |> "
    print(prompt); Console.flush()
    val line = StdIn.readLine()
    if line == null then running = false
    else
      val trimmed = line.trim
      // REPL commands only when not in the middle of a multi-line form
      if depth == 0 && trimmed.startsWith(":") then
        if !handleCommand(trimmed) then
          println("Unknown command. Try :time [on|off|?] or :quit")
      else
        buffer.append(' ').append(line)
        depth += parenDelta(line)
        if depth < 0 then
          println("Error: too many )")
          buffer.clear()
          depth = 0
        else if depth == 0 && buffer.nonEmpty then
          val input = buffer.toString
          buffer.clear()
          Reader.readMany(input) match
            case Left(err) =>
              println(s"Read error: $err")
            case Right(forms) =>
              forms.foreach { form =>
                val t0 = if timeMode then System.nanoTime() else 0L
                val res = eval(form, env)
                val t1 = if timeMode then System.nanoTime() else 0L
                val timing = if timeMode then "  ; " + fmtMs(t1 - t0) else ""
                res match
                  case Left(err) => println(s"Error: $err$timing")
                  case Right(v)  => println(Printer.show(v) + timing)
              }

private def parenDelta(s: String): Int =
  var i = 0
  var delta = 0
  var inString = false
  var escaped = false
  var inComment = false

  while i < s.length do
    val c = s.charAt(i)
    if inComment then
      // ignore rest of the line
      i = s.length
    else if inString then
      if escaped then escaped = false
      else
        c match
          case '\\' => escaped = true
          case '"'  => inString = false
          case _    => ()
      i += 1
    else
      c match
        case ';' => inComment = true; i += 1
        case '"' => inString = true; i += 1
        case '(' => delta += 1; i += 1
        case ')' => delta -= 1; i += 1
        case _   => i += 1
  delta

private def fmtMs(ns: Long): String =
  val ms = ns.toDouble / 1e6
  f"$ms%.3f ms"
