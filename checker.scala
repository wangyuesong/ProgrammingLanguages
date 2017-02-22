import scala.io._
import cs162.assign4.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main(args: Array[String]) {
    val filename = args(0)

    //    args.foreach { x => extracted(x) }
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          getType(program.e, new TypeEnv())
          println(Pretty.prettySyntax(program))
          println("This program is well-typed")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  def extracted(filename: String) = {
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          println(Pretty.prettySyntax(program))
          getType(program.e, new TypeEnv())
          println("This program " + filename + " is well-typed:\n")
          if (filename.contains("good")) println("Correct") else { println("Incorrect"); }
        } catch {
          case Illtyped =>
            println("This program " + filename + " is ill-typed")
            if (filename.contains("bad")) println("Correct") else { println("Incorrect"); }
        }

    }
  }

  // Gets a listing of the constructor names associated with a given type definition.
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructors`, along with return values:
  //
  // constructors("Either") = Set("Left", "Right")
  // constructors("Foo") = a thrown Illtyped exception
  //
  def constructors(name: Label): Set[Label] =
    typeDefs.find(_.name == name).map(_.constructors.keySet).getOrElse(throw Illtyped)

  // Takes the following parameters:
  // -The name of a user-defined constructor
  // -The types which we wish to apply to the constructor
  // Returns the type that is held within the constructor.
  //
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructorType`, along with return values:
  //
  // constructorType("Left", Seq(NumT, BoolT)) = NumT
  // constructorType("Right", Seq(NumT, BoolT)) = BoolT
  // constructorType("Left", Seq(NumT)) = a thrown Illtyped exception
  // constructorType("Right", Seq(BoolT)) = a thrown Illtyped exception
  // constructorType("Foo", Seq(UnitT)) = a thrown Illtyped exception
  // constructorType("Left", Seq(UnitT)) = a thrown Illtyped exception
  //
  def constructorType(constructor: Label, types: Seq[Type]): Type =
    (for {
      td <- typeDefs
      rawType <- td.constructors.get(constructor)
      if (types.size == td.tvars.size)
    } yield replace(rawType, td.tvars.zip(types).toMap)).headOption.getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  //
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  // Given a type and a mapping of type variables to other types, it
  // will recursively replace the type variables in `t` with the
  // types in `tv2t`, if possible.  If a type variable isn't
  // in `tv2t`, it should simply return the original type.  If a
  // `TFunT` is encountered, then whatever type variables it defines
  // (the first parameter in the `TFunT`) should overwrite whatever is in
  // `tv2t` right before a recursive `replace` call.  In other words,
  // type variables can shadow other type variables.
  //
  def replace(t: Type, tv2t: Map[TVar, Type]): Type =
    t match {
      case NumT | BoolT | UnitT => t // FILL ME IN

      // Params[var->Type], this type might be a TypeVar(which is a type)
      case FunT(params, ret) =>
        FunT(params.map(p => replace(p, tv2t)), replace(ret, tv2t))

      case RcdT(fields)     => RcdT(fields.map { f => (f._1, replace(f._2, tv2t)) })

      case TypT(name, typs) => TypT(name, typs.map { t => replace(t, tv2t) })
      // Base case
      case tv: TVar         => tv2t.getOrElse(tv, tv)
      // Need to forget about everything we had about tvars before
      case TFunT(tvars, funt) => {
        val ntv2t = tv2t.filter { vt => !tvars.contains(vt._1) }
        val replacedFunT = replace(funt, ntv2t)
        replacedFunT match {
          case ft: FunT => TFunT(tvars, ft)
          case _        => throw Illtyped
        }
      }
    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType(e: Exp, env: TypeEnv): Type =
    e match {
      case x: Var                        => env.getOrElse(x, throw Illtyped)

      case _: Num                        => NumT // FILL ME IN

      case _: Bool                       => BoolT // FILL ME IN

      case _: Unit                       => UnitT // FILL ME IN

      case Plus | Minus | Times | Divide => NumT // FILL ME IN

      case LT | EQ                       => BoolT // FILL ME IN

      case And | Or                      => BoolT // FILL ME IN

      case Not                           => BoolT // FILL ME IN

      case Fun(params, body)             => FunT(params.map(a => a._2), getType(body, env ++ params))

      case Call(fun, args) => getType(fun, env) match {
        case FunT(params, ret) => {
          if (params.zip(args.map(a => getType(a, env))).filter(z => z._1 == z._2).size == Math.max(params.length, args.length))
            ret
          else throw Illtyped
        }
        case BoolT => BoolT
        case NumT  => NumT
        case _     => throw Illtyped
      }

      case If(e1, e2, e3) => {
        getType(e1, env) match {
          case BoolT => if (getType(e2, env).equals(getType(e3, env))) getType(e2, env) else throw Illtyped
          case _     => throw Illtyped
        }
      }

      case Let(x, e1, e2) => {
        val t1 = getType(e1, env)
        getType(e2, env ++ (Seq(x -> t1)))
      }

      case Rec(x, t1, e1, e2) => {
        if (getType(e1, env ++ (Seq(x -> t1))) != t1) throw Illtyped
        getType(e2, env ++ (Seq(x -> t1)))
      }

      case Record(fields) => {
        RcdT(fields.map(f => (f._1, getType(f._2, env))))
      }

      case Access(e, field) => {
        getType(e, env) match {
          case RcdT(fields) => fields.getOrElse(field, throw Illtyped)
          case _            => throw Illtyped
        }
      }

      case Construct(constructor, typs, e) => {
        // 1. Get the type's name from constructor's name: Left => Either
        val name = typename(constructor)

        // 2. Get what this unionType's type parameter declaration: Either[A,B] => (A,B) 
        val tvars = typeDefs.find { x => x.name.equals(name) }.getOrElse(throw Illtyped).tvars;

        // 3. Check if typs parameters length match the tvars in type definition
        if (tvars.length != typs.length) throw Illtyped

        // 4. Zip tvar to typs
        //        val zipedTvarsToTyps = tvars.zip(typs).toMap

        // 5. Evaluate e under current env
        val eType = getType(e, env)

        // 6. Check if this e type matches constructor's type
        if (!eType.equals(constructorType(constructor, typs))) throw Illtyped

        TypT(name, typs)

      }

      case Match(e, cases) => {
        // 1. Get expression's type, should already substitute type: Bar<num>(3) => TypT(Foo, (Num))
        val eType = getType(e, env)

        // 2. Make sure this exp is a user defined type, and get it's label
        // don't care about the type in each constructor
        val tType = eType match {
          case t @ TypT(label, _) => t
          case _                  => throw Illtyped
        }

        // 3. Get the typs(after substituion) associated with this eType
        val typs = tType.typs;

        // 4. For this user defined type, get the typeVars associated with it
        val tvars = typeDefs.find { x => x.name.equals(tType.name) }.getOrElse(throw Illtyped).tvars

        // 5. For this user defined type, get all constructor's label
        val consLabels = constructors(tType.name)

        // 6. If cases does not match all constructors, illtyped
        if (consLabels.size != cases.size) throw Illtyped
        cases.foreach(c => { if (!consLabels.contains(c._1)) throw Illtyped })

        // 7. Each case has a constructor label and from that label we can know the type of value constructor holds by calling 
        // constructorType(label, typs), typs are the solid type. Then we evaluate each case's return type 
        val list = cases.map(c => getType(c._3, env ++ Seq((c._2 -> constructorType(c._1, typs)))))

        // 8.Check if all of them are the same
        if (!list.forall { t => t.equals(list(0)) }) throw Illtyped
        list(0)
      }

      case TAbs(tvars, fun) => {
        getType(fun, env) match {
          case f: FunT => TFunT(tvars, f)
          case _       => throw Illtyped
        }
      }

      case TApp(e, typs) => {
        getType(e, env) match {
          case TFunT(tvars, f) => {
            if (tvars.length != typs.length) throw Illtyped
            replace(f, tvars.zip(typs).toMap)
          }
          case _ => throw Illtyped
        }
      }
    }
}
