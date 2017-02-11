import scala.io._
import cs162.assign3.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main(args: Array[String]) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          getType(program.e, new TypeEnv())
          println(Pretty.prettySyntax(program))
          println("This program is well-typed:\n")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }
  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following underneath the given arguments:
  //
  // constructors(Label("Either")) = Map(Label("Left") -> NumT, Label("Right") -> BoolT)
  // constructors(Label("Maybe")) = Map(Label("Some") -> NumT, Label("None") -> UnitT)
  // constructors(Label("Fake")) throws Illtyped
  //
  // TypeDef(label, Map<label, type>)
  // Get all the constructors with label name
  def constructors(name: Label): Map[Label, Type] =
    typeDefs.find(_.name == name).map(_.constructors).getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  // Get the parent type of the constructor with label
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  def getType(e: Exp, env: TypeEnv): Type =
    e match {
      // variables
      case x: Var                        => env.getOrElse(x, throw Illtyped)

      // numeric literals
      case _: Num                        => NumT

      // boolean literals
      case _: Bool                       => BoolT

      // `nil` - the literal for unit
      case _: NilExp                     => UnitT

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => NumT // FILL ME IN

      // builtin relational operators
      case LT | EQ                       => BoolT // FILL ME IN

      // builtin logical operators
      case And | Or                      => BoolT // FILL ME IN

      // builtin logical operators
      case Not                           => BoolT // FILL ME IN

      // function creation ->I
      // Return a function type
      case Fun(params, body)             => FunT(params.map(a => a._2), getType(body, env ++ params))

      // function call
      // ->E
      // 1. Check if functions and params are defined
      // 1. Check type mapping between args and parameters(length and types), throw exception if not match
      // 2. return the fun's return type if parameter matches args
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

      // conditionals 
      // 1. e1 could be bool or a var
      //   2. If it's bool check two return type matches otherwise throw exception
      //   3. If it's var, get the var's type and do same as 2
      case If(e1, e2, e3) => {
        getType(e1, env) match {
          case BoolT => if (getType(e2, env).equals(getType(e3, env))) getType(e2, env) else throw Illtyped
          case _     => throw Illtyped
        }
      }

      // let binding
      // If we know that e1 has type t1, we add x -> t1 mapping
      // the env and try to getType(e2..) using this env, return the 
      // type we got or throw exceptions during these type getType functions
      case Let(x, e1, e2) => {
        val t1 = getType(e1, env)
        getType(e2, env ++ (Seq(x -> t1)))
      }

      // recursive binding
      // x appears in e1
      case Rec(x, t1, e1, e2) => {
        if (getType(e1, env ++ (Seq(x -> t1))) != t1) throw Illtyped
        getType(e2, env ++ (Seq(x -> t1)))
      }

      // record literals
      case Record(fields) => {
        RcdT(fields.map(f => (f._1, getType(f._2, env))))
      }

      // record access
      case Access(e, field) => {
        getType(e, env) match {
          case RcdT(fields) => fields.getOrElse(field, throw Illtyped)
          case _            => throw Illtyped
        }
      }

      // constructor use
      // TypeDef(label, Map<label, type>)
      // 1. Get the union type name of constructor, Right => Either
      // 2. Get union type's child types Either=> Right->NumT, Left->BoolT
      // 3. Evaluate the exp to a type e => Num
      // 4. Go through child types, find what type does child type need as parameter  Right=>NumT
      // 5. Check if the result of 4 match e's type e ?= NumT
      // Return type of the union type
      case Construct(constructor, e) => {
        val unionTypeName = typename(constructor)
        val unionTypeDef = constructors(unionTypeName)
        val expType = getType(e, env)
        val constructorType = unionTypeDef.getOrElse(constructor, throw Illtyped)
        if (expType.equals(constructorType)) TypT(unionTypeName) else throw Illtyped
      }

      // pattern matching (case ... of ...)
      case Match(e, cases) => {
        // e should be of some kinds of TypT(label)
        val eType = getType(e, env)
        val eTypeLabel = eType match {
          case TypT(label) => label
          case _           => throw Illtyped
        }
        val eTypeConstructors = constructors(eTypeLabel)
        // Check if constructors number matches passed in cases
        // and if var types in cases matches types in constructors
        if (eTypeConstructors.size != cases.size) throw Illtyped
        //        if(cases.filter(c=> c._2)
        //  Check each contained type in eTypeConstructors has been covered by cases
        cases.foreach(c => { eTypeConstructors.getOrElse(c._1, throw Illtyped) })

        // Check if all cases return the same t
        // Here var should get it's type from eTypeConstructors from typedef since it's the newly created variable that will
        // be used in case expression evaluation
        val list = cases.map(c => getType(c._3, env ++ Seq((c._2 -> eTypeConstructors.getOrElse(c._1, throw Illtyped)))))
        if (!list.forall { t => t.equals(list(0)) }) throw Illtyped
        list(0)
      }
    }
}
