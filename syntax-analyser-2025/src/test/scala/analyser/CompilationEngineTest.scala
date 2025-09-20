package analyser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CompilationEngineTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  private def k(s: String) = Keyword(s)

  private def sym(c: Char) = Symbol(c)

  private def id(s: String) = Identifier(s)

  private def int(n: Int) = IntConst(n)

  private def str(s: String) = StringConst(s)

  object StatementsHelper {
    val doFooDotBarString = "do foo . bar ( ) ;"
    val letCountEqual5String = "let count = 5 ;"
    val returnCountString = "return count ;"
    val whileTrueDoFooDotBarString = s"while ( true ) { $doFooDotBarString }"
    val ifTrueDoFooDotBarString = s"if ( true ) { ${doFooDotBarString} }"
    private val doFooDotBarTokens = wrapInTag(List(k("do"), id("foo"), sym('.'), id("bar"), sym('('), StartElem("expressionList"), EndElem("expressionList"), sym(')'), sym(';')), "doStatement")
    val statementsToTokens: Map[String, List[LexicalElem]] = Map(
      doFooDotBarString -> doFooDotBarTokens,
      letCountEqual5String -> wrapInTag(List(k("let"), id("count"), sym('=')) ++ wrapInDoubleTag("expression", "term", int(5)) :+ sym(';'), "letStatement"),
      returnCountString -> wrapInTag(List(k("return"), id("count"), sym(';')), "returnStatement"),
      whileTrueDoFooDotBarString -> wrapInTag(List(k("while")) ++ wrapBracket(wrapInDoubleTag("expression", "term", k("true"))) ++ wrapCurly(wrapInTag(doFooDotBarTokens, "statements")), "whileStatement"),
      ifTrueDoFooDotBarString -> wrapInTag(List(k("if"), Symbol('('), StartElem("expression"), StartElem("term"), k("true"), EndElem("term"), EndElem("expression"), Symbol(')')) ++ wrapCurly(wrapInTag(doFooDotBarTokens, "statements")), "ifStatement")
    )

  }

  def wrapInTag(elems: List[LexicalElem], statementType: String): List[LexicalElem] =
    StartElem(statementType) +: (elems :+ EndElem(statementType))

  def wrapInTag(elem: LexicalElem, statementType: String): List[LexicalElem] = wrapInTag(List(elem), statementType)

  val validLetStatements = Table(("statement", "expected Tokens"),
    ("let count = count ;", wrapInTag(List(k("let"), id("count"), sym('=')) ++ wrapInDoubleTag("expression", "term", id("count")) :+ sym(';'), "letStatement")),
    (StatementsHelper.letCountEqual5String, StatementsHelper.statementsToTokens(StatementsHelper.letCountEqual5String)),
    ("let count = 500 ;", wrapInTag(List(k("let"), id("count"), sym('=')) ++ wrapInDoubleTag("expression", "term", int(500)) :+ sym(';'), "letStatement")),
    ("let count = true ;", wrapInTag(List(k("let"), id("count"), sym('=')) ++ wrapInDoubleTag("expression", "term", k("true")) :+ sym(';'), "letStatement")),
    ("let count = \"hi\" ;", wrapInTag(List(k("let"), id("count"), sym('=')) ++ wrapInDoubleTag("expression", "term", str("hi")) :+ sym(';'), "letStatement")),
    ("let another_count = count ;", wrapInTag(List(k("let"), id("another_count"), sym('=')) ++ wrapInDoubleTag("expression", "term", id("count")) :+ sym(';'), "letStatement")),
    ("let myArray [ 5 ] = 10 ;", List(StartElem("letStatement"), k("let"), id("myArray"), sym('[')) ++ wrapInDoubleTag("expression", "term", int(5)) ++ List(sym(']'), sym('=')) ++ wrapInDoubleTag("expression", "term", int(10)) ++ List(sym(';'), EndElem("letStatement")))
  )

  //TODO more complex
  //other compile Lets
  //"let count = myArray[5] ;"
  //"let count = ( 4 + 5 ) * 10 ;"
  //"let count = ~ ~ true ;"
  //"let count = ~ true ;"
  //"let count = Foo.bar() ;"
  //"let count = Foo.bar(expressionList) ;" -expand with more examples
  //"let count = subroutineName('expressionList') ;" - expand with more examples
  //"let count = another_count * 5 ;"

  "compileLet" should "compile a valid let statement" in {
    forAll(validLetStatements) { case (statementString, expectedTokens) =>
      val tokeniser = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileLet(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid let statement" in {
    val invalidLets = Table(
      "invalid statement",
      "let count ;",
      "let 5 = 10 ;",
      "let foo = bar",
      "let foo * bar ; ",
      "let",
      "let ;",
      "let foo = ;",
      "let foo = ;"
    )

    forAll(invalidLets) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement ++ " rest of programme")
      CompilationEngine.compileLet(tokeniser).isLeft shouldBe true
    }
  }

  private def wrapInDoubleTag(outer: String, inner: String, elems: LexicalElem*): List[LexicalElem] =
    wrapInTag(wrapInTag(elems.toList, inner), outer)

  private def wrapInDoubleTag(outer: String, inner: String, elems: List[LexicalElem]): List[LexicalElem] =
    wrapInTag(wrapInTag(elems, inner), outer)


  "compileDo" should "compile a do statement" in {
    import StatementsHelper.statementsToTokens
    import StatementsHelper.doFooDotBarString

    val validDoStatements = Table(
      ("statement", "expectedTokens"),
      ("do subroutineName ( ) ;", wrapInTag(List(k("do"), id("subroutineName"), sym('('), StartElem("expressionList"), EndElem("expressionList"), sym(')'), sym(';')), "doStatement")),
      (doFooDotBarString, statementsToTokens(doFooDotBarString)),
      ("do subroutineName ( 5 ) ;", wrapInTag(List(k("do"), id("subroutineName"), sym('(')) ++ wrapInTag(wrapInDoubleTag("expression", "term", int(5)), "expressionList") ++ List(sym(')'), sym(';')), "doStatement")),
      ("do subroutineName ( 5 , 4 ) ;", wrapInTag(List(k("do"), id("subroutineName"), sym('(')) ++ wrapInTag(wrapInDoubleTag("expression", "term", int(5)) ++ List(sym(',')) ++ wrapInDoubleTag("expression", "term", int(4)), "expressionList") ++ List(sym(')'), sym(';')), "doStatement")),
      ("do foo . bar ( \"hi\" , myVar ) ;", wrapInTag(List(k("do"), id("foo"), sym('.'), id("bar"), sym('(')) ++ wrapInTag(wrapInDoubleTag("expression", "term", str("hi")) ++ List(sym(',')) ++ wrapInDoubleTag("expression", "term", id("myVar")), "expressionList") ++ List(sym(')'), sym(';')), "doStatement"))
    )
    //TODO more complex expression lists (although this should be handled by implementing expressions

    forAll(validDoStatements) { case (statement, expectedTokens) =>
      val tokeniser = testTokeniser(statement + " rest of programme")
      CompilationEngine.compileDo(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for invalid do statements" in {
    val invalidDoStatements = Table(
      "invalid statement",
      "do ;",
      "do foo ;",
      "do foo . ;",
      "do 5 . bar ( ) ;",
      "do foo . bar ( ;",
      "do foo . bar ( )",
      "do this . that ( ) ;" //this is not an identifier
    )

    forAll(invalidDoStatements) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileDo(tokeniser).isLeft shouldBe true
    }
  }

  "compile while" should "compile a valid while statement" in {
    import StatementsHelper.*
    val doFooDotBarTokens = statementsToTokens(doFooDotBarString)
    val validWhileStatements = Table(
      ("validStatements", "expectedTokens"),
      (whileTrueDoFooDotBarString, statementsToTokens(whileTrueDoFooDotBarString)),
      (s"while ( true ) { }", wrapInTag((k("while") +: wrapBracket(wrapInDoubleTag("expression", "term", k("true")))) ++ wrapCurly(wrapInTag(List(), "statements")), "whileStatement")),
      (s"while ( true ) { $doFooDotBarString $doFooDotBarString }", wrapInTag((k("while") +: wrapBracket(wrapInDoubleTag("expression", "term", k("true")))) ++ wrapCurly(wrapInTag(doFooDotBarTokens ++ doFooDotBarTokens, "statements")), "whileStatement"))
      //TODO more whiles - not super necessary though
    )

    forAll(validWhileStatements) { case (validStatement, expectedTokens) =>
      val tokeniser = testTokeniser(validStatement + " rest of programme")
      CompilationEngine.compileWhile(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid while statement" in {
    val invalidDoStatements = Table(
      "invalid statement",
      "while ;",
      "while true { do foo . bar () ; }",
      "while ( true ) do foo . bar () ; }"
    )

    forAll(invalidDoStatements) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileWhile(tokeniser).isLeft shouldBe true
    }
  }

  "compile if" should "compile a valid if statement" in {
    import StatementsHelper.*

    val validIfStatements = Table(
      ("validStatement", "expectedTokens"),
      (ifTrueDoFooDotBarString, statementsToTokens(ifTrueDoFooDotBarString)),
      (s"if ( true ) { $doFooDotBarString } else { ${doFooDotBarString} }", wrapInTag(List(k("if"), Symbol('('), StartElem("expression"), StartElem("term"), k("true"), EndElem("term"), EndElem("expression"), Symbol(')')) ++ wrapCurly(wrapInTag(statementsToTokens(doFooDotBarString), "statements")) ++ List(k("else")) ++ wrapCurly(wrapInTag(statementsToTokens(doFooDotBarString), "statements")), "ifStatement")),
      //TODO more - not super necessary
    )

    forAll(validIfStatements) { case (validStatement, expectedTokens) =>
      val tokeniser = testTokeniser(validStatement + " rest of programme")
      CompilationEngine.compileIf(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid if statement" in {
    val invalidDoStatements = Table(
      "invalid statement",
      "if ;",
      "if true { do foo . bar () ; }",
      "if ( true ) do foo . bar () ; }"
      //TODO more
    )

    forAll(invalidDoStatements) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileIf(tokeniser).isLeft shouldBe true
    }
  }

  "compile return" should "compile a valid return statement" in {
    val validReturns = Table(
      ("statement", "expectedTokens"),
      ("return ;", wrapInTag(List(k("return"), sym(';')), "returnStatement")),
      ("return 5 ;", wrapInTag(List(k("return"), StartElem("expression"), StartElem("term"), int(5), EndElem("term"), EndElem("expression"), sym(';')), "returnStatement"))
      //TODO more complex expressions (to be handled once expressions fully implemented)
    )

    forAll(validReturns) { case (statement, expectedTokens) =>
      val tokeniser = testTokeniser(statement + " rest of programme")
      CompilationEngine.compileReturn(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid return statement" in {
    val invalidReturns = Table(
      "invalidStatement",
      "retrn ;",
      "return",
      "return class ;",
      "return 4 ~ 5 ;"
      //TODO more complex expressions (to be handled once expressions fully implemented)
    )

    forAll(invalidReturns) { invalidStatement =>
      val tokeniser = testTokeniser(invalidStatement + " rest of programme")
      CompilationEngine.compileReturn(tokeniser).isLeft shouldBe true
    }
  }

  val validVarDecs = (varDecTypeString: String, enclosingElem: String) => Table(
    ("statementString", "expectedTokens"),
    (s"$varDecTypeString boolean myBool ;", List(StartElem(enclosingElem), k(varDecTypeString), k("boolean"), id("myBool"), sym(';'), EndElem(enclosingElem))),
    (s"$varDecTypeString boolean myBool , mySecondBool ;", List(StartElem(enclosingElem), k(varDecTypeString), k("boolean"), id("myBool"), sym(','), id("mySecondBool"), sym(';'), EndElem(enclosingElem))),
    (s"$varDecTypeString boolean myBool , mySecondBool , myThirdBool ;", List(StartElem(enclosingElem), k(varDecTypeString), k("boolean"), id("myBool"), sym(','), id("mySecondBool"), sym(','), id("myThirdBool"), sym(';'), EndElem(enclosingElem))),
    (s"$varDecTypeString int myNumber ;", List(StartElem(enclosingElem), k(varDecTypeString), k("int"), id("myNumber"), sym(';'), EndElem(enclosingElem))),
    (s"$varDecTypeString char myChar ;", List(StartElem(enclosingElem), k(varDecTypeString), k("char"), id("myChar"), sym(';'), EndElem(enclosingElem))),
    (s"$varDecTypeString myClass myClassInstance ;", List(StartElem(enclosingElem), k(varDecTypeString), id("myClass"), id("myClassInstance"), sym(';'), EndElem(enclosingElem)))
  )

  val invalidVarDecs = (varDecString: String) => Table(
    "invalidstatementString",
    s"$varDecString boolean myBool", //no closing ;
    s"$varDecString boolean ;", //no vars ;
    s"$varDecString boolean myBool , mySecondBool ", //no closing ;
    s"$varDecString boolean myBool , mySecondBool , ;", //dangling comma
    s"$varDecString int 5 ;", //not a var name
    s"$varDecString this myChar ;", //not a valid type (reserved keyword)
    "staticField boolean notAProperFieldName ; ", //neither $varDecString nor field
    s"$varDecString 5 int ;", //5 not a valid identifier
    s"$varDecString", //variants of dangling statements
    s"$varDecString boolean", //variants of dangling statements
    s"$varDecString boolean myBool + myOtherBool ;" //invalid joining char
  )

  "compileClassVarDec" should "compile a valid class var for a static var" in {
    forAll(validVarDecs("static", "classVarDec")) { case (statementString, expectedTokens) =>
      val t = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileClassVarDec(t) shouldBe Right(expectedTokens)
    }
  }

  it should "compile a valid class var for a field var" in {
    forAll(validVarDecs("field", "classVarDec")) { case (statementString, expectedTokens) =>
      val t = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileClassVarDec(t) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for an invalid class var dec" in {
    forAll(invalidVarDecs("static") ++ invalidVarDecs("field")) { invalidString =>
      val t = testTokeniser(invalidString ++ " rest of programme")
      CompilationEngine.compileClassVarDec(t).isLeft shouldBe true
    }
  }

  "compileVarDec" should "compile a valid var dec" in {
    forAll(validVarDecs("var", "varDec")) { case (statementString, expectedTokens) =>
      val t = testTokeniser(statementString ++ " rest of programme")
      CompilationEngine.compileVarDec(t) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for invalid var decs" in {
    forAll(invalidVarDecs("var")) { case invalidVarDecString =>
      val t = testTokeniser(invalidVarDecString ++ " rest of programme")
      CompilationEngine.compileVarDec(t).isLeft shouldBe true
    }
  }

  "compileParameterList" should "compile a valid parameter list" in {
    val validParameterLists = Table(
      ("validStatement", "expectedTokens"),
      (")", wrapInTag(List(), "parameterList")),
      ("boolean myBool", wrapInTag(List(k("boolean"), id("myBool")), "parameterList")),
      ("boolean myBool , int myInt , className myClass , char myChar", wrapInTag(List(k("boolean"), id("myBool"), sym(','), k("int"), id("myInt"), sym(','), id("className"), id("myClass"), sym(','), k("char"), id("myChar")), "parameterList"))
    )

    forAll(validParameterLists) { case (statementString, expectedTokens) =>
      val closingChar = ")"
      val t = testTokeniser(statementString ++ s" $closingChar")
      CompilationEngine.compileParameterList(t, closingChar) shouldBe Right(expectedTokens)
    }
  }

  it should "return a left for invalid parameter lists" in {
    val invalidParameterList = Table(
      "invalidParam",
      "this foo",
      "foo , bar",
      "boolean foo , this bar",
      "boolean myBool anotherBool"
    )
    forAll(invalidParameterList) { case invalidParameterListString =>
      val closingChar = ")"
      val t = testTokeniser(invalidParameterListString ++ s"$closingChar)")
      CompilationEngine.compileParameterList(t, closingChar).isLeft shouldBe true
    }
  }

  "compileSubroutineBody" should "compile a valid subroutine body" in {
    val myBoolDeclaration = "var boolean myBool ;"
    val myBoolDeclarationTokens = List(StartElem("varDec"), k("var"), k("boolean"), id("myBool"), sym(';'), EndElem("varDec"))
    val charDeclarations = "var char charA , charB ;"
    val charDeclarationTokens = List(StartElem("varDec"), k("var"), k("char"), id("charA"), sym(','), id("charB"), sym(';'), EndElem("varDec"))
    val letFooEqualStatement = "let foo = myBool ;"
    val letFooEqualTokens = wrapInTag(List(k("let"), id("foo"), sym('='), StartElem("expression"), StartElem("term"), id("myBool"), EndElem("term"), EndElem("expression"), sym(';')), "letStatement")
    val returnStatement = "return foo ;"
    val returnTokens = wrapInTag(List(k("return"), StartElem("expression"), StartElem("term"), id("foo"), EndElem("term"), EndElem("expression"), sym(';')), "returnStatement")
    val validSubroutineBodies = Table(
      ("validStatement", "expectedTokens"),
      (
        s"{ $myBoolDeclaration $letFooEqualStatement }",
        wrapInTag(wrapCurly(myBoolDeclarationTokens ++ wrapInTag(letFooEqualTokens, "statements")), "subroutineBody")
      ),
      (
        s"{ $myBoolDeclaration $charDeclarations $letFooEqualStatement }",
        wrapInTag(wrapCurly(myBoolDeclarationTokens ++ charDeclarationTokens ++ wrapInTag(letFooEqualTokens, "statements")), "subroutineBody")
      ),
      (
        s"{ $letFooEqualStatement $returnStatement }",
        wrapInTag(wrapCurly(wrapInTag(letFooEqualTokens ++ returnTokens, "statements")), "subroutineBody")
      )
    )

    forAll(validSubroutineBodies) { case (validStatement, expectedTokens) =>
      val tokeniser = testTokeniser(validStatement ++ " rest of programme")
      CompilationEngine.compileSubroutineBody(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  object SubroutineHelper {
    val functionDec = "function int returnV1 ( int v1 , int v2 ) { return v1 ; }"
    val functionDecTokens = wrapInTag(List(k("function"), k("int"), id("returnV1"), sym('('), StartElem("parameterList"), k("int"), id("v1"), sym(','), k("int"), id("v2"), EndElem("parameterList"), sym(')'), StartElem("subroutineBody"), sym('{'), StartElem("statements"), StartElem("returnStatement"), k("return"), StartElem("expression"), StartElem("term"), id("v1"), EndElem("term"), EndElem("expression"), sym(';'), EndElem("returnStatement"), EndElem("statements"), sym('}'), EndElem("subroutineBody")), "subroutineDec")
    val methodDec = "method void returnTrue ( String string , int i ) { return true ; }"
    val methodDecTokens = wrapInTag(List(k("method"), k("void"), id("returnTrue"), sym('('), StartElem("parameterList"), id("String"), id("string"), sym(','), k("int"), id("i"), EndElem("parameterList"), sym(')'), StartElem("subroutineBody"), sym('{'), StartElem("statements"), StartElem("returnStatement"), k("return"), StartElem("expression"), StartElem("term"), k("true"), EndElem("term"), EndElem("expression"), sym(';'), EndElem("returnStatement"), EndElem("statements"), sym('}'), EndElem("subroutineBody")), "subroutineDec")
    val constructorDec = "constructor myClass create ( String string ) { return myClassInstance ; }"
    val constructorTokens = wrapInTag(List(k("constructor"), id("myClass"), id("create"), sym('('), StartElem("parameterList"), id("String"), id("string"), EndElem("parameterList"), sym(')'), StartElem("subroutineBody"), sym('{'), StartElem("statements"), StartElem("returnStatement"), k("return"), StartElem("expression"), StartElem("term"), id("myClassInstance"), EndElem("term"), EndElem("expression"), sym(';'), EndElem("returnStatement"), EndElem("statements"), sym('}'), EndElem("subroutineBody")), "subroutineDec")
  }

  "compileSubroutine" should "compile a valid subroutine" in {
    import SubroutineHelper.*
    val validSubroutines = Table(
      ("validString", "expectedTokens"),
      (functionDec, functionDecTokens),
      (methodDec, methodDecTokens),
      (constructorDec, constructorTokens)
    )

    forAll(validSubroutines) { case (validString, expectedTokens) =>
      val tokeniser = testTokeniser(validString ++ " rest of input")
      CompilationEngine.compileSubroutine(tokeniser) shouldBe Right(expectedTokens)
    }


    //TODO more examples
  }

  //TODO
  "compile class" should "compile a valid class" in {
    import SubroutineHelper.*
    val classVarDecs = "static int foo ; field char charA , charB ;"
    val startClassVarDec = StartElem("classVarDec")
    val endClassVarDec = EndElem("classVarDec")
    val classVarDecTokens = List(startClassVarDec, k("static"), k("int"), id("foo"), sym(';'), endClassVarDec, startClassVarDec, k("field"), k("char"), id("charA"), sym(','), id("charB"), sym(';'), endClassVarDec)
    val validClassString =
      s"class myClass { $classVarDecs $functionDec $methodDec }"

    val tokeniser = testTokeniser(validClassString)
    CompilationEngine.compileClass(tokeniser) shouldBe Right(wrapInTag(List(k("class"), id("myClass")) ++ wrapCurly(classVarDecTokens ++ SubroutineHelper.functionDecTokens ++ SubroutineHelper.methodDecTokens), "class"))
  }

  "compileExpression" should "compile an expression with a single term" in {
    val singleExpressionTerm5 = wrapInDoubleTag("expression", "term", int(5))
    val expressions = Table(
      ("expressionString", "expectedTokens"),
      ("5", wrapInDoubleTag("expression", "term", int(5))),
      ("\"hi\"", wrapInDoubleTag("expression", "term", str("hi"))),
      ("myVar", wrapInDoubleTag("expression", "term", id("myVar"))),
      ("true", wrapInDoubleTag("expression", "term", k("true"))),
      ("foo ( 5 )", wrapInDoubleTag("expression", "term", List(id("foo"), sym('(')) ++ wrapInTag(singleExpressionTerm5, "expressionList") ++ List(sym(')')))),
      ("foo . bar ( 5 )", wrapInDoubleTag("expression", "term", List(id("foo"), sym('.'), id("bar"), sym('(')) ++ wrapInTag(singleExpressionTerm5, "expressionList") ++ List(sym(')')))),
      ("foo [ 5 ]", wrapInDoubleTag("expression", "term", List(id("foo"), sym('[')) ++ singleExpressionTerm5 :+ sym(']'))),
      ("- 5", wrapInDoubleTag("expression", "term", List(sym('-')) ++ wrapInTag(int(5), "term")))
    )

    forAll(expressions) { case (validString, expectedTokens) =>
      val tokeniser = testTokeniser(validString ++ " rest of input")
      CompilationEngine.compileExpression(tokeniser) shouldBe Right(expectedTokens)
    }
  }

  it should "compile a term op term expression" in {
    val expressionString = "foo + 5"
    val expectedElems = wrapInTag(wrapInTag(id("foo"), "term") ++ List(sym('+')) ++ wrapInTag(int(5), "term"), "expression")

    val tokeniser = testTokeniser(expressionString ++ " rest of input")
    CompilationEngine.compileExpression(tokeniser) shouldBe Right(expectedElems)
  }

  it should "compile an expression in brackets" in {
    val string = "~ ( key = 0 )"
    val expectedElems = List(StartElem("expression"),
      StartElem("term"),
      Symbol('~'),
      StartElem("term"),
      Symbol('('),
      StartElem("expression"),
      StartElem("term"),
      id("key"),
      EndElem("term"),
      Symbol('='),
      StartElem("term"),
      int(0),
      EndElem("term"),
      EndElem("expression"),
      Symbol(')'),
      EndElem("term"),
      EndElem("term"),
      EndElem("expression"))

    val tokeniser = testTokeniser(string + " rest of programme")
    CompilationEngine.compileExpression(tokeniser) shouldBe Right(expectedElems)
  }

  it should "compile a complex expression" in {
    val string = "foo . bar ( 5 * 4 , baz ( true ) )"
    val methodCallTokens = List(id("foo"), sym('.'), id("bar"))

    val term5 = wrapInTag(int(5), "term")
    val term4 = wrapInTag(int(4), "term")
    val firstParamExpressionTokens = wrapInTag(term5 ++ List(sym('*')) ++ term4, "expression")

    val trueTerm = wrapInDoubleTag("expression", "term", k("true"))
    val secondMethodCall = List(id("baz"), sym('('), StartElem("expressionList")) ++ trueTerm ++ List(EndElem("expressionList"), sym(')'))
    val secondParamExpressionTokens = wrapInDoubleTag("expression", "term", secondMethodCall)

    val expected = wrapInDoubleTag("expression", "term", methodCallTokens ++ wrapBracket(wrapInTag(firstParamExpressionTokens ++ List(sym(',')) ++ secondParamExpressionTokens, "expressionList")))
  }

  it should "return a left for an invalid expression" in {
    val invalidExpressions = Table(
      ("invalid expression string"),
      ("function")
    )

    forAll(invalidExpressions) { invalidExpressionString =>
      val tokeniser = testTokeniser(invalidExpressionString + " rest of programme")
      CompilationEngine.compileExpression(tokeniser).isLeft shouldBe true
    }
  }


  private def wrapCurly(s: String) = if (s.nonEmpty) "{ " + s + " }" else "{ }"

  private def wrapCurly(l: List[LexicalElem]) = (sym('{') +: l) :+ sym('}')

  private def wrapCurly(elems: LexicalElem*): List[LexicalElem] = (sym('{') +: elems.toList) :+ sym('}')

  private def wrapBracket(s: String) = if (s.nonEmpty) "( " + s + " )" else "( )"

  private def wrapBracket(l: List[LexicalElem]) = (sym('(') +: l) :+ sym(')')

  private def wrapBracket(elems: LexicalElem*): List[LexicalElem] = (sym('(') +: elems.toList) :+ sym(')')

  class FakeTokeniser(var tokens: List[String]) extends Tokeniser {
    override def advance(): Unit = tokens = tokens.tail

    def allTokens: Seq[String] = tokens.tail

    override def currentToken: String = tokens.head

    override def hasMoreTokens: Boolean = tokens.tail.nonEmpty

    override def safeAdvance: Either[String, Unit] =
      if (this.hasMoreTokens)
        this.advance()
        Right(())
      else Left("unexpected end of input")
  }

  def testTokeniser(l: List[String]) = new FakeTokeniser(l)

  def testTokeniser(spaceSeparatedString: String) = new FakeTokeniser(spaceSeparatedString.split(" ").toList)
}
