
#Region Constants

Var Keywords;                  // enum
Var Tokens;                    // enum
Var SelectorKinds;             // enum
Var Directives;                // enum
Var PrepInstructions;          // enum
Var UnaryOperators;            // array (one of Tokens)
Var BasicLiteralsExceptString; // array (one of Tokens)
Var RelationalOperators;       // array (one of Tokens)
Var InitialTokensOfExpression; // array (one of Tokens)
Var EmptyArray;                // array

#EndRegion // Constants

#Region Cache

Var StructureKeysCache; // structure as map[string](string)

#EndRegion // Cache

#Region Init

Procedure Init() Export

	StructureKeysCache = New Structure;

	InitEnums();

	UnaryOperators = New Array;
	UnaryOperators.Add(Tokens.Add);
	UnaryOperators.Add(Tokens.Sub);

	BasicLiteralsExceptString = New Array;
	BasicLiteralsExceptString.Add(Tokens.Number);
	BasicLiteralsExceptString.Add(Tokens.DateTime);
	BasicLiteralsExceptString.Add(Tokens.True);
	BasicLiteralsExceptString.Add(Tokens.False);
	BasicLiteralsExceptString.Add(Tokens.Undefined);

	RelationalOperators = New Array;
	RelationalOperators.Add(Tokens.Eql);
	RelationalOperators.Add(Tokens.Neq);
	RelationalOperators.Add(Tokens.Lss);
	RelationalOperators.Add(Tokens.Gtr);
	RelationalOperators.Add(Tokens.Leq);
	RelationalOperators.Add(Tokens.Geq);

	InitialTokensOfExpression = New Array;
	InitialTokensOfExpression.Add(Tokens.Add);
	InitialTokensOfExpression.Add(Tokens.Sub);
	InitialTokensOfExpression.Add(Tokens.Not);
	InitialTokensOfExpression.Add(Tokens.Ident);
	InitialTokensOfExpression.Add(Tokens.Lparen);
	InitialTokensOfExpression.Add(Tokens.Number);
	InitialTokensOfExpression.Add(Tokens.String);
	InitialTokensOfExpression.Add(Tokens.StringBeg);
	InitialTokensOfExpression.Add(Tokens.DateTime);
	InitialTokensOfExpression.Add(Tokens.Ternary);
	InitialTokensOfExpression.Add(Tokens.New);
	InitialTokensOfExpression.Add(Tokens.True);
	InitialTokensOfExpression.Add(Tokens.False);
	InitialTokensOfExpression.Add(Tokens.Undefined);

	EmptyArray = New Array;

EndProcedure // Init()

Procedure InitEnums()
	Keywords = Keywords();
	Tokens = Tokens(Keywords);
	SelectorKinds = SelectorKinds();
	Directives = Directives();
	PrepInstructions = PrepInstructions();
EndProcedure // InitEnums()

#EndRegion // Init

#Region Enums

Function Keywords() Export
	Var Keywords;

	Keywords = Enum(New Structure,
		"If.Если, Then.Тогда, ElsIf.ИначеЕсли, Else.Иначе, EndIf.КонецЕсли,
		|For.Для, Each.Каждого, In.Из, To.По, While.Пока, Do.Цикл, EndDo.КонецЦикла,
		|Procedure.Процедура, EndProcedure.КонецПроцедуры, Function.Функция, EndFunction.КонецФункции,
		|Var.Перем, Val.Знач, Return.Возврат, Continue.Продолжить, Break.Прервать,
		|And.И, Or.Или, Not.Не,
		|Try.Попытка, Except.Исключение, Raise.ВызватьИсключение, EndTry.КонецПопытки,
		|New.Новый, Execute.Выполнить, Export.Экспорт, Goto.Перейти,
		|True.Истина, False.Ложь, Undefined.Неопределено,"
	);

	Return Keywords;
EndFunction // Keywords()

Function Tokens(Keywords = Undefined) Export
	Var Tokens;

	If Keywords = Undefined Then
		Keywords = Keywords();
	EndIf;

	Tokens = Enum(New Structure(Keywords),

		// Literals

		"Ident, Number, String, DateTime,
		// parts of strings
		|StringBeg, StringMid, StringEnd,

		// Operators

		// =   <>    <    >   <=   >=    +    -    *    /    %
		|Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod,
		//    (       )       [       ]
		|Lparen, Rparen, Lbrack, Rbrack,
		//     ?      ,       .      :          ;
		|Ternary, Comma, Period, Colon, Semicolon,

		// Preprocessor instructions
		|_If, _ElsIf, _Else, _EndIf, _Region, _EndRegion,

		// Other

		//         //          &      ~
		|Eof, Comment, Directive, Label"

	);

	Return Tokens;
EndFunction // Tokens()

Function SelectorKinds() Export
	Var SelectorKinds;

	SelectorKinds = Enum(New Structure,
		"Ident," // Something._
		"Index," // Something[_]
		"Call,"  // Something(_)
	);

	Return SelectorKinds;
EndFunction // SelectorKinds()

Function Directives() Export
	Var Directives;

	Directives = Enum(New Structure,
		"AtClient.НаКлиенте,"
		"AtServer.НаСервере,"
		"AtServerNoContext.НаСервереБезКонтекста,"
		"AtClientAtServerNoContext.НаКлиентеНаСервереБезКонтекста,"
		"AtClientAtServer.НаКлиентеНаСервере,"
	);

	Return Directives;
EndFunction // Directives()

Function PrepInstructions() Export
	Var PrepInstructions;

	PrepInstructions = Enum(New Structure,
		"If.Если,"
		"ElsIf.ИначеЕсли,"
		"Else.Иначе,"
		"EndIf.КонецЕсли,"
		"Region.Область,"
		"EndRegion.КонецОбласти,"
	);

	Return PrepInstructions;
EndFunction // PrepInstructions()

Function Enum(Structure, Keys)
	Var ItemList, Value;

	For Each Items In StrSplit(Keys, ",", False) Do
		ItemList = StrSplit(Items, ".", False);
		Value = TrimAll(ItemList[0]);
		For Each Item In ItemList Do
			Structure.Insert(Item, Value);
		EndDo;
	EndDo;

	Return New FixedStructure(Structure);
EndFunction // Enum()

#EndRegion // Enums

#Region Scanner

Function Scanner(Source) Export
	Var Scanner;

	Scanner = New Structure(
		"Path,"   // string
		"Source," // string
		"Len,"    // number
		"Pos,"    // number
		"Tok,"    // string (one of Tokens)
		"Lit,"    // string
		"Char,"   // string
		"Line,"   // number
	);

	Scanner.Path = "";
	Scanner.Source = Source;
	Scanner.Len = StrLen(Source);
	Scanner.Line = 1;
	Scanner.Pos = 0;
	Scanner.Lit = "";

	Init();

	Return Scanner;
EndFunction // Scanner()

Function Scan(Scanner) Export
	Var Char, Tok, Lit;
	SkipWhitespace(Scanner);
	Char = Scanner.Char;
	If IsLetter(Char) Then
		Lit = ScanIdentifier(Scanner);
		Tok = Lookup(Lit);
	ElsIf IsDigit(Char) Then
		Lit = ScanNumber(Scanner);
		Tok = Tokens.Number;
	ElsIf Char = """" Or Char = "|" Then
		Lit = ScanString(Scanner);
		Tok = StringToken(Lit);
	ElsIf Char = "'" Then
		Lit = ScanDateTime(Scanner);
		Tok = Tokens.DateTime;
	ElsIf Char = "=" Then
		Tok = Tokens.Eql;
		NextChar(Scanner);
	ElsIf Char = "<" Then
		If NextChar(Scanner) = "=" Then
			Lit = "<=";
			Tok = Tokens.Leq;
			NextChar(Scanner);
		ElsIf Scanner.Char = ">" Then
			Lit = "<>";
			Tok = Tokens.Neq;
			NextChar(Scanner);
		Else
			Tok = Tokens.Lss;
		EndIf;
	ElsIf Char = ">" Then
		If NextChar(Scanner) = "=" Then
			Lit = ">=";
			Tok = Tokens.Geq;
			NextChar(Scanner);
		Else
			Tok = Tokens.Gtr;
		EndIf;
	ElsIf Char = "+" Then
		Tok = Tokens.Add;
		NextChar(Scanner);
	ElsIf Char = "-" Then
		Tok = Tokens.Sub;
		NextChar(Scanner);
	ElsIf Char = "*" Then
		Tok = Tokens.Mul;
		NextChar(Scanner);
	ElsIf Char = "/" Then
		If NextChar(Scanner) = "/" Then
			Lit = ScanComment(Scanner);
			Tok = Tokens.Comment;
		Else
			Tok = Tokens.Div;
		EndIf;
	ElsIf Char = "%" Then
		Tok = Tokens.Mod;
		NextChar(Scanner);
	ElsIf Char = "(" Then
		Tok = Tokens.Lparen;
		NextChar(Scanner);
	ElsIf Char = ")" Then
		Tok = Tokens.Rparen;
		NextChar(Scanner);
	ElsIf Char = "[" Then
		Tok = Tokens.Lbrack;
		NextChar(Scanner);
	ElsIf Char = "]" Then
		Tok = Tokens.Rbrack;
		NextChar(Scanner);
	ElsIf Char = "?" Then
		Tok = Tokens.Ternary;
		NextChar(Scanner);
	ElsIf Char = "," Then
		Tok = Tokens.Comma;
		NextChar(Scanner);
	ElsIf Char = "." Then
		Tok = Tokens.Period;
		NextChar(Scanner);
	ElsIf Char = ":" Then
		Tok = Tokens.Colon;
		NextChar(Scanner);
	ElsIf Char = ";" Then
		Tok = Tokens.Semicolon;
		NextChar(Scanner);
	ElsIf Char = "" Then
		Tok = Tokens.Eof;
	ElsIf Char = "&" Then
		NextChar(Scanner);
		Lit = ScanIdentifier(Scanner);
		If Not Directives.Property(Lit) Then
			Error(Scanner, StrTemplate("Unknown directive: '%1'", Lit));
		EndIf;
		Tok = Tokens.Directive;
	ElsIf Char = "#" Then
		NextChar(Scanner);
		SkipWhitespace(Scanner);
		Lit = ScanIdentifier(Scanner);
		If PrepInstructions.Property(Lit, Tok) Then
			Tok = "_" + Tok;
		Else
			Error(Scanner, StrTemplate("Unknown preprocessor instruction: '%1'", Lit));
		EndIf;
	ElsIf Char = "~" Then
		Lit = ScanIdentifier(Scanner);
		Tok = Tokens.Label;
	Else
		Error(Scanner, "Unknown char");
	EndIf;
	If ValueIsFilled(Lit) Then
		Scanner.Lit = Lit;
	Else
		Scanner.Lit = Char;
	EndIf;
	Scanner.Tok = Tok;
	Return Tok;
EndFunction // Scan()

Function NextChar(Scanner)
	If Scanner.Char <> "" Then
		Scanner.Pos = Scanner.Pos + 1;
		Scanner.Char = Mid(Scanner.Source, Scanner.Pos, 1);
	EndIf;
	Return Scanner.Char;
EndFunction // NextChar()

Function SkipWhitespace(Scanner)
	Var Char;
	Char = Scanner.Char;
	While IsBlankString(Char) And Char <> "" Do
		If Char = Chars.LF Then
			Scanner.Line = Scanner.Line + 1;
		EndIf;
		Char = NextChar(Scanner);
	EndDo;
EndFunction // SkipWhitespace()

Function ScanComment(Scanner)
	Var Len, Char;
	Len = 0;
	Char = NextChar(Scanner);
	While Char <> Chars.LF And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanComment()

Function ScanIdentifier(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While IsLetter(Char) Or IsDigit(Char) Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanIdentifier()

Function ScanNumber(Scanner)
	Var Len;
	Len = ScanIntegerLen(Scanner); // Len >= 1
	If Scanner.Char = "." Then
		Len = Len + ScanIntegerLen(Scanner);
	EndIf;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanNumber()

Function ScanIntegerLen(Scanner)
	Var Len;
	Len = 1;
	While IsDigit(NextChar(Scanner)) Do
		Len = Len + 1;
	EndDo;
	Return Len;
EndFunction // ScanIntegerLen()

Function ScanString(Scanner)
	Var Len;
	Len = ScanStringLen(Scanner);
	While NextChar(Scanner) = """" Do
		Len = Len + ScanStringLen(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanString()

Function ScanStringLen(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While Char <> """" And Char <> Chars.LF And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	If Char = Chars.LF Then
		Scanner.Line = Scanner.Line + 1;
	EndIf;
	Return Len + ?(Char <> "", 1, 0);
EndFunction // ScanStringLen()

Function ScanDateTime(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While Char <> "'" And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	If Char = "'" Then
		Len = Len + 1;
		NextChar(Scanner);
	Else
		Error(Scanner, "expected `'`");
	EndIf;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanDateTime()

#EndRegion // Scanner

#Region AbstractSyntaxTree

Function Module(Decls, AutoVars, Statements, Interface, Comments)
	Return Structure("Module",
		"Decls,"      // array (one of declarations)
		"AutoVars,"   // array (Object)
		"Statements," // array (one of statements)
		"Interface,"  // array (Object)
		"Comments,"   // map[number](string)
	, Decls, AutoVars, Statements, Interface, Comments);
EndFunction // Module()

#Region Scope

Function Scope(Outer)
	Return New Structure(
		"Outer,"    // structure (Scope)
		"Objects,"  // structure as map[string](Object)
		"AutoVars," // array (Object)
	, Outer, New Structure, New Array);
EndFunction // Scope()

Function Unknown(Name)
	Return Structure("Unknown",
		"Name" // string
	, Name);
EndFunction // Unknown()

Function Func(Name, Directive, Params, Exported)
	Return Structure("Func",
		"Name"   // string
		"Dir"    // string (one of Directives)
		"Params" // array (Parameter)
		"Export" // boolean
	, Name, Directive, Params, Exported);
EndFunction // Func()

Function Proc(Name, Directive, Params, Exported)
	Return Structure("Proc",
		"Name"   // string
		"Dir"    // string (one of Directives)
		"Params" // array (Parameter)
		"Export" // boolean
	, Name, Directive, Params, Exported);
EndFunction // Proc()

Function VarM(Name, Directive, Exported)
	Return Structure("VarM",
		"Name"   // string
		"Dir"    // string (one of Directives)
		"Export" // boolean
	, Name, Directive, Exported);
EndFunction // VarM()

Function VarL(Name, Auto = False)
	Return Structure("VarL",
		"Name" // string
		"Auto" // boolean
	, Name, Auto);
EndFunction // VarL()

Function Param(Name, ByVal, Value = Undefined)
	Return Structure("Param",
		"Name"  // string
		"ByVal" // boolean
		"Value" // structure (UnaryExpr, BasicLitExpr)
	, Name, ByVal, Value);
EndFunction // Param()

#EndRegion // Scope

#Region Declarations

Function ModVarsDecl(VarList)
	Return Structure("ModVarsDecl",
		"List" // array (VarM)
	, VarList);
EndFunction // ModVarsDecl()

Function VarsDecl(VarList)
	Return Structure("VarsDecl",
		"List" // array (VarL)
	, VarList);
EndFunction // VarsDecl()

Function ProcDecl(Object, Decls, AutoVars, Body)
	Return Structure("ProcDecl",
		"Object"   // structure (Object)
		"Decls"    // array (one of declarations)
		"AutoVars" // array (Object)
		"Body"     // array (one of statements)
	, Object, Decls, AutoVars, Body);
EndFunction // ProcDecl()

Function FuncDecl(Object, Decls, AutoVars, Body)
	Return Structure("FuncDecl",
		"Object"   // structure (Object)
		"Decls"    // array (one of declarations)
		"AutoVars" // array (Object)
		"Body"     // array (one of statements)
	, Object, Decls, AutoVars, Body);
EndFunction // FuncDecl()

Function PrepIfDecl(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined)
	Return Structure("PrepIfDecl",
		"Cond"  // structure (one of expressions)
		"Then"  // array (one of declarations)
		"ElsIf" // undefined or array (PrepIfDecl)
		"Else"  // undefined or array (one of declarations)
	, Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // PrepIfDecl()

Function PrepRegionDecl(Name, Decls, Body)
	Return Structure("PrepRegionDecl",
		"Name"  // structure (one of expressions)
		"Decls" // array (one of declarations)
		"Body"  // array (one of statements)
	, Name, Decls, Body);
EndFunction // PrepRegionDecl()

#EndRegion // Declarations

#Region Expressions

Function BasicLitExpr(Kind, Value)
	Return Structure("BasicLitExpr",
		"Kind"     // string (one of Tokens)
		"Value"    // one of basic types
	, Kind, Value);
EndFunction // BasicLitExpr()

Function Selector(Kind, Value)
	Return Structure("Selector",
		"Kind"  // string (one of SelectorKinds)
		"Value" // string or array (one of expressions)
	, Kind, Value);
EndFunction // Selector()

Function DesigExpr(Object, Selectors, Call)
	Return Structure("DesigExpr",
		"Object"    // structure (Object)
		"Select"    // array (Selector)
		"Call"      // boolean
	, Object, Selectors, Call);
EndFunction // DesigExpr()

Function UnaryExpr(Operator, Operand)
	Return Structure("UnaryExpr",
		"Operator" // string (one of Tokens)
		"Operand"  // structure (one of expressions)
	, Operator, Operand);
EndFunction // UnaryExpr()

Function BinaryExpr(Left, Operator, Right)
	Return Structure("BinaryExpr",
		"Left"     // structure (one of expressions)
		"Operator" // string (one of Tokens)
		"Right"    // structure (one of expressions)
	, Left, Operator, Right);
EndFunction // BinaryExpr()

Function NewExpr(Constr)
	Return Structure("NewExpr",
		"Constr" // structure (DesigExpr) or array (one of expressions)
	, Constr);
EndFunction // NewExpr()

Function TernaryExpr(Cond, ThenPart, ElsePart, Selectors)
	Return Structure("TernaryExpr",
		"Cond"   // structure (one of expressions)
		"Then"   // structure (one of expressions)
		"Else"   // structure (one of expressions)
		"Select" // array (Selector)
	, Cond, ThenPart, ElsePart, Selectors);
EndFunction // TernaryExpr()

Function ParenExpr(Expr)
	Return Structure("ParenExpr",
		"Expr" // structure (one of expressions)
	, Expr);
EndFunction // ParenExpr()

Function NotExpr(Expr)
	Return Structure("NotExpr",
		"Expr" // structure (one of expressions)
	, Expr);
EndFunction // NotExpr()

Function StringExpr(ExprList)
	Return Structure("StringExpr",
		"List" // array (BasicLitExpr)
	, ExprList);
EndFunction // StringExpr()

#EndRegion // Expressions

#Region Statements

Function AssignStmt(Left, Right)
	Return Structure("AssignStmt",
		"Left"  // structure (DesigExpr)
		"Right" // structure (one of expressions)
	, Left, Right);
EndFunction // AssignStmt()

Function ReturnStmt(Expr = Undefined)
	Return Structure("ReturnStmt",
		"Expr" // undefined or structure (one of expressions)
	, Expr);
EndFunction // ReturnStmt()

Function BreakStmt()
	Return Structure("BreakStmt");
EndFunction // BreakStmt()

Function ContinueStmt()
	Return Structure("ContinueStmt");
EndFunction // ContinueStmt()

Function RaiseStmt(Expr = Undefined)
	Return Structure("RaiseStmt",
		"Expr" // undefined or structure (one of expressions)
	, Expr);
EndFunction // RaiseStmt()

Function ExecuteStmt(Expr)
	Return Structure("ExecuteStmt",
		"Expr" // structure (one of expressions)
	, Expr);
EndFunction // ExecuteStmt()

Function CallStmt(DesigExpr)
	Return Structure("CallStmt",
		"Desig" // structure (DesigExpr)
	, DesigExpr);
EndFunction // CallStmt()

Function IfStmt(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined)
	Return Structure("IfStmt",
		"Cond"  // structure (one of expressions)
		"Then"  // array (one of statements)
		"ElsIf" // undefined or array (IfStmt)
		"Else"  // undefined or array (one of statements)
	, Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // IfStmt()

Function PrepIfStmt(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined)
	Return Structure("PrepIfStmt",
		"Cond"  // structure (one of expressions)
		"Then"  // array (one of statements)
		"ElsIf" // undefined or array (PrepIfStmt)
		"Else"  // undefined or array (one of statements)
	, Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // PrepIfStmt()

Function WhileStmt(Cond, Statements)
	Return Structure("WhileStmt",
		"Cond" // structure (one of expressions)
		"Body" // array (one of statements)
	, Cond, Statements);
EndFunction // WhileStmt()

Function PrepRegionStmt(Name, Statements)
	Return Structure("PrepRegionStmt",
		"Name" // structure (one of expressions)
		"Body" // array (one of statements)
	, Name, Statements);
EndFunction // PrepRegionStmt()

Function ForStmt(DesigExpr, From, Until, Statements)
	Return Structure("ForStmt",
		"Desig" // structure (DesigExpr)
		"From"  // structure (one of expressions)
		"To"    // structure (one of expressions)
		"Body"  // array (one of statements)
	, DesigExpr, From, Until, Statements);
EndFunction // ForStmt()

Function ForEachStmt(DesigExpr, Collection, Statements)
	Return Structure("ForEachStmt",
		"Desig" // structure (DesigExpr)
		"In"    // structure (one of expressions)
		"Body"  // array (one of statements)
	, DesigExpr, Collection, Statements);
EndFunction // ForEachStmt()

Function TryStmt(TryPart, ExceptPart)
	Return Structure("TryStmt",
		"Try"    // array (one of statements)
		"Except" // array (one of statements)
	, TryPart, ExceptPart);
EndFunction // TryStmt()

Function GotoStmt(Label)
	Return Structure("GotoStmt",
		"Label" // string
	, Label);
EndFunction // GotoStmt()

Function LabelStmt(Label)
	Return Structure("LabelStmt",
		"Label" // string
	, Label);
EndFunction // LabelStmt()

#EndRegion // Statements

#EndRegion // AbstractSyntaxTree

#Region Parser

Function Parser(Source) Export
	Var Parser;

	Parser = New Structure(
		"Scanner,"   // structure (Scanner)
		"Pos,"       // number
		"PrevPos,"   // number
		"Tok,"       // string (one of Tokens)
		"Lit,"       // string
		"Val,"       // number, string, date, true, false, undefined
		"Scope,"     // structure (Scope)
		"Vars,"      // structure as map[string](Object)
		"Methods,"   // structure as map[string](Object)
		"Module,"    // structure (Module)
		"Unknown,"   // structure as map[string](Object)
		"IsFunc,"    // boolean
		"Directive," // string (one of Directives)
		"Interface," // array (Object)
		"Comments,"  // map[number](string)
	);

	Parser.Scanner = Scanner(Source);
	Parser.Pos = 0;
	Parser.PrevPos = 0;
	Parser.Methods = New Structure;
	Parser.Unknown = New Structure;
	Parser.IsFunc = False;
	Parser.Interface = New Array;
	Parser.Comments = New Map;

	OpenScope(Parser);

	Return Parser;

EndFunction // Parser()

Function Next(Parser)
	Var Scanner, Tok, Lit;
	Scanner = Parser.Scanner;
	Parser.PrevPos = Scanner.Pos;
	Tok = SkipComments(Scanner, Parser.Comments);
	Lit = Scanner.Lit;
	Parser.Pos = Scanner.Pos - StrLen(Lit);
	Parser.Tok = Tok;
	Parser.Lit = Lit;
	Parser.Val = Value(Tok, Lit);
	Return Tok;
EndFunction // Next()

Function SkipComments(Scanner, Comments)
	Var Tok;
	Tok = Scan(Scanner);
	While Tok = Tokens.Comment Do
		Comments[Scanner.Line] = Scanner.Lit;
		Tok = Scan(Scanner);
	EndDo;
	Return Tok;
EndFunction // SkipComments()

Function FindObject(Parser, Name)
	Var Scope, Object;
	Scope = Parser.Scope;
	Scope.Objects.Property(Name, Object);
	While Object = Undefined And Scope.Outer <> Undefined Do
		Scope = Scope.Outer;
		Scope.Objects.Property(Name, Object);
	EndDo;
	Return Object;
EndFunction // FindObject()

Function OpenScope(Parser)
	Var Scope;
	Scope = Scope(Parser.Scope);
	Parser.Scope = Scope;
	Parser.Vars = Scope.Objects;
	Return Scope;
EndFunction // OpenScope()

Function CloseScope(Parser)
	Var Scope;
	Scope = Parser.Scope.Outer;
	Parser.Scope = Scope;
	Parser.Vars = Scope.Objects;
	Return Scope;
EndFunction // CloseScope()

Function ParseUnaryExpr(Parser)
	Var Operator, Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Operator = Parser.Tok;
	If UnaryOperators.Find(Parser.Tok) <> Undefined Then
		Next(Parser);
		Expr = UnaryExpr(Operator, ParseOperand(Parser));
	ElsIf Parser.Tok = Tokens.Eof Then
		Expr = Undefined;
	Else
		Expr = ParseOperand(Parser);
	EndIf;
	Return Place(Expr, Parser, Pos, Line);
EndFunction // ParseUnaryExpr()

Function ParseOperand(Parser)
	Var Tok, Operand;
	Tok = Parser.Tok;
	If Tok = Tokens.String Or Tok = Tokens.StringBeg Then
		Operand = ParseStringExpr(Parser);
	ElsIf BasicLiteralsExceptString.Find(Tok) <> Undefined Then
		Operand = BasicLitExpr(Tok, Parser.Val);
		Next(Parser);
	ElsIf Tok = Tokens.Ident Then
		Operand = ParseDesigExpr(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Operand = ParseParenExpr(Parser);
	ElsIf Tok = Tokens.New Then
		Operand = ParseNewExpr(Parser);
	ElsIf Tok = Tokens.Ternary Then
		Operand = ParseTernaryExpr(Parser);
	Else
		Error(Parser.Scanner, "Expected operand",, True);
	EndIf;
	Return Operand;
EndFunction // ParseOperand()

Function ParseStringExpr(Parser)
	Var Tok, ExprList, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Tok = Parser.Tok;
	ExprList = New Array;
	While True Do
		If Tok = Tokens.String Then
			ExprList.Add(BasicLitExpr(Tok, Parser.Val));
			Tok = Next(Parser);
			While Tok = Tokens.String Do
				ExprList.Add(BasicLitExpr(Tok, Parser.Val));
				Tok = Next(Parser);
			EndDo;
		ElsIf Tok = Tokens.StringBeg Then
			ExprList.Add(BasicLitExpr(Tok, Parser.Val));
			Tok = Next(Parser);
			While Tok = Tokens.StringMid Do
				ExprList.Add(BasicLitExpr(Tok, Parser.Val));
				Tok = Next(Parser);
			EndDo;
			If Tok <> Tokens.StringEnd Then
				Error(Parser.Scanner, "Expected """,, True);
			EndIf;
			ExprList.Add(BasicLitExpr(Tok, Parser.Val));
			Tok = Next(Parser);
		Else
			Break;
		EndIf;
	EndDo;
	Return Place(StringExpr(ExprList), Parser, Pos, Line);
EndFunction // ParseStringExpr()

Function ParseNewExpr(Parser)
	Var Tok, Constr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Tok = Next(Parser);
	If Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Constr = EmptyArray;
		Else
			Constr = ParseArguments(Parser);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Next(Parser);
	Else
		Constr = ParseDesigExpr(Parser);
	EndIf;
	Return Place(NewExpr(Constr), Parser, Pos, Line);
EndFunction // ParseNewExpr()

Function ParseDesigExpr(Parser, Val AllowNewVar = False)
	Var Name, Selector, Object, List, Kind, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Name = Parser.Lit;
	Next(Parser);
	Selector = ParseSelector(Parser);
	If Selector = Undefined Then
		Object = FindObject(Parser, Name);
		List = EmptyArray;
	Else
		AllowNewVar = False;
		Kind = Selector.Kind;
		If Kind = "Call" Then
			If Not Parser.Methods.Property(Name, Object) Then
				If Not Parser.Unknown.Property(Name, Object) Then
					Object = Unknown(Name);
					Parser.Unknown.Insert(Name, Object);
				EndIf;
			EndIf;
		Else
			Object = FindObject(Parser, Name);
		EndIf;
		List = New Array;
		List.Add(Selector);
		Selector = ParseSelector(Parser);
		While Selector <> Undefined Do
			Kind = Selector.Kind;
			List.Add(Selector);
			Selector = ParseSelector(Parser);
		EndDo;
	EndIf;
	If Object = Undefined Then
		If AllowNewVar Then
			Object = VarL(Name, True);
			Parser.Vars.Insert(Name, Object);
			Parser.Scope.AutoVars.Add(Object);
		Else
			Object = Unknown(Name);
			If Verbose Then
				Error(Parser.Scanner, StrTemplate("Undeclared identifier `%1`", Name), Pos);
			EndIf;
		EndIf;
	EndIf;
	Return Place(DesigExpr(Object, List, Kind = SelectorKinds.Call), Parser, Pos, Line);
EndFunction // ParseDesigExpr()

Function ParseSelector(Parser)
	Var Tok, Value, Selector, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Tok = Parser.Tok;
	If Tok = Tokens.Period Then
		Next(Parser);
		If Not Keywords.Property(Parser.Lit) Then
			Expect(Parser, Tokens.Ident);
		EndIf;
		Value = Parser.Lit;
		Selector = Selector(SelectorKinds.Ident, Value);
		Next(Parser);
	ElsIf Tok = Tokens.Lbrack Then
		Tok = Next(Parser);
		If Tok = Tokens.Rbrack Then
			Error(Parser.Scanner, "Expected expression", Pos, True);
		EndIf;
		Value = ParseExprList(Parser);
		Expect(Parser, Tokens.Rbrack);
		Selector = Selector(SelectorKinds.Index, Value);
		Next(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Value = EmptyArray;
		Else
			Value = ParseArguments(Parser);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Selector = Selector(SelectorKinds.Call, Value);
		Next(Parser);
	EndIf;
	Return Place(Selector, Parser, Pos, Line);
EndFunction // ParseSelector()

Function ParseExpression(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Expr = ParseAndExpr(Parser);
	While Parser.Tok = Tokens.Or Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Place(BinaryExpr(Expr, Operator, ParseAndExpr(Parser)), Parser, Pos, Line);
	EndDo;
	Return Expr;
EndFunction // ParseExpression()

Function ParseAndExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Expr = ParseNotExpr(Parser);
	While Parser.Tok = Tokens.And Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Place(BinaryExpr(Expr, Operator, ParseNotExpr(Parser)), Parser, Pos, Line);
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseNotExpr(Parser)
	Var Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	If Parser.Tok = Tokens.Not Then
		Next(Parser);
		Expr = Place(NotExpr(ParseRelExpr(Parser)), Parser, Pos, Line);
	Else
		Expr = ParseRelExpr(Parser);
	EndIf;
	Return Expr;
EndFunction // ParseNotExpr()

Function ParseRelExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Expr = ParseAddExpr(Parser);
	While RelationalOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Place(BinaryExpr(Expr, Operator, ParseAddExpr(Parser)), Parser, Pos, Line);
	EndDo;
	Return Expr;
EndFunction // ParseRelExpr()

Function ParseAddExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Expr = ParseMulExpr(Parser);
	While Parser.Tok = Tokens.Add Or Parser.Tok = Tokens.Sub Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Place(BinaryExpr(Expr, Operator, ParseMulExpr(Parser)), Parser, Pos, Line);
	EndDo;
	Return Expr;
EndFunction // ParseAddExpr()

Function ParseMulExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Expr = ParseUnaryExpr(Parser);
	While Parser.Tok = Tokens.Mul Or Parser.Tok = Tokens.Div Or Parser.Tok = Tokens.Mod Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Place(BinaryExpr(Expr, Operator, ParseUnaryExpr(Parser)), Parser, Pos, Line);
	EndDo;
	Return Expr;
EndFunction // ParseMulExpr()

Function ParseExprList(Parser, HeadExpr = Undefined)
	Var ExprList;
	If HeadExpr = Undefined Then
		HeadExpr = ParseExpression(Parser);
	EndIf;
	ExprList = New Array;
	ExprList.Add(HeadExpr);
	While Parser.Tok = Tokens.Comma And InitialTokensOfExpression.Find(Next(Parser)) <> Undefined Do
		ExprList.Add(ParseExpression(Parser));
	EndDo;
	Return ExprList;
EndFunction // ParseExprList()

Function ParseArguments(Parser)
	Var ExprList, ExpectExpression;
	ExprList = New Array;
	ExpectExpression = True;
	While ExpectExpression Do
		If InitialTokensOfExpression.Find(Parser.Tok) <> Undefined Then
			ExprList.Add(ParseExpression(Parser));
		Else
			ExprList.Add(Undefined);
		EndIf;
		If Parser.Tok = Tokens.Comma Then
			Next(Parser);
		Else
			ExpectExpression = False;
		EndIf;
	EndDo;
	Return ExprList;
EndFunction // ParseArguments()

Function ParseTernaryExpr(Parser)
	Var Cond, ThenPart, ElsePart, Selectors, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Next(Parser);
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	Cond = ParseExpression(Parser);
	Expect(Parser, Tokens.Comma);
	Next(Parser);
	ThenPart = ParseExpression(Parser);
	Expect(Parser, Tokens.Comma);
	Next(Parser);
	ElsePart = ParseExpression(Parser);
	Expect(Parser, Tokens.Rparen);
	If Next(Parser) = Tokens.Period Then
		Selectors = New Array;
		Selector = ParseSelector(Parser);
		While Selector <> Undefined Do
			Selectors.Add(Selector);
			Selector = ParseSelector(Parser);
		EndDo;
	Else
		Selectors = EmptyArray;
	EndIf;
	Return Place(TernaryExpr(Cond, ThenPart, ElsePart, Selectors), Parser, Pos, Line);
EndFunction // ParseTernaryExpr()

Function ParseParenExpr(Parser)
	Var Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Next(Parser);
	Expr = ParseExpression(Parser);
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	Return Place(ParenExpr(Expr), Parser, Pos, Line);
EndFunction // ParseParenExpr()

Function ParseFuncDecl(Parser)
	Var Object, Name, Decls, ParamList, Exported, AutoVars, VarObj, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Exported = False;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	OpenScope(Parser);
	ParamList = ParseParamList(Parser);
	If Parser.Tok = Tokens.Export Then
		Exported = True;
		Next(Parser);
	EndIf;
	If Parser.Unknown.Property(Name, Object) Then
		Object.Type = "Func";
		Object.Insert("Params", ParamList);
		Object.Insert("Export", Exported);
		Parser.Unknown.Delete(Name);
	Else
		Object = Func(Name, Parser.Directive, ParamList, Exported);
	EndIf;
	If Parser.Methods.Property(Name) Then
		Error(Parser.Scanner, "Method already declared", Pos, True);
	EndIf;
	Parser.Methods.Insert(Name, Object);
	If Exported Then
		Parser.Interface.Add(Object);
	EndIf;
	Decls = ParseVarDecls(Parser);
	Parser.IsFunc = True;
	Statements = ParseStatements(Parser);
	Parser.IsFunc = False;
	Expect(Parser, Tokens.EndFunction);
	AutoVars = New Array;
	For Each VarObj In Parser.Scope.AutoVars Do
		AutoVars.Add(VarObj);
	EndDo;
	CloseScope(Parser);
	Next(Parser);
	Return Place(FuncDecl(Object, Decls, AutoVars, Statements), Parser, Pos, Line);
EndFunction // ParseFuncDecl()

Function ParseParamList(Parser)
	Var ParamList;
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	If Parser.Tok = Tokens.Rparen Then
		ParamList = EmptyArray;
	Else
		ParamList = New Array;
		ParamList.Add(ParseParameter(Parser));
		While Parser.Tok = Tokens.Comma Do
			Next(Parser);
			ParamList.Add(ParseParameter(Parser));
		EndDo;
	EndIf;
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	Return ParamList;
EndFunction // ParseParamList()

Function ParseProcDecl(Parser)
	Var Object, Name, Decls, ParamList, Exported, AutoVars, VarObj, Statements, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Exported = False;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	OpenScope(Parser);
	ParamList = ParseParamList(Parser);
	If Parser.Tok = Tokens.Export Then
		Exported = True;
		Next(Parser);
	EndIf;
	If Parser.Unknown.Property(Name, Object) Then
		Object.Type = "Proc";
		Object.Insert("Params", ParamList);
		Object.Insert("Export", Exported);
		Parser.Unknown.Delete(Name);
	Else
		Object = Proc(Name, Parser.Directive, ParamList, Exported);
	EndIf;
	If Parser.Methods.Property(Name) Then
		Error(Parser.Scanner, "Method already declared", Pos, True);
	EndIf;
	Parser.Methods.Insert(Name, Object);
	If Exported Then
		Parser.Interface.Add(Object);
	EndIf;
	Decls = ParseVarDecls(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndProcedure);
	AutoVars = New Array;
	For Each VarObj In Parser.Scope.AutoVars Do
		AutoVars.Add(VarObj);
	EndDo;
	CloseScope(Parser);
	Next(Parser);
	Return Place(ProcDecl(Object, Decls, AutoVars, Statements), Parser, Pos, Line);
EndFunction // ParseProcDecl()

Function ParseReturnStmt(Parser)
	Var Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Next(Parser);
	If Parser.IsFunc Then
		Expr = ParseExpression(Parser);
	EndIf;
	Return Place(ReturnStmt(Expr), Parser, Pos, Line);
EndFunction // ParseReturnStmt()

Function ParseModVarListDecl(Parser)
	Var VarList, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	VarList = New Array;
	VarList.Add(ParseVarM(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVarM(Parser));
	EndDo;
	Return Place(ModVarsDecl(VarList), Parser, Pos, Line);
EndFunction // ParseModVarListDecl()

Function ParseVarListDecl(Parser)
	Var VarList, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	VarList = New Array;
	VarList.Add(ParseVarL(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVarL(Parser));
	EndDo;
	Return Place(VarsDecl(VarList), Parser, Pos, Line);
EndFunction // ParseVarListDecl()

Function ParseVarM(Parser)
	Var Name, Object, Exported, Pos;
	Pos = Parser.Pos;
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	If Next(Parser) = Tokens.Export Then
		Exported = True;
		Next(Parser);
	Else
		Exported = False;
	EndIf;
	Object = VarM(Name, Parser.Directive, Exported);
	If Exported Then
		Parser.Interface.Add(Object);
	EndIf;
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared", Pos, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
	Return Object;
EndFunction // ParseVarM()

Function ParseVarL(Parser)
	Var Name, Object, Exported, Pos;
	Pos = Parser.Pos;
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	If Next(Parser) = Tokens.Export Then
		Exported = True;
		Next(Parser);
	Else
		Exported = False;
	EndIf;
	Object = VarL(Name);
	If Exported Then
		Parser.Interface.Add(Object);
	EndIf;
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared", Pos, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
	Return Object;
EndFunction // ParseVarL()

Function ParseParameter(Parser)
	Var Name, Object, ByVal, Pos;
	Pos = Parser.Pos;
	If Parser.Tok = Tokens.Val Then
		ByVal = True;
		Next(Parser);
	Else
		ByVal = False;
	EndIf;
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	If Next(Parser) = Tokens.Eql Then
		Next(Parser);
		Object = Param(Name, ByVal, ParseUnaryExpr(Parser));
	Else
		Object = Param(Name, ByVal);
	EndIf;
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared", Pos, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
	Return Object;
EndFunction // ParseParameter()

Function ParseStatements(Parser)
	Var Statements, Stmt;
	Statements = New Array;
	Stmt = ParseStmt(Parser);
	If Stmt <> Undefined Then
		Statements.Add(Stmt);
	EndIf;
	While Parser.Tok = Tokens.Semicolon Do
		Next(Parser);
		Stmt = ParseStmt(Parser);
		If Stmt <> Undefined Then
			Statements.Add(Stmt);
		EndIf;
	EndDo;
	Return Statements;
EndFunction // ParseStatements()

Function ParseStmt(Parser)
	Var Tok, Stmt, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Scanner.Line;
	Tok = Parser.Tok;
	If Tok = Tokens.Ident Then
		Stmt = ParseAssignOrCallStmt(Parser);
	ElsIf Tok = Tokens.If Then
		Stmt = ParseIfStmt(Parser);
	ElsIf Tok = Tokens.Try Then
		Stmt = ParseTryStmt(Parser);
	ElsIf Tok = Tokens.While Then
		Stmt = ParseWhileStmt(Parser);
	ElsIf Tok = Tokens.For Then
		If Next(Parser) = Tokens.Each Then
			Stmt = ParseForEachStmt(Parser);
		Else
			Stmt = ParseForStmt(Parser);
		EndIf;
	ElsIf Tok = Tokens.Return Then
		Stmt = ParseReturnStmt(Parser);
	ElsIf Tok = Tokens.Break Then
		Next(Parser);
		Stmt = BreakStmt();
	ElsIf Tok = Tokens.Continue Then
		Next(Parser);
		Stmt = ContinueStmt();
	ElsIf Tok = Tokens.Raise Then
		Stmt = ParseRaiseStmt(Parser);
	ElsIf Tok = Tokens.Execute Then
		Stmt = ParseExecuteStmt(Parser);
	ElsIf Tok = Tokens.Goto Then
		Stmt = ParseGotoStmt(Parser);
	ElsIf Tok = Tokens.Label Then
		Stmt = LabelStmt(Parser.Lit);
		Next(Parser);
		Expect(Parser, Tokens.Colon);
		Next(Parser);
	ElsIf Tok = Tokens._Region Then
		Stmt = ParsePrepRegionStmt(Parser);
	ElsIf Tok = Tokens._If Then
		Stmt = ParsePrepIfStmt(Parser);
	ElsIf Tok = Tokens.Semicolon Then
		// NOP
	EndIf;
	Return Place(Stmt, Parser, Pos, Line);
EndFunction // ParseStmt()

Function ParseRaiseStmt(Parser)
	Var Tok, Expr;
	Next(Parser);
	If InitialTokensOfExpression.Find(Parser.Tok) <> Undefined Then
		Expr = ParseExpression(Parser);
	EndIf;
	Return RaiseStmt(Expr);
EndFunction // ParseRaiseStmt()

Function ParseExecuteStmt(Parser)
	Var Expr;
	Next(Parser);
	Return ExecuteStmt(ParseExpression(Parser));
EndFunction // ParseExecuteStmt()

Function ParseAssignOrCallStmt(Parser)
	Var Left, Right, Stmt, Pos;
	Pos = Parser.Pos;
	Left = ParseDesigExpr(Parser, True);
	If Left.Call Then
		Stmt = CallStmt(Left);
	Else
		Expect(Parser, Tokens.Eql);
		Next(Parser);
		Right = ParseExpression(Parser);
		Stmt = AssignStmt(Left, Right);
	EndIf;
	Return Stmt;
EndFunction // ParseAssignOrCallStmt()

Function ParseIfStmt(Parser)
	Var Tok, Cond, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen;
	Next(Parser);
	Cond = ParseExpression(Parser);
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseStatements(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens.ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens.ElsIf Do
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseStatements(Parser);
			ElsIfPart.Add(IfStmt(ElsIfCond, ElsIfThen));
			Tok = Parser.Tok;
		EndDo;
	EndIf;
	If Tok = Tokens.Else Then
		Next(Parser);
		ElsePart = ParseStatements(Parser);
	EndIf;
	Expect(Parser, Tokens.EndIf);
	Next(Parser);
	Return IfStmt(Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParseIfStmt()

Function ParsePrepIfStmt(Parser)
	Var Tok, Cond, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen;
	Next(Parser);
	Cond = ParseExpression(Parser); // todo: only logic operators
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseStatements(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens._ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens._ElsIf Do
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseStatements(Parser);
			ElsIfPart.Add(PrepIfStmt(ElsIfCond, ElsIfThen));
			Tok = Parser.Tok;
		EndDo;
	EndIf;
	If Tok = Tokens._Else Then
		Next(Parser);
		ElsePart = ParseStatements(Parser);
	EndIf;
	Expect(Parser, Tokens._EndIf);
	Parser.Tok = Tokens.Semicolon; // cheat code
	Return PrepIfStmt(Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParsePrepIfStmt()

Function ParsePrepIfDecl(Parser)
	Var Tok, Cond, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen;
	Next(Parser);
	Cond = ParseExpression(Parser); // todo: only logic operators
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseModuleDecls(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens._ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens._ElsIf Do
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseModuleDecls(Parser);
			ElsIfPart.Add(PrepIfDecl(ElsIfCond, ElsIfThen));
			Tok = Parser.Tok;
		EndDo;
	EndIf;
	If Tok = Tokens._Else Then
		Next(Parser);
		ElsePart = ParseModuleDecls(Parser);
	EndIf;
	Expect(Parser, Tokens._EndIf);
	Next(Parser);
	Return PrepIfDecl(Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParsePrepIfDecl()

Function ParsePrepRegionStmt(Parser)
	Var Name, Statements;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens._EndRegion);
	Parser.Tok = Tokens.Semicolon; // cheat code
	Return PrepRegionStmt(Name, Statements);
EndFunction // ParsePrepRegionStmt()

Function ParsePrepRegionDecl(Parser)
	Var Name, Decls, Statements, Region;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	Decls = ParseModuleDecls(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens._EndRegion);
	Next(Parser);
	Return PrepRegionDecl(Name, Decls, Statements);
EndFunction // ParsePrepRegionDecl()

Function ParseTryStmt(Parser)
	Var TryPart, ExceptPart;
	Next(Parser);
	TryPart = ParseStatements(Parser);
	Expect(Parser, Tokens.Except);
	Next(Parser);
	ExceptPart = ParseStatements(Parser);
	Expect(Parser, Tokens.EndTry);
	Next(Parser);
	Return TryStmt(TryPart, ExceptPart);
EndFunction // ParseTryStmt()

Function ParseWhileStmt(Parser)
	Var Cond, Statements;
	Next(Parser);
	Cond = ParseExpression(Parser);
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo);
	Next(Parser);
	Return WhileStmt(Cond, Statements);
EndFunction // ParseWhileStmt()

Function ParseForStmt(Parser)
	Var DesigExpr, From, Until, Statements, VarPos;
	Expect(Parser, Tokens.Ident);
	VarPos = Parser.Pos;
	DesigExpr = ParseDesigExpr(Parser, True);
	If DesigExpr.Call Then
		Error(Parser.Scanner, "Expected variable", VarPos, True);
	EndIf;
	Expect(Parser, Tokens.Eql);
	Next(Parser);
	From = ParseExpression(Parser);
	Expect(Parser, Tokens.To);
	Next(Parser);
	Until = ParseExpression(Parser);
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo);
	Next(Parser);
	Return ForStmt(DesigExpr, From, Until, Statements);
EndFunction // ParseForStmt()

Function ParseForEachStmt(Parser)
	Var DesigExpr, Left, Right, Collection, Statements, VarPos;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	VarPos = Parser.Pos;
	DesigExpr = ParseDesigExpr(Parser, True);
	If DesigExpr.Call Then
		Error(Parser.Scanner, "Expected variable", VarPos, True);
	EndIf;
	Expect(Parser, Tokens.In);
	Next(Parser);
	Collection = ParseExpression(Parser);
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo);
	Next(Parser);
	Return ForEachStmt(DesigExpr, Collection, Statements);
EndFunction // ParseForEachStmt()

Function ParseGotoStmt(Parser)
	Var Label;
	Next(Parser);
	Expect(Parser, Tokens.Label);
	Label = Parser.Lit;
	Next(Parser);
	Return GotoStmt(Label);
EndFunction // ParseGotoStmt()

Function ParseVarDecls(Parser)
	Var Tok, Decls;
	Decls = New Array;
	Tok = Parser.Tok;
	While Tok = Tokens.Var Do
		Next(Parser);
		Decls.Add(ParseVarListDecl(Parser));
		Expect(Parser, Tokens.Semicolon);
		Tok = Next(Parser);
	EndDo;
	Return Decls;
EndFunction // ParseVarDecls()

Function ParseModVarDecls(Parser)
	Var Tok, Decls;
	Decls = New Array;
	Tok = Parser.Tok;
	While Tok = Tokens.Directive Do
		Parser.Directive = Parser.Lit;
		Tok = Next(Parser);
	EndDo;
	While Tok = Tokens.Var Do
		Next(Parser);
		Decls.Add(ParseModVarListDecl(Parser));
		Expect(Parser, Tokens.Semicolon);
		Tok = Next(Parser);
		Parser.Directive = Undefined;
		While Tok = Tokens.Directive Do
			Parser.Directive = Parser.Lit;
			Tok = Next(Parser);
		EndDo;
	EndDo;
	Return Decls;
EndFunction // ParseModVarDecls()

Function ParseModuleDecls(Parser)
	Var Tok, Decls;
	Decls = ParseModVarDecls(Parser);
	Tok = Parser.Tok;
	While Tok <> Tokens.Eof Do
		If Tok = Tokens.Function Then
			Decls.Add(ParseFuncDecl(Parser));
		ElsIf Tok = Tokens.Procedure Then
			Decls.Add(ParseProcDecl(Parser));
		ElsIf Tok = Tokens._Region Then
			Decls.Add(ParsePrepRegionDecl(Parser));
		ElsIf Tok = Tokens._If Then
			Decls.Add(ParsePrepIfDecl(Parser));
		Else
			Return Decls;
		EndIf;
		Tok = Parser.Tok;
		Parser.Directive = Undefined;
		While Tok = Tokens.Directive Do
			Parser.Directive = Parser.Lit;
			Tok = Next(Parser);
		EndDo;
	EndDo;
	Return Decls;
EndFunction // ParseModuleDecls()

Function ParseModule(Parser) Export
	Var Decls, AutoVars, Statements;
	Next(Parser);
	Decls = ParseModuleDecls(Parser);
	Statements = ParseStatements(Parser);
	AutoVars = New Array;
	For Each VarObj In Parser.Scope.AutoVars Do
		AutoVars.Add(VarObj);
	EndDo;
	Parser.Module = Module(Decls, AutoVars, Statements, Parser.Interface, Parser.Comments);
	If Verbose Then
		For Each Item In Parser.Unknown Do
			Message(StrTemplate("Undeclared method `%1`", Item.Key));
		EndDo;
	EndIf;
	Expect(Parser, Tokens.Eof);
EndFunction // ParseModule()

#EndRegion // Parser

#Region Auxiliary

Function Structure(Type, Properties = "", Value1 = Undefined, Value2 = Undefined, Value3 = Undefined, Value4 = Undefined, Value5 = Undefined)
	Var Keys;
	If Not StructureKeysCache.Property(Type, Keys) Then
		Keys = "Type," + StrReplace(Properties, Chars.LF, ",");
	EndIf;
	Return New Structure(Keys, Type, Value1, Value2, Value3, Value4, Value5);
EndFunction // Structure()

Function Place(Node, Parser, Pos, Line)
	If Node = Undefined Then
		Return Undefined;
	EndIf;
	If Location Then
		Node.Insert("Line", Line);
		Node.Insert("Pos", Pos);
		Node.Insert("Len", Parser.PrevPos - Pos);
	EndIf;
	If Debug Then
		Node.Insert("Str", Mid(Parser.Scanner.Source, Pos, Parser.PrevPos - Pos));
	EndIf;
	Return Node;
EndFunction // Place()

Function Value(Tok, Lit)
	If Tok = Tokens.Number Then
		Return Number(Lit);
	ElsIf Tok = Tokens.DateTime Then
		Return AsDate(Lit);
	ElsIf StrStartsWith(Tok, Tokens.String) Then
		Return Mid(Lit, 2, StrLen(Lit) - 2);
	ElsIf Tok = Tokens.True Then
		Return True;
	ElsIf Tok = Tokens.False Then
		Return False;
	EndIf;
	Return Undefined;
EndFunction // Value()

Function AsDate(DateLit)
	Var List, Char;
	List = New Array;
	For Num = 1 To StrLen(DateLit) Do
		Char = Mid(DateLit, Num, 1);
		If IsDigit(Char) Then
			List.Add(Char);
		EndIf;
	EndDo;
	Return Date(StrConcat(List));
EndFunction // AsDate()

Procedure Expect(Parser, Tok)
	If Parser.Tok <> Tok Then
		Error(Parser.Scanner, "Expected " + Tok,, True);
	EndIf;
EndProcedure // Expect()

Function StringToken(Lit)
	If Left(Lit, 1) = """" Then
		If Right(Lit, 1) = """" Then
			Return Tokens.String;
		Else
			Return Tokens.StringBeg;
		EndIf;
	Else // |
		If Right(Lit, 1) = """" Then
			Return Tokens.StringEnd;
		Else
			Return Tokens.StringMid;
		EndIf;
	EndIf;
EndFunction // StringToken()

Function Lookup(Lit)
	Var Tok;
	If Not Keywords.Property(Lit, Tok) Then
		Tok = Tokens.Ident;
	EndIf;
	Return Tok;
EndFunction // Lookup()

Function IsLetter(Char)
	Return Char <> "" And StrFind("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZабвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", Char) > 0;
EndFunction // IsLetter()

Function IsDigit(Char)
	Return "0" <= Char And Char <= "9";
EndFunction // IsDigit()

Procedure Error(Scanner, Note, Pos = Undefined, Stop = False)
	Var ErrorText;
	If Pos = Undefined Then
		Pos = Min(Scanner.Pos - StrLen(Scanner.Lit), Scanner.Len);
	EndIf;
	ErrorText = StrTemplate("[ Ln: %1; Col: %2 ] %3" "%4",
		StrOccurrenceCount(Mid(Scanner.Source, 1, Pos), Chars.LF) + 1,
		Pos - StrFind(Scanner.Source, Chars.LF, SearchDirection.FromEnd, Pos),
		Note,
		Scanner.Path
	);
	If Stop Then
		Raise ErrorText;
	Else
		Message(ErrorText);
	EndIf;
EndProcedure // Error()

#EndRegion // Auxiliary