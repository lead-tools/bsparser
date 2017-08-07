
#Region Constants

Var Keywords;                  // enum
Var Tokens;                    // enum
Var ObjectKinds;               // enum
Var SelectorKinds;             // enum
Var UnaryOperations;           // array (one of Tokens)
Var BasicLiterals;             // array (one of Tokens)
Var RelationalOperators;       // array (one of Tokens)
Var IgnoredTokens;             // array (one of Tokens)
Var InitialTokensOfExpression; // array (one of Tokens)
Var Operators;                 // structure
Var EmptyArray;                // array

#Region PS_Constants

Var PS_Operators; // structure as map[one of Tokens](string)

#EndRegion // PS_Constants

#EndRegion // Constants

#Region Init

Procedure Init()

	InitEnums();

	UnaryOperations = New Array;
	UnaryOperations.Add(Tokens.Add);
	UnaryOperations.Add(Tokens.Sub);
	UnaryOperations.Add(Tokens.Not);

	BasicLiterals = New Array;
	BasicLiterals.Add(Tokens.Number);
	BasicLiterals.Add(Tokens.String);
	BasicLiterals.Add(Tokens.DateTime);
	BasicLiterals.Add(Tokens.True);
	BasicLiterals.Add(Tokens.False);
	BasicLiterals.Add(Tokens.Undefined);

	RelationalOperators = New Array;
	RelationalOperators.Add(Tokens.Eql);
	RelationalOperators.Add(Tokens.Neq);
	RelationalOperators.Add(Tokens.Lss);
	RelationalOperators.Add(Tokens.Gtr);
	RelationalOperators.Add(Tokens.Leq);
	RelationalOperators.Add(Tokens.Geq);

	IgnoredTokens = New Array;
	IgnoredTokens.Add(Tokens.Comment);
	IgnoredTokens.Add(Tokens.Preprocessor);
	IgnoredTokens.Add(Tokens.Directive);

	InitialTokensOfExpression = New Array;
	InitialTokensOfExpression.Add(Tokens.Add);
	InitialTokensOfExpression.Add(Tokens.Sub);
	InitialTokensOfExpression.Add(Tokens.Not);
	InitialTokensOfExpression.Add(Tokens.Ident);
	InitialTokensOfExpression.Add(Tokens.Lparen);
	InitialTokensOfExpression.Add(Tokens.Number);
	InitialTokensOfExpression.Add(Tokens.String);
	InitialTokensOfExpression.Add(Tokens.DateTime);
	InitialTokensOfExpression.Add(Tokens.Ternary);
	InitialTokensOfExpression.Add(Tokens.New);
	InitialTokensOfExpression.Add(Tokens.True);
	InitialTokensOfExpression.Add(Tokens.False);
	InitialTokensOfExpression.Add(Tokens.Undefined);

	Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"=", "<>", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "Or", "And", "Not"
	);

	PS_Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"-eq", "-ne", "-lt", "-gt", "-le", "-ge", "+", "-", "*", "/", "%", "-or", "-and", "!"
	);

	EmptyArray = New Array;

EndProcedure // Init()

Procedure InitEnums()
	Keywords = Keywords();
	Tokens = Tokens(Keywords);
	ObjectKinds = ObjectKinds();
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
		|New.Новый, Execute.Выполнить, Export.Экспорт,
		|True.Истина, False.Ложь, Undefined.Неопределено,
		|Case.Выбор, When.Когда, EndCase.КонецВыбора" // new keywords
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
		//    (       )       [       ]       {       }
		|Lparen, Rparen, Lbrack, Rbrack, Lbrace, Rbrace,
		//     ?      ,       .      :          ;
		|Ternary, Comma, Period, Colon, Semicolon,

		// New statements

		//      +=
		|AddAssign,

		// Other

		//         //             #          &
		|Eof, Comment, Preprocessor, Directive"

	);

	Return Tokens;

EndFunction // Tokens()

Function ObjectKinds() Export
	Var ObjectKinds;

	ObjectKinds = Enum(New Structure,
		"Variable,"
		"Parameter,"
		"Procedure,"
		"Function,"
		"Constructor,"
		"Unknown,"
	);

	Return ObjectKinds;

EndFunction // ObjectKinds()

Function SelectorKinds() Export
	Var SelectorKinds;

	SelectorKinds = Enum(New Structure,
		"Ident,"
		"Index,"
		"Call,"
	);

	Return SelectorKinds;

EndFunction // SelectorKinds()

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
		"Source," // string
		"Len,"    // number
		"Pos,"    // number
		"Tok,"    // string (one of Tokens)
		"Lit,"    // string
		"Char,"   // string
		"Line,"   // number
		"Column," // number
	);

	Scanner.Source = Source;
	Scanner.Len = StrLen(Source);
	Scanner.Line = 1;
	Scanner.Column = 0;
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
		If NextChar(Scanner) = "=" Then
			Lit = "+=";
			Tok = Tokens.AddAssign;
			NextChar(Scanner);
		Else
			Tok = Tokens.Add;
		EndIf;
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
		Lit = ScanComment(Scanner);
		Tok = Tokens.Directive;
	ElsIf Char = "#" Then
		Lit = ScanComment(Scanner);
		Tok = Tokens.Preprocessor;
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
		Scanner.Column = Scanner.Column + 1;
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
			Scanner.Column = 0;
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

Function Module(Decls, Statements)
	Var Module;

	Module = New Structure(
		"Decls,"      // array (one of declarations)
		"Statements," // array (one of statements)
	,
	Decls, Statements);

	Return Module;

EndFunction // Module()

#Region Scope

Function Scope(Outer)
	Var Scope;

	Scope = New Structure(
		"Outer,"   // structure (Scope)
		"Objects," // structure as map[string](Object)
	);

	Scope.Outer = Outer;
	Scope.Objects = New Structure;

	Return Scope;

EndFunction // Scope()

Function Object(Kind, Name, Type = Undefined)
	Var Object;

	Object = New Structure(
		"Kind," // string (one of ObjectKinds)
		"Name," // string
	,
	Kind, Name, Type);

	If Type <> Undefined Then
		Object.Insert("Type", Type); // structure
	EndIf;

	Return Object;

EndFunction // Object()

#EndRegion // Scope

#Region Declarations

Function VarDecl(Object, Init = False, Value = Undefined)
	Var VarDecl;

	VarDecl = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
	,
	"VarDecl", Object);

	If Init Then
		VarDecl.Insert("Value", Value); // structure (one of expressions)
	EndIf;

	Return VarDecl;

EndFunction // VarDecl()

Function VarListDecl(VarList)
	Var VarListDecl;

	VarListDecl = New Structure(
		"NodeType," // string (type of this structure)
		"VarList,"  // array (VarDecl)
	,
	"VarListDecl", VarList);

	Return VarListDecl;

EndFunction // VarListDecl()

Function ProcDecl(Object, Decls, Statements)
	Var ProcDecl;

	ProcDecl = New Structure(
		"NodeType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Decls,"      // array (one of declarations)
		"Statements," // array (one of statements)
	,
	"ProcDecl", Object, Decls, Statements);

	Return ProcDecl;

EndFunction // ProcDecl()

Function FuncDecl(Object, Decls, Statements)
	Var FuncDecl;

	FuncDecl = New Structure(
		"NodeType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Decls,"      // array (one of declarations)
		"Statements," // array (one of statements)
	,
	"FuncDecl", Object, Decls, Statements);

	Return FuncDecl;

EndFunction // FuncDecl()

Function ParamDecl(Object, Init = False, Value = Undefined)
	Var ParamDecl;

	ParamDecl = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
	,
	"ParamDecl", Object);

	If Init Then
		ParamDecl.Insert("Value", Value); // structure (one of expressions)
	EndIf;

	Return ParamDecl;

EndFunction // ParamDecl()

#EndRegion // Declarations

#Region Expressions

Function BasicLitExpr(Kind, Value)
	Var BasicLitExpr;

	BasicLitExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Kind,"     // string (one of Tokens)
		"Value,"    // one of basic types
	,
	"BasicLitExpr", Kind, Value);

	Return BasicLitExpr;

EndFunction // BasicLitExpr()

Function Selector(Kind, Value)
	Var Selector;

	Selector = New Structure(
		"Kind,"  // string (one of SelectorKinds)
		"Value," // string or array (one of expressions)
	,
	Kind, Value);

	Return Selector;

EndFunction // Selector()

Function DesignatorExpr(Object, Selectors, Call)
	Var DesignatorExpr;

	DesignatorExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
		"Call,"     // boolean
	,
	"DesignatorExpr", Object, Call);

	If Selectors.Count() > 0 Then
		DesignatorExpr.Insert("Selectors", Selectors); // array (Selector)
	EndIf;

	Return DesignatorExpr;

EndFunction // DesignatorExpr()

Function UnaryExpr(Operator, Operand)
	Var UnaryExpr;

	UnaryExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Operator," // string (one of Tokens)
		"Operand,"  // one of expressions
	,
	"UnaryExpr", Operator, Operand);

	Return UnaryExpr;

EndFunction // UnaryExpr()

Function BinaryExpr(Left, Operator, Right)
	Var BinaryExpr;

	BinaryExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // one of expressions
		"Operator," // string (one of Tokens)
		"Right,"    // one of expressions
	,
	"BinaryExpr", Left, Operator, Right);

	Return BinaryExpr;

EndFunction // BinaryExpr()

Function RangeExpr(Left, Right)
	Var RangeExpr;

	RangeExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // one of expressions
		"Right,"    // one of expressions
	,
	"RangeExpr", Left, Right);

	Return RangeExpr;

EndFunction // RangeExpr()

Function NewExpr(Constructor)
	Var NewExpr;

	NewExpr = New Structure(
		"NodeType,"    // string (type of this structure)
		"Constructor," // structure (DesignatorExpr) or array (one of expressions)
	,
	"NewExpr", Constructor);

	Return NewExpr;

EndFunction // NewExpr()

Function TernaryExpr(Condition, ThenPart, ElsePart)
	Var TernaryExpr;

	TernaryExpr = New Structure(
		"NodeType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // structure (one of expressions)
		"ElsePart"   // structure (one of expressions)
	,
	"TernaryExpr", Condition, ThenPart, ElsePart);

	Return TernaryExpr;

EndFunction // TernaryExpr()

Function ParenExpr(Expr)
	Var ParenExpr;

	ParenExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // one of expressions
	,
	"ParenExpr", Expr);

	Return ParenExpr;

EndFunction // ParenExpr()

#EndRegion // Expressions

#Region Statements

Function AssignStmt(Left, Right)
	Var AssignStmt;

	AssignStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // array (DesignatorExpr)
		"Right,"    // array (one of expressions)
	,
	"AssignStmt", Left, Right);

	Return AssignStmt;

EndFunction // AssignStmt()

Function AddAssignStmt(Left, Right)
	Var AddAssignStmt;

	AddAssignStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // array (DesignatorExpr)
		"Right,"    // array (one of expressions)
	,
	"AddAssignStmt", Left, Right);

	Return AddAssignStmt;

EndFunction // AddAssignStmt()

Function ReturnStmt(ExprList)
	Var ReturnStmt;

	ReturnStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"ReturnStmt");

	If ExprList <> Undefined Then
		ReturnStmt.Insert("ExprList", ExprList); // array (one of expressions)
	EndIf;

	Return ReturnStmt;

EndFunction // ReturnStmt()

Function BreakStmt()
	Var BreakStmt;

	BreakStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"BreakStmt");

	Return BreakStmt;

EndFunction // BreakStmt()

Function ContinueStmt()
	Var ContinueStmt;

	ContinueStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"ContinueStmt");

	Return ContinueStmt;

EndFunction // ContinueStmt()

Function RaiseStmt(Expr = Undefined)
	Var RaiseStmt;

	RaiseStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"RaiseStmt");

	If Expr <> Undefined Then
		RaiseStmt.Insert("Expr", Expr); // structure (one of expressions)
	EndIf;

	Return RaiseStmt;

EndFunction // RaiseStmt()

Function ExecuteStmt(Expr)
	Var ExecuteStmt;

	ExecuteStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // structure (one of expressions)
	,
	"ExecuteStmt", Expr);

	Return ExecuteStmt;

EndFunction // ExecuteStmt()

Function CallStmt(DesignatorExpr)
	Var CallStmt;

	CallStmt = New Structure(
		"NodeType,"       // string (type of this structure)
		"DesignatorExpr," // structure (DesignatorExpr)
	,
	"CallStmt", DesignatorExpr);

	Return CallStmt;

EndFunction // CallStmt()

Function IfStmt(Condition, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined)
	Var IfStmt;

	IfStmt = New Structure(
		"NodeType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // array (one of statements)
	,
	"IfStmt", Condition, ThenPart);

	If ElsIfPart <> Undefined Then
		IfStmt.Insert("ElsIfPart", ElsIfPart); // array (IfStmt)
	EndIf;

	If ElsePart <> Undefined Then
		IfStmt.Insert("ElsePart", ElsePart); // array (one of statements)
	EndIf;

	Return IfStmt;

EndFunction // IfStmt()

Function WhileStmt(Condition, Statements)
	Var WhileStmt;

	WhileStmt = New Structure(
		"NodeType,"   // string (type of this structure)
		"Condition,"  // structure (one of expressions)
		"Statements," // array (one of statements)
	,
	"WhileStmt", Condition, Statements);

	Return WhileStmt;

EndFunction // WhileStmt()

Function ForStmt(DesignatorExpr, Collection, Statements)
	Var ForStmt;

	ForStmt = New Structure(
		"NodeType,"       // string (type of this structure)
		"DesignatorExpr," // structure (DesignatorExpr)
		"Collection,"     // structure (one of expressions)
		"Statements,"     // array (one of statements)
	,
	"ForStmt", DesignatorExpr, Collection, Statements);

	Return ForStmt;

EndFunction // ForStmt()

Function CaseStmt(DesignatorExpr, WhenPart, ElsePart = Undefined)
	Var CaseStmt;

	CaseStmt = New Structure(
		"NodeType,"       // string (type of this structure)
		"DesignatorExpr," // structure (one of expressions)
		"WhenPart,"       // array (IfStmt)
	,
	"CaseStmt", DesignatorExpr, WhenPart);

	If ElsePart <> Undefined Then
		CaseStmt.Insert("ElsePart", ElsePart); // array (one of statements)
	EndIf;

	Return CaseStmt;

EndFunction // CaseStmt()

Function TryStmt(TryPart, ExceptPart)
	Var TryStmt;

	TryStmt = New Structure(
		"NodeType,"   // string (type of this structure)
		"TryPart,"    // array (one of statements)
		"ExceptPart," // array (one of statements)
	,
	"TryStmt", TryPart, ExceptPart);

	Return TryStmt;

EndFunction // TryStmt()

#EndRegion // Statements

#Region Types

Function Signature(ParamList)
	Var Signature;

	Signature = New Structure(
		"NodeType,"  // string (type of this structure)
		"ParamList," // array (boolean)
	,
	"Signature", ParamList);

	Return Signature;
EndFunction // Signature()

#EndRegion // Types

#EndRegion // AbstractSyntaxTree

#Region Parser

Function Parser(Source) Export
	Var Parser;

	Parser = New Structure(
		"Scanner," // structure (Scanner)
		"Tok,"     // string (one of Tokens)
		"Lit,"     // string
		"Val,"     // number, string, date, true, false, undefined
		"Scope,"   // structure (Scope)
		"Imports," // structure
		"Module,"  // structure (Module)
		"Unknown," // structure as map[string](Object)
		"IsFunc,"  // boolean
	);

	Parser.Scanner = Scanner(Source);
	Parser.Scope = Scope(Undefined);
	Parser.Imports = New Structure;
	Parser.Unknown = New Structure;
	Parser.IsFunc = False;

	Parser.Scope.Objects.Insert("Structure", Object("Constructor", "Structure"));

	Return Parser;

EndFunction // Parser()

Function Next(Parser)
	Var Tok, Lit;
	Tok = Scan(Parser.Scanner);
	While IgnoredTokens.Find(Tok) <> Undefined Do
		Tok = Scan(Parser.Scanner);
	EndDo;
	If Tok = Tokens.StringBeg Then
		Lit = ParseString(Parser);
		Tok = Tokens.String;
	Else
		Lit = Parser.Scanner.Lit;
	EndIf;
	Parser.Tok = Tok;
	Parser.Lit = Lit;
	Parser.Val = Value(Tok, Lit);
	Return Parser.Tok;
EndFunction // Next()

Function SkipIgnoredTokens(Parser)
	Var Tok;
	Tok = Parser.Tok;
	If IgnoredTokens.Find(Tok) <> Undefined Then
		Tok = Next(Parser);
	EndIf;
	Return Tok;
EndFunction // SkipIgnoredTokens()

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
	Return Scope;
EndFunction // OpenScope()

Function CloseScope(Parser)
	Var Scope;
	Scope = Parser.Scope.Outer;
	Parser.Scope = Scope;
	Return Scope;
EndFunction // CloseScope()

Function ParseString(Parser)
	Var Scanner, Tok, List;
	Scanner = Parser.Scanner;
	List = New Array;
	List.Add(Scanner.Lit);
	Tok = Scan(Scanner);
	While Tok = Tokens.Comment Do
		Tok = Scan(Scanner);
	EndDo;
	While Tok = Tokens.StringMid Do
		List.Add(Mid(Scanner.Lit, 2));
		Tok = Scan(Scanner);
		While Tok = Tokens.Comment Do
			Tok = Scan(Scanner);
		EndDo;
	EndDo;
	Expect(Scanner, Tokens.StringEnd);
	List.Add(Mid(Scanner.Lit, 2));
	Return StrConcat(List);
EndFunction // ParseString()

Function ParseUnaryExpr(Parser)
	Var Operator;
	Operator = Parser.Tok;
	If UnaryOperations.Find(Parser.Tok) <> Undefined Then
		Next(Parser);
		Return UnaryExpr(Operator, ParseOperand(Parser));
	ElsIf Parser.Tok = Tokens.Eof Then
		Return Undefined;
	EndIf;
	Return ParseOperand(Parser);
EndFunction // ParseUnaryExpr()

Function ParseOperand(Parser)
	Var Tok, StrList, Operand;
	Tok = Parser.Tok;
	If BasicLiterals.Find(Tok) <> Undefined Then
		If Tok = Tokens.String Then
			StrList = New Array;
			StrList.Add(Parser.Val);
			While Next(Parser) = Tokens.String Do
				StrList.Add(Parser.Val);
			EndDo;
			Operand = BasicLitExpr(Tok, StrConcat(StrList, Chars.LF));
		Else
			Operand = BasicLitExpr(Tok, Parser.Val);
			Next(Parser);
		EndIf;
	ElsIf Tok = Tokens.Ident Then
		Operand = ParseDesignatorExpr(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Next(Parser);
		Operand = ParenExpr(ParseExpression(Parser));
		Expect(Parser, Tokens.Rparen);
		Next(Parser);
	ElsIf Tok = Tokens.New Then
		Operand = ParseNewExpr(Parser);
	ElsIf Tok = Tokens.Ternary Then
		Operand = ParseTernaryExpr(Parser);
	Else
		Raise "Expected operand";
	EndIf;
	Return Operand;
EndFunction // ParseOperand()

Function ParseNewExpr(Parser)
	Var Tok, Constructor;
	Tok = Next(Parser);
	If Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Constructor = EmptyArray;
		Else
			Constructor = ParseExprList(Parser, True);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Next(Parser);
	Else
		Constructor = ParseDesignatorExpr(Parser);
	EndIf;
	Return NewExpr(Constructor);
EndFunction // ParseNewExpr()

Function ParseDesignatorExpr(Parser, AllowNewVar = False)
	Var Object, Selector, List, Call, Name, Column;
	Object = ParseQualident(Parser);
	If Object = Undefined Then
		Column = Parser.Scanner.Column - StrLen(Parser.Lit);
	EndIf;
	Name = Parser.Lit;
	List = New Array;
	Call = False;
	Selector = ParseSelector(Parser);
	While Selector <> Undefined Do
		List.Add(Selector);
		Call = (Selector.Kind = "Call");
		Selector = ParseSelector(Parser);
	EndDo;
	If Object = Undefined Then
		If Call Then
			If Not Parser.Unknown.Property(Name, Object) Then
				Object = Object("Unknown", Name);
				Parser.Unknown.Insert(Name, Object);
			EndIf;
		Else
			If AllowNewVar Then
				Object = Object(ObjectKinds.Variable, Name);
				Parser.Scope.Objects.Insert(Name, Object);
			Else
				Object = Object("Unknown", Name);
				If Verbose Then
					Error(Parser.Scanner, StrTemplate("Undeclared identifier `%1`", Name), Column);
				EndIf;
			EndIf;
		EndIf;
	EndIf;
	Return DesignatorExpr(Object, List, Call);
EndFunction // ParseDesignatorExpr()

Function ParseDesignatorExprList(Parser, AllowNewVar = False)
	Var List;
	List = New Array;
	List.Add(ParseDesignatorExpr(Parser, AllowNewVar));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		List.Add(ParseDesignatorExpr(Parser, AllowNewVar));
	EndDo;
	Return List;
EndFunction // ParseDesignatorExprList()

Function ParseQualident(Parser)
	Var Module, Object;
	Parser.Imports.Property(Parser.Lit, Module);
	If Module <> Undefined Then
		Next(Parser);
		Expect(Parser, Tokens.Period);
		Next(Parser);
		Expect(Parser, Tokens.Ident);
		Module.Objects.Property(Parser.Lit, Object);
	Else
		Object = FindObject(Parser, Parser.Lit);
	EndIf;
	Return Object;
EndFunction // ParseQualident()

Function ParseSelector(Parser)
	Var Tok, Value;
	Tok = Next(Parser);
	If Tok = Tokens.Period Then
		Next(Parser);
		If Not Keywords.Property(Parser.Lit) Then
			Expect(Parser, Tokens.Ident);
		EndIf;
		Value = Parser.Lit;
		Return Selector("Ident", Value);
	ElsIf Tok = Tokens.Lbrack Then
		Tok = Next(Parser);
		If Tok = Tokens.Rbrack Then
			Error(Parser.Scanner, "Expected expression",, True);
		EndIf;
		Value = ParseExprList(Parser);
		Expect(Parser, Tokens.Rbrack);
		Return Selector("Index", Value);
	ElsIf Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Value = EmptyArray;
		Else
			Value = ParseExprList(Parser, True);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Return Selector("Call", Value);
	EndIf;
	Return Undefined;
EndFunction // ParseSelector()

Function ParseExpression(Parser)
	Var Expr, Operator;
	Expr = ParseAndExpr(Parser);
	While Parser.Tok = Tokens.Or Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseAndExpr(Parser));
	EndDo;
	Return Expr;
EndFunction // ParseExpression()

Function ParseAndExpr(Parser)
	Var Expr, Operator;
	Expr = ParseRelExpr(Parser);
	While Parser.Tok = Tokens.And Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseRelExpr(Parser));
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseRelExpr(Parser)
	Var Expr, Operator;
	Expr = ParseAddExpr(Parser);
	While RelationalOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseAddExpr(Parser));
	EndDo;
	Return Expr;
EndFunction // ParseRelExpr()

Function ParseAddExpr(Parser)
	Var Expr, Operator;
	Expr = ParseMulExpr(Parser);
	While Parser.Tok = Tokens.Add Or Parser.Tok = Tokens.Sub Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseMulExpr(Parser));
	EndDo;
	Return Expr;
EndFunction // ParseAddExpr()

Function ParseMulExpr(Parser)
	Var Expr, Operator;
	Expr = ParseUnaryExpr(Parser);
	While Parser.Tok = Tokens.Mul Or Parser.Tok = Tokens.Div Or Parser.Tok = Tokens.Mod Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseUnaryExpr(Parser));
	EndDo;
	Return Expr;
EndFunction // ParseMulExpr()

Function ParseExprList(Parser, IsArguments = False)
	Var ExprList, ExpectExpression;
	ExprList = New Array;
	If IsArguments Then
		ExpectExpression = True;
		While ExpectExpression Do
			If InitialTokensOfExpression.Find(Parser.Tok) <> Undefined Then
				ExprList.Add(ParseExpression(Parser));
			Else
				ExprList.Add(BasicLitExpr(Tokens.Undefined, Undefined));
			EndIf;
			If Parser.Tok = Tokens.Comma Then
				Next(Parser);
			Else
				ExpectExpression = False;
			EndIf;
		EndDo;
	Else
		If Parser.Tok = Tokens.Comma Then
			ExprList.Add(Undefined);
		Else
			ExprList.Add(ParseExpression(Parser));
		EndIf;
		While Parser.Tok = Tokens.Comma Do
			Next(Parser);
			While Parser.Tok = Tokens.Comma Do
				ExprList.Add(Undefined);
				Next(Parser);
			EndDo;
			ExprList.Add(ParseExpression(Parser));
		EndDo;
	EndIf;
	Return ExprList;
EndFunction // ParseExprList()

Function ParseTernaryExpr(Parser)
	Var Condition, ThenPart, ElsePart;
	Next(Parser);
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	Condition = ParseExpression(Parser);
	Expect(Parser, Tokens.Comma);
	Next(Parser);
	ThenPart = ParseExpression(Parser);
	Expect(Parser, Tokens.Comma);
	Next(Parser);
	ElsePart = ParseExpression(Parser);
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	Return TernaryExpr(Condition, ThenPart, ElsePart);
EndFunction // ParseTernaryExpr()

Function ParseFuncDecl(Parser)
	Var Scope, Object, Name, Decls;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	ScopeObjects = Parser.Scope.Objects;
	OpenScope(Parser);
	Name = Parser.Lit;
	Next(Parser);
	If Parser.Unknown.Property(Name, Object) Then
		Object.Kind = ObjectKinds.Function;
		Object.Insert("Type", ParseSignature(Parser));
		Parser.Unknown.Delete(Name);
	Else
		Object = Object(ObjectKinds.Function, Name, ParseSignature(Parser));
	EndIf;
	ScopeObjects.Insert(Name, Object);
	Decls = ParseVarDecls(Parser);
	Parser.IsFunc = True;
	Statements = ParseStatements(Parser);
	Parser.IsFunc = False;
	Expect(Parser, Tokens.EndFunction);
	CloseScope(Parser);
	Next(Parser);
	Return FuncDecl(Object, Decls, Statements);
EndFunction // ParseFuncDecl()

Function ParseSignature(Parser)
	Var ParamList;
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	If Parser.Tok = Tokens.Rparen Then
		ParamList = EmptyArray;
	Else
		ParamList = ParseParamList(Parser);
	EndIf;
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	If Parser.Tok = Tokens.Export Then
		If Verbose Then
			Error(Parser.Scanner, "keyword `Export` ignored");
		EndIf;
		Next(Parser);
	EndIf;
	Return Signature(ParamList);
EndFunction // ParseSignature()

Function ParseProcDecl(Parser)
	Var Scope, Object, Name, Decls;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	ScopeObjects = Parser.Scope.Objects;
	OpenScope(Parser);
	Name = Parser.Lit;
	Next(Parser);
	If Parser.Unknown.Property(Name, Object) Then
		Object.Kind = ObjectKinds.Procedure;
		Object.Insert("Type", ParseSignature(Parser));
		Parser.Unknown.Delete(Name);
	Else
		Object = Object(ObjectKinds.Procedure, Name, ParseSignature(Parser));
	EndIf;
	ScopeObjects.Insert(Name, Object);
	Decls = ParseVarDecls(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndProcedure);
	CloseScope(Parser);
	Next(Parser);
	Return ProcDecl(Object, Decls, Statements);
EndFunction // ParseProcDecl()

Function ParseReturnStmt(Parser)
	Var ExprList;
	Next(Parser);
	If Parser.IsFunc Then
		ExprList = ParseExprList(Parser);
	EndIf;
	Return ReturnStmt(ExprList);
EndFunction // ParseReturnStmt()

Function ParseVarListDecl(Parser)
	Var VarList;
	VarList = New Array;
	VarList.Add(ParseVarDecl(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVarDecl(Parser));
	EndDo;
	If Parser.Tok = Tokens.Export Then
		If Verbose Then
			Error(Parser.Scanner, "keyword `Export` ignored");
		EndIf;
		Next(Parser);
	EndIf;
	Return VarListDecl(VarList);
EndFunction // ParseVarListDecl()

Function ParseVarDecl(Parser)
	Var Tok, Name, Object, VarDecl;
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Tok = Next(Parser);
	If Tok = Tokens.Eql Then
		Tok = Next(Parser);
		If BasicLiterals.Find(Tok) = Undefined Then
			Error(Parser.Scanner, "expected basic literal");
		EndIf;
		Object = Object(ObjectKinds.Variable, Name, Tok);
		VarDecl = VarDecl(Object, True, ParseOperand(Parser));
	Else
		Object = Object(ObjectKinds.Variable, Name, Undefined);
		VarDecl = VarDecl(Object);
	EndIf;
	Parser.Scope.Objects.Insert(Name, Object);
	Return VarDecl;
EndFunction // ParseVarDecl()

Function ParseParamList(Parser)
	Var ParamList;
	ParamList = New Array;
	ParamList.Add(ParseParamDecl(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		ParamList.Add(ParseParamDecl(Parser));
	EndDo;
	Return ParamList;
EndFunction // ParseParamList()

Function ParseParamDecl(Parser)
	Var Tok, Name, Object, ParamDecl;
	If Parser.Tok = Tokens.Val Then
		If Verbose Then
			Error(Parser.Scanner, "keyword `Val` ignored");
		EndIf;
		Next(Parser);
	EndIf;
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Tok = Next(Parser);
	If Tok = Tokens.Eql Then
		Tok = Next(Parser);
		If BasicLiterals.Find(Tok) = Undefined Then
			Error(Parser.Scanner, "expected basic literal");
		EndIf;
		Object = Object(ObjectKinds.Parameter, Name, Tok);
		ParamDecl = ParamDecl(Object, True, ParseOperand(Parser));
	Else
		Object = Object(ObjectKinds.Parameter, Name, Undefined);
		ParamDecl = ParamDecl(Object);
	EndIf;
	Parser.Scope.Objects.Insert(Name, Object);
	Return ParamDecl;
EndFunction // ParseParamDecl()

Function ParseStatements(Parser)
	Var Statements, Stmt;
	Statements = New Array;
	Stmt = ParseStmt(Parser);
	While Stmt <> Undefined Do
		Statements.Add(Stmt);
		Stmt = ParseStmt(Parser);
	EndDo;
	Return Statements;
EndFunction // ParseStatements()

Function ParseStmt(Parser)
	Var Tok;
	Tok = SkipIgnoredTokens(Parser);
	While Tok = Tokens.Semicolon Do
		Next(Parser);
		Tok = SkipIgnoredTokens(Parser);
	EndDo;
	If Tok = Tokens.Ident Then
		Return ParseAssignOrCallStmt(Parser);
	ElsIf Tok = Tokens.If Then
		Return ParseIfStmt(Parser);
	ElsIf Tok = Tokens.Try Then
		Return ParseTryStmt(Parser);
	ElsIf Tok = Tokens.While Then
		Return ParseWhileStmt(Parser);
	ElsIf Tok = Tokens.For Then
		Return ParseForStmt(Parser);
	ElsIf Tok = Tokens.Case Then
		Return ParseCaseStmt(Parser);
	ElsIf Tok = Tokens.Return Then
		Return ParseReturnStmt(Parser);
	ElsIf Tok = Tokens.Break Then
		Next(Parser);
		Return BreakStmt();
	ElsIf Tok = Tokens.Continue Then
		Next(Parser);
		Return ContinueStmt();
	ElsIf Tok = Tokens.Raise Then
		Return ParseRaiseStmt(Parser);
	ElsIf Tok = Tokens.Execute Then
		Return ParseExecuteStmt(Parser);
	EndIf;
	Return Undefined;
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
	Var Tok, Expr;
	Next(Parser);
	Expect(Parser, Tokens.Lparen);
	Tok = Next(Parser);
	If Tok = Tokens.Rparen Then
		Expr = EmptyArray;
	Else
		Expr = ParseExpression(Parser);
	EndIf;
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	Return ExecuteStmt(Expr);
EndFunction // ParseExecuteStmt()

Function ParseAssignOrCallStmt(Parser)
	Var Tok, Left, Right;
	Left = ParseDesignatorExprList(Parser, True);
	If Left.Count() = 1 And Left[0].Call Then
		Return CallStmt(Left);
	EndIf;
	Tok = Parser.Tok;
	If Tok = Tokens.Eql Then
		Next(Parser);
		Right = ParseExprList(Parser);
		Return AssignStmt(Left, Right);
	ElsIf Tok = Tokens.AddAssign Then
		Next(Parser);
		Right = ParseExprList(Parser);
		If Left.Count() <> Right.Count() Then
			Error(Parser.Scanner, "arrays have different number of elements",, True);
		EndIf; 
		Return AddAssignStmt(Left, Right);
	EndIf;
	Expect(Parser, Tokens.Eql);
EndFunction // ParseAssignOrCallStmt()

Function ParseIfStmt(Parser)
	Var Tok, Condition, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen;
	Next(Parser);
	Condition = ParseExpression(Parser);
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
	Return IfStmt(Condition, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParseIfStmt()

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

Function ParseCaseStmt(Parser)
	Var Tok, DesignatorExpr, ElsePart;
	Var WhenPart, WhenCond, WhenThen;
	Next(Parser);
	DesignatorExpr = ParseDesignatorExpr(Parser);
	Tok = Parser.Tok;
	WhenPart = New Array;
	While Tok = Tokens.When Do
		Next(Parser);
		WhenCond = ParseExpression(Parser);
		Expect(Parser, Tokens.Then);
		Next(Parser);
		WhenThen = ParseStatements(Parser);
		WhenPart.Add(IfStmt(WhenCond, WhenThen));
		Tok = Parser.Tok;
	EndDo;
	If Tok = Tokens.Else Then
		Next(Parser);
		ElsePart = ParseStatements(Parser);
	EndIf;
	Expect(Parser, Tokens.EndCase);
	Next(Parser);
	Return CaseStmt(DesignatorExpr, WhenPart, ElsePart);
EndFunction // ParseCaseStmt()

Function ParseWhileStmt(Parser)
	Var Condition, Statements;
	Next(Parser);
	Condition = ParseExpression(Parser);
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo);
	Next(Parser);
	Return WhileStmt(Condition, Statements)
EndFunction // ParseWhileStmt()

Function ParseForStmt(Parser)
	Var DesignatorExpr, Left, Right, Collection, Statements;
	Next(Parser);
	If Parser.Tok = Tokens.Each Then
		Next(Parser);
	EndIf;
	Expect(Parser, Tokens.Ident);
	DesignatorExpr = ParseDesignatorExpr(Parser, True);
	If DesignatorExpr.Call Then
		Error(Parser.Scanner, "expected variable",, True);
	EndIf;
	If Parser.Tok = Tokens.Eql Then
		Next(Parser);
		Left = ParseExpression(Parser);
		Expect(Parser, Tokens.To);
		Next(Parser);
		Right = ParseExpression(Parser);
		Collection = RangeExpr(Left, Right);
	ElsIf Parser.Tok = Tokens.In Then
		Next(Parser);
		Collection = ParseExpression(Parser);
	EndIf;
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo);
	Next(Parser);
	Return ForStmt(DesignatorExpr, Collection, Statements);
EndFunction // ParseForStmt()

Function ParseVarDecls(Parser)
	Var Tok, Decls;
	Decls = New Array;
	Tok = Parser.Tok;
	While Tok = Tokens.Var Do
		Next(Parser);
		Decls.Add(ParseVarListDecl(Parser));
		If Parser.Tok = Tokens.Semicolon Then
			Next(Parser);
		EndIf;
		Tok = Parser.Tok;
	EndDo;
	Return Decls;
EndFunction // ParseVarDecls()

Function ParseDecls(Parser)
	Var Tok, Decls;
	Decls = New Array;
	Tok = Parser.Tok;
	While Tok <> Tokens.Eof Do
		If Tok = Tokens.Var Then
			Next(Parser);
			Decls.Add(ParseVarListDecl(Parser));
			If Parser.Tok = Tokens.Semicolon Then
				Next(Parser);
			EndIf;
		ElsIf Tok = Tokens.Function Then
			Decls.Add(ParseFuncDecl(Parser));
		ElsIf Tok = Tokens.Procedure Then
			Decls.Add(ParseProcDecl(Parser));
		Else
			Return Decls;
		EndIf;
		Tok = Parser.Tok;
	EndDo;
	Return Decls;
EndFunction // ParseDecls()

Function ParseModule(Parser) Export
	Next(Parser);
	Parser.Module = Module(ParseDecls(Parser), ParseStatements(Parser));
	If Verbose Then
		For Each Item In Parser.Unknown Do
			Message(StrTemplate("Undeclared identifier `%1`", Item.Key));
		EndDo;
	EndIf;
	Expect(Parser, Tokens.Eof);
EndFunction // ParseModule()

#EndRegion // Parser

#Region Auxiliary

Function Value(Tok, Lit)
	If Tok = Tokens.Number Then
		Return Number(Lit);
	ElsIf Tok = Tokens.DateTime Then
		Return AsDate(Lit);
	ElsIf Tok = Tokens.String Then
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

Procedure Error(Scanner, Note, Column = Undefined, Stop = False)
	Var ErrorText;
	ErrorText = StrTemplate("[ Ln: %1; Col: %2 ] %3",
		Scanner.Line,
		?(Column = Undefined, Scanner.Column - StrLen(Scanner.Lit), Column),
		Note
	);
	If Stop Then
		Raise ErrorText;
	Else
		Message(ErrorText);
	EndIf;
EndProcedure // Error()

#EndRegion // Auxiliary

#Region Backends

Function Backend() Export
	Var Backend;

	Backend = New Structure(
		"Result," // array (string)
		"Indent," // number
	,
	New Array, -1);

	Return Backend;

EndFunction // Backend()

Procedure Indent(Backend)
	Var Result;
	Result = Backend.Result;
	For Index = 1 To Backend.Indent Do
		Result.Add(Chars.Tab);
	EndDo;
EndProcedure // Indent()

#Region BSL

Procedure BSL_VisitModule(Backend, Module) Export
	BSL_VisitDecls(Backend, Module.Decls);
	BSL_VisitStatements(Backend, Module.Statements);
EndProcedure // BSL_VisitModule()

Procedure BSL_VisitDecls(Backend, Decls)
	Backend.Indent = Backend.Indent + 1;
	For Each Decl In Decls Do
		BSL_VisitDecl(Backend, Decl);
	EndDo;
	Backend.Indent = Backend.Indent - 1;
EndProcedure // BSL_VisitDecls()

Procedure BSL_VisitStatements(Backend, Statements)
	Backend.Indent = Backend.Indent + 1;
	For Each Stmt In Statements Do
		BSL_VisitStmt(Backend, Stmt);
	EndDo;
	Backend.Indent = Backend.Indent - 1;
	Indent(Backend);
EndProcedure // BSL_VisitStatements()

Procedure BSL_VisitDecl(Backend, Decl)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Decl.NodeType;
	If NodeType = "VarListDecl" Then
		Indent(Backend);
		Result.Add("Var ");
		BSL_VisitVarListDecl(Backend, Decl.VarList);
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "FuncDecl" Or NodeType = "ProcDecl" Then
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent + 1;
		If NodeType = "FuncDecl" Then
			Result.Add("Function ");
		Else
			Result.Add("Procedure ");
		EndIf;
		Result.Add(Decl.Object.Name);
		Result.Add("(");
		BSL_VisitVarListDecl(Backend, Decl.Object.Type.ParamList);
		Result.Add(")");
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			BSL_VisitDecl(Backend, Stmt);
		EndDo;
		For Each Stmt In Decl.Statements Do
			BSL_VisitStmt(Backend, Stmt);
		EndDo;
		If NodeType = "FuncDecl" Then
			Result.Add(StrTemplate("EndFunction // %1()", Decl.Object.Name));
		Else
			Result.Add(StrTemplate("EndProcedure // %1()", Decl.Object.Name));
		EndIf;
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent - 1;
	EndIf;
EndProcedure // BSL_VisitDecl()

Procedure BSL_VisitVarListDecl(Backend, VarListDecl)
	Var Result, Buffer;
	If VarListDecl <> Undefined Then
		Result = Backend.Result;
		Buffer = New Array;
		For Each VarDecl In VarListDecl Do
			Buffer.Add(VarDecl.Object.Name + ?(VarDecl.Property("Value"), " = " + BSL_VisitExpr(VarDecl.Value), ""));
		EndDo;
		If Buffer.Count() > 0 Then
			Result.Add(StrConcat(Buffer, ", "));
		EndIf;
	EndIf;
EndProcedure // BSL_VisitVarListDecl()

Procedure BSL_VisitStmt(Backend, Stmt)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Stmt.NodeType;
	Indent(Backend);
	If NodeType = "AssignStmt" Then
		Result.Add(BSL_VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" = ");
		Result.Add(BSL_VisitExprList(Stmt.Right));
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "AddAssignStmt" Then
		Result.Add(BSL_VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" = ");
		Result.Add(BSL_VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" + ");
		Result.Add(BSL_VisitExprList(Stmt.Right));
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("Return ");
		If Stmt.Property("ExprList") Then
			Result.Add(BSL_VisitExprList(Stmt.ExprList));
		EndIf;
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "BreakStmt" Then
		Result.Add("Break;");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ContinueStmt" Then
		Result.Add("Continue;");
		Result.Add(Chars.LF);
	ElsIf NodeType = "RaiseStmt" Then
		Result.Add("Raise ");
		If Stmt.Property("Expr") Then
			Result.Add(BSL_VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ExecuteStmt" Then
		Result.Add("Execute(");
		Result.Add(BSL_VisitExpr(Stmt.Expr));
		Result.Add(");");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CallStmt" Then
		Result.Add(BSL_VisitDesignatorExpr(Stmt.DesignatorExpr[0]));
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "IfStmt" Then
		Result.Add("If ");
		BSL_VisitIfStmt(Backend, Stmt);
		If Stmt.Property("ElsePart") Then
			Result.Add("Else");
			Result.Add(Chars.LF);
			BSL_VisitStatements(Backend, Stmt.ElsePart);
		EndIf;
		Result.Add("EndIf");
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("While ");
		Result.Add(BSL_VisitExpr(Stmt.Condition));
		Result.Add(" Do");
		Result.Add(Chars.LF);
		BSL_VisitStatements(Backend, Stmt.Statements);
		Result.Add("EndDo");
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ForStmt" Then
		Result.Add("For ");
		If Stmt.Collection.NodeType = "RangeExpr" Then
			Result.Add(BSL_VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			Result.Add(BSL_VisitExpr(Stmt.Collection));
		Else
			Result.Add("Each ");
			Result.Add(BSL_VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" In ");
			Result.Add(BSL_VisitExpr(Stmt.Collection));
		EndIf;
		Result.Add(" Do");
		Result.Add(Chars.LF);
		BSL_VisitStatements(Backend, Stmt.Statements);
		Result.Add("EndDo");
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CaseStmt" Then
		If Stmt.WhenPart.Count() > 0 Then
			Result.Add("If ");
			Result.Add(BSL_VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			IfStmt = Stmt.WhenPart[0];
			BSL_VisitIfStmt(Backend, IfStmt);
			For Index = 1 To Stmt.WhenPart.Count() - 1 Do
				IfStmt = Stmt.WhenPart[Index];
				Result.Add("ElsIf ");
				Result.Add(BSL_VisitDesignatorExpr(Stmt.DesignatorExpr));
				Result.Add(" = ");
				BSL_VisitIfStmt(Backend, IfStmt);
			EndDo;
			If Stmt.Property("ElsePart") Then
				Result.Add("Else");
				Result.Add(Chars.LF);
				BSL_VisitStatements(Backend, Stmt.ElsePart);
			EndIf;
			Result.Add("EndIf");
			Result.Add(";");
			Result.Add(Chars.LF);
		Else
			Result.Add(Chars.LF);
			Backend.Indent = Backend.Indent - 1;
			If Stmt.Property("ElsePart") Then
				BSL_VisitStatements(Backend, Stmt.ElsePart);
			EndIf;
			Backend.Indent = Backend.Indent + 1;
			Result.Add(Chars.LF);
		EndIf;
	ElsIf NodeType = "TryStmt" Then
		Result.Add("Try");
		Result.Add(Chars.LF);
		BSL_VisitStatements(Backend, Stmt.TryPart);
		Result.Add("Except");
		Result.Add(Chars.LF);
		BSL_VisitStatements(Backend, Stmt.ExceptPart);
		Result.Add("EndTry");
		Result.Add(";");
		Result.Add(Chars.LF);
	EndIf;
EndProcedure // BSL_VisitStmt()

Procedure BSL_VisitIfStmt(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(BSL_VisitExpr(IfStmt.Condition));
	Result.Add(" Then");
	Result.Add(Chars.LF);
	BSL_VisitStatements(Backend, IfStmt.ThenPart);
	If IfStmt.Property("ElsIfPart") Then
		For Each Item In IfStmt.ElsIfPart Do
			Result.Add("ElsIf ");
			BSL_VisitIfStmt(Backend, Item);
		EndDo;
	EndIf;
EndProcedure // BSL_VisitIfStmt()

Function BSL_VisitExprList(ExprList)
	Var Buffer;
	If ExprList <> Undefined Then
		Buffer = New Array;
		For Each Expr In ExprList Do
			If Expr = Undefined Then
				Buffer.Add("");
			Else
				Buffer.Add(BSL_VisitExpr(Expr));
			EndIf;
		EndDo;
		Return StrConcat(Buffer, ", ");
	EndIf;
EndFunction // BSL_VisitExprList()

Function BSL_VisitExpr(Expr)
	Var NodeType, BasicLitKind;
	NodeType = Expr.NodeType;
	If NodeType = "BasicLitExpr" Then
		BasicLitKind = Expr.Kind;
		If BasicLitKind = Tokens.String Then
			Return StrTemplate("""%1""", StrReplace(Expr.Value, Chars.LF, """ """));
		ElsIf BasicLitKind = Tokens.Number Then
			Return Format(Expr.Value, "NZ=0; NG=");
		ElsIf BasicLitKind = Tokens.DateTime Then
			Return Format(Expr.Value, "DF='""''yyyyMMdd'''");
		ElsIf BasicLitKind = Tokens.True Or BasicLitKind = Tokens.False Then
			Return Format(Expr.Value, "BF=False; BT=True");
		ElsIf BasicLitKind = Tokens.Undefined Then
			Return "Undefined";
		Else
			Raise "Unknown basic literal";
		EndIf;
	ElsIf NodeType = "DesignatorExpr" Then
		Return BSL_VisitDesignatorExpr(Expr);
	ElsIf NodeType = "UnaryExpr" Then
		Return StrTemplate("%1 %2", Operators[Expr.Operator], BSL_VisitExpr(Expr.Operand));
	ElsIf NodeType = "BinaryExpr" Then
		Return StrTemplate("%1 %2 %3", BSL_VisitExpr(Expr.Left), Operators[Expr.Operator], BSL_VisitExpr(Expr.Right));
	ElsIf NodeType = "RangeExpr" Then
		Return StrTemplate("%1 To %2", BSL_VisitExpr(Expr.Left), BSL_VisitExpr(Expr.Right));
	ElsIf NodeType = "NewExpr" Then
		If TypeOf(Expr.Constructor) = Type("Structure") Then
			Return StrTemplate("New %1", BSL_VisitExpr(Expr.Constructor));
		Else
			Return StrTemplate("New(%1)", BSL_VisitExprList(Expr.Constructor));
		EndIf;
	ElsIf NodeType = "TernaryExpr" Then
		Return StrTemplate("?(%1, %2, %3)", BSL_VisitExpr(Expr.Condition), BSL_VisitExpr(Expr.ThenPart), BSL_VisitExpr(Expr.ElsePart));
	ElsIf NodeType = "ParenExpr" Then
		Return StrTemplate("(%1)", BSL_VisitExpr(Expr.Expr));
	EndIf;
EndFunction // BSL_VisitExpr()

Function BSL_VisitDesignatorExpr(DesignatorExpr)
	Var Buffer;
	Buffer = New Array;
	Buffer.Add(DesignatorExpr.Object.Name);
	If DesignatorExpr.Property("Selectors") Then
		For Each Selector In DesignatorExpr.Selectors Do
			If Selector.Kind = "Ident" Then
				Buffer.Add(".");
				Buffer.Add(Selector.Value);
			ElsIf Selector.Kind = "Index" Then
				Buffer.Add("[");
				Buffer.Add(BSL_VisitExprList(Selector.Value));
				Buffer.Add("]");
			ElsIf Selector.Kind = "Call" Then
				Buffer.Add("(");
				Buffer.Add(BSL_VisitExprList(Selector.Value));
				Buffer.Add(")");
			Else
				Raise "Unknown selector kind";
			EndIf;
		EndDo;
	EndIf;
	Return StrConcat(Buffer);
EndFunction // BSL_VisitDesignatorExpr()

#EndRegion // BSL

#Region PS

Procedure PS_VisitModule(Backend, Module) Export
	PS_VisitDecls(Backend, Module.Decls);
	PS_VisitStatements(Backend, Module.Statements);
EndProcedure // PS_VisitModule()

Procedure PS_VisitDecls(Backend, Decls)
	Backend.Indent = Backend.Indent + 1;
	For Each Decl In Decls Do
		PS_VisitDecl(Backend, Decl);
	EndDo;
	Backend.Indent = Backend.Indent - 1;
EndProcedure // PS_VisitDecls()

Procedure PS_VisitStatements(Backend, Statements)
	Backend.Indent = Backend.Indent + 1;
	For Each Stmt In Statements Do
		PS_VisitStmt(Backend, Stmt);
	EndDo;
	Backend.Indent = Backend.Indent - 1;
	Indent(Backend);
EndProcedure // PS_VisitStatements()

Procedure PS_VisitDecl(Backend, Decl)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Decl.NodeType;
	If NodeType = "VarListDecl" Then
		PS_VisitVarListDecl(Backend, Decl.VarList);
	ElsIf NodeType = "FuncDecl" Or NodeType = "ProcDecl" Then
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent + 1;
		Result.Add("function ");
		Result.Add(Decl.Object.Name);
		Result.Add("(");
		PS_VisitParamList(Backend, Decl.Object.Type.ParamList);
		Result.Add(") {");
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			PS_VisitDecl(Backend, Stmt);
		EndDo;
		For Each Stmt In Decl.Statements Do
			PS_VisitStmt(Backend, Stmt);
		EndDo;
		Result.Add("}");
		Result.Add(Chars.LF);
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent - 1;
	EndIf;
EndProcedure // PS_VisitDecl()

Procedure PS_VisitVarListDecl(Backend, VarListDecl)
	Var Result;
	If VarListDecl <> Undefined Then
		Result = Backend.Result;
		For Each VarDecl In VarListDecl Do
			Indent(Backend);
			Result.Add(StrTemplate("New-Variable -Name ""%1""", VarDecl.Object.Name));
			If VarDecl.Property("Value") Then
				Result.Add(" -Value " + PS_VisitExpr(VarDecl.Value));
			EndIf;
			Result.Add(Chars.LF);
		EndDo;
	EndIf;
EndProcedure // PS_VisitVarListDecl()

Procedure PS_VisitParamList(Backend, ParamList)
	Var Result, Buffer;
	If ParamList <> Undefined Then
		Result = Backend.Result;
		Buffer = New Array;
		For Each VarDecl In ParamList Do
			Buffer.Add(StrTemplate("$%1%2", VarDecl.Object.Name, ?(VarDecl.Property("Value"), " = " + PS_VisitExpr(VarDecl.Value), "")));
		EndDo;
		If Buffer.Count() > 0 Then
			Result.Add(StrConcat(Buffer, ", "));
		EndIf;
	EndIf;
EndProcedure // PS_VisitParamList()

Procedure PS_VisitStmt(Backend, Stmt)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Stmt.NodeType;
	Indent(Backend);
	If NodeType = "AssignStmt" Then
		Result.Add(PS_VisitExprList(Stmt.Left, ", "));
		Result.Add(" = ");
		Result.Add(PS_VisitExprList(Stmt.Right, ", "));
		Result.Add(Chars.LF);
	ElsIf NodeType = "AddAssignStmt" Then
		Result.Add(PS_VisitExprList(Stmt.Left, ", "));
		Result.Add(" += ");
		Result.Add(PS_VisitExprList(Stmt.Right, ", "));
		Result.Add(Chars.LF);
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("return ");
		If Stmt.Property("ExprList") Then
			Result.Add(PS_VisitExprList(Stmt.ExprList, ", "));
		EndIf;
		Result.Add(Chars.LF);
	ElsIf NodeType = "BreakStmt" Then
		Result.Add("break");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ContinueStmt" Then
		Result.Add("continue");
		Result.Add(Chars.LF);
	ElsIf NodeType = "RaiseStmt" Then
		Result.Add("throw ");
		If Stmt.Property("Expr") Then
			Result.Add(PS_VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(Chars.LF);
	ElsIf NodeType = "ExecuteStmt" Then
		Result.Add("# Execute(");
		Result.Add(PS_VisitExpr(Stmt.Expr));
		Result.Add(")");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CallStmt" Then
		Result.Add(PS_VisitDesignatorExpr(Stmt.DesignatorExpr[0], False));
		Result.Add(Chars.LF);
	ElsIf NodeType = "IfStmt" Then
		Result.Add("if (");
		PS_VisitIfStmt(Backend, Stmt);
		If Stmt.Property("ElsePart") Then
			Indent(Backend);
			Result.Add("else {");
			Result.Add(Chars.LF);
			PS_VisitStatements(Backend, Stmt.ElsePart);
			Result.Add("}");
			Result.Add(Chars.LF);
		EndIf;
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("while (");
		Result.Add(PS_VisitExpr(Stmt.Condition));
		Result.Add(") {");
		Result.Add(Chars.LF);
		PS_VisitStatements(Backend, Stmt.Statements);
		Result.Add("}");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ForStmt" Then
		If Stmt.Collection.NodeType = "RangeExpr" Then
			Result.Add("for (");
			Result.Add(PS_VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			Result.Add(PS_VisitExpr(Stmt.Collection.Left));
			Result.Add("; ");
			Result.Add(PS_VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" -le ");
			Result.Add(PS_VisitExpr(Stmt.Collection.Right));
			Result.Add(StrTemplate("; %1++ )", PS_VisitDesignatorExpr(Stmt.DesignatorExpr)));
		Else
			Result.Add("foreach (");
			Result.Add(PS_VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" In ");
			Result.Add(PS_VisitExpr(Stmt.Collection));
			Result.Add(")");
		EndIf;
		Result.Add(" {");
		Result.Add(Chars.LF);
		PS_VisitStatements(Backend, Stmt.Statements);
		Result.Add("}");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CaseStmt" Then
		Result.Add("switch ");
		Result.Add(PS_VisitDesignatorExpr(Stmt.DesignatorExpr));
		Result.Add(" {");
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent + 1;
		For Each IfStmt In Stmt.WhenPart Do
			Indent(Backend);
			PS_VisitWhenPart(Backend, IfStmt);
		EndDo;
		If Stmt.Property("ElsePart") Then
			Indent(Backend);
			Result.Add("default {");
			Result.Add(Chars.LF);
			PS_VisitStatements(Backend, Stmt.ElsePart);
			Result.Add("}");
			Result.Add(Chars.LF);
		EndIf;
		Backend.Indent = Backend.Indent - 1;
		Indent(Backend);
		Result.Add("}");
		Result.Add(Chars.LF);
	ElsIf NodeType = "TryStmt" Then
		Result.Add("try {");
		Result.Add(Chars.LF);
		PS_VisitStatements(Backend, Stmt.TryPart);
		Result.Add("}");
		Result.Add(Chars.LF);
		Indent(Backend);
		Result.Add("catch {");
		Result.Add(Chars.LF);
		PS_VisitStatements(Backend, Stmt.ExceptPart);
		Result.Add("}");
		Result.Add(Chars.LF);
	EndIf;
EndProcedure // PS_VisitStmt()

Procedure PS_VisitIfStmt(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(PS_VisitExpr(IfStmt.Condition));
	Result.Add(") {");
	Result.Add(Chars.LF);
	PS_VisitStatements(Backend, IfStmt.ThenPart);
	Result.Add("}");
	Result.Add(Chars.LF);
	If IfStmt.Property("ElsIfPart") Then
		For Each Item In IfStmt.ElsIfPart Do
			Indent(Backend);
			Result.Add("elseif (");
			PS_VisitIfStmt(Backend, Item);
		EndDo;
	EndIf;
EndProcedure // PS_VisitIfStmt()

Procedure PS_VisitWhenPart(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(PS_VisitExpr(IfStmt.Condition));
	Result.Add(" {");
	Result.Add(Chars.LF);
	PS_VisitStatements(Backend, IfStmt.ThenPart);
	Result.Add("}");
	Result.Add(Chars.LF);
EndProcedure // PS_VisitWhenPart()

Function PS_VisitExprList(ExprList, Separator)
	Var Buffer;
	If ExprList <> Undefined Then
		Buffer = New Array;
		For Each Expr In ExprList Do
			If Expr = Undefined Then
				Buffer.Add("");
			Else
				Buffer.Add(PS_VisitExpr(Expr));
			EndIf;
		EndDo;
		Return StrConcat(Buffer, Separator);
	EndIf;
EndFunction // PS_VisitExprList()

Function PS_VisitExpr(Expr)
	Var NodeType, BasicLitKind;
	NodeType = Expr.NodeType;
	If NodeType = "BasicLitExpr" Then
		BasicLitKind = Expr.Kind;
		If BasicLitKind = Tokens.String Then
			Return StrTemplate("""%1""", StrReplace(Expr.Value, Chars.LF, """ """));
		ElsIf BasicLitKind = Tokens.Number Then
			Return Format(Expr.Value, "NZ=0; NG=");
		ElsIf BasicLitKind = Tokens.DateTime Then
			Return Format(Expr.Value, "DF='""''yyyyMMdd'''");
		ElsIf BasicLitKind = Tokens.True Or BasicLitKind = Tokens.False Then
			Return Format(Expr.Value, "BF=$False; BT=$True");
		ElsIf BasicLitKind = Tokens.Undefined Then
			Return "$null";
		Else
			Raise "Unknown basic literal";
		EndIf;
	ElsIf NodeType = "DesignatorExpr" Then
		Return PS_VisitDesignatorExpr(Expr);
	ElsIf NodeType = "UnaryExpr" Then
		Return StrTemplate("%1 %2", PS_Operators[Expr.Operator], PS_VisitExpr(Expr.Operand));
	ElsIf NodeType = "BinaryExpr" Then
		Return StrTemplate("%1 %2 %3", PS_VisitExpr(Expr.Left), PS_Operators[Expr.Operator], PS_VisitExpr(Expr.Right));
	ElsIf NodeType = "RangeExpr" Then
		Return StrTemplate("%1..%2", PS_VisitExpr(Expr.Left), PS_VisitExpr(Expr.Right));
	ElsIf NodeType = "NewExpr" Then
		If TypeOf(Expr.Constructor) = Type("Structure") Then
			Return StrTemplate("<# New %1 #>", PS_VisitExpr(Expr.Constructor));
		Else
			Return StrTemplate("<# New(%1) #>", PS_VisitExprList(Expr.Constructor, ", "));
		EndIf;
	ElsIf NodeType = "TernaryExpr" Then
		Return StrTemplate("if (%1) { %2 } else { %3 }", PS_VisitExpr(Expr.Condition), PS_VisitExpr(Expr.ThenPart), PS_VisitExpr(Expr.ElsePart));
	ElsIf NodeType = "ParenExpr" Then
		Return StrTemplate("(%1)", BSL_VisitExpr(Expr.Expr));
	EndIf;
EndFunction // PS_VisitExpr()

Function PS_VisitDesignatorExpr(DesignatorExpr, IsOperand = True)
	Var Buffer, Selector;
	Buffer = New Array;
	If Not DesignatorExpr.Call Then
		Buffer.Add("$");
	EndIf;
	Buffer.Add(DesignatorExpr.Object.Name);
	If DesignatorExpr.Property("Selectors") Then
		For Each Selector In DesignatorExpr.Selectors Do
			If Selector.Kind = "Ident" Then
				Buffer.Add(".");
				Buffer.Add(Selector.Value);
			ElsIf Selector.Kind = "Index" Then
				Buffer.Add("[");
				Buffer.Add(PS_VisitExprList(Selector.Value, ", "));
				Buffer.Add("]");
			ElsIf Selector.Kind = "Call" Then
				If Selector.Value.Count() > 0 Then
					Buffer.Add(" ");
					Buffer.Add(PS_VisitExprList(Selector.Value, " "));
				EndIf;
			Else
				Raise "Unknown selector kind";
			EndIf;
		EndDo;
	EndIf;
	If IsOperand
		And DesignatorExpr.Call
		And Selector.Value.Count() > 0 Then
		Return StrTemplate("(%1)", StrConcat(Buffer));
	EndIf;
	Return StrConcat(Buffer);
EndFunction // PS_VisitDesignatorExpr()

#EndRegion // PS

#EndRegion // Backends