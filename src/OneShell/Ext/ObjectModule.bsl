
#Region Constants

Var Keywords;                  // enum
Var Tokens;                    // enum
Var ObjectKinds;               // enum
Var SelectorKinds;             // enum
Var UnaryOperators;            // array (one of Tokens)
Var BasicLiterals;             // array (one of Tokens)
Var RelationalOperators;       // array (one of Tokens)
Var IgnoredTokens;             // array (one of Tokens)
Var InitialTokensOfExpression; // array (one of Tokens)
Var Operators;                 // structure
Var EmptyArray;                // array

#EndRegion // Constants

#Region Init

Procedure Init()

	InitEnums();

	UnaryOperators = New Array;
	UnaryOperators.Add(Tokens.Add);
	UnaryOperators.Add(Tokens.Sub);

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
	SelectorKinds = SelectorKinds();
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
		+ ?(CompatibleWith1C, "", "Case.Выбор, When.Когда, EndCase.КонецВыбора, End.Конец") // new keywords
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

		//         //             #          &      ~
		|Eof, Comment, Preprocessor, Directive, Label"

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
		"Ident," // Something._
		"Index," // Something[_]
		"Call,"  // Something(_)
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

Function VarDecl(Object, Exported, Value = Undefined)
	Var VarDecl;

	VarDecl = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
		"Export,"   // boolean
	,
	"VarDecl", Object, Exported);

	If Value <> Undefined Then
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

Function ProcDecl(Object, Exported, Decls, Statements)
	Var ProcDecl;

	ProcDecl = New Structure(
		"NodeType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Export,"     // boolean
		"Decls,"      // array (one of declarations)
		"Statements," // array (one of statements)
	,
	"ProcDecl", Object, Exported, Decls, Statements);

	Return ProcDecl;

EndFunction // ProcDecl()

Function FuncDecl(Object, Exported, Decls, Statements)
	Var FuncDecl;

	FuncDecl = New Structure(
		"NodeType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Export,"     // boolean
		"Decls,"      // array (one of declarations)
		"Statements," // array (one of statements)
	,
	"FuncDecl", Object, Exported, Decls, Statements);

	Return FuncDecl;

EndFunction // FuncDecl()

Function ParamDecl(Object, ByVal, Init = False, Value = Undefined)
	Var ParamDecl;

	ParamDecl = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
		"ByVal,"    // boolean
	,
	"ParamDecl", Object, ByVal);

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

Function NotExpr(Expr)
	Var NotExpr;

	NotExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // one of expressions
	,
	"NotExpr", Expr);

	Return NotExpr;

EndFunction // NotExpr()

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

Function GotoStmt(Label)
	Var GotoStmt;

	GotoStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Label,"    // string
	,
	"GotoStmt", Label);

	Return GotoStmt;

EndFunction // GotoStmt()

Function LabelStmt(Label)
	Var LabelStmt;

	LabelStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Label,"    // string
	,
	"LabelStmt", Label);

	Return LabelStmt;

EndFunction // LabelStmt()

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
		"Vars,"    // structure as map[string](Object)
		"Methods," // structure as map[string](Object)
		"Module,"  // structure (Module)
		"Unknown," // structure as map[string](Object)
		"IsFunc,"  // boolean
	);

	Parser.Scanner = Scanner(Source);
	Parser.Methods = New Structure;
	Parser.Unknown = New Structure;
	Parser.IsFunc = False;

	OpenScope(Parser);

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
	Var Operator, Expr;
	Operator = Parser.Tok;
	If UnaryOperators.Find(Parser.Tok) <> Undefined Then
		Next(Parser);
		Expr = UnaryExpr(Operator, ParseOperand(Parser));
	ElsIf Parser.Tok = Tokens.Eof Then
		Expr = Undefined;
	Else
		Expr = ParseOperand(Parser);
	EndIf;
	Return Expr;
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
	Var Name, Selector, Object, List, Kind;
	Name = Parser.Lit;
	Selector = ParseSelector(Parser);
	If Selector = Undefined Then
		Object = FindObject(Parser, Name);
		List = EmptyArray;
	Else
		Kind = Selector.Kind;
		If Kind = "Call" Then
			If Not Parser.Methods.Property(Name, Object) Then
				If Not Parser.Unknown.Property(Name, Object) Then
					Object = Object("Unknown", Name);
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
			Object = Object(ObjectKinds.Variable, Name);
			Parser.Vars.Insert(Name, Object);
		Else
			Object = Object("Unknown", Name);
			If Verbose Then
				Error(Parser.Scanner, StrTemplate("Undeclared identifier `%1`", Name));
			EndIf;
		EndIf;
	EndIf; 
	Return DesignatorExpr(Object, List, Kind = SelectorKinds.Call);
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

Function ParseSelector(Parser)
	Var Tok, Value, Selector;
	Tok = Next(Parser);
	If Tok = Tokens.Period Then
		Next(Parser);
		If Not Keywords.Property(Parser.Lit) Then
			Expect(Parser, Tokens.Ident);
		EndIf;
		Value = Parser.Lit;
		Selector = Selector(SelectorKinds.Ident, Value);
	ElsIf Tok = Tokens.Lbrack Then
		Tok = Next(Parser);
		If Tok = Tokens.Rbrack Then
			Error(Parser.Scanner, "Expected expression",, True);
		EndIf;
		Value = ParseExprList(Parser);
		Expect(Parser, Tokens.Rbrack);
		Selector = Selector(SelectorKinds.Index, Value);
	ElsIf Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Value = EmptyArray;
		Else
			Value = ParseExprList(Parser, True);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Selector = Selector(SelectorKinds.Call, Value);
	EndIf;
	Return Selector;
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
	Expr = ParseNotExpr(Parser);
	While Parser.Tok = Tokens.And Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseNotExpr(Parser));
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseNotExpr(Parser)
	Var Expr;
	If Parser.Tok = Tokens.Not Then
		Next(Parser);
		Expr = NotExpr(ParseRelExpr(Parser));
	Else
		Expr = ParseRelExpr(Parser);
	EndIf;
	Return Expr;
EndFunction // ParseNotExpr()

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
				ExprList.Add(Undefined);
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
	Var Object, Name, Decls, Exported;
	Exported = False;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit; 
	Next(Parser);
	OpenScope(Parser);
	If Parser.Unknown.Property(Name, Object) Then
		Object.Kind = ObjectKinds.Function;
		Object.Insert("Type", ParseSignature(Parser));
		Parser.Unknown.Delete(Name);
	Else
		Object = Object(ObjectKinds.Function, Name, ParseSignature(Parser));
	EndIf;
	If Parser.Tok = Tokens.Export Then
		Exported = True;
		Next(Parser);
	EndIf;
	If Parser.Methods.Property(Name) Then
		Error(Parser.Scanner, "Method already declared",, True);
	EndIf;
	Parser.Methods.Insert(Name, Object);
	Decls = ParseVarDecls(Parser);
	Parser.IsFunc = True;
	Statements = ParseStatements(Parser);
	Parser.IsFunc = False;
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndFunction);
	EndIf; 
	CloseScope(Parser);
	Next(Parser);
	Return FuncDecl(Object, Exported, Decls, Statements);
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
	Return Signature(ParamList);
EndFunction // ParseSignature()

Function ParseProcDecl(Parser)
	Var Object, Name, Decls, Exported;
	Exported = False;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	OpenScope(Parser);
	If Parser.Unknown.Property(Name, Object) Then
		Object.Kind = ObjectKinds.Procedure;
		Object.Insert("Type", ParseSignature(Parser)); 
		Parser.Unknown.Delete(Name);
	Else
		Object = Object(ObjectKinds.Procedure, Name, ParseSignature(Parser));
	EndIf;
	If Parser.Tok = Tokens.Export Then
		Exported = True;
		Next(Parser);
	EndIf;
	If Parser.Methods.Property(Name) Then
		Error(Parser.Scanner, "Method already declared",, True);
	EndIf;
	Parser.Methods.Insert(Name, Object);
	Decls = ParseVarDecls(Parser);
	Statements = ParseStatements(Parser);
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndProcedure);
	EndIf; 
	CloseScope(Parser);
	Next(Parser);
	Return ProcDecl(Object, Exported, Decls, Statements);
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
	Return VarListDecl(VarList);
EndFunction // ParseVarListDecl()

Function ParseVarDecl(Parser)
	Var Tok, Name, Object, VarDecl, Value, Exported;
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Tok = Next(Parser);
	If Tok = Tokens.Eql Then
		Tok = Next(Parser);
		If BasicLiterals.Find(Tok) = Undefined Then
			Error(Parser.Scanner, "expected basic literal");
		EndIf;
		Object = Object(ObjectKinds.Variable, Name, Tok);
		Value = ParseOperand(Parser);
	Else
		Object = Object(ObjectKinds.Variable, Name, Undefined);
	EndIf;
	If Parser.Tok = Tokens.Export Then
		Exported = True;
		Next(Parser);
	Else
		Exported = False;
	EndIf;
	VarDecl = VarDecl(Object, Exported, Value);
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared",, True);
	EndIf; 
	Parser.Vars.Insert(Name, Object);
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
	Var Tok, Name, Object, ParamDecl, ByVal;
	If Parser.Tok = Tokens.Val Then
		ByVal = True;
		Next(Parser);
	Else
		ByVal = False;
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
		ParamDecl = ParamDecl(Object, ByVal, True, ParseOperand(Parser));
	Else
		Object = Object(ObjectKinds.Parameter, Name, Undefined);
		ParamDecl = ParamDecl(Object, ByVal);
	EndIf;
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared",, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
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
	Var Tok, Stmt;
	Tok = SkipIgnoredTokens(Parser);
	While Tok = Tokens.Semicolon Do
		Next(Parser);
		Tok = SkipIgnoredTokens(Parser);
	EndDo;
	If Tok = Tokens.Ident Then
		Stmt = ParseAssignOrCallStmt(Parser);
	ElsIf Tok = Tokens.If Then
		Stmt = ParseIfStmt(Parser);
	ElsIf Tok = Tokens.Try Then
		Stmt = ParseTryStmt(Parser);
	ElsIf Tok = Tokens.While Then
		Stmt = ParseWhileStmt(Parser);
	ElsIf Tok = Tokens.For Then
		Stmt = ParseForStmt(Parser);
	ElsIf Not CompatibleWith1C And Tok = Tokens.Case Then
		Stmt = ParseCaseStmt(Parser);
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
	EndIf;
	Return Stmt;
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
	Var Tok, Left, Right, Stmt;
	Left = ParseDesignatorExprList(Parser, True);
	If Left.Count() = 1 And Left[0].Call Then
		Stmt = CallStmt(Left[0]);
	Else
		Tok = Parser.Tok;
		If Tok = Tokens.Eql Then
			Next(Parser);
			Right = ParseExprList(Parser);
			Stmt = AssignStmt(Left, Right);
		ElsIf Tok = Tokens.AddAssign Then
			Next(Parser);
			Right = ParseExprList(Parser);
			If Left.Count() <> Right.Count() Then
				Error(Parser.Scanner, "arrays have different number of elements",, True);
			EndIf; 
			Stmt = AddAssignStmt(Left, Right);
		Else
			Expect(Parser, Tokens.Eql);
		EndIf;		
	EndIf;
	Return Stmt;
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
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndIf);
	EndIf; 
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
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndTry);
	EndIf; 
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
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndCase);
	EndIf; 
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
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndDo);
	EndIf; 
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
	If CompatibleWith1C Or Parser.Tok <> Tokens.End Then
		Expect(Parser, Tokens.EndDo);
	EndIf; 
	Next(Parser);
	Return ForStmt(DesignatorExpr, Collection, Statements);
EndFunction // ParseForStmt()

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
			Message(StrTemplate("Undeclared method `%1`", Item.Key));
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
