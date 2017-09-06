
#Region Constants

Var Keywords;                  // enum
Var Tokens;                    // enum
Var ObjectKinds;               // enum
Var SelectorKinds;             // enum
Var Directives;                // enum
Var PreprocInstructions;       // enum
Var UnaryOperators;            // array (one of Tokens)
Var BasicLiterals;             // array (one of Tokens)
Var RelationalOperators;       // array (one of Tokens)
Var IgnoredTokens;             // array (one of Tokens)
Var InitialTokensOfExpression; // array (one of Tokens)
Var EmptyArray;                // array

#EndRegion // Constants

#Region Init

Procedure Init() Export

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
	IgnoredTokens.Add(Tokens.Preproc);

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

	EmptyArray = New Array;

EndProcedure // Init()

Procedure InitEnums()
	Keywords = Keywords();
	Tokens = Tokens(Keywords);
	ObjectKinds = ObjectKinds();
	SelectorKinds = SelectorKinds();
	Directives = Directives();
	PreprocInstructions = PreprocInstructions();
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

		//         //        #          &      ~
		|Eof, Comment, Preproc, Directive, Label"

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

Function Directives() Export
	Var Directives;

	Directives = Enum(New Structure,
		"AtClient.НаКлиенте,"
		"AtServer.НаСервере,"
		"AtServerNoContext.НаСервереБезКонтекста,"
		"AtClientAtServer.НаКлиентеНаСервере,"
	);

	Return Directives;
EndFunction // Directives()

Function PreprocInstructions() Export
	Var PreprocInstructions;

	PreprocInstructions = Enum(New Structure,
		"If.Если,"
		"ElsIf.ИначеЕсли,"
		"Else.Иначе,"
		"EndIf.КонецЕсли,"
		"Region.Область,"
		"EndRegion.КонецОбласти,"
	);

	Return PreprocInstructions;
EndFunction // PreprocInstructions()

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
		If PreprocInstructions.Property(Lit, Tok) Then
			Tok = "_" + Tok;
		Else
			Tok = Tokens.Preproc; // ignore unknown instruction
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

Function Module(Decls, AutoVars, Statements)
	Var Module;

	Module = New Structure(
		"Decls,"      // array (one of declarations)
		"AutoVars,"   // array (Object)
		"Statements," // array (one of statements)
	,
	Decls, AutoVars, Statements);

	Return Module;
EndFunction // Module()

#Region Scope

Function Scope(Outer)
	Var Scope;

	Scope = New Structure(
		"Outer,"    // structure (Scope)
		"Objects,"  // structure as map[string](Object)
		"AutoVars," // array (Object)
	,
	Outer, New Structure, New Array);

	Return Scope;
EndFunction // Scope()

Function Object(Kind, Name, Directive = Undefined, Exported = Undefined)
	Var Object;

	Object = New Structure(
		"Kind," // string (one of ObjectKinds)
		"Name," // string
	,
	Kind, Name);

	If Directive <> Undefined Then
		Object.Insert("Directive", Directive); // string (one of Directives)
	EndIf;

	If Exported <> Undefined Then
		Object.Insert("Export", Exported); // boolean
	EndIf;

	Return Object;
EndFunction // Object()

Function Signature(Kind, Name, Directive, ParamList, Exported)
	Var Object;
	Object = Object(Kind, Name, Directive, Exported);
	Object.Insert("ParamList", ParamList); // array (Object)
	Return Object;
EndFunction // Signature()

Function Variable(Name, Directive, Exported, Auto = False)
	Var Object;
	Object = Object(ObjectKinds.Variable, Name, Directive, Exported);
	Object.Insert("Auto", Auto); // boolean
	Return Object;
EndFunction // Variable()

Function Parameter(Name, ByVal, Value = Undefined)
	Var Object;
	Object = Object(ObjectKinds.Parameter, Name);
	Object.Insert("ByVal", ByVal); // boolean
	If Value <> Undefined Then
		Object.Insert("Value", Value); // structure (one of expressions)
	EndIf;
	Return Object;
EndFunction // Parameter()

#EndRegion // Scope

#Region Declarations

Function VarListDecl(VarList)
	Var VarListDecl;

	VarListDecl = New Structure(
		"NodeType," // string (type of this structure)
		"VarList,"  // array (Object)
	,
	"VarListDecl", VarList);

	Return VarListDecl;
EndFunction // VarListDecl()

Function ProcDecl(Object, Decls, AutoVars, Statements)
	Var ProcDecl;

	ProcDecl = New Structure(
		"NodeType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Decls,"      // array (one of declarations)
		"AutoVars,"   // array (Object)
		"Statements," // array (one of statements)
	,
	"ProcDecl", Object, Decls, AutoVars, Statements);

	Return ProcDecl;
EndFunction // ProcDecl()

Function FuncDecl(Object, Decls, AutoVars, Statements)
	Var FuncDecl;

	FuncDecl = New Structure(
		"NodeType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Decls,"      // array (one of declarations)
		"AutoVars,"   // array (Object)
		"Statements," // array (one of statements)
	,
	"FuncDecl", Object, Decls, AutoVars, Statements);

	Return FuncDecl;
EndFunction // FuncDecl()

Function PreprocIfDecl(Condition, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined)
	Var PreprocIfDecl;

	PreprocIfDecl = New Structure(
		"NodeType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // array (one of declarations)
	,
	"PreprocIfDecl", Condition, ThenPart);

	If ElsIfPart <> Undefined Then
		PreprocIfDecl.Insert("ElsIfPart", ElsIfPart); // array (PreprocIfDecl)
	EndIf;

	If ElsePart <> Undefined Then
		PreprocIfDecl.Insert("ElsePart", ElsePart); // array (one of declarations)
	EndIf;

	Return PreprocIfDecl;
EndFunction // PreprocIfDecl()

Function PreprocRegionDecl(Name, Decls)
	Var PreprocRegionDecl;

	PreprocRegionDecl = New Structure(
		"NodeType," // string (type of this structure)
		"Name,"     // structure (one of expressions)
		"Decls,"    // array (one of statements)
	,
	"PreprocRegionDecl", Name, Decls);

	Return PreprocRegionDecl;
EndFunction // PreprocRegionDecl()

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
		"Operand,"  // structure (one of expressions)
	,
	"UnaryExpr", Operator, Operand);

	Return UnaryExpr;
EndFunction // UnaryExpr()

Function BinaryExpr(Left, Operator, Right)
	Var BinaryExpr;

	BinaryExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // structure (one of expressions)
		"Operator," // string (one of Tokens)
		"Right,"    // structure (one of expressions)
	,
	"BinaryExpr", Left, Operator, Right);

	Return BinaryExpr;
EndFunction // BinaryExpr()

Function RangeExpr(Left, Right)
	Var RangeExpr;

	RangeExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // structure (one of expressions)
		"Right,"    // structure (one of expressions)
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

Function TernaryExpr(Condition, ThenPart, ElsePart, Selectors)
	Var TernaryExpr;

	TernaryExpr = New Structure(
		"NodeType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // structure (one of expressions)
		"ElsePart"   // structure (one of expressions)
	,
	"TernaryExpr", Condition, ThenPart, ElsePart);

	If Selectors.Count() > 0 Then
		TernaryExpr.Insert("Selectors", Selectors); // array (Selector)
	EndIf;

	Return TernaryExpr;
EndFunction // TernaryExpr()

Function ParenExpr(Expr)
	Var ParenExpr;

	ParenExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // structure (one of expressions)
	,
	"ParenExpr", Expr);

	Return ParenExpr;
EndFunction // ParenExpr()

Function NotExpr(Expr)
	Var NotExpr;

	NotExpr = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // structure (one of expressions)
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
		"Left,"     // structure (DesignatorExpr)
		"Right,"    // structure (one of expressions)
	,
	"AssignStmt", Left, Right);

	Return AssignStmt;
EndFunction // AssignStmt()

Function ReturnStmt(Expr)
	Var ReturnStmt;

	ReturnStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"ReturnStmt");

	If Expr <> Undefined Then
		ReturnStmt.Insert("Expr", Expr); // structure (one of expressions)
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

Function PreprocIfStmt(Condition, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined)
	Var PreprocIfStmt;

	PreprocIfStmt = New Structure(
		"NodeType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // array (one of statements)
	,
	"PreprocIfStmt", Condition, ThenPart);

	If ElsIfPart <> Undefined Then
		PreprocIfStmt.Insert("ElsIfPart", ElsIfPart); // array (PreprocIfStmt)
	EndIf;

	If ElsePart <> Undefined Then
		PreprocIfStmt.Insert("ElsePart", ElsePart); // array (one of statements)
	EndIf;

	Return PreprocIfStmt;
EndFunction // PreprocIfStmt()

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

Function PreprocRegionStmt(Name, Statements)
	Var PreprocRegionStmt;

	PreprocRegionStmt = New Structure(
		"NodeType,"   // string (type of this structure)
		"Name,"       // structure (one of expressions)
		"Statements," // array (one of statements)
	,
	"PreprocRegionStmt", Name, Statements);

	Return PreprocRegionStmt;
EndFunction // PreprocRegionStmt()

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
	);

	Parser.Scanner = Scanner(Source);
	Parser.Pos = 0;
	Parser.PrevPos = 0;
	Parser.Methods = New Structure;
	Parser.Unknown = New Structure;
	Parser.IsFunc = False;

	OpenScope(Parser);

	Return Parser;

EndFunction // Parser()

Function Next(Parser)
	Var Scanner, Tok, Lit;
	Scanner = Parser.Scanner;
	Parser.PrevPos = Scanner.Pos;
	Tok = Scan(Scanner);
	While IgnoredTokens.Find(Tok) <> Undefined Do
		Tok = Scan(Scanner);
	EndDo;
	Parser.Pos = Scanner.Pos - StrLen(Scanner.Lit);
	If Tok = Tokens.StringBeg Then
		Lit = ParseString(Parser);
		Tok = Tokens.String;
	Else
		Lit = Scanner.Lit;
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
	If Tok <> Tokens.StringEnd Then
		Error(Parser.Scanner, "Expected """,, True);
	EndIf;
	List.Add(Mid(Scanner.Lit, 2));
	Return StrConcat(List);
EndFunction // ParseString()

Function ParseUnaryExpr(Parser)
	Var Operator, Expr, Pos;
	Pos = Parser.Pos;
	Operator = Parser.Tok;
	If UnaryOperators.Find(Parser.Tok) <> Undefined Then
		Next(Parser);
		Expr = UnaryExpr(Operator, ParseOperand(Parser));
	ElsIf Parser.Tok = Tokens.Eof Then
		Expr = Undefined;
	Else
		Expr = ParseOperand(Parser);
	EndIf;
	Return Locate(Expr, Parser, Pos);
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
		Error(Parser.Scanner, "Expected operand",, True);
	EndIf;
	Return Operand;
EndFunction // ParseOperand()

Function ParseNewExpr(Parser)
	Var Tok, Constructor, Pos;
	Pos = Parser.Pos;
	Tok = Next(Parser);
	If Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Constructor = EmptyArray;
		Else
			Constructor = ParseArguments(Parser);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Next(Parser);
	Else
		Constructor = ParseDesignatorExpr(Parser);
	EndIf;
	Return Locate(NewExpr(Constructor), Parser, Pos);
EndFunction // ParseNewExpr()

Function ParseDesignatorExpr(Parser, Val AllowNewVar = False)
	Var Name, Selector, Object, List, Kind, Pos;
	Pos = Parser.Pos;
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
			Object = Variable(Name, Undefined, False, True);
			Parser.Vars.Insert(Name, Object);
			Parser.Scope.AutoVars.Add(Object);
		Else
			Object = Object("Unknown", Name);
			If Verbose Then
				Error(Parser.Scanner, StrTemplate("Undeclared identifier `%1`", Name), Pos);
			EndIf;
		EndIf;
	EndIf;
	Return Locate(DesignatorExpr(Object, List, Kind = SelectorKinds.Call), Parser, Pos);
EndFunction // ParseDesignatorExpr()

Function ParseSelector(Parser)
	Var Tok, Value, Selector, Pos;
	Tok = Parser.Tok;
	Pos = Parser.Pos;
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
	Return Locate(Selector, Parser, Pos);
EndFunction // ParseSelector()

Function ParseExpression(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseAndExpr(Parser);
	While Parser.Tok = Tokens.Or Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseAndExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseExpression()

Function ParseAndExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseNotExpr(Parser);
	While Parser.Tok = Tokens.And Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseNotExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseNotExpr(Parser)
	Var Expr, Pos;
	Pos = Parser.Pos;
	If Parser.Tok = Tokens.Not Then
		Next(Parser);
		Expr = Locate(NotExpr(ParseRelExpr(Parser)), Parser, Pos);
	Else
		Expr = ParseRelExpr(Parser);
	EndIf;
	Return Expr;
EndFunction // ParseNotExpr()

Function ParseRelExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseAddExpr(Parser);
	While RelationalOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseAddExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseRelExpr()

Function ParseAddExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseMulExpr(Parser);
	While Parser.Tok = Tokens.Add Or Parser.Tok = Tokens.Sub Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseMulExpr(Parser)), Parser, Pos);
	EndDo;
	Return Expr;
EndFunction // ParseAddExpr()

Function ParseMulExpr(Parser)
	Var Expr, Operator, Pos;
	Pos = Parser.Pos;
	Expr = ParseUnaryExpr(Parser);
	While Parser.Tok = Tokens.Mul Or Parser.Tok = Tokens.Div Or Parser.Tok = Tokens.Mod Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = Locate(BinaryExpr(Expr, Operator, ParseUnaryExpr(Parser)), Parser, Pos);
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
	Var Condition, ThenPart, ElsePart, Selectors, Pos;
	Pos = Parser.Pos;
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
	Return Locate(TernaryExpr(Condition, ThenPart, ElsePart, Selectors), Parser, Pos);
EndFunction // ParseTernaryExpr()

Function ParseFuncDecl(Parser)
	Var Object, Name, Decls, ParamList, Exported, AutoVars, VarObj, Pos;
	Pos = Parser.Pos;
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
		Object.Kind = ObjectKinds.Function;
		Object.Insert("ParamList", ParamList);
		Object.Insert("Export", Exported);
		Parser.Unknown.Delete(Name);
	Else
		Object = Signature(ObjectKinds.Function, Name, Parser.Directive, ParamList, Exported);
	EndIf;
	If Parser.Methods.Property(Name) Then
		Error(Parser.Scanner, "Method already declared", Pos, True);
	EndIf;
	Parser.Methods.Insert(Name, Object);
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
	Return Locate(FuncDecl(Object, Decls, AutoVars, Statements), Parser, Pos);
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
	Var Object, Name, Decls, ParamList, Exported, AutoVars, VarObj, Statements, Pos;
	Pos = Parser.Pos;
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
		Object.Kind = ObjectKinds.Procedure;
		Object.Insert("ParamList", ParamList);
		Object.Insert("Export", Exported);
		Parser.Unknown.Delete(Name);
	Else
		Object = Signature(ObjectKinds.Procedure, Name, Parser.Directive, ParamList, Exported);
	EndIf;
	If Parser.Methods.Property(Name) Then
		Error(Parser.Scanner, "Method already declared", Pos, True);
	EndIf;
	Parser.Methods.Insert(Name, Object);
	Decls = ParseVarDecls(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndProcedure);
	AutoVars = New Array;
	For Each VarObj In Parser.Scope.AutoVars Do
		AutoVars.Add(VarObj);
	EndDo;
	CloseScope(Parser);
	Next(Parser);
	Return Locate(ProcDecl(Object, Decls, AutoVars, Statements), Parser, Pos);
EndFunction // ParseProcDecl()

Function ParseReturnStmt(Parser)
	Var Expr, Pos;
	Pos = Parser.Pos;
	Next(Parser);
	If Parser.IsFunc Then
		Expr = ParseExpression(Parser);
	EndIf;
	Return Locate(ReturnStmt(Expr), Parser, Pos);
EndFunction // ParseReturnStmt()

Function ParseVarListDecl(Parser)
	Var VarList, Pos;
	Pos = Parser.Pos;
	VarList = New Array;
	VarList.Add(ParseVariable(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVariable(Parser));
	EndDo;
	Return Locate(VarListDecl(VarList), Parser, Pos);
EndFunction // ParseVarListDecl()

Function ParseVariable(Parser)
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
	Object = Variable(Name, Parser.Directive, Exported);
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared", Pos, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
	Return Object;
EndFunction // ParseVariable()

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
		Object = Parameter(Name, ByVal, ParseUnaryExpr(Parser));
	Else
		Object = Parameter(Name, ByVal);
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
	Var Tok, Stmt, Pos;
	Tok = SkipIgnoredTokens(Parser);
	Pos = Parser.Pos;
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
		Stmt = ParsePreprocRegionStmt(Parser);
	ElsIf Tok = Tokens._If Then
		Stmt = ParsePreprocIfStmt(Parser);
	ElsIf Tok = Tokens.Semicolon Then
		// NOP
	EndIf;
	Return Locate(Stmt, Parser, Pos);
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
	Left = ParseDesignatorExpr(Parser, True);
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

Function ParsePreprocIfStmt(Parser)
	Var Tok, Condition, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen;
	Next(Parser);
	Condition = ParseExpression(Parser); // todo: only logic operators
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
			ElsIfPart.Add(PreprocIfStmt(ElsIfCond, ElsIfThen));
			Tok = Parser.Tok;
		EndDo;
	EndIf;
	If Tok = Tokens._Else Then
		Next(Parser);
		ElsePart = ParseStatements(Parser);
	EndIf;
	Expect(Parser, Tokens._EndIf);
	Parser.Tok = Tokens.Semicolon; // cheat code
	Return PreprocIfStmt(Condition, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParsePreprocIfStmt()

Function ParsePreprocIfDecl(Parser)
	Var Tok, Condition, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen;
	Next(Parser);
	Condition = ParseExpression(Parser); // todo: only logic operators
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseDecls(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens._ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens._ElsIf Do
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseDecls(Parser);
			ElsIfPart.Add(PreprocIfDecl(ElsIfCond, ElsIfThen));
			Tok = Parser.Tok;
		EndDo;
	EndIf;
	If Tok = Tokens._Else Then
		Next(Parser);
		ElsePart = ParseDecls(Parser);
	EndIf;
	Expect(Parser, Tokens._EndIf);
	Next(Parser);
	Return PreprocIfDecl(Condition, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParsePreprocIfDecl()

Function ParsePreprocRegionStmt(Parser)
	Var Name, Statements;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens._EndRegion);
	Parser.Tok = Tokens.Semicolon; // cheat code
	Return PreprocRegionStmt(Name, Statements);
EndFunction // ParsePreprocRegionStmt()

Function ParsePreprocRegion(Parser)
	Var Name, Decls, Statements, Region;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	Decls = ParseDecls(Parser);
	Region = PreprocRegionDecl(Name, Decls);
	If Decls.Count() = 0 Then
		Statements = ParseStatements(Parser);
		If Statements.Count() > 0 Then
			Region = PreprocRegionStmt(Name, Statements);
		EndIf;
	EndIf;
	Expect(Parser, Tokens._EndRegion);
	Next(Parser);
	Return Region;
EndFunction // ParsePreprocRegion()

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
	Var Condition, Statements;
	Next(Parser);
	Condition = ParseExpression(Parser);
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo);
	Next(Parser);
	Return WhileStmt(Condition, Statements);
EndFunction // ParseWhileStmt()

Function ParseForStmt(Parser)
	Var DesignatorExpr, Left, Right, Collection, Statements, VarPos;
	Next(Parser);
	If Parser.Tok = Tokens.Each Then
		Next(Parser);
	EndIf;
	Expect(Parser, Tokens.Ident);
	VarPos = Parser.Pos;
	DesignatorExpr = ParseDesignatorExpr(Parser, True);
	If DesignatorExpr.Call Then
		Error(Parser.Scanner, "expected variable", VarPos, True);
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
	While Tok = Tokens.Directive Do
		Parser.Directive = Parser.Lit;
		Tok = Next(Parser);
	EndDo;
	While Tok = Tokens.Var Do
		Next(Parser);
		Decls.Add(ParseVarListDecl(Parser));
		Expect(Parser, Tokens.Semicolon);
		Next(Parser);
		Tok = Parser.Tok;
		Parser.Directive = Undefined;
		While Tok = Tokens.Directive Do
			Parser.Directive = Parser.Lit;
			Tok = Next(Parser);
		EndDo;
	EndDo;
	Return Decls;
EndFunction // ParseVarDecls()

Function ParseDecls(Parser)
	Var Tok, Decls;
	Decls = ParseVarDecls(Parser);
	Tok = Parser.Tok;
	While Tok <> Tokens.Eof Do
		If Tok = Tokens.Function Then
			Decls.Add(ParseFuncDecl(Parser));
		ElsIf Tok = Tokens.Procedure Then
			Decls.Add(ParseProcDecl(Parser));
		ElsIf Tok = Tokens._Region Then
			Decls.Add(ParsePreprocRegion(Parser));
		ElsIf Tok = Tokens._If Then
			Decls.Add(ParsePreprocIfDecl(Parser));
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
EndFunction // ParseDecls()

Function ParseModule(Parser) Export
	Var Decls, AutoVars, Statements;
	Next(Parser);
	Decls = ParseDecls(Parser);
	Statements = ParseStatements(Parser);
	AutoVars = New Array;
	For Each VarObj In Parser.Scope.AutoVars Do
		AutoVars.Add(VarObj);
	EndDo;
	Parser.Module = Module(Decls, AutoVars, Statements);
	If Verbose Then
		For Each Item In Parser.Unknown Do
			Message(StrTemplate("Undeclared method `%1`", Item.Key));
		EndDo;
	EndIf;
	Expect(Parser, Tokens.Eof);
EndFunction // ParseModule()

#EndRegion // Parser

#Region Auxiliary

Function Locate(Node, Parser, Pos)
	If Node = Undefined Then
		Return Undefined;
	EndIf;
	If Location Then
		Node.Insert("Pos", Pos);
		Node.Insert("Len", Parser.PrevPos - Pos);
	EndIf;
	If Debug Then
		Node.Insert("Str", Mid(Parser.Scanner.Source, Pos, Parser.PrevPos - Pos));
	EndIf;
	Return Node;
EndFunction // Locate()

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
