
#Region Constants

Var Keywords;         // enum
Var Tokens;           // enum
Var SelectorKinds;    // enum
Var Directives;       // enum
Var PrepInstructions; // enum
Var BasicLitNoString; // array (one of Tokens)
Var RelOperators;     // array (one of Tokens)
Var AddOperators;     // array (one of Tokens)
Var MulOperators;     // array (one of Tokens)
Var InitOfExpression; // array (one of Tokens)
Var EmptyArray;       // array

#EndRegion // Constants

#Region Cache

Var StructKeysCache; // structure as map[string] (string)

#EndRegion // Cache

#Region Init

Procedure Init()

	StructKeysCache = New Structure;

	InitEnums();

	BasicLitNoString = New Array;
	BasicLitNoString.Add(Tokens.Number);
	BasicLitNoString.Add(Tokens.DateTime);
	BasicLitNoString.Add(Tokens.True);
	BasicLitNoString.Add(Tokens.False);
	BasicLitNoString.Add(Tokens.Undefined);
	BasicLitNoString.Add(Tokens.Null);

	RelOperators = New Array;
	RelOperators.Add(Tokens.Eql);
	RelOperators.Add(Tokens.Neq);
	RelOperators.Add(Tokens.Lss);
	RelOperators.Add(Tokens.Gtr);
	RelOperators.Add(Tokens.Leq);
	RelOperators.Add(Tokens.Geq);

	AddOperators = New Array;
	AddOperators.Add(Tokens.Add);
	AddOperators.Add(Tokens.Sub);

	MulOperators = New Array;
	MulOperators.Add(Tokens.Mul);
	MulOperators.Add(Tokens.Div);
	MulOperators.Add(Tokens.Mod);

	InitOfExpression = New Array;
	InitOfExpression.Add(Tokens.Add);
	InitOfExpression.Add(Tokens.Sub);
	InitOfExpression.Add(Tokens.Not);
	InitOfExpression.Add(Tokens.Ident);
	InitOfExpression.Add(Tokens.Lparen);
	InitOfExpression.Add(Tokens.Number);
	InitOfExpression.Add(Tokens.String);
	InitOfExpression.Add(Tokens.StringBeg);
	InitOfExpression.Add(Tokens.DateTime);
	InitOfExpression.Add(Tokens.Ternary);
	InitOfExpression.Add(Tokens.New);
	InitOfExpression.Add(Tokens.True);
	InitOfExpression.Add(Tokens.False);
	InitOfExpression.Add(Tokens.Undefined);
	InitOfExpression.Add(Tokens.Null);

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
	Return Enum(New Structure,
		"If.Если, Then.Тогда, ElsIf.ИначеЕсли, Else.Иначе, EndIf.КонецЕсли,
		|For.Для, Each.Каждого, In.Из, To.По, While.Пока, Do.Цикл, EndDo.КонецЦикла,
		|Procedure.Процедура, EndProcedure.КонецПроцедуры, Function.Функция, EndFunction.КонецФункции,
		|Var.Перем, Val.Знач, Return.Возврат, Continue.Продолжить, Break.Прервать,
		|And.И, Or.Или, Not.Не,
		|Try.Попытка, Except.Исключение, Raise.ВызватьИсключение, EndTry.КонецПопытки,
		|New.Новый, Execute.Выполнить, Export.Экспорт, Goto.Перейти,
		|True.Истина, False.Ложь, Undefined.Неопределено, Null"
	);
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
	Return Enum(New Structure,
		"Ident," // Something._
		"Index," // Something[_]
		"Call,"  // Something(_)
	);
EndFunction // SelectorKinds()

Function Directives() Export
	Return Enum(New Structure,
		"AtClient.НаКлиенте,"
		"AtServer.НаСервере,"
		"AtServerNoContext.НаСервереБезКонтекста,"
		"AtClientAtServerNoContext.НаКлиентеНаСервереБезКонтекста,"
		"AtClientAtServer.НаКлиентеНаСервере,"
	);
EndFunction // Directives()

Function PrepInstructions() Export
	Return Enum(New Structure,
		"If.Если,"
		"ElsIf.ИначеЕсли,"
		"Else.Иначе,"
		"EndIf.КонецЕсли,"
		"Region.Область,"
		"EndRegion.КонецОбласти,"
	);
EndFunction // PrepInstructions()

Function Enum(Structure, Keys)
	Var Items, Item, ItemList, Value;

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

	Scanner = Class("Scanner",
		"Path"   // string
		"Source" // string
		"Len"    // number
		"Pos"    // number
		"Tok"    // string (one of Tokens)
		"Lit"    // string
		"Char"   // string
		"Line"   // number
	);

	Scanner.Path = "";
	Scanner.Source = Source;
	Scanner.Len = StrLen(Source);
	Scanner.Line = 1;
	Scanner.Pos = 0;
	Scanner.Lit = "";

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

Procedure SkipWhitespace(Scanner)
	Var Char;
	Char = Scanner.Char;
	While IsBlankString(Char) And Char <> "" Do
		If Char = Chars.LF Then
			Scanner.Line = Scanner.Line + 1;
		EndIf;
		Char = NextChar(Scanner);
	EndDo;
EndProcedure // SkipWhitespace()

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

Function Module(Decls, Auto, Statements, Interface, Comments)
	Return Struct("Module",
		"Decls"     // array (one of #Declarations)
		"Auto"      // array (VarLoc)
		"Body"      // array (one of #Statements)
		"Interface" // array (Func, Proc)
		"Comments"  // map[number] (string)
	, Decls, Auto, Statements, Interface, Comments);
EndFunction // Module()

#Region Scope

Function Scope(Outer)
	Return New Structure(
		"Outer,"   // undefined, structure (Scope)
		"Objects," // structure as map[string] (Unknown, Func, Proc, VarMod, VarLoc, Param)
		"Auto,"    // array (VarLoc)
	, Outer, New Structure, New Array);
EndFunction // Scope()

Function Unknown(Name)
	Return Struct("Unknown",
		"Name" // string
	, Name);
EndFunction // Unknown()

Function Func(Name, Directive, Params, Exported)
	Return Struct("Func",
		"Name"   // string
		"Dir"    // string (one of Directives)
		"Params" // array (Parameter)
		"Export" // boolean
	, Name, Directive, Params, Exported);
EndFunction // Func()

Function Proc(Name, Directive, Params, Exported)
	Return Struct("Proc",
		"Name"   // string
		"Dir"    // string (one of Directives)
		"Params" // array (Parameter)
		"Export" // boolean
	, Name, Directive, Params, Exported);
EndFunction // Proc()

Function VarMod(Name, Directive, Exported)
	Return Struct("VarMod",
		"Name"   // string
		"Dir"    // string (one of Directives)
		"Export" // boolean
	, Name, Directive, Exported);
EndFunction // VarMod()

Function VarLoc(Name, Auto = False)
	Return Struct("VarLoc",
		"Name" // string
		"Auto" // boolean
	, Name, Auto);
EndFunction // VarLoc()

Function Param(Name, ByVal, Value = Undefined)
	Return Struct("Param",
		"Name"  // string
		"ByVal" // boolean
		"Value" // undefined, structure (UnaryExpr, BasicLitExpr)
	, Name, ByVal, Value);
EndFunction // Param()

#EndRegion // Scope

#Region Declarations

Function VarModListDecl(VarList, Place = Undefined)
	Return Struct("VarModListDecl",
		"List"  // array (VarMod)
		"Place" // undefined, structure (Place)
	, VarList, Place);
EndFunction // VarModListDecl()

Function VarLocListDecl(VarList, Place = Undefined)
	Return Struct("VarLocListDecl",
		"List"  // array (VarLoc)
		"Place" // undefined, structure (Place)
	, VarList, Place);
EndFunction // VarLocListDecl()

Function ProcDecl(Object, Decls, Auto, Body, Place = Undefined)
	Return Struct("ProcDecl",
		"Object" // structure (Proc)
		"Decls"  // array (one of #Declarations)
		"Auto"   // array (VarLoc)
		"Body"   // array (one of #Statements)
		"Place"  // undefined, structure (Place)
	, Object, Decls, Auto, Body, Place);
EndFunction // ProcDecl()

Function FuncDecl(Object, Decls, Auto, Body, Place = Undefined)
	Return Struct("FuncDecl",
		"Object" // structure (Func)
		"Decls"  // array (one of #Declarations)
		"Auto"   // array (VarLoc)
		"Body"   // array (one of #Statements)
		"Place"  // undefined, structure (Place)
	, Object, Decls, Auto, Body, Place);
EndFunction // FuncDecl()

Function PrepIfDecl(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined, Place = Undefined)
	Return Struct("PrepIfDecl",
		"Cond"  // structure (one of #Expressions)
		"Then"  // array (one of #Declarations)
		"ElsIf" // undefined, array (PrepElsIfDecl)
		"Else"  // undefined, array (one of #Declarations)
		"Place" // undefined, structure (Place)
	, Cond, ThenPart, ElsIfPart, ElsePart, Place);
EndFunction // PrepIfDecl()

Function PrepElsIfDecl(Cond, ThenPart, Place = Undefined)
	Return Struct("PrepElsIfDecl",
		"Cond"  // structure (one of #Expressions)
		"Then"  // array (one of #Declarations)
		"Place" // undefined, structure (Place)
	, Cond, ThenPart, Place);
EndFunction // PrepElsIfDecl()

Function PrepRegionDecl(Name, Decls, Body, Place = Undefined)
	Return Struct("PrepRegionDecl",
		"Name"  // structure (one of #Expressions)
		"Decls" // array (one of #Declarations)
		"Body"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Name, Decls, Body, Place);
EndFunction // PrepRegionDecl()

#EndRegion // Declarations

#Region Expressions

Function BasicLitExpr(Kind, Value, Place = Undefined)
	Return Struct("BasicLitExpr",
		"Kind"  // string (one of Tokens)
		"Value" // undefined, string, number, boolean, date, null
		"Place" // undefined, structure (Place)
	, Kind, Value, Place);
EndFunction // BasicLitExpr()

Function Selector(Kind, Value, Place = Undefined)
	Return Struct("Selector",
		"Kind"  // string (one of SelectorKinds)
		"Value" // string, array (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Kind, Value, Place);
EndFunction // Selector()

Function DesigExpr(Object, Selectors, Call, Place = Undefined)
	Return Struct("DesigExpr",
		"Object" // structure (Unknown, Func, Proc, VarMod, VarLoc, Param)
		"Select" // array (Selector)
		"Call"   // boolean
		"Place"  // undefined, structure (Place)
	, Object, Selectors, Call, Place);
EndFunction // DesigExpr()

Function UnaryExpr(Operator, Operand, Place = Undefined)
	Return Struct("UnaryExpr",
		"Operator" // string (one of Tokens)
		"Operand"  // structure (one of #Expressions)
		"Place"    // undefined, structure (Place)
	, Operator, Operand, Place);
EndFunction // UnaryExpr()

Function BinaryExpr(Left, Operator, Right, Place = Undefined)
	Return Struct("BinaryExpr",
		"Left"     // structure (one of #Expressions)
		"Operator" // string (one of Tokens)
		"Right"    // structure (one of #Expressions)
		"Place"    // undefined, structure (Place)
	, Left, Operator, Right, Place);
EndFunction // BinaryExpr()

Function NewExpr(Constr, Place = Undefined)
	Return Struct("NewExpr",
		"Constr" // structure (DesigExpr) or array (one of #Expressions)
		"Place"  // undefined, structure (Place)
	, Constr, Place);
EndFunction // NewExpr()

Function TernaryExpr(Cond, ThenPart, ElsePart, Selectors, Place = Undefined)
	Return Struct("TernaryExpr",
		"Cond"   // structure (one of #Expressions)
		"Then"   // structure (one of #Expressions)
		"Else"   // structure (one of #Expressions)
		"Select" // array (Selector)
		"Place"  // undefined, structure (Place)
	, Cond, ThenPart, ElsePart, Selectors, Place);
EndFunction // TernaryExpr()

Function ParenExpr(Expr, Place = Undefined)
	Return Struct("ParenExpr",
		"Expr"  // structure (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Expr, Place);
EndFunction // ParenExpr()

Function NotExpr(Expr, Place = Undefined)
	Return Struct("NotExpr",
		"Expr"  // structure (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Expr, Place);
EndFunction // NotExpr()

Function StringExpr(ExprList, Place = Undefined)
	Return Struct("StringExpr",
		"List"  // array (BasicLitExpr)
		"Place" // undefined, structure (Place)
	, ExprList, Place);
EndFunction // StringExpr()

#EndRegion // Expressions

#Region Statements

Function AssignStmt(Left, Right, Place = Undefined)
	Return Struct("AssignStmt",
		"Left"  // structure (DesigExpr)
		"Right" // structure (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Left, Right, Place);
EndFunction // AssignStmt()

Function ReturnStmt(Expr = Undefined, Place = Undefined)
	Return Struct("ReturnStmt",
		"Expr"  // undefined, structure (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Expr, Place);
EndFunction // ReturnStmt()

Function BreakStmt(Place = Undefined)
	Return Struct("BreakStmt",
		"Place" // undefined, structure (Place)
	, Place);
EndFunction // BreakStmt()

Function ContinueStmt(Place = Undefined)
	Return Struct("ContinueStmt",
		"Place" // undefined, structure (Place)
	, Place);
EndFunction // ContinueStmt()

Function RaiseStmt(Expr = Undefined, Place = Undefined)
	Return Struct("RaiseStmt",
		"Expr"  // undefined, structure (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Expr, Place);
EndFunction // RaiseStmt()

Function ExecuteStmt(Expr, Place = Undefined)
	Return Struct("ExecuteStmt",
		"Expr"  // structure (one of #Expressions)
		"Place" // undefined, structure (Place)
	, Expr, Place);
EndFunction // ExecuteStmt()

Function CallStmt(DesigExpr, Place = Undefined)
	Return Struct("CallStmt",
		"Desig" // structure (DesigExpr)
		"Place" // undefined, structure (Place)
	, DesigExpr, Place);
EndFunction // CallStmt()

Function IfStmt(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined, Place = Undefined)
	Return Struct("IfStmt",
		"Cond"  // structure (one of #Expressions)
		"Then"  // array (one of #Statements)
		"ElsIf" // undefined, array (ElsIfStmt)
		"Else"  // undefined, array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Cond, ThenPart, ElsIfPart, ElsePart, Place);
EndFunction // IfStmt()

Function ElsIfStmt(Cond, ThenPart, Place = Undefined)
	Return Struct("ElsIfStmt",
		"Cond"  // structure (one of #Expressions)
		"Then"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Cond, ThenPart, Place);
EndFunction // ElsIfStmt()

Function PrepIfStmt(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined, Place = Undefined)
	Return Struct("PrepIfStmt",
		"Cond"  // structure (one of #Expressions)
		"Then"  // array (one of #Statements)
		"ElsIf" // undefined, array (PrepElsIfStmt)
		"Else"  // undefined, array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Cond, ThenPart, ElsIfPart, ElsePart, Place);
EndFunction // PrepIfStmt()

Function PrepElsIfStmt(Cond, ThenPart, Place = Undefined)
	Return Struct("PrepElsIfStmt",
		"Cond"  // structure (one of #Expressions)
		"Then"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Cond, ThenPart, Place);
EndFunction // PrepElsIfStmt()

Function WhileStmt(Cond, Statements, Place = Undefined)
	Return Struct("WhileStmt",
		"Cond"  // structure (one of #Expressions)
		"Body"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Cond, Statements, Place);
EndFunction // WhileStmt()

Function PrepRegionStmt(Name, Statements, Place = Undefined)
	Return Struct("PrepRegionStmt",
		"Name"  // structure (one of #Expressions)
		"Body"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, Name, Statements, Place);
EndFunction // PrepRegionStmt()

Function ForStmt(DesigExpr, From, Until, Statements, Place = Undefined)
	Return Struct("ForStmt",
		"Desig" // structure (DesigExpr)
		"From"  // structure (one of #Expressions)
		"To"    // structure (one of #Expressions)
		"Body"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, DesigExpr, From, Until, Statements, Place);
EndFunction // ForStmt()

Function ForEachStmt(DesigExpr, Collection, Statements, Place = Undefined)
	Return Struct("ForEachStmt",
		"Desig" // structure (DesigExpr)
		"In"    // structure (one of #Expressions)
		"Body"  // array (one of #Statements)
		"Place" // undefined, structure (Place)
	, DesigExpr, Collection, Statements, Place);
EndFunction // ForEachStmt()

Function TryStmt(TryPart, ExceptPart, Place = Undefined)
	Return Struct("TryStmt",
		"Try"    // array (one of #Statements)
		"Except" // array (one of #Statements)
		"Place"  // undefined, structure (Place)
	, TryPart, ExceptPart, Place);
EndFunction // TryStmt()

Function GotoStmt(Label, Place = Undefined)
	Return Struct("GotoStmt",
		"Label" // string
		"Place" // undefined, structure (Place)
	, Label, Place);
EndFunction // GotoStmt()

Function LabelStmt(Label, Place = Undefined)
	Return Struct("LabelStmt",
		"Label" // string
		"Place" // undefined, structure (Place)
	, Label, Place);
EndFunction // LabelStmt()

#EndRegion // Statements

#EndRegion // AbstractSyntaxTree

#Region Parser

Function Parser(Source) Export
	Var Parser;

	Parser = Class("Parser",
		"Scanner"   // structure (Scanner)
		"Line"      // number
		"Pos"       // number
		"PrevPos"   // number
		"Tok"       // string (one of Tokens)
		"Lit"       // string
		"Val"       // number, string, date, boolean, undefined, null
		"Scope"     // structure (Scope)
		"Vars"      // structure as map[string] (VarMod, VarLoc)
		"Methods"   // structure as map[string] (Func, Proc)
		"Module"    // structure (Module)
		"Unknown"   // structure as map[string] (Unknown)
		"IsFunc"    // boolean
		"Directive" // string (one of Directives)
		"Interface" // array (Func, Proc)
		"Comments"  // map[number] (string)
	);

	Parser.Scanner = Scanner(Source);
	Parser.Pos = 0;
	Parser.Line = 1;
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
	Parser.Line = Scanner.Line;
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

Procedure ParseModule(Parser) Export
	Var Decls, Auto, VarObj, Item, Statements;
	Next(Parser);
	Decls = ParseModDecls(Parser);
	Statements = ParseStatements(Parser);
	Auto = New Array;
	For Each VarObj In Parser.Scope.Auto Do
		Auto.Add(VarObj);
	EndDo;
	Parser.Module = Module(Decls, Auto, Statements, Parser.Interface, Parser.Comments);
	If Verbose Then
		For Each Item In Parser.Unknown Do
			Message(StrTemplate("Undeclared method `%1`", Item.Key));
		EndDo;
	EndIf;
	Expect(Parser, Tokens.Eof);
EndProcedure // ParseModule()

#Region ParseExpr

Function ParseExpression(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Expr = ParseAndExpr(Parser);
	While Parser.Tok = Tokens.Or Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseAndExpr(Parser), Place(Parser, Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseExpression()

Function ParseAndExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Expr = ParseNotExpr(Parser);
	While Parser.Tok = Tokens.And Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseNotExpr(Parser), Place(Parser, Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseNotExpr(Parser)
	Var Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	If Parser.Tok = Tokens.Not Then
		Next(Parser);
		Expr = NotExpr(ParseRelExpr(Parser), Place(Parser, Pos, Line));
	Else
		Expr = ParseRelExpr(Parser);
	EndIf;
	Return Expr;
EndFunction // ParseNotExpr()

Function ParseRelExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Expr = ParseAddExpr(Parser);
	While RelOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseAddExpr(Parser), Place(Parser, Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseRelExpr()

Function ParseAddExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Expr = ParseMulExpr(Parser);
	While AddOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseMulExpr(Parser), Place(Parser, Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseAddExpr()

Function ParseMulExpr(Parser)
	Var Expr, Operator, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Expr = ParseUnaryExpr(Parser);
	While MulOperators.Find(Parser.Tok) <> Undefined Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseUnaryExpr(Parser), Place(Parser, Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseMulExpr()

Function ParseUnaryExpr(Parser)
	Var Operator, Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Operator = Parser.Tok;
	If AddOperators.Find(Parser.Tok) <> Undefined Then
		Next(Parser);
		Expr = UnaryExpr(Operator, ParseOperand(Parser), Place(Parser, Pos, Line));
	ElsIf Parser.Tok = Tokens.Eof Then
		Expr = Undefined;
	Else
		Expr = ParseOperand(Parser);
	EndIf;
	Return Expr;
EndFunction // ParseUnaryExpr()

Function ParseOperand(Parser)
	Var Tok, Operand;
	Tok = Parser.Tok;
	If Tok = Tokens.String Or Tok = Tokens.StringBeg Then
		Operand = ParseStringExpr(Parser);
	ElsIf BasicLitNoString.Find(Tok) <> Undefined Then
		Operand = BasicLitExpr(Tok, Parser.Val, Place(Parser));
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
	Line = Parser.Line;
	Tok = Parser.Tok;
	ExprList = New Array;
	While True Do
		If Tok = Tokens.String Then
			ExprList.Add(BasicLitExpr(Tok, Parser.Val, Place(Parser)));
			Tok = Next(Parser);
			While Tok = Tokens.String Do
				ExprList.Add(BasicLitExpr(Tok, Parser.Val, Place(Parser)));
				Tok = Next(Parser);
			EndDo;
		ElsIf Tok = Tokens.StringBeg Then
			ExprList.Add(BasicLitExpr(Tok, Parser.Val, Place(Parser)));
			Tok = Next(Parser);
			While Tok = Tokens.StringMid Do
				ExprList.Add(BasicLitExpr(Tok, Parser.Val, Place(Parser)));
				Tok = Next(Parser);
			EndDo;
			If Tok <> Tokens.StringEnd Then
				Error(Parser.Scanner, "Expected """,, True);
			EndIf;
			ExprList.Add(BasicLitExpr(Tok, Parser.Val, Place(Parser)));
			Tok = Next(Parser);
		Else
			Break;
		EndIf;
	EndDo;
	Return StringExpr(ExprList, Place(Parser, Pos, Line));
EndFunction // ParseStringExpr()

Function ParseNewExpr(Parser)
	Var Tok, Constr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
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
	Return NewExpr(Constr, Place(Parser, Pos, Line));
EndFunction // ParseNewExpr()

Function ParseDesigExpr(Parser, Val AllowNewVar = False)
	Var Name, Selector, Object, List, Kind, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
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
			Object = VarLoc(Name, True);
			Parser.Vars.Insert(Name, Object);
			Parser.Scope.Auto.Add(Object);
		Else
			Object = Unknown(Name);
			If Verbose Then
				Error(Parser.Scanner, StrTemplate("Undeclared identifier `%1`", Name), Pos);
			EndIf;
		EndIf;
	EndIf;
	Return DesigExpr(Object, List, Kind = SelectorKinds.Call, Place(Parser, Pos, Line));
EndFunction // ParseDesigExpr()

Function ParseSelector(Parser)
	Var Tok, Value, Selector, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Tok = Parser.Tok;
	If Tok = Tokens.Period Then
		Next(Parser);
		If Not Keywords.Property(Parser.Lit) Then
			Expect(Parser, Tokens.Ident);
		EndIf;
		Value = Parser.Lit;
		Selector = Selector(SelectorKinds.Ident, Value, Place(Parser, Pos, Line));
		Next(Parser);
	ElsIf Tok = Tokens.Lbrack Then
		Tok = Next(Parser);
		If Tok = Tokens.Rbrack Then
			Error(Parser.Scanner, "Expected expression", Pos, True);
		EndIf;
		Value = ParseExprList(Parser);
		Expect(Parser, Tokens.Rbrack);
		Selector = Selector(SelectorKinds.Index, Value, Place(Parser, Pos, Line));
		Next(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Tok = Next(Parser);
		If Tok = Tokens.Rparen Then
			Value = EmptyArray;
		Else
			Value = ParseArguments(Parser);
		EndIf;
		Expect(Parser, Tokens.Rparen);
		Selector = Selector(SelectorKinds.Call, Value, Place(Parser, Pos, Line));
		Next(Parser);
	EndIf;
	Return Selector;
EndFunction // ParseSelector()

Function ParseExprList(Parser, HeadExpr = Undefined)
	Var ExprList;
	If HeadExpr = Undefined Then
		HeadExpr = ParseExpression(Parser);
	EndIf;
	ExprList = New Array;
	ExprList.Add(HeadExpr);
	While Parser.Tok = Tokens.Comma And InitOfExpression.Find(Next(Parser)) <> Undefined Do
		ExprList.Add(ParseExpression(Parser));
	EndDo;
	Return ExprList;
EndFunction // ParseExprList()

Function ParseArguments(Parser)
	Var ExprList, ExpectExpression;
	ExprList = New Array;
	ExpectExpression = True;
	While ExpectExpression Do
		If InitOfExpression.Find(Parser.Tok) <> Undefined Then
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
	Var Cond, ThenPart, ElsePart, Selectors, Selector, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
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
	Return TernaryExpr(Cond, ThenPart, ElsePart, Selectors, Place(Parser, Pos, Line));
EndFunction // ParseTernaryExpr()

Function ParseParenExpr(Parser)
	Var Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Next(Parser);
	Expr = ParseExpression(Parser);
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	Return ParenExpr(Expr, Place(Parser, Pos, Line));
EndFunction // ParseParenExpr()

#EndRegion // ParseExpr

#Region ParseDecl

Function ParseModDecls(Parser)
	Var Tok, Decls;
	Decls = ParseModVarDecls(Parser);
	Tok = Parser.Tok;
	While True Do
		If Tok = Tokens.Function Then
			Decls.Add(ParseFuncDecl(Parser));
		ElsIf Tok = Tokens.Procedure Then
			Decls.Add(ParseProcDecl(Parser));
		ElsIf Tok = Tokens._Region Then
			Decls.Add(ParsePrepRegionDecl(Parser));
		ElsIf Tok = Tokens._If Then
			Decls.Add(ParsePrepIfDecl(Parser));
		Else
			Break;
		EndIf;
		Tok = Parser.Tok;
		Parser.Directive = Undefined;
		While Tok = Tokens.Directive Do
			Parser.Directive = Parser.Lit;
			Tok = Next(Parser);
		EndDo;
	EndDo;
	Return Decls;
EndFunction // ParseModDecls()

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

Function ParseModVarListDecl(Parser)
	Var VarList, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	VarList = New Array;
	VarList.Add(ParseVarMod(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVarMod(Parser));
	EndDo;
	Return VarModListDecl(VarList, Place(Parser, Pos, Line));
EndFunction // ParseModVarListDecl()

Function ParseVarMod(Parser)
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
	Object = VarMod(Name, Parser.Directive, Exported);
	If Exported Then
		Parser.Interface.Add(Object);
	EndIf;
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared", Pos, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
	Return Object;
EndFunction // ParseVarMod()

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

Function ParseVarListDecl(Parser)
	Var VarList, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	VarList = New Array;
	VarList.Add(ParseVarLoc(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVarLoc(Parser));
	EndDo;
	Return VarLocListDecl(VarList, Place(Parser, Pos, Line));
EndFunction // ParseVarListDecl()

Function ParseVarLoc(Parser)
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
	Object = VarLoc(Name);
	If Exported Then
		Parser.Interface.Add(Object);
	EndIf;
	If Parser.Vars.Property(Name) Then
		Error(Parser.Scanner, "Identifier already declared", Pos, True);
	EndIf;
	Parser.Vars.Insert(Name, Object);
	Return Object;
EndFunction // ParseVarLoc()

Function ParseFuncDecl(Parser)
	Var Object, Name, Decls, ParamList, Exported, Statements, Auto, VarObj, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
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
	Auto = New Array;
	For Each VarObj In Parser.Scope.Auto Do
		Auto.Add(VarObj);
	EndDo;
	CloseScope(Parser);
	Next(Parser);
	Return FuncDecl(Object, Decls, Auto, Statements, Place(Parser, Pos, Line));
EndFunction // ParseFuncDecl()

Function ParseProcDecl(Parser)
	Var Object, Name, Decls, ParamList, Exported, Auto, VarObj, Statements, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
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
	Auto = New Array;
	For Each VarObj In Parser.Scope.Auto Do
		Auto.Add(VarObj);
	EndDo;
	CloseScope(Parser);
	Next(Parser);
	Return ProcDecl(Object, Decls, Auto, Statements, Place(Parser, Pos, Line));
EndFunction // ParseProcDecl()

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

Function ParsePrepIfDecl(Parser)
	Var Tok, Cond, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen, Pos, Line;
	Next(Parser);
	Cond = ParseExpression(Parser); // todo: only logic operators
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseModDecls(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens._ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens._ElsIf Do
			Pos = Parser.Pos;
			Line = Parser.Line;
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseModDecls(Parser);
			ElsIfPart.Add(PrepElsIfDecl(ElsIfCond, ElsIfThen, Place(Parser, Pos, Line)));
			Tok = Parser.Tok;
		EndDo;
	EndIf;
	If Tok = Tokens._Else Then
		Next(Parser);
		ElsePart = ParseModDecls(Parser);
	EndIf;
	Expect(Parser, Tokens._EndIf);
	Next(Parser);
	Return PrepIfDecl(Cond, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParsePrepIfDecl()

Function ParsePrepRegionDecl(Parser)
	Var Name, Decls, Statements, Region, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Next(Parser);
	Decls = ParseModDecls(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens._EndRegion);
	Next(Parser);
	Return PrepRegionDecl(Name, Decls, Statements, Place(Parser, Pos, Line));
EndFunction // ParsePrepRegionDecl()

#EndRegion // ParseDecl

#Region ParseStmt

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
	Line = Parser.Line;
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
	If Stmt <> Undefined Then
		Stmt.Place = Place(Parser, Pos, Line);
	EndIf;
	Return Stmt;
EndFunction // ParseStmt()

Function ParseRaiseStmt(Parser)
	Var Tok, Expr;
	Next(Parser);
	If InitOfExpression.Find(Parser.Tok) <> Undefined Then
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
	Var ElsIfPart, ElsIfCond, ElsIfThen, Pos, Line;
	Next(Parser);
	Cond = ParseExpression(Parser);
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseStatements(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens.ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens.ElsIf Do
			Pos = Parser.Pos;
			Line = Parser.Line;
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseStatements(Parser);
			ElsIfPart.Add(ElsIfStmt(ElsIfCond, ElsIfThen, Place(Parser, Pos, Line)));
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

Function ParseReturnStmt(Parser)
	Var Expr, Pos, Line;
	Pos = Parser.Pos;
	Line = Parser.Line;
	Next(Parser);
	If Parser.IsFunc Then
		Expr = ParseExpression(Parser);
	EndIf;
	Return ReturnStmt(Expr, Place(Parser, Pos, Line));
EndFunction // ParseReturnStmt()

Function ParsePrepIfStmt(Parser)
	Var Tok, Cond, ThenPart, ElsePart;
	Var ElsIfPart, ElsIfCond, ElsIfThen, Pos, Line;
	Next(Parser);
	Cond = ParseExpression(Parser); // todo: only logic operators
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseStatements(Parser);
	Tok = Parser.Tok;
	If Tok = Tokens._ElsIf Then
		ElsIfPart = New Array;
		While Tok = Tokens._ElsIf Do
			Pos = Parser.Pos;
			Line = Parser.Line;
			Next(Parser);
			ElsIfCond = ParseExpression(Parser);
			Expect(Parser, Tokens.Then);
			Next(Parser);
			ElsIfThen = ParseStatements(Parser);
			ElsIfPart.Add(PrepElsIfStmt(ElsIfCond, ElsIfThen, Place(Parser, Pos, Line)));
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

#EndRegion // ParseStmt

#EndRegion // Parser

#Region Auxiliary

Function Struct(Type, Properties = "", Value1 = Undefined, Value2 = Undefined, Value3 = Undefined, Value4 = Undefined, Value5 = Undefined, Value6 = Undefined)
	Var Keys;
	If Not StructKeysCache.Property(Type, Keys) Then
		Keys = "Type," + StrReplace(Properties, Chars.LF, ",");
		If StrOccurrenceCount(Keys, ",") > 5 Then
			Raise "call in violation of protocol";
		EndIf;
		StructKeysCache.Insert(Type, Keys);
	EndIf;
	Return New Structure(Keys, Type, Value1, Value2, Value3, Value4, Value5, Value6);
EndFunction // Struct()

Function Class(Type, Properties = "")
	Var Keys;
	If Not StructKeysCache.Property(Type, Keys) Then
		Keys = "Type," + StrReplace(Properties, Chars.LF, ",");
		StructKeysCache.Insert(Type, Keys);
	EndIf;
	Return New Structure(Keys);
EndFunction // Class()

Function Place(Parser, Pos = Undefined, Line = Undefined)
	Var Place, Len;
	If Location Then
		If Pos = Undefined Then
			Len = StrLen(Parser.Lit);
			Pos = Parser.Pos - Len;
		Else
			Len = Parser.PrevPos - Pos;
		EndIf;
		If Line = Undefined Then
			Line = Parser.Line;
		EndIf;
		Place = New Structure(
			"Line," // number
			"Pos,"  // number
			"Len,"  // number
		, Line, Pos, Len);
		If Debug Then
			Place.Insert("Str", Mid(Parser.Scanner.Source, Pos, Len));
		EndIf;
	EndIf;
	Return Place;
EndFunction // Place()

Function Value(Tok, Lit)
	Var Value;
	If Tok = Tokens.Number Then
		Value = Number(Lit);
	ElsIf Tok = Tokens.DateTime Then
		Value = AsDate(Lit);
	ElsIf StrStartsWith(Tok, Tokens.String) Then
		Value = Mid(Lit, 2, StrLen(Lit) - 2);
	ElsIf Tok = Tokens.True Then
		Value = True;
	ElsIf Tok = Tokens.False Then
		Value = False;
	ElsIf Tok = Tokens.Null Then
		Value = Null;
	EndIf;
	Return Value;
EndFunction // Value()

Function AsDate(DateLit)
	Var List, Char, Num;
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
	Var Tok;
	If Left(Lit, 1) = """" Then
		If Right(Lit, 1) = """" Then
			Tok = Tokens.String;
		Else
			Tok = Tokens.StringBeg;
		EndIf;
	Else // |
		If Right(Lit, 1) = """" Then
			Tok = Tokens.StringEnd;
		Else
			Tok = Tokens.StringMid;
		EndIf;
	EndIf;
	Return Tok;
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

#Region Visitor

Function Visitor(Hooks) Export
	Var Visitor;

	Visitor = Class("Visitor",
		"Hooks" // structure as map[string] (array)
	);

	Visitor.Hooks = Hooks;

	Return Visitor;
EndFunction // Visitor()

Function VisitModule(Visitor, Module) Export
	Var Hook;
	VisitDeclarations(Visitor, Module.Decls);
	VisitStatements(Visitor, Module.Body);
	For Each Hook In Visitor.Hooks.VisitModule Do
		Hook.VisitModule(Module);
	EndDo;
EndFunction // VisitModule()

Function VisitDeclarations(Visitor, Declarations)
	Var Decl, Hook;
	For Each Decl In Declarations Do
		VisitDecl(Visitor, Decl);
	EndDo;
	For Each Hook In Visitor.Hooks.VisitDeclarations Do
		Hook.VisitDeclarations(Declarations);
	EndDo;
EndFunction // VisitDeclarations()

Function VisitStatements(Visitor, Statements)
	Var Stmt, Hook;
	For Each Stmt In Statements Do
		VisitStmt(Visitor, Stmt);
	EndDo;
	For Each Hook In Visitor.Hooks.VisitStatements Do
		Hook.VisitStatements(Statements);
	EndDo;
EndFunction // VisitStatements()

#Region VisitDecl

Procedure VisitDecl(Visitor, Decl)
	Var Type, Hook;
	Type = Decl.Type;
	If Type = "VarModListDecl" Then
		VisitVarModListDecl(Visitor, Decl);
	ElsIf Type = "VarLocListDecl" Then
		VisitVarLocListDecl(Visitor, Decl);
	ElsIf Type = "ProcDecl" Then
		VisitProcDecl(Visitor, Decl);
	ElsIf Type = "FuncDecl" Then
		VisitFuncDecl(Visitor, Decl);
	ElsIf Type = "PrepIfDecl" Then
		VisitPrepIfDecl(Visitor, Decl);
	ElsIf Type = "PrepElsIfDecl" Then
		VisitPrepElsIfDecl(Visitor, Decl);
	ElsIf Type = "PrepRegionDecl" Then
		VisitPrepRegionDecl(Visitor, Decl);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitDecl Do
		Hook.VisitDecl(Visitor, Decl);
	EndDo;
EndProcedure // VisitDecl()

Procedure VisitVarModListDecl(Visitor, VarModListDecl)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitVarModListDecl Do
		Hook.VisitVarModListDecl(VarModListDecl);
	EndDo;
EndProcedure // VisitVarModListDecl()

Procedure VisitVarLocListDecl(Visitor, VarLocListDecl)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitVarLocListDecl Do
		Hook.VisitVarLocListDecl(VarLocListDecl);
	EndDo;
EndProcedure // VisitVarLocListDecl()

Procedure VisitProcDecl(Visitor, ProcDecl)
	Var Hook;
	VisitDeclarations(Visitor, ProcDecl.Decls);
	VisitStatements(Visitor, ProcDecl.Body);
	For Each Hook In Visitor.Hooks.VisitProcDecl Do
		Hook.VisitProcDecl(ProcDecl);
	EndDo;
EndProcedure // VisitProcDecl()

Procedure VisitFuncDecl(Visitor, FuncDecl)
	Var Hook;
	VisitDeclarations(Visitor, FuncDecl.Decls);
	VisitStatements(Visitor, FuncDecl.Body);
	For Each Hook In Visitor.Hooks.VisitFuncDecl Do
		Hook.VisitFuncDecl(FuncDecl);
	EndDo;
EndProcedure // VisitFuncDecl()

Procedure VisitPrepIfDecl(Visitor, PrepIfDecl)
	Var Hook;
	VisitExpr(Visitor, PrepIfDecl.Cond);
	VisitStatements(Visitor, PrepIfDecl.Then);
	If PrepIfDecl.ElsIf <> Undefined Then
		VisitPrepElsIfDecl(Visitor, PrepIfDecl.ElsIf);
	EndIf;
	If PrepIfDecl.Else <> Undefined Then
		VisitStatements(Visitor, PrepIfDecl.Else);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitPrepIfDecl Do
		Hook.VisitPrepIfDecl(PrepIfDecl);
	EndDo;
EndProcedure // VisitPrepIfDecl()

Procedure VisitPrepElsIfDecl(Visitor, PrepElsIfDecl)
	Var Hook;
	VisitExpr(Visitor, PrepElsIfDecl.Cond);
	VisitStatements(Visitor, PrepElsIfDecl.Then);
	For Each Hook In Visitor.Hooks.VisitPrepElsIfDecl Do
		Hook.VisitPrepElsIfDecl(PrepElsIfDecl);
	EndDo;
EndProcedure // VisitPrepElsIfDecl()

Procedure VisitPrepRegionDecl(Visitor, PrepRegionDecl)
	Var Hook, Decl, Stmt;
	VisitDeclarations(Visitor, PrepRegionDecl.Decls);
	VisitStatements(Visitor, PrepRegionDecl.Body);
	For Each Hook In Visitor.Hooks.VisitPrepRegionDecl Do
		Hook.VisitPrepRegionDecl(PrepRegionDecl);
	EndDo;
EndProcedure // VisitPrepRegionDecl()

#EndRegion // VisitDecl

#Region VisitExpr

Procedure VisitExpr(Visitor, Expr)
	Var Type;
	Type = Expr.Type;
	If Type = "BasicLitExpr" Then
		VisitBasicLitExpr(Visitor, Expr);
	ElsIf Type = "DesigExpr" Then
		VisitDesigExpr(Visitor, Expr);
	ElsIf Type = "UnaryExpr" Then
		VisitUnaryExpr(Visitor, Expr);
	ElsIf Type = "BinaryExpr" Then
		VisitBinaryExpr(Visitor, Expr);
	ElsIf Type = "NewExpr" Then
		VisitNewExpr(Visitor, Expr);
	ElsIf Type = "TernaryExpr" Then
		VisitTernaryExpr(Visitor, Expr);
	ElsIf Type = "ParenExpr" Then
		VisitParenExpr(Visitor, Expr);
	ElsIf Type = "NotExpr" Then
		VisitNotExpr(Visitor, Expr);
	ElsIf Type = "StringExpr" Then
		VisitStringExpr(Visitor, Expr);
	EndIf;
EndProcedure // VisitExpr()

Procedure VisitBasicLitExpr(Visitor, BasicLitExpr)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitBasicLitExpr Do
		Hook.VisitBasicLitExpr(BasicLitExpr);
	EndDo;
EndProcedure // VisitBasicLitExpr()

Procedure VisitDesigExpr(Visitor, DesigExpr)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitDesigExpr Do
		Hook.VisitDesigExpr(DesigExpr);
	EndDo;
EndProcedure // VisitDesigExpr()

Procedure VisitUnaryExpr(Visitor, UnaryExpr)
	Var Hook;
	VisitExpr(Visitor, UnaryExpr.Operand);
	For Each Hook In Visitor.Hooks.VisitUnaryExpr Do
		Hook.VisitUnaryExpr(UnaryExpr);
	EndDo;
EndProcedure // VisitUnaryExpr()

Procedure VisitBinaryExpr(Visitor, BinaryExpr)
	Var Hook;
	VisitExpr(Visitor, BinaryExpr.Left);
	VisitExpr(Visitor, BinaryExpr.Right);
	For Each Hook In Visitor.Hooks.VisitBinaryExpr Do
		Hook.VisitBinaryExpr(BinaryExpr);
	EndDo;
EndProcedure // VisitBinaryExpr()

Procedure VisitNewExpr(Visitor, NewExpr)
	Var Expr, Hook;
	If TypeOf(NewExpr.Constr) = Type("Structure") Then
		VisitDesigExpr(Visitor, NewExpr.Constr);
	Else
		For Each Expr In NewExpr.Constr Do
			VisitExpr(Visitor, Expr);
		EndDo;
	EndIf;
	For Each Hook In Visitor.Hooks.VisitNewExpr Do
		Hook.VisitNewExpr(NewExpr);
	EndDo;
EndProcedure // VisitNewExpr()

Procedure VisitTernaryExpr(Visitor, TernaryExpr)
	Var Hook;
	VisitExpr(Visitor, TernaryExpr.Cond);
	VisitExpr(Visitor, TernaryExpr.Then);
	VisitExpr(Visitor, TernaryExpr.Else);
	For Each Hook In Visitor.Hooks.VisitTernaryExpr Do
		Hook.VisitTernaryExpr(TernaryExpr);
	EndDo;
EndProcedure // VisitTernaryExpr()

Procedure VisitParenExpr(Visitor, ParenExpr)
	Var Hook;
	VisitExpr(Visitor, ParenExpr.Expr);
	For Each Hook In Visitor.Hooks.VisitParenExpr Do
		Hook.VisitParenExpr(ParenExpr);
	EndDo;
EndProcedure // VisitParenExpr()

Procedure VisitNotExpr(Visitor, NotExpr)
	Var Hook;
	VisitExpr(Visitor, NotExpr.Expr);
	For Each Hook In Visitor.Hooks.VisitNotExpr Do
		Hook.VisitNotExpr(NotExpr);
	EndDo;
EndProcedure // VisitNotExpr()

Procedure VisitStringExpr(Visitor, StringExpr)
	Var Expr, Hook;
	For Each Expr In StringExpr.List Do
		VisitBasicLitExpr(Visitor, Expr);
	EndDo;
	For Each Hook In Visitor.Hooks.VisitStringExpr Do
		Hook.VisitStringExpr(StringExpr);
	EndDo;
EndProcedure // VisitStringExpr()

#EndRegion // VisitExpr

#Region VisitStmt

Procedure VisitStmt(Visitor, Stmt)
	Var Type, Hook;
	Type = Stmt.Type;
	If Type = "AssignStmt" Then
		VisitAssignStmt(Visitor, Stmt);
	ElsIf Type = "ReturnStmt" Then
		VisitReturnStmt(Visitor, Stmt);
	ElsIf Type = "BreakStmt" Then
		VisitBreakStmt(Visitor, Stmt);
	ElsIf Type = "ContinueStmt" Then
		VisitContinueStmt(Visitor, Stmt);
	ElsIf Type = "RaiseStmt" Then
		VisitRaiseStmt(Visitor, Stmt);
	ElsIf Type = "ExecuteStmt" Then
		VisitExecuteStmt(Visitor, Stmt);
	ElsIf Type = "CallStmt" Then
		VisitCallStmt(Visitor, Stmt);
	ElsIf Type = "IfStmt" Then
		VisitIfStmt(Visitor, Stmt);
	ElsIf Type = "PrepIfStmt" Then
		VisitPrepIfStmt(Visitor, Stmt);
	ElsIf Type = "WhileStmt" Then
		VisitWhileStmt(Visitor, Stmt);
	ElsIf Type = "PrepRegionStmt" Then
		VisitPrepRegionStmt(Visitor, Stmt);
	ElsIf Type = "ForStmt" Then
		VisitForStmt(Visitor, Stmt);
	ElsIf Type = "ForEachStmt" Then
		VisitForEachStmt(Visitor, Stmt);
	ElsIf Type = "TryStmt" Then
		VisitTryStmt(Visitor, Stmt);
	ElsIf Type = "GotoStmt" Then
		VisitGotoStmt(Visitor, Stmt);
	ElsIf Type = "LabelStmt" Then
		VisitLabelStmt(Visitor, Stmt);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitStmt Do
		Hook.VisitStmt(Stmt);
	EndDo;
EndProcedure // VisitStmt()

Procedure VisitAssignStmt(Visitor, AssignStmt)
	Var Hook;
	VisitDesigExpr(Visitor, AssignStmt.Left);
	VisitExpr(Visitor, AssignStmt.Right);
	For Each Hook In Visitor.Hooks.VisitAssignStmt Do
		Hook.VisitAssignStmt(AssignStmt);
	EndDo;
EndProcedure // VisitAssignStmt()

Procedure VisitReturnStmt(Visitor, ReturnStmt)
	Var Hook;
	If ReturnStmt.Expr <> Undefined Then
		VisitExpr(Visitor, ReturnStmt.Expr);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitReturnStmt Do
		Hook.VisitReturnStmt(ReturnStmt);
	EndDo;
EndProcedure // VisitReturnStmt()

Procedure VisitBreakStmt(Visitor, BreakStmt)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitBreakStmt Do
		Hook.VisitBreakStmt(BreakStmt);
	EndDo;
EndProcedure // VisitBreakStmt()

Procedure VisitContinueStmt(Visitor, ContinueStmt)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitContinueStmt Do
		Hook.VisitContinueStmt(ContinueStmt);
	EndDo;
EndProcedure // VisitContinueStmt()

Procedure VisitRaiseStmt(Visitor, RaiseStmt)
	Var Hook;
	If RaiseStmt.Expr <> Undefined Then
		VisitExpr(Visitor, RaiseStmt.Expr);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitRaiseStmt Do
		Hook.VisitRaiseStmt(RaiseStmt);
	EndDo;
EndProcedure // VisitRaiseStmt()

Procedure VisitExecuteStmt(Visitor, ExecuteStmt)
	Var Hook;
	VisitExpr(Visitor, ExecuteStmt.Expr);
	For Each Hook In Visitor.Hooks.VisitExecuteStmt Do
		Hook.VisitExecuteStmt(ExecuteStmt);
	EndDo;
EndProcedure // VisitExecuteStmt()

Procedure VisitCallStmt(Visitor, CallStmt)
	Var Hook;
	VisitDesigExpr(Visitor, CallStmt.Desig);
	For Each Hook In Visitor.Hooks.VisitCallStmt Do
		Hook.VisitCallStmt(CallStmt);
	EndDo;
EndProcedure // VisitCallStmt()

Procedure VisitIfStmt(Visitor, IfStmt)
	Var ElsIfStmt, Hook;
	VisitExpr(Visitor, IfStmt.Cond);
	VisitStatements(Visitor, IfStmt.Then);
	If IfStmt.ElsIf <> Undefined Then
		For Each ElsIfStmt In IfStmt.ElsIf Do
			VisitElsIfStmt(Visitor, ElsIfStmt);
		EndDo;
	EndIf;
	If IfStmt.Else <> Undefined Then
		VisitStatements(Visitor, IfStmt.Else);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitIfStmt Do
		Hook.VisitIfStmt(IfStmt);
	EndDo;
EndProcedure // VisitIfStmt()

Procedure VisitElsIfStmt(Visitor, ElsIfStmt)
	Var Hook;
	VisitExpr(Visitor, ElsIfStmt.Cond);
	VisitStatements(Visitor, ElsIfStmt.Then);
	For Each Hook In Visitor.Hooks.VisitElsIfStmt Do
		Hook.VisitElsIfStmt(ElsIfStmt);
	EndDo;
EndProcedure // VisitElsIfStmt()

Procedure VisitPrepIfStmt(Visitor, PrepIfStmt)
	Var PrepElsIfStmt, Hook;
	VisitExpr(Visitor, PrepIfStmt.Cond);
	VisitStatements(Visitor, PrepIfStmt.Then);
	If PrepIfStmt.ElsIf <> Undefined Then
		For Each PrepElsIfStmt In PrepIfStmt.ElsIf Do
			VisitPrepElsIfStmt(Visitor, PrepElsIfStmt);
		EndDo;
	EndIf;
	If PrepIfStmt.Else <> Undefined Then
		VisitStatements(Visitor, PrepIfStmt.Else);
	EndIf;
	For Each Hook In Visitor.Hooks.VisitPrepIfStmt Do
		Hook.VisitPrepIfStmt(PrepIfStmt);
	EndDo;
EndProcedure // VisitPrepIfStmt()

Procedure VisitPrepElsIfStmt(Visitor, PrepElsIfStmt)
	Var Hook;
	VisitExpr(Visitor, PrepElsIfStmt.Cond);
	VisitStatements(Visitor, PrepElsIfStmt.Then);
	For Each Hook In Visitor.Hooks.VisitPrepElsIfStmt Do
		Hook.VisitPrepElsIfStmt(PrepElsIfStmt);
	EndDo;
EndProcedure // VisitPrepElsIfStmt()

Procedure VisitWhileStmt(Visitor, WhileStmt)
	Var Hook;
	VisitExpr(Visitor, WhileStmt.Cond);
	VisitStatements(Visitor, WhileStmt.Body);
	For Each Hook In Visitor.Hooks.VisitWhileStmt Do
		Hook.VisitWhileStmt(WhileStmt);
	EndDo;
EndProcedure // VisitWhileStmt()

Procedure VisitPrepRegionStmt(Visitor, PrepRegionStmt)
	Var Hook;
	VisitStatements(Visitor, PrepRegionStmt.Body);
	For Each Hook In Visitor.Hooks.VisitPrepRegionStmt Do
		Hook.VisitPrepRegionStmt(PrepRegionStmt);
	EndDo;
EndProcedure // VisitPrepRegionStmt()

Procedure VisitForStmt(Visitor, ForStmt)
	Var Hook;
	VisitDesigExpr(Visitor, ForStmt.Desig);
	VisitExpr(Visitor, ForStmt.From);
	VisitExpr(Visitor, ForStmt.To);
	VisitStatements(Visitor, ForStmt.Body);
	For Each Hook In Visitor.Hooks.VisitForStmt Do
		Hook.VisitForStmt(ForStmt);
	EndDo;
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(Visitor, ForEachStmt)
	Var Hook;
	VisitDesigExpr(Visitor, ForEachStmt.Desig);
	VisitExpr(Visitor, ForEachStmt.In);
	VisitStatements(Visitor, ForEachStmt.Body);
	For Each Hook In Visitor.Hooks.VisitForEachStmt Do
		Hook.VisitForEachStmt(ForEachStmt);
	EndDo;
EndProcedure // VisitForEachStmt()

Procedure VisitTryStmt(Visitor, TryStmt)
	Var Hook;
	VisitStatements(Visitor, TryStmt.TryPart);
	VisitStatements(Visitor, TryStmt.ExceptPart);
	For Each Hook In Visitor.Hooks.VisitTryStmt Do
		Hook.VisitTryStmt(TryStmt);
	EndDo;
EndProcedure // VisitTryStmt()

Procedure VisitGotoStmt(Visitor, GotoStmt)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitGotoStmt Do
		Hook.VisitGotoStmt(GotoStmt);
	EndDo;
EndProcedure // VisitGotoStmt()

Procedure VisitLabelStmt(Visitor, LabelStmt)
	Var Hook;
	For Each Hook In Visitor.Hooks.VisitLabelStmt Do
		Hook.VisitLabelStmt(LabelStmt);
	EndDo;
EndProcedure // VisitLabelStmt()

#EndRegion // VisitStmt

#EndRegion // Visitor

Init();