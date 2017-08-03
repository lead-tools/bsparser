
#Region Constants

&AtClient
Var Keywords;

&AtClient
Var Tokens;

&AtClient
Var ObjectKinds;

&AtClient
Var SelectorKinds;

&AtClient
Var UnaryOperations;

&AtClient
Var BasicLiterals;

&AtClient
Var RelationalOperators; 

#EndRegion // Constants

#Region EventHandlers

&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)
	
	If Parameters.Property("Source") Then
		Verbose = Parameters.Verbose;
		Output = Parameters.Output;
		Source.SetText(Parameters.Source);
	Else
		Verbose = True;
		Output = "AST";
	EndIf; 
	
EndProcedure

&AtClient
Procedure Reopen(Command)
	ReopenAtServer();
	Close();
	OpenForm(FormName, New Structure("Source, Verbose, Output", Source.GetText(), Verbose, Output));
EndProcedure // Reopen()

&AtServer
Procedure ReopenAtServer()
	
	This = FormAttributeToValue("Object");
	ExternalDataProcessors.Create(This.UsedFileName, False);
	
EndProcedure // ReopenAtServer() 

&AtClient
Procedure Translate(Command)
	
	Init();
	
	Result.Clear();
	
	If Output = "Lexems" Then
		
		Scanner = Scanner(Source.GetText());
		While Scan(Scanner) <> Tokens.Eof Do
			If Verbose Then
				Result.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, Scanner.Tok, Scanner.Lit));
			EndIf; 
		EndDo;
		
	ElsIf Output = "AST" Then
		
		Parser = Parser(Source.GetText());
		ParseModule(Parser);
		JSONWriter = New JSONWriter;
		FileName = GetTempFileName(".json");
		JSONWriter.OpenFile(FileName,,, New JSONWriterSettings(, Chars.Tab));
		WriteJSON(JSONWriter, Parser.Decls);
		JSONWriter.Close();
		Result.Read(FileName);
		
	ElsIf Output = "Parser" Then
		
		Parser = Parser(Source.GetText());
		ParseModule(Parser);
		JSONWriter = New JSONWriter;
		FileName = GetTempFileName(".json");
		JSONWriter.OpenFile(FileName,,, New JSONWriterSettings(, Chars.Tab));
		WriteJSON(JSONWriter, Parser);
		JSONWriter.Close();
		Result.Read(FileName);	
		
	EndIf; 
	
EndProcedure // Translate()

#EndRegion // EventHandlers

#Region Init

&AtClient
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
	
EndProcedure // Init() 

&AtClient
Procedure InitEnums()
	
	Keywords = Keywords();
	
	Tokens = Tokens(Keywords);
	
	ObjectKinds = ObjectKinds();
	
EndProcedure // InitEnums()

#EndRegion // Init

#Region Enums

&AtClientAtServerNoContext
Function Keywords()
	Var Keywords;
	
	Keywords = Enum(New Structure,
		"If, Then, ElsIf, Else, EndIf, For, Each, In, To, While, Do, EndDo,
		|Procedure, EndProcedure, Function, EndFunction,
		|Var, Val, Return, Continue, Break,
		|And, Or, Not,
		|Try, Except, Raise, EndTry,
		|New, Execute, Export,
		|True, False, Undefined,
		|Case, When, EndCase, End" // new keywords
	);
	
	Return Keywords;
	
EndFunction // Keywords() 

&AtClientAtServerNoContext
Function Tokens(Keywords)
	Var Tokens;
	
	Tokens = Enum(New Structure(Keywords),
		
		// Literals
		
		"Ident, Number, String, DateTime,
		// parts of strings
		|StringBeg, StringMid, StringEnd,
		
		// Operators
		
		// = <> < > <= >= + - * / %
		|Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod,
		// ( ) [ ] { }
		|Lparen, Rparen, Lbrack, Rbrack, Lbrace, Rbrace,
		// ? , . : ;
		|Ternary, Comma, Period, Colon, Semicolon,
		
		// Other
		
		|Illegal, Eof, Comment, Preprocessor, Directive"
		
	);
	
	Return Tokens;
	
EndFunction // Tokens() 

&AtClientAtServerNoContext
Function ObjectKinds()
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

&AtClientAtServerNoContext
Function SelectorKinds()
	Var SelectorKinds;
	
	SelectorKinds = Enum(New Structure,
		"Ident,"
		"Index,"
		"Call,"
	);
	
	Return SelectorKinds;
	
EndFunction // SelectorKinds()

&AtClientAtServerNoContext
Function Enum(Structure, Keys)
	
	For Each Item In StrSplit(Keys, ",", False) Do
		Structure.Insert(Item, TrimAll(Item));
	EndDo;
	
	Return New FixedStructure(Structure);
	
EndFunction // Enum()

#EndRegion // Enums

#Region Scanner

&AtClientAtServerNoContext
Function Scanner(Source)
	Var Scanner;
	
	Scanner = New Structure(
		"Source," // string
		"Len,"    // number
		"Pos,"    // number
		"Tok,"    // string (one of Tokens)
		"Lit,"    // string
		"Char,"   // string
		"Line,"   // number
	);
	
	Scanner.Source = Source;
	Scanner.Len = StrLen(Source);
	Scanner.Line = 1;
	Scanner.Pos = 0;
	
	Return Scanner;
	
EndFunction // Scanner() 

&AtClient
Function Scan(Scanner)
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
		NextChar(Scanner);
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
		Lit = ScanComment(Scanner);
		Tok = Tokens.Directive;
	ElsIf Char = "#" Then
		Lit = ScanComment(Scanner);
		Tok = Tokens.Preprocessor;
	Else
		Raise "Unknown char";
	EndIf; 
	If ValueIsFilled(Lit) Then
		Scanner.Lit = Lit;
	Else
		Scanner.Lit = Char;
	EndIf; 	
	Scanner.Tok = Tok; 
	Return Tok;
EndFunction // Scan() 

&AtClientAtServerNoContext
Function NextChar(Scanner)
	If Scanner.Char <> "" Then
		Scanner.Pos = Scanner.Pos + 1;
		Scanner.Char = Mid(Scanner.Source, Scanner.Pos, 1); 
	EndIf; 
	Return Scanner.Char;
EndFunction // NextChar()  

&AtClientAtServerNoContext
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

&AtClient
Function Skip(Parser, Token)
	Var Tok, Scanner;
	Scanner = Parser.Scanner;
	Tok = Scanner.Tok;
	While Tok = Token Do
		Tok = Scan(Scanner);
	EndDo;
	Parser.Tok = Tok;
	Parser.Lit = Scanner.Lit;
	Return Tok;
EndFunction // Skip()

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function ScanNumber(Scanner)
	Var Len;
	Len = ScanIntegerLen(Scanner); // Len >= 1
	If Scanner.Char = "." Then
		Len = Len + ScanIntegerLen(Scanner);	
	EndIf; 
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanNumber()

&AtClientAtServerNoContext
Function ScanIntegerLen(Scanner)
	Var Len;
	Len = 1;
	While IsDigit(NextChar(Scanner)) Do
		Len = Len + 1;
	EndDo;
	Return Len;
EndFunction // ScanIntegerLen()

&AtClientAtServerNoContext
Function ScanString(Scanner)
	Var Len;
	Len = ScanStringLen(Scanner);
	While NextChar(Scanner) = """" Do
		Len = Len + ScanStringLen(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanString()

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function ScanDateTime(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While Char <> "'" And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);	
EndFunction // ScanDateTime() 

#EndRegion // Scanner

#Region AbstractSyntaxTree

#Region Scope

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function Object(Kind, Name, Type = Undefined)
	Var Object;
	
	Object = New Structure(
		"Kind,"     // string (one of ObjectKinds)
		"Name,"     // string
		"Type,"     // structure
	,
	Kind, Name, Type);
	
	Return Object;
	
EndFunction // Object()

#EndRegion // Scope

#Region Declarations

&AtClientAtServerNoContext
Function VarDecl(Object, Init = False, Value = Undefined)
	Var VarDecl;
	
	VarDecl = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
	,
	"VarDecl", Object);
	
	If Init Then
		VarDecl.Insert("Value", Value); // one of main types
	EndIf; 
	
	Return VarDecl;
	
EndFunction // VarDecl() 

&AtClientAtServerNoContext
Function VarListDecl(VarList)
	Var VarListDecl;
	
	VarListDecl = New Structure(
		"NodeType," // string (type of this structure)
		"VarList,"  // array (VarDecl)
	,
	"VarListDecl", VarList); 
	
	Return VarListDecl;
	
EndFunction // VarListDecl()

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
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

#EndRegion // Declarations 

#Region Expressions

&AtClientAtServerNoContext
Function BasicLit(Kind, Value)
	Var BasicLit;
	
	BasicLit = New Structure(
		"NodeType," // string (type of this structure)
		"Kind,"     // string (one of Tokens)
		"Value,"    // string
	,
	"BasicLit", Kind, Value);
		
	Return BasicLit;
	
EndFunction // BasicLit() 

&AtClientAtServerNoContext
Function Selector(Kind, Value)
	Var Selector;
	
	Selector = New Structure(
		"Kind,"      // string (one of SelectorKinds)
		"Value,"     // string or array (one of expressions)
	,
	Kind, Value);
	
	Return Selector;
	
EndFunction // Selector()

&AtClientAtServerNoContext
Function Designator(Object, Selectors, Call)
	Var Designator;
	
	Designator = New Structure(
		"NodeType," // string (type of this structure)
		"Object,"   // structure (Object)
		"Call,"     // boolean
	,
	"Designator", Object, Call);
	
	If Selectors.Count() > 0 Then
		Designator.Insert("Selectors", Selectors); // array (Selector)
	EndIf; 
	
	Return Designator;
	
EndFunction // Designator() 

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function NewExpr(Designator)
	Var NewExpr;
	
	NewExpr = New Structure(
		"NodeType,"   // string (type of this structure)
		"Designator," // structure (Designator)
	,
	"NewExpr", Designator);
			
	Return NewExpr;
	
EndFunction // NewExpr()

&AtClientAtServerNoContext
Function TernaryExpr(Condition, ThenPart, ElsePart)
	Var TernaryExpr;
	
	TernaryExpr = New Structure(
		"NodeType,"   // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // structure (one of expressions)
		"ElsePart"   // structure (one of expressions)
	,
	"TernaryExpr", Condition, ThenPart, ElsePart);
			
	Return TernaryExpr;
	
EndFunction // TernaryExpr()

#EndRegion // Expressions

#Region Statements

&AtClientAtServerNoContext
Function AssignStmt(Left, Right)
	Var AssignStmt;
	
	AssignStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Left,"     // array (Designator)
		"Right,"    // array (one of expressions)
	,
	"AssignStmt", Left, Right);
	
	Return AssignStmt;
	
EndFunction // AssignStmt()

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function BreakStmt()
	Var BreakStmt;
	
	BreakStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"BreakStmt");
		
	Return BreakStmt;
	
EndFunction // BreakStmt()

&AtClientAtServerNoContext
Function ContinueStmt()
	Var ContinueStmt;
	
	ContinueStmt = New Structure(
		"NodeType," // string (type of this structure)
	,
	"ContinueStmt");
		
	Return ContinueStmt;
	
EndFunction // ContinueStmt()

&AtClientAtServerNoContext
Function RaiseStmt(Expr)
	Var RaiseStmt;
	
	RaiseStmt = New Structure(
		"NodeType," // string (type of this structure)
		"Expr,"     // structure (one of expressions)
	,
	"RaiseStmt", Expr);
		
	Return RaiseStmt;
	
EndFunction // RaiseStmt()

&AtClientAtServerNoContext
Function CallStmt(Designator)
	Var CallStmt;
	
	CallStmt = New Structure(
		"NodeType,"   // string (type of this structure)
		"Designator," // structure (Designator)
	,
	"CallStmt", Designator);
	
	Return CallStmt;
	
EndFunction // CallStmt()

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function ForStmt(Designator, Collection, Statements)
	Var ForStmt;
	
	ForStmt = New Structure(
		"NodeType,"   // string (type of this structure)
		"Designator," // structure (Designator)
		"Collection," // structure (one of expressions)
		"Statements," // array (one of statements)
	,
	"ForStmt", Designator, Collection, Statements);
	
	Return ForStmt;
	
EndFunction // ForStmt()

&AtClientAtServerNoContext
Function CaseStmt(Designator, WhenPart, ElsePart = Undefined)
	Var CaseStmt;
	
	CaseStmt = New Structure(
		"NodeType,"   // string (type of this structure)
		"Designator," // structure (one of expressions)
		"WhenPart,"   // array (IfStmt)
	,
	"CaseStmt", Designator, WhenPart);
		
	If ElsePart <> Undefined Then
		CaseStmt.Insert("ElsePart", ElsePart); // array (one of statements)
	EndIf; 
	
	Return CaseStmt;
	
EndFunction // CaseStmt()

&AtClientAtServerNoContext
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

&AtClientAtServerNoContext
Function FuncType(ParameterList)
	Var FuncType;
	
	FuncType = New Structure(
		"NodeType,"      // string (type of this structure)
		"ParameterList," // array (boolean)
	,
	"FuncType", ParameterList);
	
	Return FuncType;
EndFunction // FuncType()

#EndRegion // Types

#EndRegion // AbstractSyntaxTree

#Region Parser

&AtClientAtServerNoContext
Function Parser(Source)
	Var Parser;
	
	Parser = New Structure(
		"Scanner," // structure (Scanner)
		"Tok,"     // string (one of Tokens)
		"Lit,"     // string
		"Val,"     // number, string, date, true, false, undefined 
		"Scope,"   // structure (Scope)
		"Imports," // structure
		"Decls,"   // array (one of declarations)
		"IsFunc,"  // boolean
	);
	
	Parser.Scanner = Scanner(Source);
	Parser.Scope = Scope(Undefined);
	Parser.Imports = New Structure;
	Parser.IsFunc = False;
	
	Parser.Scope.Objects.Insert("Structure", Object("Constructor", "Structure"));
	
	Return Parser;
	
EndFunction // Parser() 

&AtClient
Function Next(Parser)
	Scan(Parser.Scanner);
	Skip(Parser, Tokens.Comment);
	Skip(Parser, Tokens.Preprocessor);
	Skip(Parser, Tokens.Directive);
	If Parser.Tok = Tokens.StringBeg Then
		Parser.Lit = ParseString(Parser);
		Parser.Tok = Tokens.String;
	Else 
		Parser.Lit = Parser.Scanner.Lit;
	EndIf; 
	Parser.Val = Parser.Lit;
	Return Parser.Tok;
EndFunction // Next() 

&AtClient
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

&AtClient
Function OpenScope(Parser)
	Var Scope;
	Scope = Scope(Parser.Scope);
	Parser.Scope = Scope;
	Return Scope;
EndFunction // OpenScope() 

&AtClient
Function CloseScope(Parser)
	Var Scope;
	Scope = Parser.Scope.Outer;
	Parser.Scope = Scope;
	Return Scope;
EndFunction // CloseScope()

&AtClient
Function ParseString(Parser)
	Var Scanner, Tok, List;
	Scanner = Parser.Scanner;
	List = New Array;
	List.Add(Scanner.Lit);
	Scan(Scanner);
	Tok = Skip(Parser, Tokens.Comment);
	While Tok = Tokens.StringMid Do
		List.Add(Scanner.Lit);
		Scan(Scanner);
		Tok = Skip(Parser, Tokens.Comment);
	EndDo; 
	Expect(Scanner, Tokens.StringEnd);
	List.Add(Scanner.Lit);	
	Return StrConcat(List);
EndFunction // ParseString() 

&AtClient
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

&AtClient
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
			Operand = BasicLit(Tok, StrConcat(StrList, Chars.LF));
		Else
			Operand = BasicLit(Tok, Parser.Val); 
			Next(Parser);
		EndIf; 
	ElsIf Tok = Tokens.Ident Then
		Operand = ParseDesignator(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Next(Parser);
		Operand = ParseExpression(Parser);
		Expect(Parser, Tokens.Rparen);
		Next(Parser);
	ElsIf Tok = Tokens.New Then
		Next(Parser);
		Operand = NewExpr(ParseDesignator(Parser));
	ElsIf Tok = Tokens.Ternary Then
		Operand = ParseTernaryExpr(Parser);
	Else
		Raise "Expected operand";
	EndIf; 
	Return Operand;
EndFunction // ParseOperand() 

&AtClient 
Function ParseDesignator(Parser)
	Var Object, Selector, List, Call;
	Object = ParseQualident(Parser);
	List = New Array;
	Call = False;
	Selector = ParseSelector(Parser);
	While Selector <> Undefined Do
		List.Add(Selector);
		Call = (Selector.Kind = "Call");
		Selector = ParseSelector(Parser);
	EndDo; 
	Return Designator(Object, List, Call);
EndFunction // ParseDesignator() 

&AtClient
Function ParseDesignatorList(Parser)
	Var List;
	List = New Array;
	List.Add(ParseDesignator(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		List.Add(ParseDesignator(Parser));
	EndDo;  
	Return List;
EndFunction // ParseDesignatorList() 

&AtClient
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
	If Object = Undefined Then
		Message(StrTemplate("Undeclared identifier `%1`", Parser.Lit));
		Object = Object("Unknown", Parser.Lit);
	EndIf;
	Return Object;
EndFunction // ParseQualident() 

&AtClient 
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
		Next(Parser);
		Value = ParseExprList(Parser);
		Expect(Parser, Tokens.Rbrack);
		Return Selector("Index", Value);
	ElsIf Tok = Tokens.Lparen Then
		Next(Parser);
		If Parser.Tok <> Tokens.Rparen Then
			Value = ParseExprList(Parser);
		EndIf; 
		Expect(Parser, Tokens.Rparen);
		Return Selector("Call", Value);
	EndIf; 
	Return Undefined;	
EndFunction // ParseSelector()

&AtClient 
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

&AtClient 
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

&AtClient 
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

&AtClient 
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

&AtClient 
Function ParseMulExpr(Parser)
	Var Expr, Operator;
	Expr = ParseUnaryExpr(Parser);
	While Parser.Tok = Tokens.Mul Or Parser.Tok = Tokens.Div Do
		Operator = Parser.Tok;
		Next(Parser);
		Expr = BinaryExpr(Expr, Operator, ParseUnaryExpr(Parser));
	EndDo; 
	Return Expr;	
EndFunction // ParseMulExpr()

&AtClient 
Function ParseExprList(Parser)
	Var ExprList;
	ExprList = New Array;
	While Parser.Tok = Tokens.Comma Do
		ExprList.Add(Undefined);
		Next(Parser);
	EndDo;
	ExprList.Add(ParseExpression(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		While Parser.Tok = Tokens.Comma Do
			ExprList.Add(Undefined);
			Next(Parser);
		EndDo; 
		ExprList.Add(ParseExpression(Parser));
	EndDo; 
	Return ExprList;
EndFunction // ParseExprList()

&AtClient
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

&AtClient
Function ParseFuncDecl(Parser)
	Var Scope, Object, Name, Decls;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	ScopeObjects = Parser.Scope.Objects;
	OpenScope(Parser);	
	Name = Parser.Lit; 
	Next(Parser);
	Object = Object(ObjectKinds.Function, Name, ParseFuncType(Parser)); 
	ScopeObjects.Insert(Name, Object);
	Decls = ParseDecls(Parser);
	Parser.IsFunc = True;
	Statements = ParseStatements(Parser);
	Parser.IsFunc = False;
	Expect(Parser, Tokens.EndFunction, Tokens.End);
	CloseScope(Parser);
	Next(Parser);
	Return FuncDecl(Object, Decls, Statements);
EndFunction // ParseFuncDecl() 

&AtClient
Function ParseFuncType(Parser)
	Var VarListDecl;
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	If Parser.Tok <> Tokens.Rparen Then
		VarListDecl = ParseVarListDecl(Parser);
	EndIf; 
	Expect(Parser, Tokens.Rparen);
	Next(Parser);
	If Parser.Tok = Tokens.Export Then
		Next(Parser);
		Message("keyword `Export` ignored");
	EndIf; 
	Return FuncType(VarListDecl);
EndFunction // ParseFuncType()  

&AtClient
Function ParseProcDecl(Parser)
	Var Scope, Object, Name, Decls;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	ScopeObjects = Parser.Scope.Objects;
	OpenScope(Parser);	
	Name = Parser.Lit; 
	Next(Parser);
	Object = Object(ObjectKinds.Function, Name, ParseFuncType(Parser)); 
	ScopeObjects.Insert(Name, Object);
	Decls = ParseDecls(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndProcedure, Tokens.End);
	CloseScope(Parser);
	Next(Parser);
	Return ProcDecl(Object, Decls, Statements);
EndFunction // ParseProcDecl()

&AtClient
Function ParseReturnStmt(Parser)
	Var ExprList;
	Next(Parser);
	If Parser.IsFunc Then
		ExprList = ParseExprList(Parser);
	EndIf; 
	Return ReturnStmt(ExprList);
EndFunction // ParseReturnStmt() 

&AtClient
Function ParseVarListDecl(Parser)
	Var VarList;
	VarList = New Array;	
	VarList.Add(ParseVarDecl(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		VarList.Add(ParseVarDecl(Parser));
	EndDo;
	If Parser.Tok = Tokens.Export Then
		Next(Parser);
		Message("keyword `Export` ignored");
	EndIf;
	Return VarListDecl(VarList);
EndFunction // ParseVarListDecl() 

&AtClient
Function ParseVarDecl(Parser)
	Var Tok, Name, Object, VarDecl;
	If Parser.Tok = Tokens.Val Then
		Next(Parser);
		Message("keyword `Val` ignored");
	EndIf; 
	Expect(Parser, Tokens.Ident);
	Name = Parser.Lit;
	Tok = Next(Parser);
	If Tok = Tokens.Eql Then
		Tok = Next(Parser);
		// TODO: check token (basic lit)
		Object = Object(ObjectKinds.Variable, Name, Tok);
		VarDecl = VarDecl(Object, True, Value(Tok, Parser.Lit));
		Next(Parser);
	Else
		Object = Object(ObjectKinds.Variable, Name, Undefined);
		VarDecl = VarDecl(Object);
	EndIf;
	Parser.Scope.Objects.Insert(Name, Object);
	Return VarDecl;
EndFunction // ParseVarDecl() 

&AtClient
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

&AtClient
Function ParseStmt(Parser)
	Var Tok;
	While Skip(Parser, Tokens.Comment) = Tokens.Semicolon Do
		Skip(Parser, Tokens.Semicolon);
	EndDo;
	Tok = Parser.Tok;
	If Tok = Tokens.Ident Then
		Return ParseAssignOrCall(Parser);
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
		Next(Parser);
		Return RaiseStmt(ParseExpression(Parser));
	EndIf; 
	Return Undefined;
EndFunction // ParseStmt()

&AtClient
Function ParseAssignOrCall(Parser)
	Var Left, Right;
	Left = ParseDesignatorList(Parser);
	If Left.Count() = 1 And Left[0].Call Then
		Return CallStmt(Left);
	EndIf; 
	Expect(Parser, Tokens.Eql);
	Next(Parser);
	Right = ParseExprList(Parser);
	Return AssignStmt(Left, Right);
EndFunction // ParseAssignOrCall() 

&AtClient
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
	Expect(Parser, Tokens.EndIf, Tokens.End);
	Next(Parser);
	Return IfStmt(Condition, ThenPart, ElsIfPart, ElsePart);
EndFunction // ParseIfStmt()

&AtClient
Function ParseTryStmt(Parser)
	Var TryPart, ExceptPart;
	Next(Parser);
	TryPart = ParseStatements(Parser);
	Expect(Parser, Tokens.Except);
	Next(Parser);
	ExceptPart = ParseStatements(Parser);
	Expect(Parser, Tokens.EndTry, Tokens.End);
	Next(Parser);
	Return TryStmt(TryPart, ExceptPart);
EndFunction // ParseTryStmt()

&AtClient
Function ParseCaseStmt(Parser)
	Var Tok, Designator, ElsePart;
	Var WhenPart, WhenCond, WhenThen;
	Next(Parser);
	Designator = ParseDesignator(Parser);
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
	Expect(Parser, Tokens.EndCase, Tokens.End);
	Next(Parser);
	Return CaseStmt(Designator, WhenPart, ElsePart);
EndFunction // ParseCaseStmt()

&AtClient
Function ParseWhileStmt(Parser)
	Var Condition, Statements;
	Next(Parser);
	Condition = ParseExpression(Parser);
	Expect(Parser, Tokens.Do);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndDo, Tokens.End);
	Next(Parser);
	Return WhileStmt(Condition, Statements)
EndFunction // ParseWhileStmt()

&AtClient
Function ParseForStmt(Parser)
	Var Designator, Left, Right, Collection, Statements;
	Next(Parser);
	Skip(Parser, Tokens.Each);
	Expect(Parser, Tokens.Ident);
	Designator = ParseDesignator(Parser);	
	If Designator.Call Then
		Raise "expected variable";
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
	Expect(Parser, Tokens.EndDo, Tokens.End);
	Next(Parser);
	Return ForStmt(Designator, Collection, Statements);
EndFunction // ParseForStmt()

&AtClient
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

&AtClient
Function ParseModule(Parser)
	Next(Parser);
	Parser.Decls = ParseDecls(Parser);
	Expect(Parser, Tokens.Eof);
EndFunction // ParseModule() 

#EndRegion // Parser

#Region Auxiliary

&AtClientAtServerNoContext
Function Value(Tok, Lit)
	Return Lit;
EndFunction // Value()

&AtClient
Procedure Expect(Parser, Tok1, Tok2 = Undefined)
	If Parser.Tok <> Tok1 And Parser.Tok <> Tok2 Then 
		Raise "Expected " + Tok1;
	EndIf; 
EndProcedure // Expect()

&AtClient
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

&AtClient
Function Lookup(Lit)
	Var Tok;
	If Not Keywords.Property(Lit, Tok) Then
		Tok = Tokens.Ident;
	EndIf; 
	Return Tok;
EndFunction // Lookup() 

&AtClientAtServerNoContext
Function IsLetter(Char)
	Return "a" <= Char And Char <= "z" Or "A" <= Char And Char <= "Z" Or Char = "_";
EndFunction // IsLetter()

&AtClientAtServerNoContext
Function IsDigit(Char)
	Return "0" <= Char And Char <= "9";	
EndFunction // IsLetter()

#EndRegion // Auxiliary
