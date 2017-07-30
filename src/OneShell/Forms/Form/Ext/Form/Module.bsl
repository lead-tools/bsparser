
#Region Constants

&AtClient
Var Keywords;

&AtClient
Var Tokens;

&AtClient
Var ObjectKinds;

&AtClient
Var UnaryOperations;

&AtClient
Var BasicLiterals;

&AtClient
Var RelationalOperators; 

#EndRegion // Constants

#Region EventHandlers

&AtClient
Procedure Translate(Command)
	
	InitEnums();
	
	Result.Clear();
	
	Scanner = Scanner(Source.GetText());
	While Scan(Scanner) <> Tokens.Eof Do
		If Verbose Then
			Result.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, Scanner.Tok, Scanner.Lit));
		EndIf; 
	EndDo; 
	
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
		|Var, Return, Continue, Break,
		|And, Or, Not,
		|Try, Except, Raise, EndTry,
		|New, Execute, Export,
		|True, False, Undefined"
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
		
		|Illegal, Eof, Comment"
		
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
	);
	
	Return ObjectKinds;
	
EndFunction // ObjectKinds() 

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
	ElsIf Char = "&" Or Char = "#" Then
		Lit = ScanComment(Scanner);
		Tok = Tokens.Comment;
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
		"ThisType," // string (type of this structure)
		"Kind,"     // string (one of ObjectKinds)
		"Name,"     // string
		"Type,"     // structure
	,
	"Object", Kind, Name, Type);
	
	Return Object;
	
EndFunction // Object()

#Region Declarations

&AtClientAtServerNoContext
Function VarDecl(Object)
	Var VarDecl;
	
	VarDecl = New Structure(
		"ThisType," // string (type of this structure)
		"Object,"   // structure (Object)
	,
	"VarDecl", Object);
		
	Return VarDecl;
	
EndFunction // VarDecl() 

&AtClientAtServerNoContext
Function ProcDecl(Object, Statements)
	Var ProcDecl;
	
	ProcDecl = New Structure(
		"ThisType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Statements," // array (one of statements)
	,
	"ProcDecl", Object, Statements);
		
	Return ProcDecl;
	
EndFunction // ProcDecl()

&AtClientAtServerNoContext
Function FuncDecl(Object, Statements)
	Var FuncDecl;
	
	FuncDecl = New Structure(
		"ThisType,"   // string (type of this structure)
		"Object,"     // structure (Object)
		"Statements," // array (one of statements)
	,
	"FuncDecl", Object, Statements);
		
	Return FuncDecl;
	
EndFunction // FuncDecl() 

#EndRegion // Declarations 

#Region Expressions

&AtClientAtServerNoContext
Function BasicLit(Kind, Value)
	Var BasicLit;
	
	BasicLit = New Structure(
		"ThisType," // string (type of this structure)
		"Kind,"     // string (one of Tokens)
		"Value,"    // string
	,
	"BasicLit", Kind, Value);
		
	Return BasicLit;
	
EndFunction // BasicLit() 

&AtClientAtServerNoContext
Function Selector(Attribute, Index, Arguments, Next)
	Var Selector;
	
	Selector = New Structure(
		"ThisType,"  // string (type of this structure)
		"Attribute," // string
		"Index,"     // array (one of expressions)
		"Arguments," // array (one of expressions)
		"Next,"      // structure (Selector)
	,
	"Selector", Attribute, Index, Arguments, Next);
	
	Return Selector;
	
EndFunction // Selector()

&AtClientAtServerNoContext
Function Designator(Object, Selector)
	Var Designator;
	
	Designator = New Structure(
		"ThisType,"  // string (type of this structure)
		"Object,"    // structure (Object)
		"Selector,"  // structure (Selector)
	,
	"Designator", Object, Selector);
	
	Return Designator;
	
EndFunction // Designator() 

&AtClientAtServerNoContext
Function UnaryExpr(Operator, Operand)
	Var UnaryExpr;
	
	UnaryExpr = New Structure(
		"ThisType," // string (type of this structure)
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
		"ThisType," // string (type of this structure)
		"Left,"     // one of expressions
		"Operator," // string (one of Tokens)
		"Right,"    // one of expressions
	,
	"BinaryExpr", Left, Operator, Right);
	
	Return BinaryExpr;
	
EndFunction // BinaryExpr()

#EndRegion // Expressions

#Region Statements

&AtClientAtServerNoContext
Function AssignStmt(Left, Right)
	Var AssignStmt;
	
	AssignStmt = New Structure(
		"ThisType," // string (type of this structure)
		"Left,"     // array (Designator)
		"Right,"    // array (one of expressions)
	,
	"AssignStmt", Left, Right);
	
	Return AssignStmt;
	
EndFunction // AssignStmt()

&AtClientAtServerNoContext
Function CallStmt(Designator)
	Var CallStmt;
	
	CallStmt = New Structure(
		"ThisType,"   // string (type of this structure)
		"Designator," // structure (Designator)
	,
	"CallStmt", Designator);
	
	Return CallStmt;
	
EndFunction // CallStmt()

&AtClientAtServerNoContext
Function IfStmt(Condition, ThenPart, ElsePart)
	Var IfStmt;
	
	IfStmt = New Structure(
	    "ThisType,"  // string (type of this structure)
		"Condition," // structure (one of expressions)
		"ThenPart,"  // array (one of statements)
		"ElsePart,"  // array (one of statements)
	,
	"IfStmt", Condition, ThenPart, ElsePart);
	
	Return IfStmt;
	
EndFunction // IfStmt()

&AtClientAtServerNoContext
Function WhileStmt(Condition, Statements)
	Var WhileStmt;
	
	WhileStmt = New Structure(
		"ThisType,"   // string (type of this structure)
		"Condition,"  // structure (one of expressions)
		"Statements," // array (one of statements)
	,
	"WhileStmt", Condition, Statements);
	
	Return WhileStmt;
	
EndFunction // WhileStmt()

#EndRegion // Statements

#Region Types

&AtClientAtServerNoContext
Function FuncType(ParameterList)
	Var FuncType;
	
	FuncType = New Structure(
		"ThisType,"      // string (type of this structure)
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
		"Tok,"     // string(one of Tokens)
		"Lit,"     // string
		"Val,"     // number, string, date, true, false, undefined 
		"Scope,"   // structure (Scope)
		"Imports," // structure
	);
	
	Parser.Scanner = Scanner(Source);
	Parser.Scope = Scope(Undefined);
	
	Return Parser;
	
EndFunction // Parser() 

&AtClient
Function Next(Parser)
	Var Tok;
	Tok = Scan(Parser.Scanner);
	While Tok = Tokens.Comment Do
		Tok = Scan(Parser.Scanner);
	EndDo;  
	Parser.Tok = Tok;
	Parser.Lit = Parser.Scanner.Lit;
	Parser.Val = Parser.Lit;
	Return Tok;
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
EndFunction // OpenScope(Parser) 

&AtClient
Function CloseScope(Parser)
	Var Scope;
	Scope = Parser.Scope.Outer;
	Parser.Scope = Scope;
	Return Scope;
EndFunction // CloseScope(Parser)

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
	Var Tok, Operand;
	Tok = Parser.Tok;
	If BasicLiterals.Find(Tok) Then
		Operand = BasicLit(Tok, Parser.Val); 
	    Next(Parser);
	ElsIf Tok = Tokens.Ident Then
		Operand = ParseDesignator(Parser);
	ElsIf Tok = Tokens.Lparen Then
		Operand = ParseExpression(Parser);
		Expect(Parser, Tokens.Rparen);
	Else
		Raise "Expected operand";
	EndIf; 
	Return Operand;
EndFunction // ParseOperand() 

&AtClient 
Function ParseDesignator(Parser)
	Var Object, Selector;
	Object = ParseQualident(Parser);
	Selector = ParseSelector(Parser);
	Return Designator(Object, Selector);
EndFunction // ParseDesignator() 

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
		Raise "Undeclared identifier";	
	EndIf;
	Return Object;
EndFunction // ParseQualident() 

&AtClient 
Function ParseSelector(Parser)
	Var Tok, Attribute, Index;
	Tok = Next(Parser);
	If Tok = Tokens.Period Then
		Next(Parser);
		Expect(Parser, Tokens.Ident);
		Attribute = Parser.Lit;
		Return Selector(Attribute, Undefined, Undefined, ParseSelector(Parser));
	ElsIf Tok = Tokens.Lbrack Then
		Index = ParseExprList(Parser);
		Expect(Parser, Tokens.Rbrack);
		Return Selector(Undefined, Index, Undefined, ParseSelector(Parser));
	ElsIf Tok = Tokens.Lparen Then
		Index = ParseExprList(Parser);
		Expect(Parser, Tokens.Rparen);
		Return Selector(Undefined, Index, Undefined, ParseSelector(Parser));
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
	ExprList.Add(ParseExpression(Parser));
	While Parser.Tok = Tokens.Comma Do
		Next(Parser);
		ExprList.Add(ParseExpression(Parser));
	EndDo; 
	Return ExprList;
EndFunction // ParseExprList()

&AtClient
Function ParseFuncDecl(Parser)
	Var Scope, Object, Name;
	Next(Parser);
	Expect(Parser, Tokens.Ident);
	ScopeObjects = OpenScope(Parser).Objects;
	Name = Parser.Lit; 
	Object = Object(ObjectKinds.Function, Name, ParseFuncType(Parser)); 
	ScopeObjects.Insert(Name, Object);
	Next(Parser);
	Statements = ParseStatements(Parser);
	Expect(Parser, Tokens.EndFunction);
	CloseScope(Parser);
	Return FuncDecl(Object, Statements);
EndFunction // ParseFuncDecl() 

&AtClient
Function ParseFuncType(Parser)
	Var Parameters;
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	ParamList = ParseParamList(Parser);
	Expect(Parser, Tokens.Lparen);
	Next(Parser);
	Return FuncType(ParamList);
EndFunction // ParseFuncType()  

&AtClient
Function ParseParamList(Parser)
	
EndFunction // ParseParamList() 

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
EndFunction // ParseStatements(Parser) 

&AtClient
Function ParseStmt(Parser)
	Var Tok;
	Tok = Parser.Tok;
	If Tok = Tokens.Ident Then
		
	ElsIf Tok = Tokens.If Then
		Return ParseIfStmt(Parser);	
	ElsIf Tok = Tokens.While Then
		Return ParseWhileStmt(Parser);
	EndIf; 
	Return Undefined;
EndFunction // ParseStmt(Parser)

&AtClient
Function ParseIfStmt(Parser)
	Var Condition, ThenPart, ElsePart;
	Next(Parser);
	Condition = ParseExpression(Parser);
	Expect(Parser, Tokens.Then);
	Next(Parser);
	ThenPart = ParseStatements(Parser);
	If Parser.Tok = Tokens.Else Then
		Next(Parser);
		ElsePart = ParseStatements(Parser);
	EndIf;
	Expect(Parser, Tokens.EndIf);
	Next(Parser);
	Return IfStmt(Condition, ThenPart, ElsePart);
EndFunction // ParseIfStmt(Parser)

&AtClient
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
EndFunction // ParseWhileStmt(Parser)

&AtClient
Function ParseDecls(Parser)
	
EndFunction // ParseDecls() 

#EndRegion // Parser

#Region Auxiliary  

&AtClient
Procedure Expect(Parser, Tok)
	If Parser.Tok <> Tok Then
		Raise "Expected " + Tok;
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
