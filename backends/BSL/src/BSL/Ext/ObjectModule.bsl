
Var Tokens;        // enum
Var SelectorKinds; // enum
Var Operators;     // structure as map[one of Tokens](string)

Procedure Init(OneShellProcessor) Export
	
	Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"=", "<>", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "Or", "And", "Not"
	);
	
	Tokens = OneShellProcessor.Tokens();
	SelectorKinds = OneShellProcessor.SelectorKinds();
	
EndProcedure // Init() 

Function Backend()
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

Function VisitModule(Module) Export
	Backend = Backend();
	VisitDecls(Backend, Module.Decls);
	VisitStatements(Backend, Module.Statements);
	Return StrConcat(Backend.Result);
EndFunction // VisitModule()

Procedure VisitDecls(Backend, Decls)
	Backend.Indent = Backend.Indent + 1;
	For Each Decl In Decls Do
		VisitDecl(Backend, Decl);
	EndDo;
	Backend.Indent = Backend.Indent - 1;
EndProcedure // VisitDecls()

Procedure VisitStatements(Backend, Statements)
	Backend.Indent = Backend.Indent + 1;
	For Each Stmt In Statements Do
		VisitStmt(Backend, Stmt);
	EndDo;
	Backend.Indent = Backend.Indent - 1;
	Indent(Backend);
EndProcedure // VisitStatements()

Procedure VisitDecl(Backend, Decl)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Decl.NodeType;
	If NodeType = "VarListDecl" Then
		Indent(Backend);
		Result.Add("Var ");
		VisitVarListDecl(Backend, Decl.VarList);
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
		VisitVarListDecl(Backend, Decl.Object.Type.ParamList);
		Result.Add(")");
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			VisitDecl(Backend, Stmt);
		EndDo;
		For Each Stmt In Decl.Statements Do
			VisitStmt(Backend, Stmt);
		EndDo;
		If NodeType = "FuncDecl" Then
			Result.Add(StrTemplate("EndFunction // %1()", Decl.Object.Name));
		Else
			Result.Add(StrTemplate("EndProcedure // %1()", Decl.Object.Name));
		EndIf;
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent - 1;
	EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarListDecl(Backend, VarListDecl)
	Var Result, Buffer;
	If VarListDecl <> Undefined Then
		Result = Backend.Result;
		Buffer = New Array;
		For Each VarDecl In VarListDecl Do
			Buffer.Add(VarDecl.Object.Name + ?(VarDecl.Property("Value"), " = " + VisitExpr(VarDecl.Value), ""));
		EndDo;
		If Buffer.Count() > 0 Then
			Result.Add(StrConcat(Buffer, ", "));
		EndIf;
	EndIf;
EndProcedure // VisitVarListDecl()

Procedure VisitStmt(Backend, Stmt)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Stmt.NodeType;
	Indent(Backend);
	If NodeType = "AssignStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" = ");
		Result.Add(VisitExprList(Stmt.Right));
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "AddAssignStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" = ");
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" + ");
		Result.Add(VisitExprList(Stmt.Right));
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("Return ");
		If Stmt.Property("ExprList") Then
			Result.Add(VisitExprList(Stmt.ExprList));
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
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ExecuteStmt" Then
		Result.Add("Execute(");
		Result.Add(VisitExpr(Stmt.Expr));
		Result.Add(");");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CallStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "IfStmt" Then
		Result.Add("If ");
		VisitIfStmt(Backend, Stmt);
		If Stmt.Property("ElsePart") Then
			Result.Add("Else");
			Result.Add(Chars.LF);
			VisitStatements(Backend, Stmt.ElsePart);
		EndIf;
		Result.Add("EndIf");
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("While ");
		Result.Add(VisitExpr(Stmt.Condition));
		Result.Add(" Do");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.Statements);
		Result.Add("EndDo");
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ForStmt" Then
		Result.Add("For ");
		If Stmt.Collection.NodeType = "RangeExpr" Then
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			Result.Add(VisitExpr(Stmt.Collection));
		Else
			Result.Add("Each ");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" In ");
			Result.Add(VisitExpr(Stmt.Collection));
		EndIf;
		Result.Add(" Do");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.Statements);
		Result.Add("EndDo");
		Result.Add(";");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CaseStmt" Then
		If Stmt.WhenPart.Count() > 0 Then
			Result.Add("If ");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			IfStmt = Stmt.WhenPart[0];
			VisitIfStmt(Backend, IfStmt);
			For Index = 1 To Stmt.WhenPart.Count() - 1 Do
				IfStmt = Stmt.WhenPart[Index];
				Result.Add("ElsIf ");
				Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
				Result.Add(" = ");
				VisitIfStmt(Backend, IfStmt);
			EndDo;
			If Stmt.Property("ElsePart") Then
				Result.Add("Else");
				Result.Add(Chars.LF);
				VisitStatements(Backend, Stmt.ElsePart);
			EndIf;
			Result.Add("EndIf");
			Result.Add(";");
			Result.Add(Chars.LF);
		Else
			Result.Add(Chars.LF);
			Backend.Indent = Backend.Indent - 1;
			If Stmt.Property("ElsePart") Then
				VisitStatements(Backend, Stmt.ElsePart);
			EndIf;
			Backend.Indent = Backend.Indent + 1;
			Result.Add(Chars.LF);
		EndIf;
	ElsIf NodeType = "TryStmt" Then
		Result.Add("Try");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.TryPart);
		Result.Add("Except");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.ExceptPart);
		Result.Add("EndTry");
		Result.Add(";");
		Result.Add(Chars.LF);
	EndIf;
EndProcedure // VisitStmt()

Procedure VisitIfStmt(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(VisitExpr(IfStmt.Condition));
	Result.Add(" Then");
	Result.Add(Chars.LF);
	VisitStatements(Backend, IfStmt.ThenPart);
	If IfStmt.Property("ElsIfPart") Then
		For Each Item In IfStmt.ElsIfPart Do
			Result.Add("ElsIf ");
			VisitIfStmt(Backend, Item);
		EndDo;
	EndIf;
EndProcedure // VisitIfStmt()

Function VisitExprList(ExprList)
	Var Buffer;
	If ExprList <> Undefined Then
		Buffer = New Array;
		For Each Expr In ExprList Do
			If Expr = Undefined Then
				Buffer.Add("");
			Else
				Buffer.Add(VisitExpr(Expr));
			EndIf;
		EndDo;
		Return StrConcat(Buffer, ", ");
	EndIf;
EndFunction // VisitExprList()

Function VisitExpr(Expr)
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
		Return VisitDesignatorExpr(Expr);
	ElsIf NodeType = "UnaryExpr" Then
		Return StrTemplate("%1 %2", Operators[Expr.Operator], VisitExpr(Expr.Operand));
	ElsIf NodeType = "BinaryExpr" Then
		Return StrTemplate("%1 %2 %3", VisitExpr(Expr.Left), Operators[Expr.Operator], VisitExpr(Expr.Right));
	ElsIf NodeType = "RangeExpr" Then
		Return StrTemplate("%1 To %2", VisitExpr(Expr.Left), VisitExpr(Expr.Right));
	ElsIf NodeType = "NewExpr" Then
		If TypeOf(Expr.Constructor) = Type("Structure") Then
			Return StrTemplate("New %1", VisitExpr(Expr.Constructor));
		Else
			Return StrTemplate("New(%1)", VisitExprList(Expr.Constructor));
		EndIf;
	ElsIf NodeType = "TernaryExpr" Then
		Return StrTemplate("?(%1, %2, %3)", VisitExpr(Expr.Condition), VisitExpr(Expr.ThenPart), VisitExpr(Expr.ElsePart));
	ElsIf NodeType = "ParenExpr" Then
		Return StrTemplate("(%1)", VisitExpr(Expr.Expr));
	ElsIf NodeType = "NotExpr" Then
		Return StrTemplate("Not %1", VisitExpr(Expr.Expr));	
	EndIf;
EndFunction // VisitExpr()

Function VisitDesignatorExpr(DesignatorExpr)
	Var Buffer;
	Buffer = New Array;
	Buffer.Add(DesignatorExpr.Object.Name);
	If DesignatorExpr.Property("Selectors") Then
		For Each Selector In DesignatorExpr.Selectors Do
			If Selector.Kind = SelectorKinds.Ident Then
				Buffer.Add(".");
				Buffer.Add(Selector.Value);
			ElsIf Selector.Kind = SelectorKinds.Index Then
				Buffer.Add("[");
				Buffer.Add(VisitExprList(Selector.Value));
				Buffer.Add("]");
			ElsIf Selector.Kind = SelectorKinds.Call Then
				Buffer.Add("(");
				Buffer.Add(VisitExprList(Selector.Value));
				Buffer.Add(")");
			Else
				Raise "Unknown selector kind";
			EndIf;
		EndDo;
	EndIf;
	Return StrConcat(Buffer);
EndFunction // VisitDesignatorExpr()
