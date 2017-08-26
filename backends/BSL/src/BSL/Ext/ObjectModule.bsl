
Var Result; // array (string)
Var Indent; // number

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

	Result = New Array;
	Indent = -1;

EndProcedure // Init()

Procedure Indent(Result)
	For Index = 1 To Indent Do
		Result.Add(Chars.Tab);
	EndDo;
EndProcedure // Indent()

Function VisitModule(Module) Export
	VisitDecls(Module.Decls);
	VisitStatements(Module.Statements);
	Return StrConcat(Result);
EndFunction // VisitModule()

Procedure VisitDecls(Decls)
	Indent = Indent + 1;
	For Each Decl In Decls Do
		VisitDecl(Decl);
	EndDo;
	Indent = Indent - 1;
EndProcedure // VisitDecls()

Procedure VisitStatements(Statements)
	Indent = Indent + 1;
	For Each Stmt In Statements Do
		VisitStmt(Stmt);
	EndDo;
	Indent = Indent - 1;
	Indent(Result);
EndProcedure // VisitStatements()

Procedure VisitDecl(Decl)
	Var NodeType;
	NodeType = Decl.NodeType;
	If NodeType = "VarListDecl" Then
		Indent(Result);
		Result.Add("Var ");
		VisitVarList(Decl.VarList);
		Result.Add(";" "");
	ElsIf NodeType = "FuncDecl" Or NodeType = "ProcDecl" Then
		Result.Add(Chars.LF);
		Indent = Indent + 1;
		If NodeType = "FuncDecl" Then
			Result.Add("Function ");
		Else
			Result.Add("Procedure ");
		EndIf;
		Result.Add(Decl.Object.Name);
		Result.Add("(");
		VisitParamList(Decl.Object.ParamList);
		Result.Add(")");
		If Decl.Object.Export Then
			Result.Add(" Export");
		EndIf;
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			VisitDecl(Stmt);
		EndDo;
		For Each Stmt In Decl.Statements Do
			VisitStmt(Stmt);
		EndDo;
		If NodeType = "FuncDecl" Then
			Result.Add(StrTemplate("EndFunction // %1()", Decl.Object.Name));
		Else
			Result.Add(StrTemplate("EndProcedure // %1()", Decl.Object.Name));
		EndIf;
		Result.Add(Chars.LF);
		Indent = Indent - 1;
	EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarList(VarList)
	Var Buffer, Object, Value;
	Buffer = New Array;
	For Each Object In VarList Do
		Buffer.Add(Object.Name +
			?(Object.Property("Value", Value), " = " + VisitExpr(Value), "") +
			?(Object.Export, " Export", "")
		);
	EndDo;
	If Buffer.Count() > 0 Then
		Result.Add(StrConcat(Buffer, ", "));
	EndIf;
EndProcedure // VisitVarList()

Procedure VisitParamList(ParamList)
	Var Buffer, Object, Value;
	Buffer = New Array;
	For Each Object In ParamList Do
		Buffer.Add(
			?(Object.ByVal, "Val ", "") +
			Object.Name +
			?(Object.Property("Value", Value), " = " + VisitExpr(Value), "")
		);
	EndDo;
	If Buffer.Count() > 0 Then
		Result.Add(StrConcat(Buffer, ", "));
	EndIf;
EndProcedure // VisitParamList()

Procedure VisitStmt(Stmt)
	Var NodeType;
	NodeType = Stmt.NodeType;
	Indent(Result);
	If NodeType = "AssignStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.Left));
		Result.Add(" = ");
		Result.Add(VisitExpr(Stmt.Right));
		Result.Add(";" "");
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("Return ");
		If Stmt.Property("Expr") Then
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(";" "");
	ElsIf NodeType = "BreakStmt" Then
		Result.Add("Break;" "");
	ElsIf NodeType = "ContinueStmt" Then
		Result.Add("Continue;" "");
	ElsIf NodeType = "RaiseStmt" Then
		Result.Add("Raise ");
		If Stmt.Property("Expr") Then
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(";" "");
	ElsIf NodeType = "ExecuteStmt" Then
		Result.Add("Execute(");
		Result.Add(VisitExpr(Stmt.Expr));
		Result.Add(");" "");
	ElsIf NodeType = "CallStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
		Result.Add(";" "");
	ElsIf NodeType = "IfStmt" Then
		Result.Add("If ");
		VisitIfStmt(Stmt);
		If Stmt.Property("ElsePart") Then
			Result.Add("Else" "");
			VisitStatements(Stmt.ElsePart);
		EndIf;
		Result.Add("EndIf;" "");
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("While ");
		Result.Add(VisitExpr(Stmt.Condition));
		Result.Add(" Do" "");
		VisitStatements(Stmt.Statements);
		Result.Add("EndDo;" "");
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
		Result.Add(" Do" "");
		VisitStatements(Stmt.Statements);
		Result.Add("EndDo;" "");
	ElsIf NodeType = "TryStmt" Then
		Result.Add("Try" "");
		VisitStatements(Stmt.TryPart);
		Result.Add("Except" "");
		VisitStatements(Stmt.ExceptPart);
		Result.Add("EndTry;" "");
	EndIf;
EndProcedure // VisitStmt()

Procedure VisitIfStmt(IfStmt)
	Result.Add(VisitExpr(IfStmt.Condition));
	Result.Add(" Then" "");
	VisitStatements(IfStmt.ThenPart);
	If IfStmt.Property("ElsIfPart") Then
		For Each Item In IfStmt.ElsIfPart Do
			Result.Add("ElsIf ");
			VisitIfStmt(Item);
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
	If Expr = Undefined Then
		Return "";
	EndIf;
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
