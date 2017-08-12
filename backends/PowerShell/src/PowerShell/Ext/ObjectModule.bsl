
Var Tokens;        // enum
Var SelectorKinds; // enum
Var Operators;     // structure as map[one of Tokens](string)

Procedure Init(OneShellProcessor) Export
	
	Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"-eq", "-ne", "-lt", "-gt", "-le", "-ge", "+", "-", "*", "/", "%", "-or", "-and", "!"
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
		VisitVarList(Backend, Decl.VarList);
	ElsIf NodeType = "FuncDecl" Or NodeType = "ProcDecl" Then
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent + 1;
		Result.Add("function ");
		Result.Add(Decl.Object.Name);
		Result.Add("(");
		VisitParamList(Backend, Decl.Object.Type.ParamList);
		Result.Add(") {");
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			VisitDecl(Backend, Stmt);
		EndDo;
		For Each Stmt In Decl.Statements Do
			VisitStmt(Backend, Stmt);
		EndDo;
		Result.Add("}");
		Result.Add(Chars.LF);
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent - 1;
	EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarList(Backend, VarListDecl)
	Var Result;
	If VarListDecl <> Undefined Then
		Result = Backend.Result;
		For Each VarDecl In VarListDecl Do
			Indent(Backend);
			Result.Add(StrTemplate("New-Variable -Name ""%1""", VarDecl.Object.Name));
			If VarDecl.Property("Value") Then
				Result.Add(" -Value " + VisitExpr(VarDecl.Value));
			EndIf;
			Result.Add(Chars.LF);
		EndDo;
	EndIf;
EndProcedure // VisitVarList()

Procedure VisitParamList(Backend, ParamList)
	Var Result, Buffer;
	If ParamList <> Undefined Then
		Result = Backend.Result;
		Buffer = New Array;
		For Each VarDecl In ParamList Do
			Buffer.Add(StrTemplate("$%1%2", VarDecl.Object.Name, ?(VarDecl.Property("Value"), " = " + VisitExpr(VarDecl.Value), "")));
		EndDo;
		If Buffer.Count() > 0 Then
			Result.Add(StrConcat(Buffer, ", "));
		EndIf;
	EndIf;
EndProcedure // VisitParamList()

Procedure VisitStmt(Backend, Stmt)
	Var Result, NodeType;
	Result = Backend.Result;
	NodeType = Stmt.NodeType;
	Indent(Backend);
	If NodeType = "AssignStmt" Then
		Result.Add(VisitExprList(Stmt.Left, ", "));
		Result.Add(" = ");
		Result.Add(VisitExprList(Stmt.Right, ", "));
		Result.Add(Chars.LF);
	ElsIf NodeType = "AddAssignStmt" Then
		Result.Add(VisitExprList(Stmt.Left, ", "));
		Result.Add(" += ");
		Result.Add(VisitExprList(Stmt.Right, ", "));
		Result.Add(Chars.LF);
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("return ");
		If Stmt.Property("ExprList") Then
			Result.Add(VisitExprList(Stmt.ExprList, ", "));
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
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(Chars.LF);
	ElsIf NodeType = "ExecuteStmt" Then
		Result.Add("# Execute(");
		Result.Add(VisitExpr(Stmt.Expr));
		Result.Add(")");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CallStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr, False));
		Result.Add(Chars.LF);
	ElsIf NodeType = "IfStmt" Then
		Result.Add("if (");
		VisitIfStmt(Backend, Stmt);
		If Stmt.Property("ElsePart") Then
			Indent(Backend);
			Result.Add("else {");
			Result.Add(Chars.LF);
			VisitStatements(Backend, Stmt.ElsePart);
			Result.Add("}");
			Result.Add(Chars.LF);
		EndIf;
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("while (");
		Result.Add(VisitExpr(Stmt.Condition));
		Result.Add(") {");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.Statements);
		Result.Add("}");
		Result.Add(Chars.LF);
	ElsIf NodeType = "ForStmt" Then
		If Stmt.Collection.NodeType = "RangeExpr" Then
			Result.Add("for (");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			Result.Add(VisitExpr(Stmt.Collection.Left));
			Result.Add("; ");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" -le ");
			Result.Add(VisitExpr(Stmt.Collection.Right));
			Result.Add(StrTemplate("; %1++ )", VisitDesignatorExpr(Stmt.DesignatorExpr)));
		Else
			Result.Add("foreach (");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" In ");
			Result.Add(VisitExpr(Stmt.Collection));
			Result.Add(")");
		EndIf;
		Result.Add(" {");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.Statements);
		Result.Add("}");
		Result.Add(Chars.LF);
	ElsIf NodeType = "CaseStmt" Then
		Result.Add("switch ");
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
		Result.Add(" {");
		Result.Add(Chars.LF);
		Backend.Indent = Backend.Indent + 1;
		For Each IfStmt In Stmt.WhenPart Do
			Indent(Backend);
			VisitWhenPart(Backend, IfStmt);
		EndDo;
		If Stmt.Property("ElsePart") Then
			Indent(Backend);
			Result.Add("default {");
			Result.Add(Chars.LF);
			VisitStatements(Backend, Stmt.ElsePart);
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
		VisitStatements(Backend, Stmt.TryPart);
		Result.Add("}");
		Result.Add(Chars.LF);
		Indent(Backend);
		Result.Add("catch {");
		Result.Add(Chars.LF);
		VisitStatements(Backend, Stmt.ExceptPart);
		Result.Add("}");
		Result.Add(Chars.LF);
	EndIf;
EndProcedure // VisitStmt()

Procedure VisitIfStmt(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(VisitExpr(IfStmt.Condition));
	Result.Add(") {");
	Result.Add(Chars.LF);
	VisitStatements(Backend, IfStmt.ThenPart);
	Result.Add("}");
	Result.Add(Chars.LF);
	If IfStmt.Property("ElsIfPart") Then
		For Each Item In IfStmt.ElsIfPart Do
			Indent(Backend);
			Result.Add("elseif (");
			VisitIfStmt(Backend, Item);
		EndDo;
	EndIf;
EndProcedure // VisitIfStmt()

Procedure VisitWhenPart(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(VisitExpr(IfStmt.Condition));
	Result.Add(" {");
	Result.Add(Chars.LF);
	VisitStatements(Backend, IfStmt.ThenPart);
	Result.Add("}");
	Result.Add(Chars.LF);
EndProcedure // VisitWhenPart()

Function VisitExprList(ExprList, Separator)
	Var Buffer;
	If ExprList <> Undefined Then
		Buffer = New Array;
		For Each Expr In ExprList Do
			Buffer.Add(VisitExpr(Expr));
		EndDo;
		Return StrConcat(Buffer, Separator);
	EndIf;
EndFunction // VisitExprList()

Function VisitExpr(Expr)
	Var NodeType, BasicLitKind;
	If Expr = Undefined Then
		Return "$null";
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
			Return Format(Expr.Value, "BF=$False; BT=$True");
		ElsIf BasicLitKind = Tokens.Undefined Then
			Return "$null";
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
		Return StrTemplate("%1..%2", VisitExpr(Expr.Left), VisitExpr(Expr.Right));
	ElsIf NodeType = "NewExpr" Then
		If TypeOf(Expr.Constructor) = Type("Structure") Then
			Return StrTemplate("<# New %1 #>", VisitExpr(Expr.Constructor));
		Else
			Return StrTemplate("<# New(%1) #>", VisitExprList(Expr.Constructor, ", "));
		EndIf;
	ElsIf NodeType = "TernaryExpr" Then
		Return StrTemplate("if (%1) { %2 } else { %3 }", VisitExpr(Expr.Condition), VisitExpr(Expr.ThenPart), VisitExpr(Expr.ElsePart));
	ElsIf NodeType = "ParenExpr" Then
		Return StrTemplate("(%1)", VisitExpr(Expr.Expr));
	ElsIf NodeType = "NotExpr" Then
		Return StrTemplate("-not (%1)", VisitExpr(Expr.Expr));
	EndIf;
EndFunction // VisitExpr()

Function VisitDesignatorExpr(DesignatorExpr, IsOperand = True)
	Var Buffer, Selector;
	Buffer = New Array;
	If Not DesignatorExpr.Call Then
		Buffer.Add("$");
	EndIf;
	Buffer.Add(DesignatorExpr.Object.Name);
	If DesignatorExpr.Property("Selectors") Then
		For Each Selector In DesignatorExpr.Selectors Do
			If Selector.Kind = SelectorKinds.Ident Then
				Buffer.Add(".");
				Buffer.Add(Selector.Value);
			ElsIf Selector.Kind = SelectorKinds.Index Then
				Buffer.Add("[");
				Buffer.Add(VisitExprList(Selector.Value, ", "));
				Buffer.Add("]");
			ElsIf Selector.Kind = SelectorKinds.Call Then
				If Selector.Value.Count() > 0 Then
					Buffer.Add(" ");
					Buffer.Add(VisitExprList(Selector.Value, " "));
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
EndFunction // VisitDesignatorExpr()
