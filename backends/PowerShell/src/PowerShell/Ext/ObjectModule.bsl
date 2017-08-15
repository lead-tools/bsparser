
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
	Var Result;
	Backend = Backend();
	Result = Backend.Result;
	Result.Add("$1C = New-Module -AsCustomObject {" "");
	Result.Add("	$Missing = [Type]::Missing" "");
	Result.Add("}" "" "");
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
		Indent(Backend);
		Result.Add("function ");
		Result.Add(Decl.Object.Name);
		VisitParamList(Backend, Decl.Object.ParamList);
		For Each Stmt In Decl.Decls Do
			VisitDecl(Backend, Stmt);
		EndDo;
		For Each Stmt In Decl.Statements Do
			VisitStmt(Backend, Stmt);
		EndDo;
		Backend.Indent = Backend.Indent - 1;
		Indent(Backend);
		Result.Add("}" "");
	EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarList(Backend, VarListDecl)
	Var Result, Object, Value;
	Result = Backend.Result;
	For Each VarDecl In VarListDecl Do
		Object = VarDecl.Object;
		Indent(Backend);
		Result.Add(StrTemplate("New-Variable -Name ""%1""", Object.Name));
		If Object.Property("Value", Value) Then
			Result.Add(" -Value " + VisitExpr(Value));
		EndIf;
		Result.Add(Chars.LF);
	EndDo;
EndProcedure // VisitVarList()

Procedure VisitParamList(Backend, ParamList)
	Var Result, Buffer, Defaults, Object, Value;
		Result = Backend.Result;
		Result.Add("(");
		Defaults = New Array;
		Defaults.Add("switch ($1C.Missing) { ");
		Buffer = New Array;
		For Each ParamDecl In ParamList Do
			Object = ParamDecl.Object;
			Buffer.Add("$" + Object.Name);
			If Object.Property("Value", Value) Then
				Defaults.Add(StrTemplate("$%1 {$%1 = %2} ", Object.Name, VisitExpr(Value)));
			EndIf; 
		EndDo; 
		If Buffer.Count() > 0 Then
			Result.Add(StrConcat(Buffer, ", "));
		EndIf;
		Result.Add(") {" "");
		Backend.Indent = Backend.Indent + 1;
		If Defaults.Count() > 1 Then
			Indent(Backend);
			Defaults.Add("}" "");
			Result.Add(StrConcat(Defaults));
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
		Result.Add(VisitExpr(Stmt.Right));
		Result.Add(Chars.LF);
	ElsIf NodeType = "AddAssignStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" += ");
		Result.Add(VisitExpr(Stmt.Right));
		Result.Add(Chars.LF);
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("return ");
		If Stmt.Property("Expr") Then
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(Chars.LF);
	ElsIf NodeType = "BreakStmt" Then
		Result.Add("break" "");
	ElsIf NodeType = "ContinueStmt" Then
		Result.Add("continue" "");
	ElsIf NodeType = "RaiseStmt" Then
		Result.Add("throw ");
		If Stmt.Property("Expr") Then
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(Chars.LF);
	ElsIf NodeType = "ExecuteStmt" Then
		Result.Add("# Execute(");
		Result.Add(VisitExpr(Stmt.Expr));
		Result.Add(")" "");
	ElsIf NodeType = "CallStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr, False));
		Result.Add(Chars.LF);
	ElsIf NodeType = "IfStmt" Then
		Result.Add("if (");
		VisitIfStmt(Backend, Stmt);
		If Stmt.Property("ElsePart") Then
			Indent(Backend);
			Result.Add("else {" "");
			VisitStatements(Backend, Stmt.ElsePart);
			Result.Add("}" "");
		EndIf;
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("while (");
		Result.Add(VisitExpr(Stmt.Condition));
		Result.Add(") {" "");
		VisitStatements(Backend, Stmt.Statements);
		Result.Add("}" "");
	ElsIf NodeType = "ForStmt" Then
		If Stmt.Collection.NodeType = "RangeExpr" Then
			Result.Add("for (");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" = ");
			Result.Add(VisitExpr(Stmt.Collection.Left));
			Result.Add("; ");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" -le ");
			Result.Add(VisitExpr(Stmt.Collection.Right, True));
			Result.Add(StrTemplate("; %1++ )", VisitDesignatorExpr(Stmt.DesignatorExpr)));
		Else
			Result.Add("foreach (");
			Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
			Result.Add(" In ");
			Result.Add(VisitExpr(Stmt.Collection));
			Result.Add(")");
		EndIf;
		Result.Add(" {" "");
		VisitStatements(Backend, Stmt.Statements);
		Result.Add("}" "");
	ElsIf NodeType = "CaseStmt" Then
		Result.Add("switch ");
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr));
		Result.Add(" {" "");
		Backend.Indent = Backend.Indent + 1;
		For Each IfStmt In Stmt.WhenPart Do
			Indent(Backend);
			VisitWhenPart(Backend, IfStmt);
		EndDo;
		If Stmt.Property("ElsePart") Then
			Indent(Backend);
			Result.Add("default {" "");
			VisitStatements(Backend, Stmt.ElsePart);
			Result.Add("}" "");
		EndIf;
		Backend.Indent = Backend.Indent - 1;
		Indent(Backend);
		Result.Add("}" "");
	ElsIf NodeType = "TryStmt" Then
		Result.Add("try {" "");
		VisitStatements(Backend, Stmt.TryPart);
		Result.Add("}" "");
		Indent(Backend);
		Result.Add("catch {" "");
		VisitStatements(Backend, Stmt.ExceptPart);
		Result.Add("}" "");
	EndIf;
EndProcedure // VisitStmt()

Procedure VisitIfStmt(Backend, IfStmt)
	Var Result;
	Result = Backend.Result;
	Result.Add(VisitExpr(IfStmt.Condition));
	Result.Add(") {" "");
	VisitStatements(Backend, IfStmt.ThenPart);
	Result.Add("}" "");
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
	Result.Add(" {" "");
	VisitStatements(Backend, IfStmt.ThenPart);
	Result.Add("}" "");
EndProcedure // VisitWhenPart()

Function VisitExprList(ExprList, Separator)
	Var Buffer;
	Buffer = New Array;
	If ExprList.Count() = 1 Then
		Buffer.Add(VisitExpr(ExprList[0]));
	Else
		For Each Expr In ExprList Do
			If Expr = Undefined Then
				Buffer.Add("$1C.Missing");
			ElsIf IsComplexExpr(Expr) Then
				Buffer.Add(StrTemplate("(%1)", VisitExpr(Expr)));
			Else
				Buffer.Add(VisitExpr(Expr));
			EndIf; 
		EndDo;
	EndIf; 
	Return StrConcat(Buffer, Separator);
EndFunction // VisitExprList()

Function VisitExpr(Expr, IsOperand = False)
	Var NodeType, BasicLitKind;
	NodeType = Expr.NodeType;
	If NodeType = "BasicLitExpr" Then
		BasicLitKind = Expr.Kind;
		If BasicLitKind = Tokens.String Then
			Return StrTemplate("""%1""", StrReplace(Expr.Value, Chars.LF, "`n"));
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
		Return VisitDesignatorExpr(Expr, IsOperand);
	ElsIf NodeType = "UnaryExpr" Then
		Return StrTemplate("%1 %2",
			Operators[Expr.Operator],
			VisitExpr(Expr.Operand, True)
		);
	ElsIf NodeType = "BinaryExpr" Then
		Return StrTemplate("%1 %2 %3",
			VisitExpr(Expr.Left, True),
			Operators[Expr.Operator],
			VisitExpr(Expr.Right, True)
		);
	ElsIf NodeType = "RangeExpr" Then
		Return StrTemplate("%1..%2",
			VisitExpr(Expr.Left, True),
			VisitExpr(Expr.Right, True)
		);
	ElsIf NodeType = "NewExpr" Then
		If TypeOf(Expr.Constructor) = Type("Structure") Then
			Return StrTemplate("<# New %1 #>", VisitExpr(Expr.Constructor));
		Else
			Return StrTemplate("<# New(%1) #>", VisitExprList(Expr.Constructor, ", "));
		EndIf;
	ElsIf NodeType = "TernaryExpr" Then
		Return StrTemplate("if (%1) { %2 } else { %3 }",
			VisitExpr(Expr.Condition),
			VisitExpr(Expr.ThenPart),
			VisitExpr(Expr.ElsePart)
		);
	ElsIf NodeType = "ParenExpr" Then
		Return StrTemplate("(%1)", VisitExpr(Expr.Expr));
	ElsIf NodeType = "NotExpr" Then
		Return StrTemplate("-not (%1)", VisitExpr(Expr.Expr));
	ElsIf NodeType = "ArrayExpr" Then
	ElsIf NodeType = "StructExpr" Then
	EndIf;
EndFunction // VisitExpr()

Function VisitDesignatorExpr(DesignatorExpr, IsOperand = True)
	Var Buffer, Selector;
	Buffer = New Array;
	If Not DesignatorExpr.Call Then
		If DesignatorExpr.Object.Kind = "Variable" And Not DesignatorExpr.Object.Local Then
			Buffer.Add("$script:")
		Else
			Buffer.Add("$");
		EndIf; 
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
	If IsOperand And DesignatorExpr.Call Then
		Return StrTemplate("(%1)", StrConcat(Buffer));
	EndIf;
	Return StrConcat(Buffer);
EndFunction // VisitDesignatorExpr()

Function IsComplexExpr(Expr)
	Return Expr.NodeType = "UnaryExpr"
		Or Expr.NodeType = "BinaryExpr"
		Or Expr.NodeType = "RangeExpr"
		Or Expr.NodeType = "NewExpr"
		Or Expr.NodeType = "TernaryExpr"
		Or Expr.NodeType = "NotExpr"
		Or Expr.NodeType = "DesignatorExpr" And Expr.Call;	
EndFunction // IsComplexExpr() 