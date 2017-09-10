
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
	VisitStatements(Module.Body);
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
	NodeType = Decl.Type;
	If NodeType = "VarsDecl" Or NodeType = "ModVarsDecl" Then
		Indent(Result);
		Result.Add("Var ");
		VisitVarList(Decl.List);
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
		VisitParams(Decl.Object.Params);
		Result.Add(")");
		If Decl.Object.Export Then
			Result.Add(" Export");
		EndIf;
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			VisitDecl(Stmt);
		EndDo;
		For Each Stmt In Decl.Body Do
			VisitStmt(Stmt);
		EndDo;
		If NodeType = "FuncDecl" Then
			Result.Add(StrTemplate("EndFunction // %1()", Decl.Object.Name));
		Else
			Result.Add(StrTemplate("EndProcedure // %1()", Decl.Object.Name));
		EndIf;
		Result.Add(Chars.LF);
		Indent = Indent - 1;
	ElsIf NodeType = "PrepRegionDecl" Then
		Result.Add("#Region ");
		Result.Add(Decl.Name);
		Result.Add(Chars.LF);
		VisitDecls(Decl.Decls);
		VisitStatements(Decl.Body);
		Result.Add("#EndRegion // ");
		Result.Add(Decl.Name);
		Result.Add(Chars.LF);
		Result.Add(Chars.LF);
	EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarList(VarList)
	Var Buffer, Object;
	Buffer = New Array;
	For Each Object In VarList Do
		Buffer.Add(Object.Name);
		If Object.Property("Export")
			And Object.Export Then
			Buffer.Add(" Export");
		EndIf; 
	EndDo;
	If Buffer.Count() > 0 Then
		Result.Add(StrConcat(Buffer, ", "));
	EndIf;
EndProcedure // VisitVarList()

Procedure VisitParams(ParamList)
	Var Buffer, Object;
	Buffer = New Array;
	For Each Object In ParamList Do
		Buffer.Add(
			?(Object.ByVal, "Val ", "") +
			Object.Name +
			?(Object.Value <> Undefined, " = " + VisitExpr(Object.Value), "")
		);
	EndDo;
	If Buffer.Count() > 0 Then
		Result.Add(StrConcat(Buffer, ", "));
	EndIf;
EndProcedure // VisitParams()

Procedure VisitStmt(Stmt)
	Var NodeType;
	NodeType = Stmt.Type;
	Indent(Result);
	If NodeType = "AssignStmt" Then
		Result.Add(VisitDesigExpr(Stmt.Left));
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
		Result.Add(VisitDesigExpr(Stmt.Desig));
		Result.Add(";" "");
	ElsIf NodeType = "IfStmt" Then
		Result.Add("If ");
		Result.Add(VisitExpr(Stmt.Cond));
		Result.Add(" Then" "");
		VisitStatements(Stmt.Then);
		If Stmt.ElsIf <> Undefined Then
			For Each Item In Stmt.ElsIf Do
				Result.Add("ElsIf ");
				Result.Add(VisitExpr(Item.Cond));
				Result.Add(" Then" "");
				VisitStatements(Item.Then);
			EndDo;
		EndIf;
		If Stmt.Else <> Undefined Then
			Result.Add("Else" "");
			VisitStatements(Stmt.Else);
		EndIf;
		Result.Add("EndIf;" "");
	ElsIf NodeType = "WhileStmt" Then
		Result.Add("While ");
		Result.Add(VisitExpr(Stmt.Cond));
		Result.Add(" Do" "");
		VisitStatements(Stmt.Body);
		Result.Add("EndDo;" "");
	ElsIf NodeType = "ForStmt" Then
		Result.Add("For ");
		Result.Add(VisitDesigExpr(Stmt.Desig));
		Result.Add(" = ");
		Result.Add(VisitExpr(Stmt.From));
		Result.Add(" To ");
		Result.Add(VisitExpr(Stmt.To));
		Result.Add(" Do" "");
		VisitStatements(Stmt.Body);
		Result.Add("EndDo;" "");
	ElsIf NodeType = "ForEachStmt" Then
		Result.Add("For Each ");
		Result.Add(VisitDesigExpr(Stmt.Desig));
		Result.Add(" In ");
		Result.Add(VisitExpr(Stmt.In));
		Result.Add(" Do" "");
		VisitStatements(Stmt.Body);
		Result.Add("EndDo;" "");
	ElsIf NodeType = "TryStmt" Then
		Result.Add("Try" "");
		VisitStatements(Stmt.TryPart);
		Result.Add("Except" "");
		VisitStatements(Stmt.ExceptPart);
		Result.Add("EndTry;" "");
	EndIf;
EndProcedure // VisitStmt()

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
	NodeType = Expr.Type;
	If NodeType = "BasicLitExpr" Then
		BasicLitKind = Expr.Kind;
		If StrStartsWith(BasicLitKind, Tokens.String) Then
			Return StrTemplate("""%1""", Expr.Value);
		ElsIf BasicLitKind = Tokens.Number Then
			Return Format(Expr.Value, "NZ=0; NG=");
		ElsIf BasicLitKind = Tokens.DateTime Then
			Return Format(Expr.Value, "DF='""''yyyyMMdd'''");
		ElsIf BasicLitKind = Tokens.True Or BasicLitKind = Tokens.False Then
			Return Format(Expr.Value, "BF=False; BT=True");
		ElsIf BasicLitKind = Tokens.Undefined Then
			Return "Undefined";
		ElsIf BasicLitKind = Tokens.Null Then
			Return "Null";
		Else
			Raise "Unknown basic literal";
		EndIf;
	ElsIf NodeType = "DesigExpr" Then
		Return VisitDesigExpr(Expr);
	ElsIf NodeType = "UnaryExpr" Then
		Return StrTemplate("%1 %2", Operators[Expr.Operator], VisitExpr(Expr.Operand));
	ElsIf NodeType = "BinaryExpr" Then
		Return StrTemplate("%1 %2 %3", VisitExpr(Expr.Left), Operators[Expr.Operator], VisitExpr(Expr.Right));
	ElsIf NodeType = "RangeExpr" Then
		Return StrTemplate("%1 To %2", VisitExpr(Expr.Left), VisitExpr(Expr.Right));
	ElsIf NodeType = "NewExpr" Then
		If TypeOf(Expr.Constr) = Type("Structure") Then
			Return StrTemplate("New %1", VisitExpr(Expr.Constr));
		Else
			Return StrTemplate("New(%1)", VisitExprList(Expr.Constr));
		EndIf;
	ElsIf NodeType = "TernaryExpr" Then
		Return StrTemplate("?(%1, %2, %3)", VisitExpr(Expr.Cond), VisitExpr(Expr.Then), VisitExpr(Expr.Else));
	ElsIf NodeType = "ParenExpr" Then
		Return StrTemplate("(%1)", VisitExpr(Expr.Expr));
	ElsIf NodeType = "NotExpr" Then
		Return StrTemplate("Not %1", VisitExpr(Expr.Expr));
	ElsIf NodeType = "StringExpr" Then
		Return VisitStringExpr(Expr);
	EndIf;
EndFunction // VisitExpr()

Function VisitDesigExpr(DesigExpr)
	Var Buffer;
	Buffer = New Array;
	Buffer.Add(DesigExpr.Object.Name);
	For Each Selector In DesigExpr.Select Do
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
	Return StrConcat(Buffer);
EndFunction // VisitDesigExpr()

Function VisitStringExpr(StringExpr)
	Var Buffer;
	Buffer = New Array;
	For Each Item In StringExpr.List Do
		Buffer.Add(VisitExpr(Item));
	EndDo;
	Return StrConcat(Buffer, " ");
EndFunction // VisitStringExpr()  