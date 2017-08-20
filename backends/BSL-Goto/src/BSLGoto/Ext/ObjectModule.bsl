
Var Tokens;        // enum
Var SelectorKinds; // enum
Var Operators;     // structure as map[one of Tokens](string)

// state:
Var Result;   // array (string)
Var Indent;   // number
Var EndProc;  // string
Var VarIndex; // number
Var Prefix;   // string

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
	Result.Add("M = New Array(1024);" "");
	Result.Add("SP = 0;" "");
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
	Var NodeType, Object;
	NodeType = Decl.NodeType;
	If NodeType = "VarListDecl" Then
		Indent(Result);
		VisitVarList(Decl.VarList);
		Result.Add(";" "");
	ElsIf NodeType = "FuncDecl" Or NodeType = "ProcDecl" Then
		Object = Decl.Object;
		Prefix = Object.Name;
		Result.Add(Chars.LF);
		Indent = Indent + 1;
		Result.Add(StrTemplate("~%1:" "", Object.Name));
		EndProc = "~End" + Object.Name;
		Indent(Result);
		Result.Add("// prolog:" "");
		RenameItems(Object.ParamList);
		VisitParamList(Object.ParamList);
		Indent(Result);
		Result.Add("// code:" "");
		For Each VarListDecl In Decl.Decls Do
			RenameItems(VarListDecl.VarList);
			VisitDecl(VarListDecl);
		EndDo;
		For Each AutoVar In Decl.AutoVars Do
			AutoVar.Name = StrTemplate("%1_%2", Prefix, AutoVar.Name);
		EndDo;
		For Each Stmt In Decl.Statements Do
			VisitStmt(Stmt);
		EndDo;
		Result.Add(EndProc + ":" "");
		Indent(Result);
		Result.Add("// epilog:" "");
		Indent(Result);
		Result.Add("SP = SP-1;" "");
		VisitParamList(Decl.Object.ParamList, True);
		Result.Add("Goto ~Return;");
		Result.Add(Chars.LF);
		Indent = Indent - 1;
	EndIf;
EndProcedure // VisitDecl()

Procedure RenameItems(List)
	For Each Item In List Do
		Item.Object.Name = StrTemplate("%1_%2", Prefix, Item.Object.Name);
	EndDo;
EndProcedure // RenameItems()

Procedure VisitVarList(VarList)
	Var Buffer, Object, Value;
	Buffer = New Array;
	For Each VarDecl In VarList Do
		Object = VarDecl.Object;
		Buffer.Add(Object.Name + " = Undefined");
	EndDo;
	If Buffer.Count() > 0 Then
		Result.Add(StrConcat(Buffer, "; "));
	EndIf;
EndProcedure // VisitVarList()

Procedure VisitParamList(ParamList, IsReturn = False)
	Var Offset;
	Offset = 1;
	For Each ParamDecl In ParamList Do
		If IsReturn Then
			If Not ParamDecl.Object.ByVal Then
				Indent(Result);
				Result.Add(StrTemplate("M[SP-%1] = %2;" "", Format(Offset, "NZ=0; NG=0"), ParamDecl.Object.Name));
			EndIf;
		Else
			Indent(Result);
			Result.Add(StrTemplate("%1 = M[SP-%2];" "", ParamDecl.Object.Name, Format(Offset, "NZ=0; NG=0")));
		EndIf;
		Offset = Offset + 1;
	EndDo;
EndProcedure // VisitParamList()

Procedure VisitStmt(Stmt)
	Var NodeType;
	NodeType = Stmt.NodeType;
	Indent(Result);
	If NodeType = "AssignStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" = ");
		Result.Add(VisitExpr(Stmt.Right));
		Result.Add(";" "");
		If VarIndex > 0 Then
			Indent(Result);
			Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
		EndIf;
	ElsIf NodeType = "AddAssignStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" = ");
		Result.Add(VisitDesignatorExpr(Stmt.Left[0]));
		Result.Add(" + ");
		Result.Add(VisitExpr(Stmt.Right));
		Result.Add(";" "");
		If VarIndex > 0 Then
			Indent(Result);
			Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
		EndIf;
	ElsIf NodeType = "ReturnStmt" Then
		Result.Add("SP = SP+1;" "");
		Indent(Result);
		If Stmt.Property("Expr") Then
			Result.Add(StrTemplate("M[SP] = %1;" "", VisitExpr(Stmt.Expr)));
			If VarIndex > 0 Then
				Indent(Result);
				Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
			EndIf;
		Else
			Result.Add("M[SP] = Undefined;" "");
		EndIf;
		Indent(Result);
		Result.Add("Goto " + EndProc); // todo: rename label
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
		If VarIndex > 0 Then
			Indent(Result);
			Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
		EndIf;
	ElsIf NodeType = "CallStmt" Then
		Result.Add(VisitDesignatorExpr(Stmt.DesignatorExpr, False));
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
		If VarIndex > 0 Then
			Indent(Result);
			Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
		EndIf;
		VisitStatements(Stmt.Statements);
		Result.Add("EndDo;" "");
	ElsIf NodeType = "ForStmt" Then
		Result.Add("For "); //
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
		If VarIndex > 0 Then
			Indent(Result);
			Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
		EndIf;
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
	If VarIndex > 0 Then
		Indent(Result);
		Result.Add(StrTemplate("SP = SP = %1;" "", VarIndex));
	EndIf;
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
	Var NodeType, BasicLitKind, ExprString;
	If Expr = Undefined Then
		Return "";
	EndIf;
	VarIndex = 0;
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

Function GenerateCall(Object, ArgList)
	Var TempVar, Index, ArgExpr, Arg, ParamDecl;
	Result.Add("// call:" "");
	Indent(Result);
	Result.Add("SP = SP+1; M[SP] = Undefined; // ret val" "");
	Indent(Result);
	For Index = ArgList.Count() - 1 To 0 Do
		ArgExpr = ArgList[Index];
		If ArgExpr = Undefined Then
			ParamDecl = Object.ParamList[Index];
			ParamDecl.Object.Property("Value", ArgExpr);
		EndIf;
		If ArgExpr = Undefined Then
			Arg = "Undefined";
		Else
			Arg = VisitExpr(ArgExpr);
		EndIf;
		Result.Add(StrTemplate("SP = SP+1; M[SP] = %1; // arg #%2" "", Arg, Index));
		Indent(Result);
	EndDo;
	Result.Add(StrTemplate("SP = SP+1; M[SP] = ""%1_%2""; // ret adr" "", Prefix, Object.Name));
	Indent(Result);
	Result.Add(StrTemplate("Goto ~%1;" "", Object.Name));
	Result.Add(StrTemplate("~%1_%2:" "", Prefix, Object.Name));
	TempVar = StrTemplate("%1_%2", Prefix, Format(VarIndex, "NZ=0; NG="));
	Indent(Result);
	Result.Add(TempVar + " = M[SP]");
	VarIndex = VarIndex + 1;
	Return TempVar;
EndFunction // GenerateCall()

Function VisitDesignatorExpr(DesignatorExpr, IsExpr = True)
	Var Buffer, Object, Index, Selectors, Selector, TempVar;
	Buffer = New Array;
	Index = 0;
	Object = DesignatorExpr.Object;
	If DesignatorExpr.Property("Selectors", Selectors) Then
		If Object.Kind = "Procedure" Then
			GenerateCall(Object, Selectors[Index].Value);
			Index = Index + 1;
		ElsIf Object.Kind = "Function" Then
			TempVar = GenerateCall(Object, Selectors[Index].Value);
			If IsExpr Then
				Buffer.Add(TempVar);
			EndIf;
			Index = Index + 1;
		Else
			Buffer.Add(DesignatorExpr.Object.Name);
		EndIf;
		For Index = Index To Selectors.Count() - 1 Do
			Selector = Selectors[Index];
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
	Else
		Buffer.Add(DesignatorExpr.Object.Name);
	EndIf;
	Return StrConcat(Buffer);
EndFunction // VisitDesignatorExpr()
