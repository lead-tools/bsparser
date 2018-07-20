
// Транслятор BSL -> BSL

Var Result; // array (string)
Var Indent; // number

Var Nodes;         // enum
Var Tokens;        // enum
Var SelectKinds;   // enum
Var Operators;     // structure as map[one of Tokens](string)

Var LastLine;
Var Comments;      // map[number](string)

Procedure Init(BSLParserProcessor) Export

	Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"=", "<>", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "Or", "And", "Not"
	);

	Nodes = BSLParserProcessor.Nodes();
	Tokens = BSLParserProcessor.Tokens();
	SelectKinds = BSLParserProcessor.SelectKinds();

	LastLine = 1;

	Result = New Array;
	Indent = -1;

EndProcedure // Init()

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitModule");
	Return Interface;
EndFunction // Interface()

Function Result() Export
	Return StrConcat(Result);
EndFunction // Refult()

Procedure VisitModule(Module, Stack, Counters) Export
	Comments = Module.Comments;
	VisitDeclarations(Module.Decls);
	VisitStatements(Module.Body);
EndProcedure // VisitModule()

Procedure VisitDeclarations(Declarations)
	Indent = Indent + 1; // >>
	For Each Decl In Declarations Do
		VisitDecl(Decl);
	EndDo;
	Indent = Indent - 1; // <<
EndProcedure // VisitDeclarations()

Procedure VisitStatements(Statements)
	Indent = Indent + 1; // >>
	For Each Stmt In Statements Do
		VisitStmt(Stmt);
	EndDo;
	Indent = Indent - 1; // <<
	Indent();
EndProcedure // VisitStatements()

#Region VisitDecl

Procedure VisitDecl(Decl)
    Var Type;
	AlignLine(Decl.Place.BegLine);
	Type = Decl.Type;
    If Type = Nodes.VarModDecl Then
        VisitVarModDecl(Decl);
    ElsIf Type = Nodes.VarLocDecl Then
        VisitVarLocDecl(Decl);
    ElsIf Type = Nodes.ProcDecl Then
        VisitProcDecl(Decl);
    ElsIf Type = Nodes.FuncDecl Then
        VisitFuncDecl(Decl);
	ElsIf Type = Nodes.PrepRegionInst
		Or Type = Nodes.PrepEndRegionInst
		Or Type = Nodes.PrepIfInst
		Or Type = Nodes.PrepElsIfInst
		Or Type = Nodes.PrepElseInst
		Or Type = Nodes.PrepEndIfInst
		Or Type = Nodes.PrepUseInst Then
		VisitPrepInst(Decl);
    EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarModDecl(VarModDecl)
	If VarModDecl.Directive <> Undefined Then
		Result.Add(StrTemplate("&%1%2", VarModDecl.Directive, Chars.LF));
	EndIf;
	Result.Add("Var ");
	VisitVarList(VarModDecl.List);
	Result.Add(";");
EndProcedure // VisitVarModDecl()

Procedure VisitVarLocDecl(VarLocDecl)
	Result.Add("Var ");
	VisitVarList(VarLocDecl.List);
	Result.Add(";");
EndProcedure // VisitVarLocDecl()

Procedure VisitProcDecl(ProcDecl)
	Var Object;
	Object = ProcDecl.Object;
	If Object.Directive <> Undefined Then
		Result.Add(StrTemplate("&%1%2", Object.Directive, Chars.LF));
	EndIf;
	Result.Add("Procedure ");
	Result.Add(Object.Name);
	VisitParams(Object.Params);
	If Object.Export Then
		Result.Add(" Export");
	EndIf;
	VisitDeclarations(ProcDecl.Decls);
    VisitStatements(ProcDecl.Body);
	AlignLine(ProcDecl.Place.EndLine);
	Result.Add("EndProcedure");
EndProcedure // VisitProcDecl()

Procedure VisitFuncDecl(FuncDecl)
	Var Object;
	Object = FuncDecl.Object;
	If Object.Directive <> Undefined Then
		Result.Add(StrTemplate("&%1%2", Object.Directive, Chars.LF));
	EndIf;
	Result.Add("Function ");
	Result.Add(Object.Name);
	VisitParams(Object.Params);
	If Object.Export Then
		Result.Add(" Export");
	EndIf;
	VisitDeclarations(FuncDecl.Decls);
    VisitStatements(FuncDecl.Body);
	AlignLine(FuncDecl.Place.EndLine);
	Result.Add("EndFunction");
EndProcedure // VisitFuncDecl()

#EndRegion // VisitDecl

#Region VisitExpr

Procedure VisitExpr(Expr)
    Var Type, Hook;
	AlignLine(Expr.Place.BegLine);
	Type = Expr.Type;
	If Type = Nodes.BasicLitExpr Then
        VisitBasicLitExpr(Expr);
    ElsIf Type = Nodes.DesigExpr Then
        VisitDesigExpr(Expr);
    ElsIf Type = Nodes.UnaryExpr Then
        VisitUnaryExpr(Expr);
    ElsIf Type = Nodes.BinaryExpr Then
        VisitBinaryExpr(Expr);
    ElsIf Type = Nodes.NewExpr Then
        VisitNewExpr(Expr);
    ElsIf Type = Nodes.TernaryExpr Then
        VisitTernaryExpr(Expr);
    ElsIf Type = Nodes.ParenExpr Then
        VisitParenExpr(Expr);
    ElsIf Type = Nodes.NotExpr Then
        VisitNotExpr(Expr);
    ElsIf Type = Nodes.StringExpr Then
        VisitStringExpr(Expr);
	EndIf;
EndProcedure // VisitExpr()

Procedure VisitBasicLitExpr(BasicLitExpr)
	BasicLitKind = BasicLitExpr.Kind;
	If BasicLitKind = Tokens.String Then
		Result.Add(StrTemplate("""%1""", BasicLitExpr.Value));
	ElsIf BasicLitKind = Tokens.StringBeg Then
		Result.Add(StrTemplate("""%1", BasicLitExpr.Value));
	ElsIf BasicLitKind = Tokens.StringMid Then
		Result.Add(StrTemplate("|%1", BasicLitExpr.Value));
	ElsIf BasicLitKind = Tokens.StringEnd Then
		Result.Add(StrTemplate("|%1""", BasicLitExpr.Value));
	ElsIf BasicLitKind = Tokens.Number Then
		Result.Add(Format(BasicLitExpr.Value, "NZ=0; NG="));
	ElsIf BasicLitKind = Tokens.DateTime Then
		Result.Add(Format(BasicLitExpr.Value, "DF='""''yyyyMMdd'''"));
	ElsIf BasicLitKind = Tokens.True Or BasicLitKind = Tokens.False Then
		Result.Add(Format(BasicLitExpr.Value, "BF=False; BT=True"));
	ElsIf BasicLitKind = Tokens.Undefined Then
		Result.Add("Undefined");
	ElsIf BasicLitKind = Tokens.Null Then
		Result.Add("Null");
	Else
		Raise "Unknown basic literal";
	EndIf;
EndProcedure // VisitBasicLitExpr()

Procedure VisitDesigExpr(DesigExpr)
	Result.Add(DesigExpr.Object.Name);
	VisitSelectList(DesigExpr.Select);
EndProcedure // VisitDesigExpr()

Procedure VisitUnaryExpr(UnaryExpr)
	Result.Add(Operators[UnaryExpr.Operator]);
	VisitExpr(UnaryExpr.Operand);
EndProcedure // VisitUnaryExpr()

Procedure VisitBinaryExpr(BinaryExpr)
	VisitExpr(BinaryExpr.Left);
	Result.Add(StrTemplate(" %1 ", Operators[BinaryExpr.Operator]));
    VisitExpr(BinaryExpr.Right);
EndProcedure // VisitBinaryExpr()

Procedure VisitNewExpr(NewExpr)
    If NewExpr.Name <> Undefined Then
		Result.Add("New " + NewExpr.Name);
    Else
		Result.Add("New ");
	EndIf;
	If NewExpr.Args.Count() > 0 Then
		Result.Add("(");
		Indent = Indent + 1; // >>
		VisitExprList(NewExpr.Args);
		Indent = Indent - 1; // <<
		AlignLine(NewExpr.Place.EndLine);
		Result.Add(")");
	EndIf;
EndProcedure // VisitNewExpr()

Procedure VisitTernaryExpr(TernaryExpr)
	Result.Add("?(");
	VisitExpr(TernaryExpr.Cond);
	Result.Add(", ");
	VisitExpr(TernaryExpr.Then);
	Result.Add(", ");
	VisitExpr(TernaryExpr.Else);
	Result.Add(")");
	VisitSelectList(TernaryExpr.Select);
EndProcedure // VisitTernaryExpr()

Procedure VisitParenExpr(ParenExpr)
	Result.Add("(");
	Indent = Indent + 1; // >>
	VisitExpr(ParenExpr.Expr);
	Indent = Indent - 1; // <<
	AlignLine(ParenExpr.Place.EndLine);
	Result.Add(")");
EndProcedure // VisitParenExpr()

Procedure VisitNotExpr(NotExpr)
	Result.Add("Not ");
	VisitExpr(NotExpr.Expr);
EndProcedure // VisitNotExpr()

Procedure VisitStringExpr(StringExpr)
	If StringExpr.List.Count() > 1 Then
		For Each Expr In StringExpr.List Do
			AlignLine(Expr.Place.BegLine);
			VisitBasicLitExpr(Expr);
		EndDo;
	Else
		VisitExpr(StringExpr.List[0]);
	EndIf;
EndProcedure // VisitStringExpr()

#EndRegion // VisitExpr

#Region VisitStmt

Procedure VisitStmt(Stmt)
	AlignLine(Stmt.Place.BegLine);
	Type = Stmt.Type;
	If Type = Nodes.AssignStmt Then
        VisitAssignStmt(Stmt);
    ElsIf Type = Nodes.ReturnStmt Then
        VisitReturnStmt(Stmt);
    ElsIf Type = Nodes.BreakStmt Then
        VisitBreakStmt(Stmt);
    ElsIf Type = Nodes.ContinueStmt Then
        VisitContinueStmt(Stmt);
    ElsIf Type = Nodes.RaiseStmt Then
        VisitRaiseStmt(Stmt);
    ElsIf Type = Nodes.ExecuteStmt Then
        VisitExecuteStmt(Stmt);
    ElsIf Type = Nodes.CallStmt Then
        VisitCallStmt(Stmt);
    ElsIf Type = Nodes.IfStmt Then
        VisitIfStmt(Stmt);
    ElsIf Type = Nodes.WhileStmt Then
        VisitWhileStmt(Stmt);
    ElsIf Type = Nodes.ForStmt Then
        VisitForStmt(Stmt);
    ElsIf Type = Nodes.ForEachStmt Then
        VisitForEachStmt(Stmt);
    ElsIf Type = Nodes.TryStmt Then
        VisitTryStmt(Stmt);
    ElsIf Type = Nodes.GotoStmt Then
        VisitGotoStmt(Stmt);
    ElsIf Type = Nodes.LabelStmt Then
        VisitLabelStmt(Stmt);
	ElsIf Type = Nodes.PrepRegionInst
		Or Type = Nodes.PrepEndRegionInst
		Or Type = Nodes.PrepIfInst
		Or Type = Nodes.PrepElsIfInst
		Or Type = Nodes.PrepElseInst
		Or Type = Nodes.PrepEndIfInst Then
		VisitPrepInst(Stmt);
    EndIf;
EndProcedure // VisitStmt()

Procedure VisitAssignStmt(AssignStmt)
    VisitDesigExpr(AssignStmt.Left);
	Result.Add(" = ");
	VisitExpr(AssignStmt.Right);
	Result.Add(";");
EndProcedure // VisitAssignStmt()

Procedure VisitReturnStmt(ReturnStmt)
	Result.Add("Return ");
	If ReturnStmt.Expr <> Undefined Then
        VisitExpr(ReturnStmt.Expr);
	EndIf;
	Result.Add(";");
EndProcedure // VisitReturnStmt()

Procedure VisitBreakStmt(BreakStmt)
	Result.Add("Break;");
EndProcedure // VisitBreakStmt()

Procedure VisitContinueStmt(ContinueStmt)
	Result.Add("Continue;");
EndProcedure // VisitContinueStmt()

Procedure VisitRaiseStmt(RaiseStmt)
	Result.Add("Raise ");
	If RaiseStmt.Expr <> Undefined Then
        VisitExpr(RaiseStmt.Expr);
	EndIf;
	Result.Add(";");
EndProcedure // VisitRaiseStmt()

Procedure VisitExecuteStmt(ExecuteStmt)
	Result.Add("Execute(");
	VisitExpr(ExecuteStmt.Expr);
	Result.Add(");");
EndProcedure // VisitExecuteStmt()

Procedure VisitCallStmt(CallStmt)
    VisitDesigExpr(CallStmt.Desig);
	Result.Add(";");
EndProcedure // VisitCallStmt()

Procedure VisitIfStmt(IfStmt)
	Result.Add("If ");
	VisitExpr(IfStmt.Cond);
	Result.Add(" Then");
    VisitStatements(IfStmt.Then);
    If IfStmt.ElsIf <> Undefined Then
        For Each ElsIfStmt In IfStmt.ElsIf Do
			AlignLine(ElsIfStmt.Place.BegLine);
			VisitElsIfStmt(ElsIfStmt);
        EndDo;
    EndIf;
    If IfStmt.Else <> Undefined Then
		AlignLine(LastLine + 1);
		Result.Add("Else ");
		VisitStatements(IfStmt.Else);
	EndIf;
	AlignLine(IfStmt.Place.EndLine);
	Result.Add("EndIf;");
EndProcedure // VisitIfStmt()

Procedure VisitElsIfStmt(ElsIfStmt)
	Result.Add("ElsIf ");
	VisitExpr(ElsIfStmt.Cond);
	Result.Add(" Then");
    VisitStatements(ElsIfStmt.Then);
EndProcedure // VisitElsIfStmt()

Procedure VisitWhileStmt(WhileStmt)
	Result.Add("While ");
	VisitExpr(WhileStmt.Cond);
	Result.Add(" Do");
    VisitStatements(WhileStmt.Body);
	AlignLine(WhileStmt.Place.EndLine);
	Result.Add("EndDo;");
EndProcedure // VisitWhileStmt()

Procedure VisitForStmt(ForStmt)
	Result.Add("For ");
	VisitDesigExpr(ForStmt.Desig);
	Result.Add(" = ");
	VisitExpr(ForStmt.From);
	Result.Add(" To ");
	VisitExpr(ForStmt.To);
	Result.Add(" Do");
	VisitStatements(ForStmt.Body);
	AlignLine(ForStmt.Place.EndLine);
	Result.Add("EndDo;");
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(ForEachStmt)
	Result.Add("For Each ");
	VisitDesigExpr(ForEachStmt.Desig);
	Result.Add(" In ");
	VisitExpr(ForEachStmt.In);
	Result.Add(" Do");
	VisitStatements(ForEachStmt.Body);
	AlignLine(ForEachStmt.Place.EndLine);
	Result.Add("EndDo;");
EndProcedure // VisitForEachStmt()

Procedure VisitTryStmt(TryStmt)
	Result.Add("Try");
	VisitStatements(TryStmt.Try);
	Result.Add("Except");
	VisitStatements(TryStmt.Except);
	AlignLine(TryStmt.Place.EndLine);
	Result.Add("EndTry;");
EndProcedure // VisitTryStmt()

Procedure VisitGotoStmt(GotoStmt)
	Result.Add(StrTemplate("Goto ~%1%2", GotoStmt.Label, ";"));
EndProcedure // VisitGotoStmt()

Procedure VisitLabelStmt(LabelStmt)
	Result.Add(StrTemplate("~%1:", LabelStmt.Label));
EndProcedure // VisitLabelStmt()

#EndRegion // VisitStmt

#Region VisitPrep

Procedure VisitPrepExpr(PrepExpr)
	Var Type;
	AlignLine(PrepExpr.Place.BegLine);
	Type = PrepExpr.Type;
	If Type = Nodes.PrepSymExpr Then
		VisitPrepSymExpr(PrepExpr);
	ElsIf Type = Nodes.PrepBinaryExpr Then
		VisitPrepBinaryExpr(PrepExpr);
	ElsIf Type = Nodes.PrepNotExpr Then
		VisitPrepNotExpr(PrepExpr);
	EndIf;
EndProcedure // VisitPrepExpr()

Procedure VisitPrepSymExpr(PrepSymExpr)
	Result.Add(PrepSymExpr.Symbol);
EndProcedure // VisitPrepSymExpr()

Procedure VisitPrepBinaryExpr(PrepBinaryExpr)
	VisitPrepExpr(PrepBinaryExpr.Left);
	Result.Add(StrTemplate(" %1 ", Operators[PrepBinaryExpr.Operator]));
	VisitPrepExpr(PrepBinaryExpr.Right);
EndProcedure // VisitPrepBinaryExpr()

Procedure VisitPrepNotExpr(PrepNotExpr)
	Result.Add("Not ");
	VisitPrepExpr(PrepNotExpr.Expr);
EndProcedure // VisitPrepNotExpr()

Procedure VisitPrepInst(PrepInst)
	Var Type;
	Type = PrepInst.Type;
	If Type = Nodes.PrepRegionInst Then
		Result.Add("#Region ");
		Result.Add(PrepInst.Name);
	ElsIf Type = Nodes.PrepEndRegionInst Then
		Result.Add("#EndRegion");
	ElsIf Type = Nodes.PrepIfInst Then
		Result.Add("#If ");
		VisitPrepExpr(PrepInst.Cond);
		Result.Add(" Then");
	ElsIf Type = Nodes.PrepElsIfInst Then
		Result.Add("#ElsIf ");
		VisitPrepExpr(PrepInst.Cond);
		Result.Add(" Then");
	ElsIf Type = Nodes.PrepElseInst Then
		Result.Add("#Else");
	ElsIf Type = Nodes.PrepEndIfInst Then
		Result.Add("#EndIf");
	ElsIf Type = Nodes.PrepUseInst Then	
		Result.Add("#Use ");
		Result.Add(PrepInst.Path);
	EndIf;
EndProcedure // VisitPrepInst()

#EndRegion // VisitPrep

#Region Aux

Procedure Indent()
	For Index = 1 To Indent Do
		Result.Add(Chars.Tab);
	EndDo;
EndProcedure // Indent()

Procedure AlignLine(NewLine)
	For LastLine = LastLine To NewLine - 1 Do
		Comment = Comments[LastLine];
		If Comment <> Undefined Then
			Result.Add(" //" + Comment);
		EndIf;
		Result.Add(Chars.LF); Indent();	
	EndDo;
EndProcedure // AlignLine()

Procedure VisitVarList(VarList)
	Var Buffer, Object;
	Buffer = New Array;
	For Each Object In VarList Do
		If Object.Property("Export") And Object.Export Then
			Buffer.Add(Object.Name + " Export");
		Else
			Buffer.Add(Object.Name);
		EndIf;
	EndDo;
	If Buffer.Count() > 0 Then
		Result.Add(StrConcat(Buffer, ", "));
	EndIf;
EndProcedure // VisitVarList()

Procedure VisitParams(ParamList)
	Var Object;
	Result.Add("(");
	If ParamList.Count() > 0 Then
		For Each Object In ParamList Do
			If Object.ByVal Then
				Result.Add("Val ");
			EndIf;
			Result.Add(Object.Name);
			If Object.Value <> Undefined Then
				Result.Add(" = ");
				VisitExpr(Object.Value);
			EndIf;
			Result.Add(", ");
		EndDo;
		Result[Result.UBound()] = ")";
	Else
		Result.Add(")");
	EndIf;
EndProcedure // VisitParams()

Procedure VisitExprList(ExprList)
	If ExprList.Count() > 0 Then
		For Each Expr In ExprList Do
			If Expr = Undefined Then
				Result.Add("");
			Else
				VisitExpr(Expr);
			EndIf;
			Result.Add(", ");
		EndDo;
		Result[Result.UBound()] = "";
	EndIf;
EndProcedure // VisitExprList()

Procedure VisitSelectList(SelectList)
	For Each SelectExpr In SelectList Do
		If SelectExpr.Kind = SelectKinds.Ident Then
			Result.Add(".");
			Result.Add(SelectExpr.Value);
		ElsIf SelectExpr.Kind = SelectKinds.Index Then
			Result.Add("[");
			VisitExpr(SelectExpr.Value);
			Result.Add("]");
		ElsIf SelectExpr.Kind = SelectKinds.Call Then
			Result.Add("(");
			Indent = Indent + 1; // >>
			VisitExprList(SelectExpr.Value);
			Indent = Indent - 1; // <<
			AlignLine(SelectExpr.Place.EndLine);
			Result.Add(")");
		Else
			Raise "Unknown selector kind";
		EndIf;
	EndDo;
EndProcedure // VisitSelectList()

#EndRegion // Aux