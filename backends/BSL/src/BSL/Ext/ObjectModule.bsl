
Var Result; // array (string)
Var Indent; // number

Var Nodes;         // enum
Var Tokens;        // enum
Var SelectorKinds; // enum
Var Operators;     // structure as map[one of Tokens](string)

Var LastLine, LastComment;
Var Comments;      // map[number](string)

Procedure Init(BSLParserProcessor) Export

	BSLParserProcessor.Location = True;

	Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"=", "<>", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "Or", "And", "Not"
	);

	Nodes = BSLParserProcessor.Nodes();
	Tokens = BSLParserProcessor.Tokens();
	SelectorKinds = BSLParserProcessor.SelectorKinds();

	LastLine = 1;

	Result = New Array;
	Indent = -1;

EndProcedure // Init()

Function VisitModule(Module) Export
	Comments = Module.Comments;
	VisitDeclarations(Module.Decls);
	VisitStatements(Module.Body);
	Return StrConcat(Result);
EndFunction // VisitModule()

Procedure VisitDeclarations(Declarations)
	Indent = Indent + 1;
	For Each Decl In Declarations Do
		VisitDecl(Decl);
	EndDo;
	Indent = Indent - 1;
EndProcedure // VisitDeclarations()

Procedure VisitStatements(Statements)
	Indent = Indent + 1;
	For Each Stmt In Statements Do
		VisitStmt(Stmt);
	EndDo;
	Indent = Indent - 1;
	Indent();
EndProcedure // VisitStatements()

#Region VisitDecl

Procedure VisitDecl(Decl)
    Var Type;
    Type = Decl.Type;
	LastLine = Decl.Place.Line;
    If Type = Nodes.VarModListDecl Then
        VisitVarModListDecl(Decl);
    ElsIf Type = Nodes.VarLocListDecl Then
        VisitVarLocListDecl(Decl);
    ElsIf Type = Nodes.ProcDecl Then
        VisitProcDecl(Decl);
    ElsIf Type = Nodes.FuncDecl Then
        VisitFuncDecl(Decl);
    ElsIf Type = Nodes.PrepIfDecl Then
        VisitPrepIfDecl(Decl);
    ElsIf Type = Nodes.PrepElsIfDecl Then
        VisitPrepElsIfDecl(Decl);
    ElsIf Type = Nodes.PrepRegionDecl Then
        VisitPrepRegionDecl(Decl);
    EndIf;
EndProcedure // VisitDecl()

Procedure VisitVarModListDecl(VarModListDecl)
	Indent();
	Result.Add("Var ");
	VisitVarList(VarModListDecl.List);
	Result.Add(";"); Comment(VarModListDecl); Result.Add(Chars.LF);
EndProcedure // VisitVarModListDecl()

Procedure VisitVarLocListDecl(VarLocListDecl)
	Indent();
	Result.Add("Var ");
	VisitVarList(VarLocListDecl.List);
	Result.Add(";"); Comment(VarLocListDecl); Result.Add(Chars.LF);
EndProcedure // VisitVarLocListDecl()

Procedure VisitProcDecl(ProcDecl)
	Result.Add(Chars.LF);
	Result.Add("Procedure ");
	Result.Add(ProcDecl.Object.Name);
	VisitParams(ProcDecl.Object.Params);
	If ProcDecl.Object.Export Then
		Result.Add(" Export");
	EndIf;
	Comment(ProcDecl); Result.Add(Chars.LF);
	VisitDeclarations(ProcDecl.Decls);
    VisitStatements(ProcDecl.Body);
	Result.Add(StrTemplate("EndProcedure // %1()", ProcDecl.Object.Name));
	Result.Add(Chars.LF);
EndProcedure // VisitProcDecl()

Procedure VisitFuncDecl(FuncDecl)
	Result.Add(Chars.LF);
	Result.Add("Function ");
	Result.Add(FuncDecl.Object.Name);
	VisitParams(FuncDecl.Object.Params);
	If FuncDecl.Object.Export Then
		Result.Add(" Export");
	EndIf;
	Comment(FuncDecl); Result.Add(Chars.LF);
	VisitDeclarations(FuncDecl.Decls);
    VisitStatements(FuncDecl.Body);
	Result.Add(StrTemplate("EndFunction // %1()", FuncDecl.Object.Name));
	Result.Add(Chars.LF);
EndProcedure // VisitFuncDecl()

Procedure VisitPrepIfDecl(PrepIfDecl)
	Result.Add("#If ");
	VisitExpr(PrepIfDecl.Cond);
	Result.Add(" Then"); Comment(PrepIfDecl); Result.Add(Chars.LF);
	Indent = Indent - 1; // <<
	VisitDeclarations(PrepIfDecl.Then);
    If PrepIfDecl.ElsIf <> Undefined Then
		For Each PrepElsIfDecl In PrepIfDecl.ElsIf Do
            VisitPrepElsIfDecl(PrepElsIfDecl);
        EndDo;
    EndIf;
    If PrepIfDecl.Else <> Undefined Then
		Result.Add("#Else"); Result.Add(Chars.LF);
		VisitDeclarations(PrepIfDecl.Else);
	EndIf;
	Indent = Indent + 1; // >>
	Result.Add("#EndIf;"); Result.Add(Chars.LF);
EndProcedure // VisitPrepIfDecl()

Procedure VisitPrepElsIfDecl(PrepElsIfDecl)
	Result.Add("#ElsIf "); LastLine = PrepElsIfDecl.Cond.Place.Line;
	VisitExpr(PrepElsIfDecl.Cond);
	Result.Add(" Then"); Result.Add(Chars.LF);
	VisitDeclarations(PrepElsIfDecl.Then);
EndProcedure // VisitPrepElsIfDecl()

Procedure VisitPrepRegionDecl(PrepRegionDecl)
	Result.Add("#Region ");
	Result.Add(PrepRegionDecl.Name);
	Result.Add(Chars.LF);
	Indent = Indent - 1; // <<
	VisitDeclarations(PrepRegionDecl.Decls);
    VisitStatements(PrepRegionDecl.Body);
	Indent = Indent + 1; // >>
	Result.Add("#EndRegion // ");
	Result.Add(PrepRegionDecl.Name);
	Result.Add(Chars.LF);
	Result.Add(Chars.LF);
EndProcedure // VisitPrepRegionDecl()

#EndRegion // VisitDecl

#Region VisitExpr

Procedure VisitExpr(Expr)
    Var Type, Hook;
    Type = Expr.Type;
	For LastLine = LastLine To Expr.Place.Line - 1 Do
		If LastComment <> Undefined Then
			Result.Add(" //" + LastComment);
		EndIf;
		Result.Add(Chars.LF); Indent();
		LastComment = Comments[Expr.Place.Line];
	EndDo;
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
	LastLine = DesigExpr.Place.Line;
	VisitSelectors(DesigExpr.Select);
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
    If TypeOf(NewExpr.Constr) = Type("Structure") Then
		Result.Add("New ");
		VisitDesigExpr(NewExpr.Constr);
    Else
		Result.Add("New (");
		Indent = Indent + 1; // >>
		VisitExprList(NewExpr.Constr);
		Indent = Indent - 1; // <<
		If LastLine > NewExpr.Place.Line Then
			Result.Add(Chars.LF); Indent();
		EndIf;
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
	VisitSelectors(TernaryExpr.Select);
EndProcedure // VisitTernaryExpr()

Procedure VisitParenExpr(ParenExpr)
	Result.Add("(");
	Indent = Indent + 1; // >>
	VisitExpr(ParenExpr.Expr);
	Indent = Indent - 1; // <<
	If LastLine > ParenExpr.Place.Line Then
		Result.Add(Chars.LF); Indent();
	EndIf;
	Result.Add(")");
EndProcedure // VisitParenExpr()

Procedure VisitNotExpr(NotExpr)
	Result.Add("Not ");
	VisitExpr(NotExpr.Expr);
EndProcedure // VisitNotExpr()

Procedure VisitStringExpr(StringExpr)
	For Each Expr In StringExpr.List Do
		VisitExpr(Expr);
		Result.Add(" ");
	EndDo;
EndProcedure // VisitStringExpr()

#EndRegion // VisitExpr

#Region VisitStmt

Procedure VisitStmt(Stmt)
    Type = Stmt.Type;
	Indent();
	LastLine = Stmt.Place.Line;
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
    ElsIf Type = Nodes.PrepIfStmt Then
        VisitPrepIfStmt(Stmt);
    ElsIf Type = Nodes.WhileStmt Then
        VisitWhileStmt(Stmt);
    ElsIf Type = Nodes.PrepRegionStmt Then
        VisitPrepRegionStmt(Stmt);
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
    EndIf;
EndProcedure // VisitStmt()

Procedure VisitAssignStmt(AssignStmt)
    VisitDesigExpr(AssignStmt.Left);
	Result.Add(" = ");
	VisitExpr(AssignStmt.Right);
	Result.Add(";"); Comment(AssignStmt); Result.Add(Chars.LF);
EndProcedure // VisitAssignStmt()

Procedure VisitReturnStmt(ReturnStmt)
	Result.Add("Return ");
	If ReturnStmt.Expr <> Undefined Then
        VisitExpr(ReturnStmt.Expr);
	EndIf;
	Result.Add(";"); Comment(ReturnStmt); Result.Add(Chars.LF);
EndProcedure // VisitReturnStmt()

Procedure VisitBreakStmt(BreakStmt)
	Result.Add("Break;"); Comment(BreakStmt); Result.Add(Chars.LF);
EndProcedure // VisitBreakStmt()

Procedure VisitContinueStmt(ContinueStmt)
	Result.Add("Continue;"); Comment(ContinueStmt); Result.Add(Chars.LF);
EndProcedure // VisitContinueStmt()

Procedure VisitRaiseStmt(RaiseStmt)
	Result.Add("Raise ");
	If RaiseStmt.Expr <> Undefined Then
        VisitExpr(RaiseStmt.Expr);
	EndIf;
	Result.Add(";"); Comment(RaiseStmt); Result.Add(Chars.LF);
EndProcedure // VisitRaiseStmt()

Procedure VisitExecuteStmt(ExecuteStmt)
	Result.Add("Execute(");
	VisitExpr(ExecuteStmt.Expr);
	Result.Add(");"); Comment(ExecuteStmt); Result.Add(Chars.LF);
EndProcedure // VisitExecuteStmt()

Procedure VisitCallStmt(CallStmt)
    VisitDesigExpr(CallStmt.Desig);
	Result.Add(";"); Comment(CallStmt); Result.Add(Chars.LF);
EndProcedure // VisitCallStmt()

Procedure VisitIfStmt(IfStmt)
	Result.Add("If ");
	VisitExpr(IfStmt.Cond);
	Result.Add(" Then"); Comment(IfStmt); Result.Add(Chars.LF);
    VisitStatements(IfStmt.Then);
    If IfStmt.ElsIf <> Undefined Then
        For Each ElsIfStmt In IfStmt.ElsIf Do
			VisitElsIfStmt(ElsIfStmt);
        EndDo;
    EndIf;
    If IfStmt.Else <> Undefined Then
		Result.Add("Else"); Result.Add(Chars.LF);
		VisitStatements(IfStmt.Else);
	EndIf;
	Result.Add("EndIf;"); Result.Add(Chars.LF);
EndProcedure // VisitIfStmt()

Procedure VisitElsIfStmt(ElsIfStmt)
	Result.Add("ElsIf "); LastLine = ElsIfStmt.Cond.Place.Line;
	VisitExpr(ElsIfStmt.Cond);
	Result.Add(" Then"); Result.Add(Chars.LF);
    VisitStatements(ElsIfStmt.Then);
EndProcedure // VisitElsIfStmt()

Procedure VisitPrepIfStmt(PrepIfStmt)
	Result.Add("#If ");
	VisitExpr(PrepIfStmt.Cond);
	Result.Add(" Then"); Comment(PrepIfStmt); Result.Add(Chars.LF);
	VisitStatements(PrepIfStmt.Then);
    If PrepIfStmt.ElsIf <> Undefined Then
        For Each PrepElsIfStmt In PrepIfStmt.ElsIf Do
            VisitPrepElsIfStmt(PrepElsIfStmt);
        EndDo;
    EndIf;
    If PrepIfStmt.Else <> Undefined Then
		Result.Add("#Else"); Result.Add(Chars.LF);
		VisitStatements(PrepIfStmt.Else);
	EndIf;
	Result.Add("#EndIf;"); Result.Add(Chars.LF);
EndProcedure // VisitPrepIfStmt()

Procedure VisitPrepElsIfStmt(PrepElsIfStmt)
	Result.Add("#ElsIf "); LastLine = PrepElsIfStmt.Cond.Place.Line;
	VisitExpr(PrepElsIfStmt.Cond);
	Result.Add(" Then"); Result.Add(Chars.LF);
	VisitStatements(PrepElsIfStmt.Then);
EndProcedure // VisitPrepElsIfStmt()

Procedure VisitWhileStmt(WhileStmt)
	Result.Add("While ");
	VisitExpr(WhileStmt.Cond);
	Result.Add(" Do"); Comment(WhileStmt); Result.Add(Chars.LF);
    VisitStatements(WhileStmt.Body);
	Result.Add("EndDo;"); Result.Add(Chars.LF);
EndProcedure // VisitWhileStmt()

Procedure VisitPrepRegionStmt(PrepRegionStmt)
    VisitStatements(PrepRegionStmt.Body);
EndProcedure // VisitPrepRegionStmt()

Procedure VisitForStmt(ForStmt)
	Result.Add("For ");
	VisitDesigExpr(ForStmt.Desig);
	Result.Add(" = ");
	VisitExpr(ForStmt.From);
	Result.Add(" To ");
	VisitExpr(ForStmt.To);
	Result.Add(" Do"); Comment(ForStmt); Result.Add(Chars.LF);
	VisitStatements(ForStmt.Body);
	Result.Add("EndDo;"); Result.Add(Chars.LF);
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(ForEachStmt)
	Result.Add("For Each ");
	VisitDesigExpr(ForEachStmt.Desig);
	Result.Add(" In ");
	VisitExpr(ForEachStmt.In);
	Result.Add(" Do"); Comment(ForEachStmt); Result.Add(Chars.LF);
	VisitStatements(ForEachStmt.Body);
	Result.Add("EndDo;"); Result.Add(Chars.LF);
EndProcedure // VisitForEachStmt()

Procedure VisitTryStmt(TryStmt)
	Result.Add("Try"); Comment(TryStmt); Result.Add(Chars.LF);
	VisitStatements(TryStmt.Try);
	Result.Add("Except"); Result.Add(Chars.LF);
	VisitStatements(TryStmt.Except);
	Result.Add("EndTry;"); Result.Add(Chars.LF);
EndProcedure // VisitTryStmt()

Procedure VisitGotoStmt(GotoStmt)
	Result.Add(StrTemplate("Goto ~%1%2%3", GotoStmt.Label, ";", Chars.LF));
EndProcedure // VisitGotoStmt()

Procedure VisitLabelStmt(LabelStmt)
	Result.Add(StrTemplate("~%1:%2", LabelStmt.Label, Chars.LF));
EndProcedure // VisitLabelStmt()

#EndRegion // VisitStmt

#Region Aux

Procedure Indent()
	For Index = 1 To Indent Do
		Result.Add(Chars.Tab);
	EndDo;
EndProcedure // Indent()

Procedure Comment(Node)
	If Node = Undefined Then
		Return;
	EndIf;
	Comment = Comments[Node.Place.Line];
	If Comment <> Undefined Then
		Result.Add(" //" + Comment);
	EndIf;
EndProcedure // Comment()

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

Procedure VisitSelectors(Selectors)
	For Each Selector In Selectors Do
		If Selector.Kind = SelectorKinds.Ident Then
			Result.Add(".");
			Result.Add(Selector.Value);
		ElsIf Selector.Kind = SelectorKinds.Index Then
			Result.Add("[");
			VisitExprList(Selector.Value);
			Result.Add("]");
		ElsIf Selector.Kind = SelectorKinds.Call Then
			Result.Add("(");
			Indent = Indent + 1; // >>
			VisitExprList(Selector.Value);
			Indent = Indent - 1;
			If LastLine > Selector.Place.Line Then
				If LastComment <> Undefined Then
					Result.Add(" //" + LastComment);
					LastComment = Undefined;
				EndIf;
				Result.Add(Chars.LF); Indent();
			EndIf;
			Result.Add(")");
		Else
			Raise "Unknown selector kind";
		EndIf;
	EndDo;
EndProcedure // VisitSelectors()

#EndRegion // Aux