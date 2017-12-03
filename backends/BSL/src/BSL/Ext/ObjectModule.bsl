
Var Result; // array (string)
Var Indent; // number

Var Nodes;         // enum
Var Tokens;        // enum
Var SelectorKinds; // enum
Var Operators;     // structure as map[one of Tokens](string)

Var Line;
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

	Result = New Array;
	Indent = -1;

EndProcedure // Init()

Procedure Indent(Result)
	For Index = 1 To Indent Do
		Result.Add(Chars.Tab);
	EndDo;
EndProcedure // Indent()

Function VisitModule(Module) Export
	Comments = Module.Comments;
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
	If NodeType = Nodes.VarLocListDecl Or NodeType = Nodes.VarModListDecl Then
		Indent(Result);
		Result.Add("Var ");
		VisitVarList(Decl.List);
		Result.Add(";"); Comment(Result, Decl); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.FuncDecl Or NodeType = Nodes.ProcDecl Then
		Result.Add(Chars.LF);
		Indent = Indent + 1;
		If NodeType = Nodes.FuncDecl Then
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
		Comment(Result, Decl);
		Result.Add(Chars.LF);
		For Each Stmt In Decl.Decls Do
			VisitDecl(Stmt);
		EndDo;
		For Each Stmt In Decl.Body Do
			VisitStmt(Stmt);
		EndDo;
		If NodeType = Nodes.FuncDecl Then
			Result.Add(StrTemplate("EndFunction // %1()", Decl.Object.Name));
		Else
			Result.Add(StrTemplate("EndProcedure // %1()", Decl.Object.Name));
		EndIf;
		Result.Add(Chars.LF);
		Indent = Indent - 1;
	ElsIf NodeType = Nodes.PrepRegionDecl Then
		Result.Add("#Region ");
		Result.Add(Decl.Name);
		Result.Add(Chars.LF);
		Indent = Indent - 1;
		VisitDecls(Decl.Decls);
		VisitStatements(Decl.Body);
		Indent = Indent + 1;
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
	Line = Stmt.Place.Line;
	Indent(Result);
	If NodeType = Nodes.AssignStmt Then
		Result.Add(VisitDesigExpr(Stmt.Left));
		Result.Add(" = ");
		Result.Add(VisitExpr(Stmt.Right));
		Result.Add(";"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.ReturnStmt Then
		Result.Add("Return ");
		If Stmt.Property("Expr") Then
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(";"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.BreakStmt Then
		Result.Add("Break;"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.ContinueStmt Then
		Result.Add("Continue;"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.RaiseStmt Then
		Result.Add("Raise ");
		If Stmt.Property("Expr") Then
			Result.Add(VisitExpr(Stmt.Expr));
		EndIf;
		Result.Add(";"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.ExecuteStmt Then
		Result.Add("Execute(");
		Result.Add(VisitExpr(Stmt.Expr));
		Result.Add(");"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.CallStmt Then
		Result.Add(VisitDesigExpr(Stmt.Desig));
		Result.Add(";"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.IfStmt Then
		Result.Add("If ");
		Result.Add(VisitExpr(Stmt.Cond));
		Result.Add(" Then"); Comment(Result, Stmt); Result.Add(Chars.LF);
		VisitStatements(Stmt.Then);
		If Stmt.ElsIf <> Undefined Then
			For Each Item In Stmt.ElsIf Do
				Result.Add("ElsIf ");
				Result.Add(VisitExpr(Item.Cond));
				Result.Add(" Then"); Comment(Result, Stmt); Result.Add(Chars.LF);
				VisitStatements(Item.Then);
			EndDo;
		EndIf;
		If Stmt.Else <> Undefined Then
			Result.Add("Else"); Comment(Result, Stmt); Result.Add(Chars.LF); 
			VisitStatements(Stmt.Else);
		EndIf;
		Result.Add("EndIf;"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.WhileStmt Then
		Result.Add("While ");
		Result.Add(VisitExpr(Stmt.Cond));
		Result.Add(" Do"); Comment(Result, Stmt); Result.Add(Chars.LF);
		VisitStatements(Stmt.Body);
		Result.Add("EndDo;"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.ForStmt Then
		Result.Add("For ");
		Result.Add(VisitDesigExpr(Stmt.Desig));
		Result.Add(" = ");
		Result.Add(VisitExpr(Stmt.From));
		Result.Add(" To ");
		Result.Add(VisitExpr(Stmt.To));
		Result.Add(" Do"); Comment(Result, Stmt); Result.Add(Chars.LF);
		VisitStatements(Stmt.Body);
		Result.Add("EndDo;"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.ForEachStmt Then
		Result.Add("For Each ");
		Result.Add(VisitDesigExpr(Stmt.Desig));
		Result.Add(" In ");
		Result.Add(VisitExpr(Stmt.In));
		Result.Add(" Do"); Comment(Result, Stmt); Result.Add(Chars.LF);
		VisitStatements(Stmt.Body);
		Result.Add("EndDo;"); Comment(Result, Stmt); Result.Add(Chars.LF);
	ElsIf NodeType = Nodes.TryStmt Then
		Result.Add("Try"); Comment(Result, Stmt); Result.Add(Chars.LF);
		VisitStatements(Stmt.TryPart);
		Result.Add("Except"); Comment(Result, Stmt); Result.Add(Chars.LF);
		VisitStatements(Stmt.ExceptPart);
		Result.Add("EndTry;"); Comment(Result, Stmt); Result.Add(Chars.LF);
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
	If NodeType = Nodes.BasicLitExpr Then
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
	ElsIf NodeType = Nodes.DesigExpr Then
		Return VisitDesigExpr(Expr);
	ElsIf NodeType = Nodes.UnaryExpr Then
		Return StrTemplate("%1 %2", Operators[Expr.Operator], VisitExpr(Expr.Operand));
	ElsIf NodeType = Nodes.BinaryExpr Then
		Return StrTemplate("%1 %2 %3", VisitExpr(Expr.Left), Operators[Expr.Operator], VisitExpr(Expr.Right));
	ElsIf NodeType = Nodes.NewExpr Then
		If TypeOf(Expr.Constr) = Type("Structure") Then
			Return StrTemplate("New %1", VisitExpr(Expr.Constr));
		Else
			Return StrTemplate("New(%1)", VisitExprList(Expr.Constr));
		EndIf;
	ElsIf NodeType = Nodes.TernaryExpr Then
		Return StrTemplate("?(%1, %2, %3)", VisitExpr(Expr.Cond), VisitExpr(Expr.Then), VisitExpr(Expr.Else));
	ElsIf NodeType = Nodes.ParenExpr Then
		Return StrTemplate("(%1)", VisitExpr(Expr.Expr));
	ElsIf NodeType = Nodes.NotExpr Then
		Return StrTemplate("Not %1", VisitExpr(Expr.Expr));
	ElsIf NodeType = Nodes.StringExpr Then
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
	Var Buffer, Index, PrevItem;
	Buffer = New Array;
	If StringExpr.List.Count() > 0 Then
		
		Item = StringExpr.List[0];
		
		If Item.Place.Line <> Line Then
			Comment(Buffer, PrevItem);
			Buffer.Add(Chars.LF);
			Indent(Buffer);
		EndIf;
		Buffer.Add(VisitExpr(Item));
		Line = Item.Place.Line;
		PrevItem = Item;
		
		For Index = 1 To StringExpr.List.Count() - 1 Do
			
			Item = StringExpr.List[Index];
			
			If Item.Place.Line = Line Then
				Buffer.Add(" ");
				Buffer.Add(VisitExpr(Item));
			Else
				Comment(Buffer, PrevItem);
				Buffer.Add(Chars.LF);
				Indent(Buffer);
				Buffer.Add(VisitExpr(Item));
				Line = Item.Place.Line;
			EndIf;
			
			PrevItem = Item;
			
		EndDo; 
	
	EndIf; 
	
	Return StrConcat(Buffer, " ");
EndFunction // VisitStringExpr()

Procedure Comment(Result, Node)
	If Node = Undefined Then
		Return;
	EndIf; 
	Comment = Comments[Node.Place.Line];
	If Comment <> Undefined Then
		Result.Add(" //" + Comment);
	EndIf;
EndProcedure 