
Var Result;

Procedure Init(OneShellProcessor) Export
	Result = New Array;	
EndProcedure // Init() 

Function VisitModule(Module) Export
	VisitDecls(Module.Decls);
	Return StrConcat(Result);
EndFunction // VisitModule()

Procedure VisitDecls(Decls)
	For Each Decl In Decls Do
		VisitDecl(Decl);
	EndDo;
EndProcedure // VisitDecls()

Procedure VisitDecl(Decl)
	Var ReturnCount, Stmt;
	ReturnCount = 0;
	If Decl.Type = "FuncDecl" Then
		Stmt = VisitStatements(Decl.Body, ReturnCount);
		If Stmt = Undefined
			Or Stmt.Type <> "ReturnStmt"
			Or ReturnCount <> 1 Then
			Result.Add(StrTemplate("Функция %1() должна иметь один Возврат в конце" "", Decl.Object.Name));
		EndIf;
	ElsIf Decl.Type = "PrepRegionDecl" Then
		VisitDecls(Decl.Decls);
	EndIf;
EndProcedure // VisitDecl()

Function VisitStatements(Statements, ReturnCount)
	Var Stmt;
	For Each Stmt In Statements Do
		VisitStmt(Stmt, ReturnCount);
	EndDo;
	Return Stmt;
EndFunction // VisitStatements()

Procedure VisitStmt(Stmt, ReturnCount)
	NodeType = Stmt.Type;
	If NodeType = "ReturnStmt" Then
		ReturnCount = ReturnCount + 1;
	ElsIf NodeType = "IfStmt" Then
		VisitStatements(Stmt.Then, ReturnCount);
		If Stmt.ElsIf <> Undefined Then
			VisitStatements(Stmt.ElsIf, ReturnCount)
		EndIf;
		If Stmt.Else <> Undefined Then
			VisitStatements(Stmt.Else, ReturnCount);
		EndIf;
	ElsIf NodeType = "WhileStmt" Then
		VisitStatements(Stmt.Body, ReturnCount);
	ElsIf NodeType = "ForStmt" Then
		VisitStatements(Stmt.Body, ReturnCount);
	ElsIf NodeType = "TryStmt" Then
		VisitStatements(Stmt.Try, ReturnCount);
		VisitStatements(Stmt.Except, ReturnCount);
	EndIf;
EndProcedure // VisitStmt()
