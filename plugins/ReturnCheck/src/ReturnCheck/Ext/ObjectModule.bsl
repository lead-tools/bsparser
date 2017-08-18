
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
	Var ReturnExists;
	ReturnExists = False;
	If Decl.NodeType = "FuncDecl" Then
		VisitStatements(Decl.Statements, ReturnExists);
		If Not ReturnExists Then
			Result.Add(StrTemplate("Функция %1() должна возвращать значение", Decl.Object.Name));
		EndIf; 
	EndIf;
EndProcedure // VisitDecl()

Procedure VisitStatements(Statements, ReturnExists)
	For Each Stmt In Statements Do
		VisitStmt(Stmt, ReturnExists);
	EndDo;
EndProcedure // VisitStatements()

Procedure VisitStmt(Stmt, ReturnExists)
	If ReturnExists Then
		Return;
	EndIf; 
	NodeType = Stmt.NodeType;
	If NodeType = "ReturnStmt" Then
		ReturnExists = True;
	ElsIf NodeType = "IfStmt" Then
		VisitStatements(Stmt.ThenPart, ReturnExists);
		If Stmt.Property("ElsIfPart") Then
			VisitStatements(Stmt.ElsIfPart, ReturnExists)
		EndIf;
		If Stmt.Property("ElsePart") Then
			VisitStatements(Stmt.ElsePart, ReturnExists);
		EndIf;
	ElsIf NodeType = "WhileStmt" Then
		VisitStatements(Stmt.Statements, ReturnExists);
	ElsIf NodeType = "ForStmt" Then
		VisitStatements(Stmt.Statements, ReturnExists);
	ElsIf NodeType = "TryStmt" Then
		VisitStatements(Stmt.TryPart, ReturnExists);
		VisitStatements(Stmt.ExceptPart, ReturnExists);
	EndIf;
EndProcedure // VisitStmt()
