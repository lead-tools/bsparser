
Procedure Init(BSLParser) Export
		
EndProcedure // Init() 

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitFuncDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitFuncDecl(FuncDecl) Export
	Var ReturnCount, Stmt;
	ReturnCount = 0;
	Stmt = VisitStatements(FuncDecl.Body, ReturnCount);
	If Stmt = Undefined
		Or Stmt.Type <> "ReturnStmt"
		Or ReturnCount <> 1 Then
		Message(StrTemplate("Функция %1() должна иметь один Возврат в конце" "", FuncDecl.Object.Name));
	EndIf;
EndProcedure // VisitFuncDecl()

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
