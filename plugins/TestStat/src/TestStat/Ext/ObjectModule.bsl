
// Пример метода сбора (бесполезной) статистики.

Var Nodes;
Var Result;

Var AssignCount;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	Result = New Array;
	AssignCount = 0;
EndProcedure // Init() 

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitAssignStmt");
	Interface.Add("VisitMethodDecl");
	Interface.Add("AfterVisitMethodDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitAssignStmt(AssignStmt, Stack, Counters) Export
	AssignCount = AssignCount + 1;
EndProcedure // VisitAssignStmt()

Procedure VisitMethodDecl(MethodDecl, Stack, Counters) Export
	AssignCount = 0;
EndProcedure // VisitMethodDecl()

Procedure AfterVisitMethodDecl(MethodDecl, Stack, Counters) Export
	Result.Add(StrTemplate("Метод `%1()` содержит %2 присваиваний", MethodDecl.Sign.Name, AssignCount));
EndProcedure // AfterVisitMethodDecl()

