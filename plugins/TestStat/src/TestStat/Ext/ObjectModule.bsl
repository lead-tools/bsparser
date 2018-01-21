
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
	Interface.Add("VisitFuncDecl");
	Interface.Add("AfterVisitFuncDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitAssignStmt(AssignStmt, Stack, Counters) Export
	AssignCount = AssignCount + 1;
EndProcedure // VisitAssignStmt()

Procedure VisitFuncDecl(FuncDecl, Stack, Counters) Export
	AssignCount = 0;
EndProcedure // VisitFuncDecl()

Procedure AfterVisitFuncDecl(FuncDecl, Stack, Counters) Export
	Result.Add(StrTemplate("Функция `%1()` содержит %2 присваиваний", FuncDecl.Object.Name, AssignCount));
EndProcedure // AfterVisitFuncDecl()

