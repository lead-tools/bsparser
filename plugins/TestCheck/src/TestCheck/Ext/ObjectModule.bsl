
// Тестовый плагин.

Var Nodes;
Var Result;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	Result = New Array;
EndProcedure // Init() 

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitWhileStmt");
	Interface.Add("VisitForStmt");
	Interface.Add("VisitForEachStmt");
	Return Interface;
EndFunction // Interface() 

Procedure VisitWhileStmt(WhileStmt, Stack, Counters) Export
	VisitLoopStmt(WhileStmt, Stack, Counters);
EndProcedure // VisitWhileStmt()

Procedure VisitForStmt(ForStmt, Stack, Counters) Export
	VisitLoopStmt(ForStmt, Stack, Counters);
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(ForEachStmt, Stack, Counters) Export
	VisitLoopStmt(ForEachStmt, Stack, Counters);
EndProcedure // VisitForEachStmt()

// Использование информации из счетчика узлов.
Procedure VisitLoopStmt(Stmt, Stack, Counters) Export
	Var LoopCount;
	LoopCount = Counters.WhileStmt + Counters.ForStmt + Counters.ForEachStmt;
	If LoopCount > 0 Then
		Result.Add(StrTemplate("Вложенный цикл `%1` в строке %2", Stmt.Type, Stmt.Place.Line));
	EndIf;
EndProcedure // VisitLoopStmt()