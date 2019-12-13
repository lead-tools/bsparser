
// Тестовый плагин.

Var Nodes;
Var Result;

Procedure Init(BSParser) Export
	Nodes = BSParser.Nodes();
	Result = New Array;
EndProcedure // Init()

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Hooks() Export
	Var Hooks;
	Hooks = New Array;
	Hooks.Add("VisitWhileStmt");
	Hooks.Add("VisitForStmt");
	Hooks.Add("VisitForEachStmt");
	Return Hooks;
EndFunction // Hooks()

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
		Result.Add(StrTemplate("Вложенный цикл `%1` в строке %2", Stmt.Type, Stmt.Place.BegLine));
	EndIf;
EndProcedure // VisitLoopStmt()