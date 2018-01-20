
// Тестовый плагин.

Var Nodes;
Var Result;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	If Not BSLParser.Location Then
		Raise "Необходимо включить флаг Location";
	EndIf;
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

Procedure VisitWhileStmt(WhileStmt, Stack, Count) Export
	VisitLoopStmt(WhileStmt, Stack, Count);
EndProcedure // VisitWhileStmt()

Procedure VisitForStmt(ForStmt, Stack, Count) Export
	VisitLoopStmt(ForStmt, Stack, Count);
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(ForEachStmt, Stack, Count) Export
	VisitLoopStmt(ForEachStmt, Stack, Count);
EndProcedure // VisitForEachStmt()

// Использование информации из счетчика узлов.
Procedure VisitLoopStmt(Stmt, Stack, Count) Export
	Var LoopCount;
	LoopCount = Count.WhileStmt + Count.ForStmt + Count.ForEachStmt;
	If LoopCount > 0 Then
		Result.Add(StrTemplate("Вложенный цикл `%1` в строке %2", Stmt.Type, Stmt.Place.Line));
	EndIf;
EndProcedure // VisitLoopStmt()