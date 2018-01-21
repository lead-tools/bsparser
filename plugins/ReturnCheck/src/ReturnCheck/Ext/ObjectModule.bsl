
// Простая и эффективная проверка на наличие возврата из функции.
// Чаще всего возврат просто забывают написать.
// Нет никакого смысла разбирать сложные случаи,
// когда пропущен возврат в одной из логических ветвей,
// так как такие ошибки встречаются редко.
// Гораздо проще договориться с командой, что функция всегда
// должна оканчиваться инструкцией Возврат и автоматически
// проверять код подобным плагином.

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
	Interface.Add("VisitFuncDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitFuncDecl(FuncDecl, Stack, Counters) Export
	Var StmtCount;
	StmtCount = FuncDecl.Body.Count();
	If StmtCount = 0 Or FuncDecl.Body[StmtCount - 1].Type <> "ReturnStmt" Then
		Result.Add(StrTemplate("Последней инструкцией функции `%1()` должен быть `Возврат`" "", FuncDecl.Object.Name));
	EndIf;
EndProcedure // VisitFuncDecl()