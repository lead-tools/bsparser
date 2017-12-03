
// Простая и эффективная проверка на наличие возврата из функции.
// Чаще всего возврат просто забывают написать.
// Нет никакого смысла разбирать сложные случаи,
// когда пропущен возврат в одной из логических ветвей,
// так как такие ошибки встречаются редко.
// Гораздо проще договориться с командой, что функция всегда
// должна оканчиваться инструкцией Возврат и автоматически
// проверять код подобным плагином.

Procedure Init(BSLParser) Export
		
EndProcedure // Init() 

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitFuncDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitFuncDecl(FuncDecl, Info) Export
	Var ReturnCount, Stmt;
	ReturnCount = 0;
	StmtCount = FuncDecl.Body.Count();
	If StmtCount = 0 Or FuncDecl.Body[StmtCount - 1].Type <> "ReturnStmt" Then
		Message(StrTemplate("Последней инструкцией функции `%1()` должен быть `Возврат`" "", FuncDecl.Object.Name));
	EndIf;
EndProcedure // VisitFuncDecl()