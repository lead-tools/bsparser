
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
	Interface.Add("VisitMethodDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitMethodDecl(MethodDecl, Stack, Counters) Export
	Var StmtCount;
	If MethodDecl.Sign.Type <> Nodes.FuncSign Then
		Return;
	EndIf; 
	StmtCount = MethodDecl.Body.Count();
	If StmtCount = 0 Or MethodDecl.Body[StmtCount - 1].Type <> "ReturnStmt" Then
		Result.Add(StrTemplate("Последней инструкцией функции `%1()` должен быть `Возврат`" "", MethodDecl.Sign.Name));
	EndIf;
EndProcedure // VisitMethodDecl()