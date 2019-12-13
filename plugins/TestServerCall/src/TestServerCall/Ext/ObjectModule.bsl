
// Рекурсивный подсчет серверных вызовов в модулях форм.
// Выводятся только случаи с количеством серверных вызовов > 1.

// Вызовы других модулей не учитываются.
// Вызовы типа Таблица.НайтиСтроки() не учитываются.

Var Nodes;
Var Directives;
Var Result;

Var Caller, CallerAtClient;
Var CallTable;
Var Methods;

Procedure Init(BSParser) Export
	Nodes = BSParser.Nodes();
	Directives = BSParser.Directives();
	Result = New Array;
	CallTable = New ValueTable;
	CallTable.Columns.Add("Caller");
	CallTable.Columns.Add("AtClient", New TypeDescription("Boolean"));
	CallTable.Columns.Add("Method");
	CallTable.Columns.Add("ServerCall", New TypeDescription("Number"));
	CallTable.Indexes.Add("Method, AtClient");
	Methods = New Map;
EndProcedure // Init()

Function Hooks() Export
	Var Hooks;
	Hooks = New Array;
	Hooks.Add("VisitMethodDecl");
	Hooks.Add("VisitIdentExpr");
	Return Hooks;
EndFunction // Hooks()

Procedure VisitMethodDecl(MethodDecl, Stack, Counters) Export
	Caller = MethodDecl.Sign;
	CallerAtClient = (Caller.Directive = Directives.AtClient);
EndProcedure // VisitMethodDecl()

Procedure VisitIdentExpr(IdentExpr, Stack, Counters) Export
	Var CallRow, Method;
	If IdentExpr.Args <> Undefined And IdentExpr.Tail.Count() = 0 Then // только простые вызовы методов данного модуля
		Method = IdentExpr.Head.Decl;
		If Method <> Undefined Then // только известные методы
			CallRow = CallTable.Add();
			Callrow.Caller = Caller;
			Callrow.AtClient = CallerAtClient;
			CallRow.Method = Method;
			Methods[Method] = True;
		EndIf;
	EndIf;
EndProcedure // VisitIdentExpr()

Function Result() Export
	For Each Method In Methods Do
		If Method.Key.Directive = Directives.AtServer Then
			CountServerCalls(Method.Key);
		EndIf;
	EndDo;
	CallTable.GroupBy("Caller", "ServerCall");
	For Each Row In CallTable Do
		If Row.ServerCall > 1 Then
			Result.Add(StrTemplate("Метод `%1()` содержит %2 серверных вызовов", Row.Caller.Name, Row.ServerCall));
		EndIf;
	EndDo;
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function CountServerCalls(Method)
	Var CallsAtClient;
	CallsAtClient = CallTable.FindRows(New Structure("Method, AtClient", Method, True));
	For Each CallRow In CallsAtClient Do
		CallRow.ServerCall = CallRow.ServerCall + 1;
		CountServerCalls(CallRow.Caller)
	EndDo;
EndFunction // CountServerCalls()