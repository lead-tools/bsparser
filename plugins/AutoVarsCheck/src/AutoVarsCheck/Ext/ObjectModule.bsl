
// Простая проверка на наличие авто-переменных в методах.
// Возможны ложные срабатывания, т.к. не учитывается контекст выполнения.

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
	Hooks.Add("VisitMethodDecl");
	Return Hooks;
EndFunction // Hooks()

Procedure VisitMethodDecl(MethodDecl, Stack, Counters) Export
	Var AutoDecl;
	If MethodDecl.Auto.Count() > 0 Then
		Result.Add(StrTemplate("Метод `%1()` содержит авто-переменные:", MethodDecl.Sign.Name));
		For Each AutoDecl In MethodDecl.Auto Do
			Result.Add(Chars.Tab + AutoDecl.Name);
		EndDo;
	EndIf;
EndProcedure // VisitMethodDecl()
