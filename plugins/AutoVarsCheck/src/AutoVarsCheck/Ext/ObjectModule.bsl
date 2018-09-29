
// Простая проверка на наличие авто-переменных в методах.
// Возможны ложные срабатывания, т.к. не учитывается контекст выполнения.

Var Nodes;
Var Result;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
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
	Var Item;
	If MethodDecl.Auto.Count() > 0 Then
		Result.Add(StrTemplate("Метод `%1()` содержит авто-переменные:", MethodDecl.Sign.Name));
		For Each Item In MethodDecl.Auto Do
			Result.Add(Chars.Tab + Item.Name);
		EndDo; 
	EndIf; 
EndProcedure // VisitMethodDecl()
