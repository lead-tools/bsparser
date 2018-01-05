
// Простая проверка на наличие авто-переменных в процедурах и функциях.
// Возможны ложные срабатывания, т.к. не учитывается контекст выполнения.

Var Nodes;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();	
EndProcedure // Init() 

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitFuncDecl");
	Interface.Add("VisitProcDecl");
	Return Interface;
EndFunction // Interface() 

Procedure VisitFuncDecl(FuncDecl, Stack, Count) Export
	Var Item;
	If FuncDecl.Auto.Count() > 0 Then
		Message(StrTemplate("Функция `%1()` содержит авто-переменные:", FuncDecl.Object.Name));
		For Each Item In FuncDecl.Auto Do
			Message(Chars.Tab + Item.Name);
		EndDo; 
	EndIf; 
EndProcedure // VisitFuncDecl()

Procedure VisitProcDecl(ProcDecl, Stack, Count) Export
	Var Item;
	If ProcDecl.Auto.Count() > 0 Then
		Message(StrTemplate("Процедура `%1()` содержит авто-переменные:", ProcDecl.Object.Name));
		For Each Item In ProcDecl.Auto Do
			Message(Chars.Tab + Item.Name);
		EndDo; 
	EndIf;
EndProcedure // VisitProcDecl()