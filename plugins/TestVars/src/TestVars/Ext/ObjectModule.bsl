
// Плагин для проверки использования переменных и параметров.
// Отслеживаются следующие ситуации:
// - значение переменной не читается после присваивания (объявление тоже считается присваиванием)
// - значение параметра-значения не читается после присваивания
// - к параметру-ссылке нет обращений
//
// примечания:
// Анализ в целом выполняется поверхностно и возможны ложные срабатывания.

// todo: проверять два присваивания одной переменной подряд

Var Nodes;
Var Result;

Var Vars, Params;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	Result = New Array;
	Vars = New Map;
	Params = New Map;
EndProcedure // Init() 

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("AfterVisitAssignStmt");
	Interface.Add("VisitIdentExpr");
	Interface.Add("VisitMethodDecl");
	Interface.Add("AfterVisitMethodDecl");
	Return Interface;
EndFunction // Interface() 

Procedure AfterVisitAssignStmt(AssignStmt, Stack, Counters) Export
	Var Name, Decl, Operation; 
	If AssignStmt.Left.Args <> Undefined Or AssignStmt.Left.Tail.Count() > 0 Then
		Return;
	EndIf;
	Name = AssignStmt.Left.Head.Name; 
	Operation = Vars[Name];
	If Operation <> Undefined Then
		If Operation = "GetInLoop" Then
			Vars[Name] = "Get";
		Else
			Vars[Name] = "Set";
		EndIf;
		Return;
	EndIf; 	
	Decl = AssignStmt.Left.Head.Decl;
	Operation = Params[Decl];	
	If Operation <> Undefined Then
		If Operation = "GetInLoop" Then
			Params[Decl] = "Get";
		Else
			Params[Decl] = "Set";
		EndIf; 
	EndIf; 
EndProcedure // AfterVisitAssignStmt()

Procedure VisitIdentExpr(IdentExpr, Stack, Counters) Export
	Var Name, Decl, Operation;
	If IdentExpr.Tail.Count() = 0
		And Stack.Parent.Type = Nodes.AssignStmt
		And Stack.Parent.Left = IdentExpr Then
		Return;
	EndIf;
	If Counters.WhileStmt + Counters.ForStmt + Counters.ForEachStmt > 0 Then
		Operation = "GetInLoop";
	Else
		Operation = "Get";
	EndIf; 
	Name = IdentExpr.Head.Name;
	Decl = IdentExpr.Head.Decl;
	If Vars[Name] <> Undefined Then
		Vars[Name] = Operation;
	ElsIf Params[Decl] <> Undefined Then
		Params[Decl] = Operation;	
	EndIf; 
EndProcedure // VisitIdentExpr()

Procedure VisitMethodDecl(MethodDecl, Stack, Counters) Export
	Vars = New Map;
	Params = New Map;		
	For Each Param In MethodDecl.Sign.Params Do
		Params[Param] = "Get";
		//Params[Param] = "Nil"; <- чтобы чекать все параметры (в формах адъ)
	EndDo;
	For Each VarLocDecl In MethodDecl.Vars Do
		Vars[VarLocDecl.Name] = "Set";
	EndDo;
	For Each Object In MethodDecl.Auto Do
		Vars[Object.Name] = "Set";
	EndDo;
EndProcedure // VisitMethodDecl()

Procedure AfterVisitMethodDecl(MethodDecl, Stack, Counters) Export
	Var Method;
	If MethodDecl.Sign.Type = Nodes.FuncSign Then
		Method = "Функция";
	Else
		Method = "Процедура";
	EndIf; 
	For Each Item In Vars Do
		If Not StrStartsWith(Item.Value, "Get") Then
			Result.Add(StrTemplate("%1 `%2()` содержит неиспользуемую переменную `%3`", Method, MethodDecl.Sign.Name, Item.Key));
		EndIf; 
	EndDo;
	For Each Item In Params Do
		If Item.Value = "Nil" Or Item.Value = "Set" And Item.Key.ByVal Then
			Result.Add(StrTemplate("%1 `%2()` содержит неиспользуемый параметр `%3`", Method, MethodDecl.Sign.Name, Item.Key.Name));
		EndIf; 
	EndDo;
EndProcedure // AfterVisitMethodDecl()