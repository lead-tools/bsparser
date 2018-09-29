
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
Var Errors;
Var Result;

Var Vars, Params;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	Errors = BSLParser.Errors();
	Result = New Array;
	Vars = New Map;
	Params = New Map;
EndProcedure // Init() 

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Hooks() Export
	Var Hooks;
	Hooks = New Array;
	Hooks.Add("AfterVisitAssignStmt");
	Hooks.Add("VisitIdentExpr");
	Hooks.Add("VisitMethodDecl");
	Hooks.Add("AfterVisitMethodDecl");
	Return Hooks;
EndFunction // Hooks() 

Procedure AfterVisitAssignStmt(AssignStmt, Stack, Counters) Export
	Var Decl, Operation; 
	If AssignStmt.Left.Args <> Undefined Or AssignStmt.Left.Tail.Count() > 0 Then
		Return;
	EndIf;
	Decl = AssignStmt.Left.Head.Decl; 
	Operation = Vars[Decl];
	If Operation <> Undefined Then
		If Operation = "GetInLoop" Then
			Vars[Decl] = "Get";
		Else
			Vars[Decl] = "Set";
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
	Var Decl, Operation;
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
	Decl = IdentExpr.Head.Decl;
	If Vars[Decl] <> Undefined Then
		Vars[Decl] = Operation;
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
		Vars[VarLocDecl] = "Set";
	EndDo;
	For Each Item In MethodDecl.Auto Do
		Vars[Item.Decl] = "Set";
	EndDo;
EndProcedure // VisitMethodDecl()

Procedure AfterVisitMethodDecl(MethodDecl, Stack, Counters) Export
	Var Method, Error, Text;
	If MethodDecl.Sign.Type = Nodes.FuncSign Then
		Method = "Функция";
	Else
		Method = "Процедура";
	EndIf; 
	For Each Item In Vars Do
		If Not StrStartsWith(Item.Value, "Get") Then
			Text = StrTemplate("%1 `%2()` содержит неиспользуемую переменную `%3`", Method, MethodDecl.Sign.Name, Item.Key.Name);
			Result.Add(Text);
			Error = Errors.Add();
			Error.Text = Text;
			Error.Line = Item.Key.Place.BegLine;
			Error.Pos = Item.Key.Place.Pos;
		EndIf; 
	EndDo;
	For Each Item In Params Do
		If Item.Value = "Nil" Or Item.Value = "Set" And Item.Key.ByVal Then
			Text = StrTemplate("%1 `%2()` содержит неиспользуемый параметр `%3`", Method, MethodDecl.Sign.Name, Item.Key.Name);
			Result.Add(Text);
			Error = Errors.Add();
			Error.Text = Text;
			Error.Line = Item.Key.Place.BegLine;
			Error.Pos = Item.Key.Place.Pos;
		EndIf; 
	EndDo;
EndProcedure // AfterVisitMethodDecl()