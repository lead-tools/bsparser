
// Проверка комментариев в окончаниях инструкций

Var Nodes;
Var Result;
Var Comments;
Var RegionLevel;
Var RegionStack;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	Result = New Array;
	RegionLevel = 0;
	RegionStack = New Map;
EndProcedure // Init() 

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitModule");
	Interface.Add("VisitMethodDecl");
	Interface.Add("VisitPrepInst");
	Return Interface;
EndFunction // Interface() 

Procedure VisitModule(Module, Stack, Counters) Export
	Comments = Module.Comments;	
EndProcedure // VisitModule()

Procedure VisitMethodDecl(MethodDecl, Stack, Counters) Export 
	Comment = Comments[MethodDecl.Place.EndLine];
	If Comment <> Undefined And TrimR(Comment) <> StrTemplate(" %1%2", MethodDecl.Sign.Name, "()") Then
		Result.Add(StrTemplate("Метод `%1()` имеет неправильный замыкающий комментарий в строке %2", MethodDecl.Sign.Name, MethodDecl.Place.EndLine));
	EndIf; 
EndProcedure // VisitMethodDecl()

Procedure VisitPrepInst(PrepInst, Stack, Counters) Export
	If PrepInst.Type = Nodes.PrepRegionInst Then
		RegionLevel = RegionLevel + 1;
		RegionStack[RegionLevel] = PrepInst.Name;
	ElsIf PrepInst.Type = Nodes.PrepEndRegionInst Then
		Comment = Comments[PrepInst.Place.BegLine];
		RegionName = RegionStack[RegionLevel];
		If Comment <> Undefined And TrimR(Comment) <> StrTemplate(" %1", RegionName) Then
			Result.Add(StrTemplate("Область `%1` имеет неправильный замыкающий комментарий в строке %2", RegionName, PrepInst.Place.BegLine));
		EndIf;
		RegionLevel = RegionLevel - 1;
	EndIf; 
EndProcedure // VisitPrepInst()
