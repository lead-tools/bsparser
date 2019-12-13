
// Проверка комментариев в окончаниях инструкций

Var Nodes;
Var Source;
Var Result;
Var Comments;
Var RegionLevel;
Var RegionStack;

Procedure Init(BSParser) Export
	Nodes = BSParser.Nodes();
	Source = BSParser.Source();
	Result = New Array;
	RegionLevel = 0;
	RegionStack = New Map;
EndProcedure // Init()

Function Result() Export
	Return StrConcat(Result, Chars.LF);
EndFunction // Result()

Function Hooks() Export
	Var Hooks;
	Hooks = New Array;
	Hooks.Add("VisitModule");
	Hooks.Add("VisitMethodDecl");
	Hooks.Add("VisitPrepInst");
	Return Hooks;
EndFunction // Hooks()

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
			Result.Add(StrTemplate("Область `%1` имеет неправильный замыкающий комментарий в строке %2:", RegionName, PrepInst.Place.BegLine));
			Result.Add(StrTemplate("%1`%2%3`", Chars.Tab, Mid(Source, PrepInst.Place.Pos, PrepInst.Place.Len), Comment));
		EndIf;
		RegionLevel = RegionLevel - 1;
	EndIf;
EndProcedure // VisitPrepInst()
