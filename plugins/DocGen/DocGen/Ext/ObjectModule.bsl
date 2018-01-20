
// ...

Var Nodes;
Var Region, SubRegion;
Var Comments; 
Var Result;

Procedure Init(BSLParser) Export
	Nodes = BSLParser.Nodes();
	Result = New Array;
	Result.Add(
		"<!DOCTYPE html>
		|<html>
		|<head>
		|<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
		|<title>BSL-Parser</title>
		|<style>
		|	body {
		|		font-family: Menlo, monospace;
		|		font-size: 14px;
		|	}
		|</style>
		|</head>
		|<body>
		|	<h1>Abstract syntax tree</h1>" ""
	);
EndProcedure // Init() 

Function Result() Export
	Result.Add(
		"</body>
		|</html>"
	);
	Return StrConcat(Result);
EndFunction // Refult() 

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitModule");
	Interface.Add("VisitPrepRegionDecl");
	Interface.Add("VisitDesigExpr");
	Return Interface;
EndFunction // Interface() 

Procedure VisitModule(Module, Stack, Count) Export
	Comments = Module.Comments;
EndProcedure // VisitModule 

Procedure VisitPrepRegionDecl(PrepRegionDecl, Stack, Count) Export	
	If Count.PrepRegionDecl = 0 Then
		Region = PrepRegionDecl.Name;
		SubRegion = "";
	ElsIf Count.PrepRegionDecl = 1 Then
		SubRegion = PrepRegionDecl.Name;
	EndIf; 	
EndProcedure // VisitPrepRegionDecl()

Procedure VisitDesigExpr(DesigExpr, Stack, Count) Export
	
	If Region = "AbstractSyntaxTree" Then
		
		If DesigExpr.Call Then
			
			If DesigExpr.Object.Name = "Struct" Then
				CallSelector = DesigExpr.Select[0]; 
				FirstArg = CallSelector.Value[0];
				If FirstArg.Object.Name = "Nodes" Then
					NodeName = FirstArg.Select[0].Value;
					NodeFields = CallSelector.Value[1].List;
					
					//Message(NodeName + ":");
					Result.Add(StrTemplate(
						"		<h3 id='%1'>%1</h3>
						|		<ul>" "",
						NodeName
					));
					
					For Each Field In NodeFields Do
						//Message(Chars.Tab + Field.Value + ": " + Comments[Field.Place.Line]);
						Result.Add(StrTemplate(
							"				<li>%1: %2</li>" "",
							Field.Value,
							Comments[Field.Place.Line]
						));
					EndDo; 
					
					Result.Add("		</ul>" "");
					
				EndIf; 
			EndIf; 
		
		EndIf; 
		
	EndIf; 
	
EndProcedure // VisitDesigExpr()