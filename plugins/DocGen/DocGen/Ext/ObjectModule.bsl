
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
		|<link rel='stylesheet' type='text/css' href='ast.css'>
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
					
					Result.Add(StrTemplate(
						"	<h3 id='%1'>%1</h3>
						|	<ul>" "",
						NodeName
					));
					
					For Each Field In NodeFields Do
						TypeList = ParseTypes(Comments[Field.Place.Line]);
						Result.Add(StrTemplate(
							"		<li><strong>%1</strong>: %2</li>" "",
							Field.Value,
							GenerateTypeLinks(TypeList)
						));
					EndDo; 
					
					Result.Add("	</ul>" "");
					
				EndIf; 
			EndIf; 
		
		EndIf; 
		
	EndIf; 
	
EndProcedure // VisitDesigExpr()

Function GenerateTypeLinks(TypeList)
	Var Buffer;	
	Buffer = New Array;
	For Each Item In TypeList Do
		If Item.Child = Undefined Then
			If Lower(Item.Ident) = Item.Ident Then
				Buffer.Add(Item.Ident);
			Else
				Buffer.Add(StrTemplate(
					"<a href='#%1'>%1</a>",
					Item.Ident
				));
			EndIf; 
		ElsIf TypeOf(Item.Child) = Type("String") Then
			Buffer.Add(StrTemplate(
				"%1 <a href='#%2'>%2</a>",
				Item.Ident,
				Item.Child
			));
		Else
			Buffer.Add(StrTemplate(
				"%1 (%2)",
				Item.Ident,
				GenerateTypeLinks(Item.Child)
			));
		EndIf; 
	EndDo; 
	Return StrConcat(Buffer, ", ");	
EndFunction // GenerateTypeLinks() 

Function ParseTypes(Types)
	Var Pos, Ident, List;
	Pos = 1; List = New Array;
	~Scan:
	Child = Undefined;
	SkipSpace(Types, Pos); 
	Ident = ScanIdent(Types, Pos); 
	SkipSpace(Types, Pos);
	If Ident = "one" Then
		If Mid(Types, Pos, 2) <> "of" Then
			Raise "error";
		EndIf; 
		Pos = Pos + 2;
		Ident = "one of";
		SkipSpace(Types, Pos);
		Child = ScanIdent(Types, Pos);
	ElsIf Mid(Types, Pos, 1) = "(" Then
		Pos = Pos + 1;
		Beg = Pos;
		SkipUntil(Types, Pos, ")");
		Child = ParseTypes(Mid(Types, Beg, Pos - Beg));
		Pos = Pos + 1;
	EndIf;
	List.Add(New Structure("Ident, Child", Ident, Child));
	If Mid(Types, Pos, 1) = "," Then
		Pos = Pos + 1;
		Goto ~Scan;
	EndIf;
	Return List;
EndFunction // ParseType()

Procedure SkipSpace(Str, Pos)
	For Pos = Pos To StrLen(Str) Do
		If Not IsBlankString(Mid(Str, Pos, 1)) Then
			Break;
		EndIf; 
	EndDo;
EndProcedure // SkipSpace()

Procedure SkipUntil(Str, Pos, Chr)
	For Pos = Pos To StrLen(Str) Do
		If Mid(Str, Pos, 1) = Chr Then
			Break;
		EndIf;
	EndDo;
EndProcedure // SkipUntil()

Function ScanIdent(Str, Pos)
	Var Beg, Chr;
	Beg = Pos;
	For Pos = Pos To StrLen(Str) Do
		Chr = Mid(Str, Pos, 1);
		If IsBlankString(Chr) Or Chr = "," Then
			Break;
		EndIf;
	EndDo;
	Return Mid(Str, Beg, Pos - Beg);
EndFunction // ScanIdent() 