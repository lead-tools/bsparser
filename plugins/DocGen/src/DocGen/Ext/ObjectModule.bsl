﻿
// Генератор технической документации по парсеру

Var Tokens, Nodes, SelectKinds, Directives, PrepInstructions;
Var Region, SubRegion;
Var Comments;
Var Result;

Procedure Init(BSLParser) Export
	Tokens = BSLParser.Tokens();
	Nodes = BSLParser.Nodes();
	SelectKinds = BSLParser.SelectKinds();
	Directives = BSLParser.Directives();
	PrepInstructions = BSLParser.PrepInstructions();
	Result = New Array;
	Result.Add(
		"<!DOCTYPE html>
		|<html>
		|<head>
		|<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
		|<title>BSL-Parser</title>
		|<link rel='stylesheet' type='text/css' href='ast.css'>
		|</head>
		|<body>" ""
	);
EndProcedure // Init()

Function Result() Export
	Result.Add(
		"<h2 id='#Other'>#Other</h2>
		|<h3 id='Place'>Place</h3>
		|<ul>
		|	<li><strong>Line</strong>: number</li>
		|	<li><strong>Pos</strong>: number</li>
		|	<li><strong>Len</strong>: number</li>
		|</ul>
		|</body>
		|</html>"
	);
	Result.Add("<h2 id='#Enums'>#Enums</h2>");
	Result.Add(GenerateEnum("SelectKinds", SelectKinds));
	Result.Add(GenerateEnum("Directives", Directives));
	Result.Add(GenerateEnum("PrepInstructions", PrepInstructions));
	Result.Add(GenerateEnum("Nodes", Nodes, True));
	Result.Add(GenerateEnum("Tokens", Tokens));
	Return StrConcat(Result);
EndFunction // Refult()

Function GenerateEnum(Name, Enum, Links = False)
	Var Buffer;
	Buffer = New Array;
	Buffer.Add(StrTemplate(
		"<h3 id='%1'>%1</h3>
		|<ul>",
		Name
	));
	EnumValues = New Structure;
	For Each Item In Enum Do
		EnumValues.Insert(Item.Value);
	EndDo;
	For Each Item In EnumValues Do
		If Links Then
			Buffer.Add(StrTemplate("<li>""<a href='#%1'>%1</a>""</li>" "", Item.Key));
		Else
			Buffer.Add(StrTemplate("<li>""%1""</li>" "", Item.Key));
		EndIf;
	EndDo;
	Buffer.Add("</ul>" "");
	Return StrConcat(Buffer);
EndFunction // GenerateEnum()

Function Interface() Export
	Var Interface;
	Interface = New Array;
	Interface.Add("VisitModule");
	Interface.Add("VisitPrepRegionDecl");
	Interface.Add("VisitDesigExpr");
	Return Interface;
EndFunction // Interface()

Procedure VisitModule(Module, Stack, Counters) Export
	Comments = Module.Comments;
EndProcedure // VisitModule

Procedure VisitPrepRegionDecl(PrepRegionDecl, Stack, Counters) Export
	If Counters.PrepRegionDecl = 0 Then
		Region = PrepRegionDecl.Name;
		SubRegion = "";
		If Region = "AbstractSyntaxTree" Then
			Result.Add("	<h1>Abstract syntax tree</h1>" "");
		EndIf;
	ElsIf Counters.PrepRegionDecl = 1 Then
		SubRegion = PrepRegionDecl.Name;
		If Region = "AbstractSyntaxTree" Then
			Result.Add(StrTemplate("	<h2 id='#%1'>#%1</h2>" "", SubRegion));
		EndIf;
	EndIf;
EndProcedure // VisitPrepRegionDecl()

Procedure VisitDesigExpr(DesigExpr, Stack, Counters) Export

	If Region = "AbstractSyntaxTree" Then

		If DesigExpr.Call Then

			If DesigExpr.Object.Name = "Structure" Then
				Tag = Comments[DesigExpr.Place.Line];
				If Tag <> Undefined And StrFind(Tag, "@Node") Then
					
					DescriptionList = New ValueList;
					
					DescriptionLine = DesigExpr.Place.Line - 1;
					Description = Comments[DescriptionLine];
					While Description <> Undefined Do
						DescriptionList.Insert(0, Description);
						DescriptionLine = DescriptionLine - 1;
						Description = Comments[DescriptionLine];
					EndDo;  
					
					CallExpr = DesigExpr.Select[0];
					NodeFields = CallExpr.Value[0].List;
					NodeName = CallExpr.Value[1].Select[0].Value;

					Result.Add(StrTemplate(
						"	<h3 id='%1'>%1</h3>
						|	<ul>" "",
						NodeName
					));
					
					DescriptionListCount = DescriptionList.Count();
					Index = 0;
					While Index < DescriptionListCount Do
						Item = DescriptionList[Index]; 
						If TrimAll(Item.Value) = "<pre>" Then
							Buffer = New Array;
							While TrimAll(Item.Value) <> "</pre>" Do
								Buffer.Add(Item.Value);
								Index = Index + 1;
								Item = DescriptionList[Index];
							EndDo;
							Result.Add(StrConcat(Buffer, Chars.LF));
							Result.Add("</pre>");
						Else
							Result.Add(StrTemplate("	<i>%1</i><br>" "", Item.Value));
						EndIf; 
						Index = Index + 1;
					EndDo;
					
					Result.Add("	<p>");
					
					For Each Field In NodeFields Do
						FieldName = TrimAll(Field.Value);
						If Right(FieldName, 1) = "," Then
							FieldName = Left(FieldName, StrLen(FieldName) - 1);
						EndIf;
						TypeList = ParseTypes(Comments[Field.Place.Line]);
						Result.Add(StrTemplate(
							"		<li><strong>%1</strong>: %2%3</li>" "",
							FieldName,
							GenerateTypeLinks(TypeList),
							?(FieldName = "Type", " = Nodes." + NodeName, "")
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
		ElsIf TypeOf(Item.Child) = Type("Строка") Then
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

#Region TypeParser

Function ParseTypes(Types)
	Var Pos, Ident, List;
	Pos = 1; List = New Array;
	While True Do
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
		If Mid(Types, Pos, 1) <> "," Then
			Break;
		EndIf; 
		Pos = Pos + 1;
	EndDo; 
	Return List;
EndFunction // ParseTypes()

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

#EndRegion // TypeParser