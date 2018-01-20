
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)

	If StrFind(InfoBaseConnectionString(), "File=") = 0 Then
		Message("Only for file bases")
	EndIf;

	Output = "Tree";

	SetVisibilityOfAttributes(ThisObject);

EndProcedure // OnCreateAtServer()

&AtClient
Procedure Translate(Command)

	Result.Clear();
	ClearMessages();
	TranslateAtServer();

EndProcedure // Translate()

&AtServer
Procedure TranslateAtServer()
	Var Start;

	This = FormAttributeToValue("Object");
	ThisFile = New File(This.UsedFileName);

	BSLParser = ExternalDataProcessors.Create(ThisFile.Path + "BSLParser.epf", False);

	BSLParser.Verbose = Verbose;
	BSLParser.Location = Location;
	BSLParser.Debug = Debug;

	Start = CurrentUniversalDateInMilliseconds();

	If Output = "Lexems" Then

		Eof = BSLParser.Tokens().Eof;

		Parser = BSLParser.Parser(Source.GetText());
		Lexems = New Array;
		While BSLParser.Next(Parser) <> Eof Do
			Lexems.Add(StrTemplate("%1: %2 -- `%3`", Parser.Line, Parser.Tok, Parser.Lit));
		EndDo;
		
		Result.SetText(StrConcat(Lexems, Chars.LF));
		
	ElsIf Output = "AST" Then

		Parser = BSLParser.Parser(Source.GetText());
		BSLParser.ParseModule(Parser);
		JSONWriter = New JSONWriter;
		JSONWriter.SetString(New JSONWriterSettings(, Chars.Tab));
		If ShowComments Then
			Comments = New Map;
			For Each Item In Parser.Module.Comments Do
				Comments[Format(Item.Key, "NZ=0; NG=")] = Item.Value;
			EndDo;
			Parser.Module.Comments = Comments;
		Else
			Parser.Module.Delete("Comments");
		EndIf;
		WriteJSON(JSONWriter, Parser.Module,, "ConvertJSON", ThisObject);
		Result.SetText(JSONWriter.Close());
		
	ElsIf Output = "Tree" Then

		Parser = BSLParser.Parser(Source.GetText());
		BSLParser.ParseModule(Parser);
		FillTree(Parser.Module);
		
	ElsIf Output = "Backend" Then

		BackendProcessor = ExternalDataProcessors.Create(BackendPath, False);
		BackendProcessor.Init(BSLParser);
		Parser = BSLParser.Parser(Source.GetText());
		BSLParser.ParseModule(Parser);
		BackendResult = BackendProcessor.VisitModule(Parser.Module);
		Result.SetText(BackendResult);
		
	ElsIf Output = "Plugin" Then

		PluginProcessor = ExternalDataProcessors.Create(BackendPath, False);
		PluginProcessor.Init(BSLParser);
		Parser = BSLParser.Parser(Source.GetText());
		BSLParser.ParseModule(Parser);
		Hooks = BSLParser.Hooks();
		List = Undefined;
		For Each MethodName In PluginProcessor.Interface() Do
			If Hooks.Property(MethodName, List) Then
				List.Add(PluginProcessor);
			EndIf; 
		EndDo; 
		Visitor = BSLParser.Visitor(Hooks);
		BSLParser.VisitModule(Visitor, Parser.Module);

	EndIf;

	If Measure Then
		Message(StrTemplate("%1 sec.", (CurrentUniversalDateInMilliseconds() - Start) / 1000));
	EndIf;

EndProcedure // TranslateAtServer()

&AtServer
Function FillTree(Module)
	Var Place;
	TreeItems = Tree.GetItems();
	TreeItems.Clear();
	Row = TreeItems.Add();
	Row.Name = "Module";
	Row.Type = Module.Type;
	Row.Value = "<...>";
	FillNode(Row, Module);
EndFunction // FillTree() 

&AtServer
Function FillNode(Row, Node)
	Var Place;
	If Node.Property("Place", Place) And Place <> Undefined Then
		Row.Line = Place.Line;
		Row.Pos = Place.Pos;
		Row.Len = Place.Len;
	EndIf;
	TreeItems = Row.GetItems();
	For Each Item In Node Do
		If Item.Key = "Place"
			Or Item.Key = "Type" Then
			Continue;
		EndIf; 
		If TypeOf(Item.Value) = Type("Array") Then
			Row = TreeItems.Add();
			Row.Name = Item.Key;
			Row.Type = StrTemplate("List (%1)", Item.Value.Count());
			Row.Value = "<...>";
			RowItems = Row.GetItems();
			Index = 0;
			For Each Item In Item.Value Do
				Row = RowItems.Add();
				Index = Index + 1;
				Row.Name = Index;
				If Item = Undefined Then
					Row.Value = "Undefined";
				Else
					Item.Property("Type", Row.Type);
					Row.Value = "<...>";
					FillNode(Row, Item);
				EndIf; 
			EndDo;			
		ElsIf TypeOf(Item.Value) = Type("Structure") Then
			Row = TreeItems.Add();
			Row.Name = Item.Key;
			Row.Type = Item.Value.Type;
			Row.Value = "<...>";
			FillNode(Row, Item.Value);
		Else
			Row = TreeItems.Add();
			Row.Name = Item.Key;
			Row.Value = Item.Value;
			Row.Type = TypeOf(Item.Value);
		EndIf; 
	EndDo;
EndFunction // FillNode() 

&AtServer
Function ConvertJSON(Property, Value, Other, Cancel) Export
	If Value = Null Then
		Return Undefined;
	EndIf;
EndFunction // ConvertJSON()

&AtClientAtServerNoContext
Procedure SetVisibilityOfAttributes(ThisObject, Reason = Undefined)

	Items = ThisObject.Items;

	If Reason = Items.Output Or Reason = Undefined Then

		Items.BackendPath.Visible = (ThisObject.Output = "Backend" Or ThisObject.Output = "Plugin");
		Items.ShowComments.Visible = (ThisObject.Output = "AST");
		Items.Tree.Visible = (ThisObject.Output = "Tree");
		Items.Result.Visible = (ThisObject.Output <> "Tree");
		
	EndIf;

EndProcedure // SetVisibilityOfAttributes()

&AtClient
Procedure OutputOnChange(Item)

	SetVisibilityOfAttributes(ThisObject, Item);

EndProcedure // OutputOnChange()

&AtClient
Procedure BackendPathStartChoice(Item, ChoiceData, StandardProcessing)

	StandardProcessing = False;
	ChoosePath(Item, ThisObject, FileDialogMode.Open, "(*.epf)|*.epf");

EndProcedure // BackendPathStartChoice()

&AtClient
Procedure ChoosePath(Item, Form, DialogMode = Undefined, Filter = Undefined)

	If DialogMode = Undefined Then
		DialogMode = FileDialogMode.ChooseDirectory;
	EndIf;

	FileOpeningDialog = New FileDialog(DialogMode);
	FileOpeningDialog.Filter = Filter;

	FileOpeningDialog.Show(New NotifyDescription("ChoosePathNotifyChoice", ThisObject));

EndProcedure // ChoosePath()

&AtClient
Procedure ChoosePathNotifyChoice(Result, AdditionalParameters) Export

	If Result <> Undefined Then
		BackendPath = Result[0];
	EndIf;

EndProcedure // ChoosePathHandle()

&AtClient
Procedure TreeSelection(Item, SelectedRow, Field, StandardProcessing)
	Row = Tree.FindByID(SelectedRow);
	If Row.Line > 0 Then
		Items.Source.SetTextSelectionBounds(Row.Pos, Row.Pos + Row.Len);
		CurrentItem = Items.Source;
	EndIf; 
EndProcedure

