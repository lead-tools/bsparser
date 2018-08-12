
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)

	If StrFind(InfoBaseConnectionString(), "File=") = 0 Then
		Message("Only for file bases")
	EndIf;

	Output = "Tree";

EndProcedure // OnCreateAtServer()

&AtClient
Procedure OnOpen(Cancel)
	
	SetVisibilityOfAttributes(ThisObject);
	
EndProcedure // OnOpen()

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

	If Output = "NULL" Then

		BSLParser.ParseModule(Source.GetText());
		
	ElsIf Output = "AST" Then

		Parser_Module = BSLParser.ParseModule(Source.GetText());
		JSONWriter = New JSONWriter;
		JSONWriter.SetString(New JSONWriterSettings(, Chars.Tab));
		If ShowComments Then
			Comments = New Map;
			For Each Item In Parser_Module.Comments Do
				Comments[Format(Item.Key, "NZ=0; NG=")] = Item.Value;
			EndDo;
			Parser_Module.Comments = Comments;
		Else
			Parser_Module.Delete("Comments");
		EndIf;
		WriteJSON(JSONWriter, Parser_Module,, "ConvertJSON", ThisObject);
		Result.SetText(JSONWriter.Close());
		
	ElsIf Output = "Tree" Then

		Parser_Module = BSLParser.ParseModule(Source.GetText());
		FillTree(Parser_Module);
		
	ElsIf Output = "Plugin" Then
		
		BSLParser.Location = True;
		
		PluginProcessor = ExternalDataProcessors.Create(PluginPath, False);
		Parser_Module = BSLParser.ParseModule(Source.GetText());
		BSLParser.HookUp(PluginProcessor);
		BSLParser.VisitModule(Parser_Module);
		Result.SetText(PluginProcessor.Result());
		
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
	If Node.Property("Place", Place) And TypeOf(Place) = Type("Structure") Then
		Row.Line = Place.BegLine;
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

		Items.PluginPath.Visible = (ThisObject.Output = "Plugin");
		Items.Location.Visible = (ThisObject.Output <> "Plugin");
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
Procedure PluginPathStartChoice(Item, ChoiceData, StandardProcessing)

	StandardProcessing = False;
	ChoosePath(Item, ThisObject, FileDialogMode.Open, "(*.epf)|*.epf");

EndProcedure // PluginPathStartChoice()

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
		PluginPath = Result[0];
	EndIf;

EndProcedure // ChoosePathNotifyChoice()

&AtClient
Procedure TreeSelection(Item, SelectedRow, Field, StandardProcessing)
	Row = Tree.FindByID(SelectedRow);
	If Row.Line > 0 Then
		Items.Source.SetTextSelectionBounds(Row.Pos, Row.Pos + Row.Len);
		CurrentItem = Items.Source;
	EndIf; 
EndProcedure // TreeSelection()


