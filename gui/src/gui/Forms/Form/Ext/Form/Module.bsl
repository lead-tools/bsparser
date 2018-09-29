
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

	Start = CurrentUniversalDateInMilliseconds();

	If Output = "NULL" Then
		
		Try
			BSLParser.Parse(Source.GetText());
		Except
			Message("syntax error!");
		EndTry;
		
	ElsIf Output = "AST" Then

		Try
			Parser_Module = BSLParser.Parse(Source.GetText());
		Except
			Message("syntax error!");
		EndTry;
		If Parser_Module <> Undefined Then
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
		EndIf; 
		
	ElsIf Output = "Tree" Then
		
		Try
			Parser_Module = BSLParser.Parse(Source.GetText());
		Except
			Message("syntax error!");
		EndTry;
		If Parser_Module <> Undefined Then
			FillTree(Parser_Module);
		EndIf; 
		
	ElsIf Output = "Plugin" Then
		
		Try
			Parser_Module = BSLParser.Parse(Source.GetText());
		Except
			Message("syntax error!");
		EndTry;
		If Parser_Module <> Undefined Then
			PluginsList = New Array;
			For Each Row In Plugins Do
				PluginsList.Add(ExternalDataProcessors.Create(Row.Path, False));
			EndDo;
			BSLParser.HookUp(PluginsList);
			BSLParser.Visit(Parser_Module);
			ResultArray = New Array;
			For Each Plugin In PluginsList Do
				ResultArray.Add(Plugin.Result());
			EndDo; 
			Result.SetText(StrConcat(ResultArray));
		EndIf; 
		
	EndIf;

	If Measure Then
		Message(StrTemplate("%1 sec.", (CurrentUniversalDateInMilliseconds() - Start) / 1000));
	EndIf;

	Errors.Load(BSLParser.Errors());  
	
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

		Items.PagePlugins.Visible = (ThisObject.Output = "Plugin");
		Items.ShowComments.Visible = (ThisObject.Output = "AST");
		Items.PageResultTree.Visible = (ThisObject.Output = "Tree");
		Items.PageResultText.Visible = (ThisObject.Output <> "Tree");
		
	EndIf;

EndProcedure // SetVisibilityOfAttributes()

&AtClient
Procedure OutputOnChange(Item)

	SetVisibilityOfAttributes(ThisObject, Item);

EndProcedure // OutputOnChange()

&AtClient
Procedure PluginsPathStartChoice(Item, ChoiceData, StandardProcessing)
	
	StandardProcessing = False;
	ChoosePath(Item, ThisObject, FileDialogMode.Open, "(*.epf)|*.epf");
	
EndProcedure // PluginsPathStartChoice()

&AtClient
Procedure ChoosePath(Item, Form, DialogMode = Undefined, Filter = Undefined) Export
	
	If DialogMode = Undefined Then
		DialogMode = FileDialogMode.ChooseDirectory;
	EndIf; 
	
	FileOpeningDialog = New FileDialog(DialogMode);
	FileOpeningDialog.Multiselect = False;
	FileOpeningDialog.Filter = Filter;
	If DialogMode = FileDialogMode.ChooseDirectory Then
		FileOpeningDialog.Directory = Item.EditText;
	Else
		FileOpeningDialog.FullFileName = Item.EditText;
	EndIf; 
	
	AdditionalParameters = New Structure("Item, Form", Item, Form);
	
	NotifyDescription = New NotifyDescription("ChoosePathNotifyChoice", ThisObject, AdditionalParameters);
	
	FileOpeningDialog.Show(NotifyDescription);
	
EndProcedure // ChoosePath()

&AtClient
Procedure ChoosePathNotifyChoice(Result, AdditionalParameters) Export
	
	If Result <> Undefined Then
		InteractivelySetValueOfFormItem(
			Result[0],
			AdditionalParameters.Item,
			AdditionalParameters.Form
		);
	EndIf; 
	
EndProcedure // ChoosePathHandle()

&AtClient
Procedure InteractivelySetValueOfFormItem(Value, Item, Form) Export
	
	FormOwner = Form.FormOwner;
	CloseOnChoice = Form.CloseOnChoice;
	
	Form.FormOwner = Item;
	Form.CloseOnChoice = False;
	
	Form.NotifyChoice(Value);
	
	If Form.FormOwner = Item Then
		Form.FormOwner = FormOwner;
	EndIf;
	
	If Form.CloseOnChoice = False Then
		Form.CloseOnChoice = CloseOnChoice;
	EndIf;  
	
EndProcedure // InteractivelySetValueOfFormItem()

&AtClient
Procedure TreeSelection(Item, SelectedRow, Field, StandardProcessing)
	Row = Tree.FindByID(SelectedRow);
	If Row.Line > 0 Then
		Items.Source.SetTextSelectionBounds(Row.Pos, Row.Pos + Row.Len);
		CurrentItem = Items.Source;
	EndIf; 
EndProcedure // TreeSelection()

&AtClient
Procedure PluginsPathOpening(Item, StandardProcessing)	
	StandardProcessing = False;
	ShowFile(Items.Plugins.CurrentData.Path);
EndProcedure

&AtClient
Procedure ShowFile(FullName) Export
	If FullName <> Undefined Then
		BeginRunningApplication(
			New NotifyDescription("ShowFileHandleResult", ThisObject, FullName),
			"explorer.exe /select, " + FullName
		);
	EndIf; 	
EndProcedure // ShowFolder()

&AtClient
Procedure ShowFileHandleResult(ReturnCode, FullName) Export
	// silently continue
EndProcedure // ShowFileHandleResult()

&AtClient
Procedure ErrorsSelection(Item, SelectedRow, Field, StandardProcessing)
	Row = Errors.FindByID(SelectedRow);
	If Row.Line > 0 Then
		Items.Source.SetTextSelectionBounds(Row.Pos, Row.Pos + 1);
		CurrentItem = Items.Source;
	EndIf;
EndProcedure
