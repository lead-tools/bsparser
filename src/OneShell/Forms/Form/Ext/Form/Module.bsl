
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)

	If Parameters.Property("Source") Then
		Object.Verbose = Parameters.Verbose;
		Output = Parameters.Output;
		Object.CompatibleWith1C = Parameters.CompatibleWith1C;
		BackendPath = Parameters.BackendPath;
		Source.SetText(Parameters.Source);
	Else
		Output = "AST";
	EndIf; 
	
	SetVisibilityOfAttributes(ThisObject);
	
EndProcedure // OnCreateAtServer()

&AtClient
Procedure Reopen(Command)
	
	ReopenAtServer();
	Close();
	OpenForm(FormName, New Structure("Source, Verbose, Output, CompatibleWith1C, BackendPath", Source.GetText(), Object.Verbose, Output, Object.CompatibleWith1C, BackendPath));
	
EndProcedure // Reopen()

&AtServer
Procedure ReopenAtServer()
	
	This = FormAttributeToValue("Object");
	ExternalDataProcessors.Create(This.UsedFileName, False);
	
EndProcedure // ReopenAtServer() 

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
	
	Start = CurrentUniversalDateInMilliseconds();
	
	If Output = "Lexems" Then
		
		Eof = This.Tokens().Eof;
		
		Scanner = This.Scanner(Source.GetText());
		While This.Scan(Scanner) <> Eof Do
			Result.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, Scanner.Tok, Scanner.Lit));
		EndDo;
		
	ElsIf Output = "AST" Then
		
		Parser = This.Parser(Source.GetText());
		This.ParseModule(Parser);
		JSONWriter = New JSONWriter;
		FileName = GetTempFileName(".json");
		JSONWriter.OpenFile(FileName,,, New JSONWriterSettings(, Chars.Tab));
		WriteJSON(JSONWriter, Parser.Module);
		JSONWriter.Close();
		Result.Read(FileName, TextEncoding.UTF8);		
		
	ElsIf Output = "Backend" Then
		
		BackendProcessor = ExternalDataProcessors.Create(BackendPath, False);
		BackendProcessor.Init(This);
		Parser = This.Parser(Source.GetText());
		This.ParseModule(Parser);
		BackendResult = BackendProcessor.VisitModule(Parser.Module); 
		Result.SetText(BackendResult);
		
	EndIf;
	
	If Measure Then
		Message(StrTemplate("%1 sec.", (CurrentUniversalDateInMilliseconds() - Start) / 1000));
	EndIf;
		
EndProcedure // TranslateAtServer()

&AtClientAtServerNoContext
Procedure SetVisibilityOfAttributes(ThisObject, Reason = Undefined)
	
	Items = ThisObject.Items;
		
	If Reason = Items.Output Or Reason = Undefined Then
		
		Items.BackendPath.Visible = (ThisObject.Output = "Backend");
		
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

