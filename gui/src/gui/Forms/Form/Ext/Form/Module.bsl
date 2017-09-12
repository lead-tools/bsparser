
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)

	If StrFind(InfoBaseConnectionString(), "File=") = 0 Then
		Message("Only for file bases")
	EndIf;

	Output = "AST";

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

	BSLParser = ExternalDataProcessors.Create(ThisFile.Path + "BSL-Parser.epf", False);

	BSLParser.Verbose = Verbose;
	BSLParser.Location = Location;
	BSLParser.Debug = Debug;

	Start = CurrentUniversalDateInMilliseconds();

	If Output = "Lexems" Then

		Eof = BSLParser.Tokens().Eof;

		Scanner = BSLParser.Scanner(Source.GetText());
		While BSLParser.Scan(Scanner) <> Eof Do
			Result.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, Scanner.Tok, Scanner.Lit));
		EndDo;

	ElsIf Output = "AST" Then

		Parser = BSLParser.Parser(Source.GetText());
		BSLParser.ParseModule(Parser);
		JSONWriter = New JSONWriter;
		FileName = GetTempFileName(".json");
		JSONWriter.OpenFile(FileName,,, New JSONWriterSettings(, Chars.Tab));
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
		JSONWriter.Close();
		Result.Read(FileName, TextEncoding.UTF8);

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
		Hooks = New Structure(
			"VisitModule,"
			"VisitDeclarations,"
			"VisitStatements,"
			"VisitDecl,"
			"VisitVarModListDecl,"
			"VisitVarLocListDecl,"
			"VisitProcDecl,"
			"VisitFuncDecl,"
			"VisitPrepIfDecl,"
			"VisitPrepElsIfDecl,"
			"VisitPrepRegionDecl,"
			"VisitExpr,"
			"VisitBasicLitExpr,"
			"VisitDesigExpr,"
			"VisitUnaryExpr,"
			"VisitBinaryExpr,"
			"VisitNewExpr,"
			"VisitTernaryExpr,"
			"VisitParenExpr,"
			"VisitNotExpr,"
			"VisitStringExpr,"
			"VisitStmt,"
			"VisitAssignStmt,"
			"VisitReturnStmt,"
			"VisitBreakStmt,"
			"VisitContinueStmt,"
			"VisitRaiseStmt,"
			"VisitExecuteStmt,"
			"VisitCallStmt,"
			"VisitIfStmt,"
			"VisitElsIfStmt,"
			"VisitPrepIfStmt,"
			"VisitPrepElsIfStmt,"
			"VisitWhileStmt,"
			"VisitPrepRegionStmt,"
			"VisitForStmt,"
			"VisitForEachStmt,"
			"VisitTryStmt,"
			"VisitGotoStmt,"
			"VisitLabelStmt,"
		);
		For Each Item In Hooks Do
			Hooks[Item.Key] = New Array;	
		EndDo;
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

