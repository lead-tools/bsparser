
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)

	If Parameters.Property("Source") Then
		Object.Verbose = Parameters.Verbose;
		Output = Parameters.Output;
		Source.SetText(Parameters.Source);
	Else
		Output = "AST";
	EndIf; 
	
EndProcedure // OnCreateAtServer()

&AtClient
Procedure Reopen(Command)
	
	ReopenAtServer();
	Close();
	OpenForm(FormName, New Structure("Source, Verbose, Output", Source.GetText(), Object.Verbose, Output));
	
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
		
	ElsIf Output = "BSL" Then	
		
		Backend = This.Backend();
		Parser = This.Parser(Source.GetText());
		This.ParseModule(Parser);
		This.BSL_VisitModule(Backend, Parser.Module);
		Result.SetText(StrConcat(Backend.Result));
		
	ElsIf Output = "PS" Then	
		
		Backend = This.Backend();
		Parser = This.Parser(Source.GetText());
		This.ParseModule(Parser);
		This.PS_VisitModule(Backend, Parser.Module);
		Result.SetText(StrConcat(Backend.Result));
		
	ElsIf Output = "measure" Then
		
		Start = CurrentUniversalDateInMilliseconds();
		Parser = This.Parser(Source.GetText());
		This.ParseModule(Parser);
		Message(StrTemplate("%1 sec.", (CurrentUniversalDateInMilliseconds() - Start) / 1000));
		
	EndIf; 
		
EndProcedure // TranslateAtServer()