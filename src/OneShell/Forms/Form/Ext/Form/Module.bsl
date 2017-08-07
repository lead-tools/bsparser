
&AtServer
Procedure OnCreateAtServer(Cancel, StandardProcessing)
	
	If Parameters.Property("Source") Then
		FormObject.Verbose = Parameters.Verbose;
		FormOutput = Parameters.Output;
		FormSource.SetText(Parameters.Source);
	Else
		FormOutput = "PS";
	EndIf; 
	
EndProcedure // OnCreateAtServer()

&AtClient
Procedure Reopen(Command)
	
	ReopenAtServer();
	Close();
	OpenForm(FormName, New Structure("Source, Verbose, Output", FormSource.GetText(), FormObject.Verbose, FormOutput));
	
EndProcedure // Reopen()

&AtServer
Procedure ReopenAtServer()
	
	This = FormAttributeToValue("FormObject");
	ExternalDataProcessors.Create(This.UsedFileName, False);
	
EndProcedure // ReopenAtServer() 

&AtClient
Procedure Translate(Command)
	
	FormResult.Clear();
	ClearMessages();
	TranslateAtServer();
	
EndProcedure // Translate()

&AtServer
Procedure TranslateAtServer()
	Var Start;
	
	This = FormAttributeToValue("FormObject"); 
	
	Start = CurrentUniversalDateInMilliseconds();
	
	If FormOutput = "Lexems" Then
		
		Eof = This.Tokens().Eof;
		
		Scanner = This.Scanner(FormSource.GetText());
		While This.Scan(Scanner) <> Eof Do
			FormResult.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, Scanner.Tok, Scanner.Lit));
		EndDo;
		
	ElsIf FormOutput = "AST" Then
		
		Parser = This.Parser(FormSource.GetText());
		This.ParseModule(Parser);
		JSONWriter = New JSONWriter;
		FileName = GetTempFileName(".json");
		JSONWriter.OpenFile(FileName,,, New JSONWriterSettings(, Chars.Tab));
		WriteJSON(JSONWriter, Parser.Module);
		JSONWriter.Close();
		FormResult.Read(FileName, TextEncoding.UTF8);	
		
	ElsIf FormOutput = "BSL" Then	
		
		Backend = This.Backend();
		Parser = This.Parser(FormSource.GetText());
		This.ParseModule(Parser);
		This.BSL_VisitModule(Backend, Parser.Module);
		FormResult.SetText(StrConcat(Backend.Result));
		
	ElsIf FormOutput = "PS" Then	
		
		Backend = This.Backend();
		Parser = This.Parser(FormSource.GetText());
		This.ParseModule(Parser);
		This.PS_VisitModule(Backend, Parser.Module);
		FormResult.SetText(StrConcat(Backend.Result));
		
	EndIf; 
	
	If FormObject.Verbose Then
		Message((CurrentUniversalDateInMilliseconds() - Start) / 1000);
	EndIf;
	
EndProcedure // TranslateAtServer()