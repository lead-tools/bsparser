
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

If CommandLineArguments.Count() = 0 Then
	Raise "Укажите в качестве параметра путь к папке с общими модулями bsl";
EndIf;

CommonModulesPath = CommandLineArguments[0];
Files = FindFiles(CommonModulesPath, "*.bsl", True);

BSLParser = New BSLParser;
PluginTestVars = New PluginTestVars;
BSLParser.HookUp(PluginTestVars);

TextReader = New TextReader;

For Each File In Files Do
	TextReader.Open(File.FullName);
	Source = TextReader.Read();
	Try
		Module = BSLParser.Parse(Source);
		BSLParser.Visit(Module);
	Except
		Message(DetailErrorDescription(ErrorInfo()));
	EndTry;
	Result = PluginTestVars.Result();
	If ValueIsFilled(Result) Then
		Message(Chars.LF);
		Message(File.FullName);
		Message(Result);
	EndIf;
	TextReader.Close()
EndDo;