
AttachScript("..\src\BSParser\Ext\ObjectModule.bsl", "BSParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

If CommandLineArguments.Count() = 0 Then
	Raise "Укажите в качестве параметра путь к папке с общими модулями bsl";
EndIf;

CommonModulesPath = CommandLineArguments[0];
Files = FindFiles(CommonModulesPath, "*.bsl", True);

BSParser = New BSParser;
PluginTestVars = New PluginTestVars;
BSParser.HookUp(PluginTestVars);

TextReader = New TextReader;

For Each File In Files Do
	TextReader.Open(File.FullName);
	Source = TextReader.Read();
	Try
		Module = BSParser.Parse(Source);
		BSParser.Visit(Module);
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