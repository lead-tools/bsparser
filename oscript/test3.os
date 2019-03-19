
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");
AttachScript("..\plugins\TestEnd\src\TestEnd\Ext\ObjectModule.bsl", "PluginTestEnd");
AttachScript("..\plugins\ReturnCheck\src\ReturnCheck\Ext\ObjectModule.bsl", "PluginReturnCheck");

If CommandLineArguments.Count() = 0 Then
	Raise "Укажите в качестве параметра путь к папке с общими модулями os";
EndIf;

CommonModulesPath = CommandLineArguments[0];
Files = FindFiles(CommonModulesPath, "*.os", True);

BSLParser = New BSLParser;

Plugins = New Array;
Plugins.Add(New PluginTestVars);
Plugins.Add(New PluginTestEnd);
Plugins.Add(New PluginReturnCheck);

BSLParser.HookUp(Plugins);

TextReader = New TextReader;

For Each File In Files Do
	TextReader.Open(File.FullName, "UTF-8");
	Source = TextReader.Read();
	Try
		Module = BSLParser.Parse(Source);
		BSLParser.Visit(Module);
	Except
		Message(Chars.LF);
		Message(File.FullName);
		Message(DetailErrorDescription(ErrorInfo()));
	EndTry;
	For Each Plugin In Plugins Do
		Result = Plugin.Result();
		If ValueIsFilled(Result) Then
			Message(Chars.LF);
			Message(File.FullName);
			Message(Result);
		EndIf;
	EndDo;
	TextReader.Close()
EndDo;