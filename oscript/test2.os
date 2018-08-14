
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

CommonModulesPath = CommandLineArguments[0];
Files = FindFiles(CommonModulesPath, "*.bsl", True);

BSLParser = New BSLParser;
PluginTestVars = New PluginTestVars;
BSLParser.HookUp(PluginTestVars);

TextReader = New TextReader;

For Each File In Files Do
	Message(Chars.LF);
	Message(File.FullName);
	TextReader.Open(File.FullName);
	Source = TextReader.Read();
	Try
		Module = BSLParser.ParseModule(Source);
		BSLParser.VisitModule(Module);
	Except
		Message(DetailErrorDescription(ErrorInfo()));
	EndTry;
	Message(PluginTestVars.Result());
	TextReader.Close()
EndDo;