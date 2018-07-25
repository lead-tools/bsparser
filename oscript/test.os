
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

BSLParser = New BSLParser;
BSLParser.Location = False;
Module = BSLParser.ParseModule(Source);

PluginTestVars = New PluginTestVars;
BSLParser.HookUp(PluginTestVars);
BSLParser.VisitModule(Module);

Message(PluginTestVars.Result());