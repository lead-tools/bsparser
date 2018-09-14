
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");
AttachScript("..\plugins\TestEnd\src\TestEnd\Ext\ObjectModule.bsl", "PluginTestEnd");
AttachScript("..\plugins\ReturnCheck\src\ReturnCheck\Ext\ObjectModule.bsl", "PluginReturnCheck");
AttachScript("..\plugins\AutoVarsCheck\src\AutoVarsCheck\Ext\ObjectModule.bsl", "PluginAutoVarsCheck");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();
TextReader.Close();

BSLParser = New BSLParser;
BSLParser.Location = True;
Module = BSLParser.ParseModule(Source);

Plugins = New Array;

Plugins.Add(New PluginTestVars);
Plugins.Add(New PluginTestEnd);
Plugins.Add(New PluginReturnCheck);
Plugins.Add(New PluginAutoVarsCheck);
BSLParser.HookUp(Plugins);
BSLParser.VisitModule(Module);

For Each Plugin In Plugins Do
	Message(Plugin.Result());
EndDo;