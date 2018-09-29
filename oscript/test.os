
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");
AttachScript("..\plugins\TestEnd\src\TestEnd\Ext\ObjectModule.bsl", "PluginTestEnd");
AttachScript("..\plugins\ReturnCheck\src\ReturnCheck\Ext\ObjectModule.bsl", "PluginReturnCheck");
AttachScript("..\plugins\AutoVarsCheck\src\AutoVarsCheck\Ext\ObjectModule.bsl", "PluginAutoVarsCheck");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();
TextReader.Close();

Plugins = New Array;
Plugins.Add(New PluginTestVars);
Plugins.Add(New PluginTestEnd);
Plugins.Add(New PluginReturnCheck);
Plugins.Add(New PluginAutoVarsCheck);

BSLParser = New BSLParser;
BSLParser.Go(Source, Plugins);

For Each Plugin In Plugins Do
	Message(Plugin.Result());
EndDo;