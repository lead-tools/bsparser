
AttachScript("..\src\BSParser\Ext\ObjectModule.bsl", "BSParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

TextReader = New TextReader("..\src\BSParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();
TextReader.Close();

Plugins = New Array;
Plugins.Add(New PluginTestVars);

BSParser = New BSParser;

Context = BSParser.Context();

Scope = Context.Scope;
Item = BSParser.Item("Catalogs");
Scope.Insert("Catalogs", Item);

Methods = Context.Methods;
Item = BSParser.Item("FindMarkedForDeletion");
Methods.Insert("FindMarkedForDeletion", Item);

BSParser.Go(Source, Plugins, Context);

For Each Plugin In Plugins Do
	Message(Plugin.Result());
EndDo;