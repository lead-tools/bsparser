
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();
TextReader.Close();

Plugins = New Array;
Plugins.Add(New PluginTestVars);

BSLParser = New BSLParser;

Context = BSLParser.Context();

Scope = Context.Scope;
Item = BSLParser.Item("Catalogs");
Scope.Insert("Catalogs", Item);

Methods = Context.Methods;
Item = BSLParser.Item("FindMarkedForDeletion");
Methods.Insert("FindMarkedForDeletion", Item);

BSLParser.Go(Source, Plugins, Context);

For Each Plugin In Plugins Do
	Message(Plugin.Result());
EndDo;