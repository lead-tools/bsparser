
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

BSLParser = New BSLParser;
Parser = BSLParser.Parser(Source);
BSLParser.ParseModule(Parser);

PluginTestVars = New PluginTestVars;
PluginTestVars.Init(BSLParser);
Hooks = BSLParser.Hooks();
List = Undefined;
For Each MethodName In PluginTestVars.Interface() Do
	If Hooks.Property(MethodName, List) Then
		List.Add(PluginTestVars);
	EndIf; 
EndDo; 
Visitor = BSLParser.Visitor(Hooks);
BSLParser.VisitModule(Visitor, Parser.Module);

Message(PluginTestVars.Result());