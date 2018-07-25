
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\DocGen\src\DocGen\Ext\ObjectModule.bsl", "PluginDocGen");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

BSLParser = New BSLParser;
BSLParser.Parser(Source);
Module = BSLParser.ParseModule();

PluginDocGen = New PluginDocGen;
PluginDocGen.Init(BSLParser);
Hooks = BSLParser.Hooks();
List = Undefined;
For Each MethodName In PluginDocGen.Interface() Do
	If Hooks.Property(MethodName, List) Then
		List.Add(PluginDocGen);
	EndIf;
EndDo;
BSLParser.Visitor(Hooks);
BSLParser.VisitModule(Module);

TextWriter = New TextWriter("..\docs\index.html");
TextWriter.Write(PluginDocGen.Result());