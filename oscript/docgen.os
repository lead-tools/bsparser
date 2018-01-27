
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\DocGen\src\DocGen\Ext\ObjectModule.bsl", "PluginDocGen");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

BSLParser = New BSLParser;
Parser = BSLParser.Parser(Source);
BSLParser.ParseModule(Parser);

PluginDocGen = New PluginDocGen;
PluginDocGen.Init(BSLParser);
Hooks = BSLParser.Hooks();
List = Undefined;
For Each MethodName In PluginDocGen.Interface() Do
	If Hooks.Property(MethodName, List) Then
		List.Add(PluginDocGen);
	EndIf;
EndDo;
Visitor = BSLParser.Visitor(Hooks);
BSLParser.VisitModule(Visitor, Parser.Module);

TextWriter = New TextWriter("..\docs\index.html");
TextWriter.Write(PluginDocGen.Result());