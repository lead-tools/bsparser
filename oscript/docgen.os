
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\DocGen\src\DocGen\Ext\ObjectModule.bsl", "PluginDocGen");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

BSLParser = New BSLParser;
Module = BSLParser.Parse(Source);

PluginDocGen = New PluginDocGen;
BSLParser.HookUp(PluginDocGen);
BSLParser.Visit(Module);

TextWriter = New TextWriter("..\docs\index.html");
TextWriter.Write(PluginDocGen.Result());