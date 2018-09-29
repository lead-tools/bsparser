
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript("..\plugins\DocGen\src\DocGen\Ext\ObjectModule.bsl", "PluginDocGen");

TextReader = New TextReader("..\src\BSLParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

PluginDocGen = New PluginDocGen;

BSLParser = New BSLParser;
BSLParser.Go(Source, PluginDocGen);

TextWriter = New TextWriter("..\docs\index.html");
TextWriter.Write(PluginDocGen.Result());