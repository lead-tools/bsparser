
AttachScript("..\src\BSParser\Ext\ObjectModule.bsl", "BSParser");
AttachScript("..\plugins\DocGen\src\DocGen\Ext\ObjectModule.bsl", "PluginDocGen");

TextReader = New TextReader("..\src\BSParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();

PluginDocGen = New PluginDocGen;

BSParser = New BSParser;
BSParser.Go(Source, PluginDocGen);

TextWriter = New TextWriter("..\docs\index.html");
TextWriter.Write(PluginDocGen.Result());