
AttachScript("..\src\BSLParser\Ext\ObjectModule.bsl", "BSLParser");
AttachScript(".\TestRunner\src\TestRunner\Ext\ObjectModule.bsl", "TestRunner");


BSLParser = New BSLParser;
TestRunner = New TestRunner;

TestRunner.Run(BSLParser);