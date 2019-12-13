
AttachScript("..\src\BSParser\Ext\ObjectModule.bsl", "BSParser");
AttachScript(".\TestRunner\src\TestRunner\Ext\ObjectModule.bsl", "TestRunner");


BSParser = New BSParser;
TestRunner = New TestRunner;

TestRunner.Run(BSParser);