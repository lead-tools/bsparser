
&AtServer
Procedure RunAtServer()
	
	This = FormAttributeToValue("Object");
	ThisFile = New File(This.UsedFileName);
	BSLParser = ExternalDataProcessors.Create(ThisFile.Path + "BSLParser.epf", False);
	This.Run(BSLParser);
	
EndProcedure

&AtClient
Procedure Run(Command)
	RunAtServer();
EndProcedure
