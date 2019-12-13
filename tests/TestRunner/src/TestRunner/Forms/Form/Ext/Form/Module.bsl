
&AtServer
Procedure RunAtServer()

	This = FormAttributeToValue("Object");
	ThisFile = New File(This.UsedFileName);
	BSParser = ExternalDataProcessors.Create(ThisFile.Path + "BSParser.epf", False);
	This.Run(BSParser);

EndProcedure

&AtClient
Procedure Run(Command)
	RunAtServer();
EndProcedure
