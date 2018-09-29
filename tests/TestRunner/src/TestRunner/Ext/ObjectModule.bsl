
Procedure Run(Parser) Export

	Parser.Parse(
	"x = x + 1;
	|");

	Errors = Parser.Errors();

	If Errors.Count() <> 1
		Or Not Match(Errors[0],
			"Undeclared identifier `x`", 1, 5) Then
		Raise "Test failed"
	EndIf;

	Message("All tests passed!");

EndProcedure

Function Match(Error, Text, Line, Pos)
	If StrFind(Error.Text, Text) > 0
		And Error.Line = Line
		And Error.Pos = Pos Then
		Return True;
	EndIf;
	Return False;
EndFunction