
Var Parser;

Procedure Run(BSParser) Export

	Parser = BSParser;

	//#Region Correct

	If Not Correct("Var x; x = x + 1") Then
		Raise "Test failed"
	EndIf;

	If Not Correct("Var x; x = +x + -1") Then
		Raise "Test failed"
	EndIf;

	If Not Correct("Var x; y = ?(x, 1, 2).y(1)") Then
		Raise "Test failed"
	EndIf;

	If Not Correct("Var x; y = x[0]") Then
		Raise "Test failed"
	EndIf;

	If Not Correct("Var x; y = x[0].z()") Then
		Raise "Test failed"
	EndIf;

	If Not Correct("Var x; y = x[0].z[1].q.q()") Then
		Raise "Test failed"
	EndIf;

	If Not Correct("Var x; x.y = x") Then
		Raise "Test failed"
	EndIf;

	//#EndRegion // Correct

	//#Region Failed

	If Not Failed("x = x + 1", "Undeclared identifier `x`", 1, 5) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("x = y + 1", "Undeclared identifier `y`", 1, 5) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("x[0] = 1 + 1", "Undeclared identifier `x`", 1, 1) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("For Each x In x Do EndDo", "Undeclared identifier `x`", 1, 15) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("For Each In x do EndDo", "Expected Ident", 1, 10) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("For x In x do EndDo", "Expected Eql", 1, 7) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("For x = 1 In x do EndDo", "Expected To", 1, 11) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("For x = 1 To 2 EndDo", "Expected Do", 1, 16) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("x = .1", "Expected operand", 1, 5) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("x = 1.x", "Expected Eof", 1, 7) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("x = 1 1", "Expected Eof", 1, 7) Then
		Raise "Test failed"
	EndIf;

	If Not Failed("x = / 2", "Expected operand", 1, 5) Then
		Raise "Test failed"
	EndIf;

	//#EndRegion // Failed

	Message("All tests passed!");

EndProcedure

Function Correct(Src)

	Try
		Parser.Parse(Src);
	Except
		Return False;
	EndTry;

	If Parser.Errors().Count() > 0 Then
		Return False;
	EndIf;

	Return True;

EndFunction

Function Failed(Src, ErrorText, Line, Pos)

	Try
		Parser.Parse(Src);
	Except
		If StrFind(ErrorInfo().Description, ErrorText) = 0 Then
			Return False;
		EndIf;
	EndTry;

	Errors = Parser.Errors();

	If Errors.Count() <> 1
		Or Not MatchError(Errors[0],
		ErrorText, Line, Pos) Then
		Return False;
	EndIf;

	Return True;

EndFunction

Function MatchError(Error, Text, Line, Pos)
	If StrFind(Error.Text, Text) > 0
		And Error.Line = Line
		And Error.Pos = Pos Then
		Return True;
	EndIf;
	Return False;
EndFunction