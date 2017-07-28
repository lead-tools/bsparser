
#Region Constants

&AtClient
Var Keywords;

&AtClient
Var Tokens;

&AtClient
Var TokenToString;

#EndRegion // Constants

#Region EventHandlers

&AtClient
Procedure Translate(Command)
	
	PrepareEnums();
	
	TokenToString = New Array(Tokens.Count());
	For Each Item In Tokens Do
		TokenToString[Item.Value] = Item.Key;
	EndDo;
	
	Result.Clear();
	
	Scanner = Scanner(Source.GetText());
	While Scan(Scanner) <> Tokens.Eof Do
		If Verbose Then
			Result.AddLine(StrTemplate("%1: %2 -- `%3`", Scanner.Line, TokenToString[Scanner.Tok], Scanner.Lit));
		EndIf; 
	EndDo; 
	
EndProcedure // Translate()

#EndRegion // EventHandlers

#Region Enums

&AtClient
Procedure PrepareEnums()
	
	Keywords = Enum(New Structure,
		"If, Then, ElsIf, Else, EndIf, For, Each, In, To, While, Do, EndDo,
		|Procedure, EndProcedure, Function, EndFunction,
		|Var, Return, Continue, Break,
		|And, Or, Not,
		|Try, Except, Raise, EndTry,
		|New, Execute, Export"
	);
	
	Tokens = Enum(New Structure(Keywords),
		
		// Literals
		
		"Ident, Number, String, DateTime,
		// parts of strings
		|StringBeg, StringMid, StringEnd,
		
		// Operators
		
		// = < > <= >= + - * / %
		|Eql, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod,
		// ( ) [ ] { }
		|Lparen, Rparen, Lbrack, Rbrack, Lbrace, Rbrace,
		// ? , . : ;
		|Ternary, Comma, Period, Colon, Semicolon,
		
		// Other
		
		|Illegal, Eof, Comment"
		
	);
	
EndProcedure // PrepareEnums()

&AtClientAtServerNoContext
Function Enum(Structure, Keys)
	Var i;
	
	i = Structure.Count();
	For Each Item In StrSplit(Keys, ",", False) Do
		Structure.Insert(Item, i);
		i = i + 1;
	EndDo;
	
	Return New FixedStructure(Structure);
	
EndFunction // Enum()

#EndRegion // Enums

#Region Scanner

&AtClientAtServerNoContext
Function Scanner(Source)
	Var Scanner;
	
	Scanner = New Structure(
		"Source,
		|Len,
		|Pos,
		|Tok,
		|Lit,
		|Char,
		|Line,
		|Error"
	);
	
	Scanner.Source = Source;
	Scanner.Len = StrLen(Source);
	Scanner.Line = 1;
	Scanner.Pos = 0;
	
	Return Scanner;
	
EndFunction // Scanner() 

&AtClient
Function Scan(Scanner)
	Var Char, Tok, Lit;
	SkipWhitespace(Scanner);
	Char = Scanner.Char;
	If IsLetter(Char) Then
		Lit = ScanIdentifier(Scanner);
		Tok = Lookup(Lit);
	ElsIf IsDigit(Char) Then 
		Lit = ScanNumber(Scanner);
		Tok = Tokens.Number;
	ElsIf Char = """" Or Char = "|" Then
		Lit = ScanString(Scanner);
		Tok = StringToken(Lit);
	ElsIf Char = "'" Then
		Lit = ScanDateTime(Scanner);
		Tok = Tokens.DateTime;
		NextChar(Scanner);
	ElsIf Char = "=" Then
		Tok = Tokens.Eql;
		NextChar(Scanner);
	ElsIf Char = "<" Then
		If NextChar(Scanner) = "=" Then
			Lit = "<=";
			Tok = Tokens.Leq;
			NextChar(Scanner);
		Else
			Tok = Tokens.Lss;
		EndIf;
	ElsIf Char = ">" Then
		If NextChar(Scanner) = "=" Then
			Lit = ">=";
			Tok = Tokens.Geq;
			NextChar(Scanner);
		Else
			Tok = Tokens.Gtr;
		EndIf;
	ElsIf Char = "+" Then
		Tok = Tokens.Add;
		NextChar(Scanner);
	ElsIf Char = "-" Then
		Tok = Tokens.Sub;
		NextChar(Scanner);
	ElsIf Char = "*" Then
		Tok = Tokens.Mul;
		NextChar(Scanner);
	ElsIf Char = "/" Then
		If NextChar(Scanner) = "/" Then
			Lit = ScanComment(Scanner);
			Tok = Tokens.Comment;
		Else
			Tok = Tokens.Div;
		EndIf;
	ElsIf Char = "%" Then
		Tok = Tokens.Mod;
		NextChar(Scanner);
	ElsIf Char = "(" Then
		Tok = Tokens.Lparen;
		NextChar(Scanner);
	ElsIf Char = ")" Then
		Tok = Tokens.Rparen;
		NextChar(Scanner);
	ElsIf Char = "[" Then
		Tok = Tokens.Lbrack;
		NextChar(Scanner);
	ElsIf Char = "]" Then
		Tok = Tokens.Rbrack;
		NextChar(Scanner);
	ElsIf Char = "?" Then
		Tok = Tokens.Ternary;
		NextChar(Scanner);
	ElsIf Char = "," Then
		Tok = Tokens.Comma;
		NextChar(Scanner);
	ElsIf Char = "." Then
		Tok = Tokens.Period;
		NextChar(Scanner);
	ElsIf Char = ":" Then
		Tok = Tokens.Colon;
		NextChar(Scanner);
	ElsIf Char = ";" Then
		Tok = Tokens.Semicolon;
		NextChar(Scanner);
	ElsIf Char = "" Then
		Tok = Tokens.Eof;
	ElsIf Char = "&" Or Char = "#" Then
		Lit = ScanComment(Scanner);
		Tok = Tokens.Comment;
	Else
		Raise "Unknown char";
	EndIf; 
	If ValueIsFilled(Lit) Then
		Scanner.Lit = Lit;
	Else
		Scanner.Lit = Char;
	EndIf; 	
	Scanner.Tok = Tok; 
	Return Tok;
EndFunction // Scan() 

&AtClientAtServerNoContext
Function NextChar(Scanner)
	If Scanner.Char <> "" Then
		Scanner.Pos = Scanner.Pos + 1;
		Scanner.Char = Mid(Scanner.Source, Scanner.Pos, 1); 
	EndIf; 
	Return Scanner.Char;
EndFunction // NextChar()  

&AtClientAtServerNoContext
Function SkipWhitespace(Scanner)
	Var Char;
	Char = Scanner.Char;
	While IsBlankString(Char) And Char <> "" Do
		If Char = Chars.LF Then
			Scanner.Line = Scanner.Line + 1;
		EndIf; 
		Char = NextChar(Scanner);
	EndDo; 
EndFunction // SkipWhitespace() 

&AtClientAtServerNoContext
Function ScanComment(Scanner)
	Var Len, Char;
	Len = 0;
	Char = NextChar(Scanner);
	While Char <> Chars.LF And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanComment()

&AtClientAtServerNoContext
Function ScanIdentifier(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While IsLetter(Char) Or IsDigit(Char) Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanIdentifier()

&AtClientAtServerNoContext
Function ScanNumber(Scanner)
	Var Len;
	Len = ScanIntegerLen(Scanner); // Len >= 1
	If Scanner.Char = "." Then
		Len = Len + ScanIntegerLen(Scanner);	
	EndIf; 
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanNumber()

&AtClientAtServerNoContext
Function ScanIntegerLen(Scanner)
	Var Len;
	Len = 1;
	While IsDigit(NextChar(Scanner)) Do
		Len = Len + 1;
	EndDo;
	Return Len;
EndFunction // ScanIntegerLen()

&AtClientAtServerNoContext
Function ScanString(Scanner)
	Var Len;
	Len = ScanStringLen(Scanner);
	While NextChar(Scanner) = """" Do
		Len = Len + ScanStringLen(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);
EndFunction // ScanString()

&AtClientAtServerNoContext
Function ScanStringLen(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While Char <> """" And Char <> Chars.LF And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	If Char = Chars.LF Then
		Scanner.Line = Scanner.Line + 1;
	EndIf;
	Return Len + ?(Char <> "", 1, 0);
EndFunction // ScanStringLen()

&AtClientAtServerNoContext
Function ScanDateTime(Scanner)
	Var Len, Char;
	Len = 1;
	Char = NextChar(Scanner);
	While Char <> "'" And Char <> "" Do
		Len = Len + 1;
		Char = NextChar(Scanner);
	EndDo;
	Return Mid(Scanner.Source, Scanner.Pos - Len, Len);	
EndFunction // ScanDateTime() 

#EndRegion // Scanner

#Region Auxiliary

&AtClient
Function StringToken(Lit)
	If Left(Lit, 1) = """" Then
		If Right(Lit, 1) = """" Then
			Return Tokens.String;
		Else
			Return Tokens.StringBeg;
		EndIf; 		
	Else // |
		If Right(Lit, 1) = """" Then
			Return Tokens.StringEnd;
		Else
			Return Tokens.StringMid;
		EndIf;
	EndIf; 	
EndFunction // StringToken()

&AtClient
Function Lookup(Lit)
	Var Tok;
	If Not Keywords.Property(Lit, Tok) Then
		Tok = Tokens.Ident;
	EndIf; 
	Return Tok;
EndFunction // Lookup() 

&AtClientAtServerNoContext
Function IsLetter(Char)
	Return "a" <= Char And Char <= "z" Or "A" <= Char And Char <= "Z" Or Char = "_";
EndFunction // IsLetter()

&AtClientAtServerNoContext
Function IsDigit(Char)
	Return "0" <= Char And Char <= "9";	
EndFunction // IsLetter()

#EndRegion // Auxiliary
