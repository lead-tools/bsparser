
#Region Constants

Var Keywords;         // enum
Var Tokens;           // enum
Var Nodes;            // enum
Var Directives;       // enum
Var PrepInstructions; // enum
Var PrepSymbols;      // enum
Var BasicLitNoString; // array (one of Tokens)
Var RelOperators;     // array (one of Tokens)
Var AddOperators;     // array (one of Tokens)
Var MulOperators;     // array (one of Tokens)
Var InitOfExpression; // array (one of Tokens)
Var EmptyArray;       // array
Var TokenMap;         // map[string] (string)
Var AlphaDigitMap;    // map[string] (string)
Var Alpha, Digit;     // string
Var Chars_LF;         // string

#EndRegion // Constants

#Region Settings

Var Verbose Export;  // boolean
Var Debug Export;    // boolean
Var Location Export; // boolean

#EndRegion // Settings

#Region ParserState

Var Parser_Source;    // string
Var Parser_Len;       // number
Var Parser_CurLine;   // number
Var Parser_EndLine;   // number
Var Parser_CurPos;    // number
Var Parser_BegPos;    // number
Var Parser_EndPos;    // number
Var Parser_Char;      // string
Var Parser_Tok;       // string (one of Tokens)
Var Parser_Lit;       // string
Var Parser_Val;       // number, string, date, boolean, undefined, null
Var Parser_Scope;     // structure (Scope)
Var Parser_Vars;      // structure as map[string] (Object)
Var Parser_Methods;   // structure as map[string] (Object)
Var Parser_Unknown;   // structure as map[string] (Object)
Var Parser_IsFunc;    // boolean
Var Parser_AllowVar;  // boolean
Var Parser_Directive; // string (one of Directives)
Var Parser_Interface; // array (Object)
Var Parser_Comments;  // map[number] (string)

#EndRegion // ParserState

#Region VisitorState

Var Visitor_Plugins;  // array (DataProcessorObject)
Var Visitor_Hooks;    // structure as map[string] (array)
Var Visitor_Stack;    // structure
Var Visitor_Counters; // structure as map[string] (number)

#EndRegion // VisitorState

#Region Init

Procedure Init()
	Var Letters, Index, Char;

	Verbose = False;
	Debug = False;
	Location = True;

	InitEnums();

	BasicLitNoString = New Array;
	BasicLitNoString.Add(Tokens.Number);
	BasicLitNoString.Add(Tokens.DateTime);
	BasicLitNoString.Add(Tokens.True);
	BasicLitNoString.Add(Tokens.False);
	BasicLitNoString.Add(Tokens.Undefined);
	BasicLitNoString.Add(Tokens.Null);

	RelOperators = New Array;
	RelOperators.Add(Tokens.Eql);
	RelOperators.Add(Tokens.Neq);
	RelOperators.Add(Tokens.Lss);
	RelOperators.Add(Tokens.Gtr);
	RelOperators.Add(Tokens.Leq);
	RelOperators.Add(Tokens.Geq);

	AddOperators = New Array;
	AddOperators.Add(Tokens.Add);
	AddOperators.Add(Tokens.Sub);

	MulOperators = New Array;
	MulOperators.Add(Tokens.Mul);
	MulOperators.Add(Tokens.Div);
	MulOperators.Add(Tokens.Mod);

	InitOfExpression = New Array;
	InitOfExpression.Add(Tokens.Add);
	InitOfExpression.Add(Tokens.Sub);
	InitOfExpression.Add(Tokens.Not);
	InitOfExpression.Add(Tokens.Ident);
	InitOfExpression.Add(Tokens.Lparen);
	InitOfExpression.Add(Tokens.Number);
	InitOfExpression.Add(Tokens.String);
	InitOfExpression.Add(Tokens.StringBeg);
	InitOfExpression.Add(Tokens.DateTime);
	InitOfExpression.Add(Tokens.Ternary);
	InitOfExpression.Add(Tokens.New);
	InitOfExpression.Add(Tokens.True);
	InitOfExpression.Add(Tokens.False);
	InitOfExpression.Add(Tokens.Undefined);
	InitOfExpression.Add(Tokens.Null);

	EmptyArray = New Array;

	Chars_LF = Chars.LF;

	Alpha = "Alpha";
	Digit = "Digit";

	TokenMap = New Map;
	AlphaDigitMap = New Map;

	Letters = (
		"abcdefghijklmnopqrstuvwxyz"
		+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		+ "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
		+ "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
	);
	Index = 1;
	Char = "_";
	While Char <> "" Do
		TokenMap[Char] = Alpha;
		AlphaDigitMap[Char] = Alpha;
		Char = Mid(Letters, Index, 1);
		Index = Index + 1;
	EndDo;

	For Index = 0 To 9 Do
		TokenMap[String(Index)] = Digit;
		AlphaDigitMap[String(Index)] = Digit;
	EndDo;

	TokenMap[""""] = Tokens.String;
	TokenMap["|"] = Tokens.String;
	TokenMap["'"] = Tokens.DateTime;
	TokenMap["="] = Tokens.Eql;
	TokenMap["+"] = Tokens.Add;
	TokenMap["-"] = Tokens.Sub;
	TokenMap["*"] = Tokens.Mul;
	TokenMap["%"] = Tokens.Mod;
	TokenMap["("] = Tokens.Lparen;
	TokenMap[")"] = Tokens.Rparen;
	TokenMap["["] = Tokens.Lbrack;
	TokenMap["]"] = Tokens.Rbrack;
	TokenMap["?"] = Tokens.Ternary;
	TokenMap[","] = Tokens.Comma;
	TokenMap["."] = Tokens.Period;
	TokenMap[":"] = Tokens.Colon;
	TokenMap[";"] = Tokens.Semicolon;
	TokenMap[""] = Tokens.Eof;

EndProcedure // Init()

Procedure InitEnums()
	Keywords = Keywords();
	Tokens = Tokens(Keywords);
	Nodes = Nodes();
	Directives = Directives();
	PrepInstructions = PrepInstructions();
	PrepSymbols = PrepSymbols();
EndProcedure // InitEnums()

#EndRegion // Init

#Region Enums

Function Keywords() Export
	Return Enum(New Structure,
		"If.Если, Then.Тогда, ElsIf.ИначеЕсли, Else.Иначе, EndIf.КонецЕсли,
		|For.Для, Each.Каждого, In.Из, To.По, While.Пока, Do.Цикл, EndDo.КонецЦикла,
		|Procedure.Процедура, EndProcedure.КонецПроцедуры, Function.Функция, EndFunction.КонецФункции,
		|Var.Перем, Val.Знач, Return.Возврат, Continue.Продолжить, Break.Прервать,
		|And.И, Or.Или, Not.Не,
		|Try.Попытка, Except.Исключение, Raise.ВызватьИсключение, EndTry.КонецПопытки,
		|New.Новый, Execute.Выполнить, Export.Экспорт, Goto.Перейти,
		|True.Истина, False.Ложь, Undefined.Неопределено, Null"
	);
EndFunction // Keywords()

Function Tokens(Keywords = Undefined) Export
	Var Tokens;

	If Keywords = Undefined Then
		Keywords = Keywords();
	EndIf;

	Tokens = Enum(New Structure(Keywords),

		// Literals

		"Ident, Number, String, DateTime,
		// parts of strings
		|StringBeg, StringMid, StringEnd,

		// Operators

		// =   <>    <    >   <=   >=    +    -    *    /    %
		|Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod,
		//    (       )       [       ]
		|Lparen, Rparen, Lbrack, Rbrack,
		//     ?      ,       .      :          ;
		|Ternary, Comma, Period, Colon, Semicolon,

		// Preprocessor instructions
		|_If, _ElsIf, _Else, _EndIf, _Region, _EndRegion, _Use,

		// Other

		//         //          &      ~
		|Eof, Comment, Directive, Label"

	);

	Return Tokens;
EndFunction // Tokens()

Function Nodes() Export
	Return Enum(New Structure,
		"Module, Object,
		|VarModDecl, VarModListDecl, VarLocDecl, AutoDecl, ParamDecl, MethodDecl, ProcSign, FuncSign,
		|BasicLitExpr, FieldExpr, IndexExpr, IdentExpr, UnaryExpr, BinaryExpr, NewExpr, TernaryExpr, ParenExpr, NotExpr, StringExpr,
		|AssignStmt, ReturnStmt, BreakStmt, ContinueStmt, RaiseStmt, ExecuteStmt, WhileStmt, ForStmt, ForEachStmt,
		|TryStmt, ExceptStmt, GotoStmt, LabelStmt, CallStmt, IfStmt, ElsIfStmt, ElseStmt,
		|PrepIfInst, PrepElsIfInst, PrepElseInst, PrepEndIfInst, PrepRegionInst, PrepEndRegionInst,
		|PrepBinaryExpr, PrepNotExpr, PrepSymExpr, PrepUseInst"
	);
EndFunction // Nodes()

Function Directives() Export
	Return Enum(New Structure,
		"AtClient.НаКлиенте,"
		"AtServer.НаСервере,"
		"AtServerNoContext.НаСервереБезКонтекста,"
		"AtClientAtServerNoContext.НаКлиентеНаСервереБезКонтекста,"
		"AtClientAtServer.НаКлиентеНаСервере"
	);
EndFunction // Directives()

Function PrepInstructions() Export
	Return Enum(New Structure,
		"If.Если,"
		"ElsIf.ИначеЕсли,"
		"Else.Иначе,"
		"EndIf.КонецЕсли,"
		"Region.Область,"
		"EndRegion.КонецОбласти,"
		"Use.Использовать" // onescript
	);
EndFunction // PrepInstructions()

Function PrepSymbols() Export
	Return Enum(New Structure,
		"Client.Клиент,"
		"AtClient.НаКлиенте,"
		"AtServer.НаСервере,"
		"MobileAppClient.МобильноеПриложениеКлиент,"
		"MobileAppServer.МобильноеПриложениеСервер,"
		"ThickClientOrdinaryApplication.ТолстыйКлиентОбычноеПриложение,"
		"ThickClientManagedApplication.ТолстыйКлиентУправляемоеПриложение,"
		"Server.Сервер,"
		"ExternalConnection.ВнешнееСоединение,"
		"ThinClient.ТонкийКлиент,"
		"WebClient.ВебКлиент"
	);
EndFunction // PrepSymbols()

Function Enum(Structure, Keys)
	Var Items, Item, ItemList, Value;

	For Each Items In StrSplit(Keys, ",", False) Do
		ItemList = StrSplit(Items, ".", False);
		Value = TrimAll(ItemList[0]);
		For Each Item In ItemList Do
			Structure.Insert(TrimAll(Item), Value);
		EndDo;
	EndDo;

	Return New FixedStructure(Structure);
EndFunction // Enum()

#EndRegion // Enums

#Region AbstractSyntaxTree

Function Module(Decls, Auto, Statements, Interface, Comments)
	// Корень AST. Узел хранит информацию о модуле в целом.
	Return New Structure( // @Node
		"Type,"      // string (one of Nodes)
		"Decls,"     // array (one of #Declarations)
		"Auto,"      // array (Object)
		"Body,"      // array (one of #Statements)
		"Interface," // array (Object)
		"Comments",  // map[number] (string)
		Nodes.Module, Decls, Auto, Statements, Interface, Comments);
EndFunction // Module()

#Region Scope

Function Scope(Outer)
	Return New Structure(
		"Outer,"   // undefined, structure (Scope)
		"Objects," // structure as map[string] (Object)
		"Auto",    // array (Object)
		Outer, New Structure, New Array);
EndFunction // Scope()

Function Object(Name, Decl = Undefined)
	// Узел хранит информацию об объекте области видимости.
	// Поле Decl хранит объявление данного объекта (undefined = объявление не обнаружено).
	Return New Structure( // @Node
		"Type," // string (one of Nodes)
		"Name," // string
		"Decl", // undefined, structure (one of #Declarations)
		Nodes.Object, Name, Decl);
EndFunction // Object()

#EndRegion // Scope

#Region Declarations

Function VarModListDecl(Directive, VarList, Place)
	// Хранит информацию об инструкции объявления переменных уровня модуля.
	// Пример:
	// <pre>
	// &НаКлиенте            // поле "Directive"
	// Перем П1 Экспорт, П2; // поле "List"
	// </pre>
	Return New Structure( // @Node
		"Type,"      // string (one of Nodes)
		"Directive," // string (one of Directives)
		"List,"      // array (VarModDecl)
		"Place",     // number, structure (Place)
		Nodes.VarModListDecl, Directive, VarList, Place);
EndFunction // VarModListDecl()

Function VarModDecl(Name, Directive, Exported, Place)
	// Хранит информацию об объявлении переменной уровня модуля.
	// Пример:
	// Объявления переменных заключены в скобки <...>
	// <pre>
	// &НаКлиенте
	// Перем <П1 Экспорт>, <П2>;
	// </pre>
	Return New Structure( // @Node
		"Type,"      // string (one of Nodes)
		"Name,"      // string
		"Directive," // string (one of Directives)
		"Export,"    // boolean
		"Place",     // number, structure (Place)
		Nodes.VarModDecl, Name, Directive, Exported, Place);
EndFunction // VarModDecl()

Function VarLocDecl(Name, Place)
	// Хранит информацию об объявлении локальной переменной.
	// Пример:
	// Объявления переменных заключены в скобки <...>
	// <pre>
	// Перем <П1>, <П2>;
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Name,"  // string
		"Place", // number, structure (Place)
		Nodes.VarLocDecl, Name, Place);
EndFunction // VarLocDecl()

Function AutoDecl(Place)
	// Хранит информацию об объявлении авто-переменной.
	// Пример:
	// Объявления переменных заключены в скобки <...>
	// <pre>
	// <Макс> = 0;
	// Для <Индекс> = 0 По Массив.ВГраница() Цикл
	//	<Структура> = Массив[Индекс];
	// 	Для Каждого <Элемент> Из Структура Цикл
	//		Если Макс < Элемент.Значение Тогда
	// 			Макс = Элемент.Значение;
	// 		КонецЕсли;
	// 	КонецЦикла;
	// КонецЦикла
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Place", // number, structure (Place)
		Nodes.AutoDecl, Place);
EndFunction // AutoDecl()

Function ParamDecl(Name, ByVal, Value = Undefined, Place)
	// Хранит информацию об объявлении параметра.
	// Пример:
	// Объявления параметров заключены в скобки <...>
	// <pre>
	// Процедура(<П1>, <Знач П2 = Неопределено>)
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Name,"  // string
		"ByVal," // boolean
		"Value," // undefined, structure (UnaryExpr, BasicLitExpr)
		"Place", // number, structure (Place)
		Nodes.ParamDecl, Name, ByVal, Value, Place);
EndFunction // ParamDecl()

Function MethodDecl(Sign, Decls, Auto, Body, Place)
	// Хранит информацию об объявлении метода.
	// Сигнатура метода хранится в поле Sign.
	// &НаКлиенте
	// Функция Тест() Экспорт
	//     Перем П1;    // поле "Vars" хранит объявления переменных.
	//     П1 = 2;      // операторы собираются в поле Body
	//     П2 = П1 + 2; // Авто-переменные (П2) собираются в поле "Auto".
	// КонецФункции
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Sign,"  // structure (ProcSign, FuncSign)
		"Vars,"  // array (VarLocDecl)
		"Auto,"  // array (Object)
		"Body,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.MethodDecl, Sign, Decls, Auto, Body, Place);
EndFunction // MethodDecl()

Function ProcSign(Name, Directive, Params, Exported, Place)
	// Хранит информацию о сигнатуре объявления процедуры.
	// Пример:
	// <pre>
	// &НаКлиенте
	// Процедура Тест(П1, П2) Экспорт
	// </pre>
	Return New Structure( // @Node
		"Type,"      // string (one of Nodes)
		"Name,"      // string
		"Directive," // string (one of Directives)
		"Params,"    // array (ParamDecl)
		"Export,"    // boolean
		"Place",     // number, structure (Place)
		Nodes.ProcSign, Name, Directive, Params, Exported, Place);
EndFunction // ProcSign()

Function FuncSign(Name, Directive, Params, Exported, Place)
	// Хранит информацию о сигнатуре объявления функции.
	// Пример:
	// <pre>
	// &НаКлиенте
	// Функция Тест(П1, П2) Экспорт
	// </pre>
	Return New Structure( // @Node
		"Type,"      // string (one of Nodes)
		"Name,"      // string
		"Directive," // string (one of Directives)
		"Params,"    // array (ParamDecl)
		"Export,"    // boolean
		"Place",     // number, structure (Place)
		Nodes.FuncSign, Name, Directive, Params, Exported, Place);
EndFunction // FuncSign()

#EndRegion // Declarations

#Region Expressions

Function BasicLitExpr(Kind, Value, Place)
	// Хранит информацию о литерале примитивного типа.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Kind,"  // string (one of Tokens)
		"Value," // undefined, string, number, boolean, date, null
		"Place", // number, structure (Place)
		Nodes.BasicLitExpr, Kind, Value, Place);
EndFunction // BasicLitExpr()

Function FieldExpr(Name, Args, Place)
	// Хранит информацию об обращении к полю объекта через точку.
	// В поле Name содержится имя поля.
	// В поле Args содержатся аргументы вызова (если это вызов).
	// Пример:
	// <pre>
	// // обращения через точку заключены в скобки <...>
	// Значение = Объект<.Поле>
	// Значение = Объект<.Добавить(П1, П2)>
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Name,"  // string
		"Args,"  // undefined, array (undefined, one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.FieldExpr, Name, Args, Place);
EndFunction // FieldExpr()

Function IndexExpr(Expr, Place)
	// Хранит информацию об обращении к элементу объекта по индексу.
	// Пример:
	// <pre>
	// // обращение по индексу заключено в скобки <...>
	// Значение = Объект<[Ключ]>
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.IndexExpr, Expr, Place);
EndFunction // IndexExpr()

Function IdentExpr(Object, Tail, Args, Place)
	// Хранит информацию об обращении к идентификатору.
	// В поле Head содержится объект области видимости соответствующий идентификатору.
	// В поле Tail содержится последовательность обращений через точку и по индексу.
	// В поле Args содержатся аргументы вызова (если это вызов).
	// Пример:
	// <pre>
	// // идентификатор заключен в скобки <...>
	// // поле "Head" будет содержать объект переменной "Запрос";
	// // поле "Tail" будет содержать три обращения;
	// // поле "Args" будет равно Неопределено, т.к. обращение к "Запрос" не является вызовом.
	// Возврат <Запрос.Выполнить().Выгрузить()[0]>;
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Head,"  // structure (Object)
		"Tail,"  // array (FieldExpr, IndexExpr)
		"Args,"  // undefined, array (undefined, one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.IdentExpr, Object, Tail, Args, Place);
EndFunction // IdentExpr()

Function UnaryExpr(Operator, Operand, Place)
	// Хранит унарное выражение.
	// Пример:
	// <pre>
	// // унарные выражения заключены в скобки <...>
	// // поле "Operator" равно либо Tokens.Add, либо Tokens.Sub
	// // поле "Operand" хранит операнд-выражение
	// Значение = <-Сумма> * 2;
	// Значение = <+Сумма>;
	// Значение = <-(Сумма1 + Сумма2)> / 2;
	// </pre>
	Return New Structure( // @Node
		"Type,"     // string (one of Nodes)
		"Operator," // string (one of Tokens)
		"Operand,"  // structure (one of #Expressions)
		"Place",    // number, structure (Place)
		Nodes.UnaryExpr, Operator, Operand, Place);
EndFunction // UnaryExpr()

Function BinaryExpr(Left, Operator, Right, Place)
	// Хранит бинарное выражение.
	// Пример:
	// <pre>
	// // бинарные выражения заключены в скобки <...>
	// // поле "Operator" равно одному из допустимых операторов:
	// // - логических (кроме "Не")
	// // - реляционных
	// // - арифметических
	// // поля "Left" и "Right" содержат операнды-выражения
	// Если <Не Отмена И Продолжить> Тогда
	//     Значение = <Сумма1 + <Сумма2 * Коэффициент>>;
	// КонецЕсли;
	// </pre>
	Return New Structure( // @Node
		"Type,"     // string (one of Nodes)
		"Left,"     // structure (one of #Expressions)
		"Operator," // string (one of Tokens)
		"Right,"    // structure (one of #Expressions)
		"Place",    // number, structure (Place)
		Nodes.BinaryExpr, Left, Operator, Right, Place);
EndFunction // BinaryExpr()

Function NewExpr(Name, Args, Place)
	// Хранит выражение "Новый".
	// Пример:
	// <pre>
	// // выражения "Новый" заключены в скобки <...>
	// // в этом варианте поле "Name" хранит имя типа "Массив",
	// // а поле "Args" хранит массив из одного выражения
	// Параметры = <Новый Массив(1)>;
	// Параметры[0] = 10;
	// // в этом варианте поле "Name" равно Неопределено,
	// // а поле "Args" хранит массив из двух выражений
	// Массив = <Новый (Тип("Массив"), Параметры)>;
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Name,"  // undefined, string
		"Args,"  // array (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.NewExpr, Name, Args, Place);
EndFunction // NewExpr()

Function TernaryExpr(Cond, ThenPart, ElsePart, Tail, Place)
	// Хранит тернарное выражение "?(,,)".
	// Пример:
	// <pre>
	// Значение = ?(Ложь,   // поле "Cond"
	//     Неопределено,    // поле "Then"
	//     Новый Массив     // поле "Else"
	// ).Количество();      // поле "Tail"
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Cond,"  // structure (one of #Expressions)
		"Then,"  // structure (one of #Expressions)
		"Else,"  // structure (one of #Expressions)
		"Tail,"  // array (FieldExpr, IndexExpr)
		"Place", // number, structure (Place)
		Nodes.TernaryExpr, Cond, ThenPart, ElsePart, Tail, Place);
EndFunction // TernaryExpr()

Function ParenExpr(Expr, Place)
	// Хранит скобочное выражение.
	// Пример:
	// <pre>
	// // скобочное выражение заключено в скобки <...>
	// Сумма = <(Сумма1 + Сумма2)> * Количество;
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.ParenExpr, Expr, Place);
EndFunction // ParenExpr()

Function NotExpr(Expr, Place)
	// Хранит выражение, к которому применено логическое отрицание "Не".
	// Пример:
	// <pre>
	// // выражение-отрицание заключено в скобки <...>
	// НеРавны = <Не Сумма1 = Сумма2>;
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.NotExpr, Expr, Place);
EndFunction // NotExpr()

Function StringExpr(ExprList, Place)
	// Хранит строковое выражение.
	// Поле "List" хранит упорядоченный список частей строки.
	// Пример:
	// <pre>
	// Строка1 = "Часть1" "Часть2"; // эта строка состоит из двух частей типа Nodes.String
	// Строка2 =                    // эта строка состоит из пяти частей типа:
	// "Начало строки               // Nodes.StringBeg
	// | продолжение строки         // Nodes.StringMid
	// | еще продолжение строки     // Nodes.StringMid
	// | окончание строки"          // Nodes.StringEnd
	// "еще часть";                 // Nodes.String
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"List,"  // array (BasicLitExpr)
		"Place", // number, structure (Place)
		Nodes.StringExpr, ExprList, Place);
EndFunction // StringExpr()

#EndRegion // Expressions

#Region Statements

Function AssignStmt(Left, Right, Place)
	// Хранит оператор присваивания.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Left,"  // structure (IdentExpr)
		"Right," // structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.AssignStmt, Left, Right, Place);
EndFunction // AssignStmt()

Function ReturnStmt(Expr = Undefined, Place)
	// Хранит оператор "Возврат".
	// Поле "Expr" равно Неопределено если это возврат из процедуры.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // undefined, structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.ReturnStmt, Expr, Place);
EndFunction // ReturnStmt()

Function BreakStmt(Place)
	// Хранит оператор "Прервать".
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Place", // number, structure (Place)
		Nodes.BreakStmt, Place);
EndFunction // BreakStmt()

Function ContinueStmt(Place)
	// Хранит оператор "Продолжить".
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Place", // number, structure (Place)
		Nodes.ContinueStmt, Place);
EndFunction // ContinueStmt()

Function RaiseStmt(Expr = Undefined, Place)
	// Хранит оператор "ВызватьИсключение".
	// Поле "Expr" равно Неопределено если это вариант оператора без выражения.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // undefined, structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.RaiseStmt, Expr, Place);
EndFunction // RaiseStmt()

Function ExecuteStmt(Expr, Place)
	// Хранит оператор "Выполнить".
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // structure (one of #Expressions)
		"Place", // number, structure (Place)
		Nodes.ExecuteStmt, Expr, Place);
EndFunction // ExecuteStmt()

Function CallStmt(IdentExpr, Place)
	// Хранит вызов процедуры или функции как процедуры.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Ident," // structure (IdentExpr)
		"Place", // number, structure (Place)
		Nodes.CallStmt, IdentExpr, Place);
EndFunction // CallStmt()

Function IfStmt(Cond, ThenPart, ElsIfPart = Undefined, ElsePart = Undefined, Place)
	// Хранит оператор "Если".
	// Пример:
	// <pre>
	// Если Сумма > 0 Тогда // поле "Cond" хранит условие (выражение)
	//     // поле "Then" хранит операторы в этом блоке
	// ИначеЕсли Сумма = 0 Тогда
	//     // поле-массив "ElsIf" хранит последовательность блоков ИначеЕсли
	// Иначе
	//     // поле "Else" хранит операторы в этом блоке
	// КонецЕсли
	// </pre>
	// Поля "ElsIf" и "Else" равны Неопределено если
	// соответствующие блоки отсутствуют в исходном коде.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Cond,"  // structure (one of #Expressions)
		"Then,"  // array (one of #Statements)
		"ElsIf," // undefined, array (ElsIfStmt)
		"Else,"  // undefined, structure (ElseStmt)
		"Place", // number, structure (Place)
		Nodes.IfStmt, Cond, ThenPart, ElsIfPart, ElsePart, Place);
EndFunction // IfStmt()

Function ElseStmt(Body, Place)
	// Хранит блок "Иначе"
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Body,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.ElseStmt, Body, Place);
EndFunction // ElseStmt()

Function ElsIfStmt(Cond, ThenPart, Place)
	// Хранит блок "ИначеЕсли" оператора "Если".
	// Пример:
	// <pre>
	// ...
	// ИначеЕсли Сумма < 0 Тогда // поле "Cond" хранит условие (выражение)
	//     // поле "Then" хранит операторы в этом блоке
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Cond,"  // structure (one of #Expressions)
		"Then,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.ElsIfStmt, Cond, ThenPart, Place);
EndFunction // ElsIfStmt()

Function WhileStmt(Cond, Body, Place)
	// Хранит оператор цикла "Пока".
	// Пример:
	// <pre>
	// Пока Индекс > 0 Цикл // поле "Cond" хранит условие (выражение)
	//     // поле "Body" хранит операторы в этом блоке
	// КонецЦикла
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Cond,"  // structure (one of #Expressions)
		"Body,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.WhileStmt, Cond, Body, Place);
EndFunction // WhileStmt()

Function ForStmt(IdentExpr, From, Until, Body, Place)
	// Хранит оператор цикла "Для".
	// Пример:
	// <pre>
	// Для Индекс = 0      // поля "Ident" и "From" хранят переменную и выражение инициализации.
	//   По Длина - 1 Цикл // поле "To" хранит выражение границы
	//     // поле "Body" хранит операторы в этом блоке
	// КонецЦикла
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Ident," // structure (IdentExpr)
		"From,"  // structure (one of #Expressions)
		"To,"    // structure (one of #Expressions)
		"Body,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.ForStmt, IdentExpr, From, Until, Body, Place);
EndFunction // ForStmt()

Function ForEachStmt(IdentExpr, Collection, Body, Place)
	// Хранит оператор цикла "Для Каждого".
	// Пример:
	// <pre>
	// Для Каждого Элемент // поле "Ident" хранит переменную.
	//   Из Список Цикл    // поле "In" хранит выражение коллекции.
	//     // поле "Body" хранит операторы в этом блоке
	// КонецЦикла
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Ident," // structure (IdentExpr)
		"In,"    // structure (one of #Expressions)
		"Body,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.ForEachStmt, IdentExpr, Collection, Body, Place);
EndFunction // ForEachStmt()

Function TryStmt(TryPart, ExceptPart, Place)
	// Хранит оператор "Попытка"
	// Пример:
	// <pre>
	// Попытка
	//     // поле "Try" хранит операторы в этом блоке.
	// Исключение
	//     // поле "Except" хранит операторы в этом блоке
	// КонецПопытки
	// </pre>
	Return New Structure( // @Node
		"Type,"   // string (one of Nodes)
		"Try,"    // array (one of #Statements)
		"Except," // structure (ExceptStmt)
		"Place",  // number, structure (Place)
		Nodes.TryStmt, TryPart, ExceptPart, Place);
EndFunction // TryStmt()

Function ExceptStmt(Body, Place)
	// Хранит блок "Исключение"
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Body,"  // array (one of #Statements)
		"Place", // number, structure (Place)
		Nodes.ExceptStmt, Body, Place);
EndFunction // ExceptStmt()

Function GotoStmt(Label, Place)
	// Хранит оператор "Перейти"
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Label," // string
		"Place", // number, structure (Place)
		Nodes.GotoStmt, Label, Place);
EndFunction // GotoStmt()

Function LabelStmt(Label, Place)
	// Хранит оператор метки.
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Label," // string
		"Place", // number, structure (Place)
		Nodes.LabelStmt, Label, Place);
EndFunction // LabelStmt()

#EndRegion // Statements

#Region PrepInst

Function PrepIfInst(Cond, Place)
	// Хранит информацию об инструкции препроцессора #Если,
	// Пример:
	// <pre>
	// ...
	// #Если Сервер Тогда // поле "Cond" хранит условие (выражение)
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Cond,"  // structure (one of #PrepExpr)
		"Place", // number, structure (Place)
		Nodes.PrepIfInst, Cond, Place);
EndFunction // PrepIfInst()

Function PrepElsIfInst(Cond, Place)
	// Хранит информацию об инструкции препроцессора #ИначеЕсли
	// Пример:
	// <pre>
	// ...
	// #ИначеЕсли Клиент Тогда // поле "Cond" хранит условие (выражение)
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Cond,"  // structure (one of #PrepExpr)
		"Place", // number, structure (Place)
		Nodes.PrepElsIfInst, Cond, Place);
EndFunction // PrepElsIfInst()

Function PrepElseInst(Place)
	// Хранит информацию об инструкции препроцессора #Иначе
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Place", // number, structure (Place)
		Nodes.PrepElseInst, Place);
EndFunction // PrepElseInst()

Function PrepEndIfInst(Place)
	// Хранит информацию об инструкции препроцессора #КонецЕсли
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Place", // number, structure (Place)
		Nodes.PrepEndIfInst, Place);
EndFunction // PrepEndIfInst()

Function PrepRegionInst(Name, Place)
	// Хранит информацию об инструкции препроцессора #Обрасть,
	// Пример:
	// <pre>
	// ...
	// #Область Интерфейс   // поле "Name" хранит имя области
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Name,"  // string
		"Place", // number, structure (Place)
		Nodes.PrepRegionInst, Name, Place);
EndFunction // PrepRegionInst()

Function PrepEndRegionInst(Place)
	// Хранит информацию об инструкции препроцессора #КонецОбласти,
	// Пример:
	// <pre>
	// ...
	// #КонецОбласти
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Place", // number, structure (Place)
		Nodes.PrepEndRegionInst, Place);
EndFunction // PrepEndRegionInst()

Function PrepUseInst(Path, Place)
	// Хранит информацию об инструкции препроцессора #Использовать,
	// Это нестандартная инструкция из OneScript
	// Пример:
	// <pre>
	// #Использовать 1commands // поле "Path" хранит имя библиотеки или путь в кавычках
	// </pre>
	Return New Structure( // @Node @OneScript
		"Type,"  // string (one of Nodes)
		"Path,"  // string
		"Place", // number, structure (Place)
		Nodes.PrepUseInst, Path, Place);
EndFunction // PrepUseInst()

#EndRegion // PrepInst

#Region PrepExpr

Function PrepBinaryExpr(Left, Operator, Right, Place)
	// Хранит бинарное выражение препроцессора.
	// Пример:
	// <pre>
	// // бинарные выражения заключены в скобки <...>
	// // поле "Operator" равно либо Tokens.Or либо Tokens.And:
	// // поля "Left" и "Right" содержат операнды-выражения препроцессора
	// #Если <Сервер Или ВнешнееСоединение> Тогда
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"     // string (one of Nodes)
		"Left,"     // structure (one of #PrepExpr)
		"Operator," // string (one of Tokens)
		"Right,"    // structure (one of #PrepExpr)
		"Place",    // number, structure (Place)
		Nodes.PrepBinaryExpr, Left, Operator, Right, Place);
EndFunction // PrepBinaryExpr()

Function PrepNotExpr(Expr, Place)
	// Хранит выражение препроцессора, к которому применено логическое отрицание "Не".
	// Пример:
	// <pre>
	// // выражение-отрицание заключено в скобки <...>
	// #Если <Не ВебКлиент> Тогда
	// ...
	// </pre>
	Return New Structure( // @Node
		"Type,"  // string (one of Nodes)
		"Expr,"  // structure (one of #PrepExpr)
		"Place", // number, structure (Place)
		Nodes.PrepNotExpr, Expr, Place);
EndFunction // PrepNotExpr()

Function PrepSymExpr(Symbol, Exist, Place)
	// Узел хранит информацию о символе препроцессора.
	// Поле Exist = True если такой символ существует.
	// Пример:
	// <pre>
	// // символ заключен в скобки <...>
	// #Если <Сервер> Тогда
	// </pre>
	Return New Structure( // @Node
		"Type,"   // string (one of Nodes)
		"Symbol," // string (one of PrepSymbols)
		"Exist,"  // boolean
		"Place",  // number, structure (Place)
		Nodes.PrepSymExpr, Symbol, Exist, Place);
EndFunction // PrepSymExpr()

#EndRegion // PrepExpr

#EndRegion // AbstractSyntaxTree

#Region Parser

Function Scan()
	Var Beg, Prev, Comment;

	Parser_EndPos = Parser_CurPos;
	Parser_EndLine = Parser_CurLine;

	If Right(Parser_Lit, 1) = Chars_LF Then
		Parser_CurLine = Parser_CurLine + 1;
	EndIf;

	While True Do

		Comment = False;

		// skip space
		While IsBlankString(Parser_Char) And Parser_Char <> "" Do
			If Parser_Char = Chars_LF Then
				Parser_CurLine = Parser_CurLine + 1;
			EndIf;
			Parser_CurPos = Parser_CurPos + 1;
			Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
		EndDo;

		Parser_BegPos = Parser_CurPos;

		Parser_Tok = TokenMap[Parser_Char];
		If Parser_Tok = Alpha Then

			// scan ident
			Beg = Parser_CurPos;
			Parser_CurPos = Parser_CurPos + 1;
			While AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] <> Undefined Do
				Parser_CurPos = Parser_CurPos + 1;
			EndDo;
			Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
			Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);

			// lookup
			If Not Keywords.Property(Parser_Lit, Parser_Tok) Then
				Parser_Tok = Tokens.Ident;
			EndIf;

		ElsIf Parser_Tok = Tokens.String Then

			Beg = Parser_CurPos;
			Parser_Char = """"; // cheat code
			While Parser_Char = """" Do
				Parser_CurPos = Parser_CurPos + 1;
				Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				While Parser_Char <> """" And Parser_Char <> Chars_LF And Parser_Char <> "" Do
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				EndDo;
				If Parser_Char <> "" Then
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				EndIf;
			EndDo;
			Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);

			Parser_Tok = StringToken(Parser_Lit);

		ElsIf Parser_Tok = Digit Then

			Beg = Parser_CurPos;
			Parser_CurPos = Parser_CurPos + 1;
			While AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] = Digit Do
				Parser_CurPos = Parser_CurPos + 1;
			EndDo;
			Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
			If Parser_Char = "." Then
				Parser_CurPos = Parser_CurPos + 1;
				While AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] = Digit Do
					Parser_CurPos = Parser_CurPos + 1;
				EndDo;
				Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
			EndIf;
			Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);

			Parser_Tok = Tokens.Number;

		ElsIf Parser_Tok = Tokens.DateTime Then

			Parser_CurPos = Parser_CurPos + 1;
			Beg = Parser_CurPos;
			Parser_CurPos = StrFind(Parser_Source, "'",, Parser_CurPos);
			If Parser_CurPos = 0 Then
				Parser_Char = "";
			Else
				Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);
				Parser_CurPos = Parser_CurPos + 1;
				Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
			EndIf;

		ElsIf Parser_Tok = Undefined Then

			Prev = Parser_Char;
			Parser_CurPos = Parser_CurPos + 1;
			Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);

			If Prev = "/" Then

				If Parser_Char = "/" Then
					// scan comment
					Beg = Parser_CurPos + 1;
					Parser_CurPos = StrFind(Parser_Source, Chars_LF,, Beg);
					Parser_Comments[Parser_CurLine] = Mid(Parser_Source, Beg, Parser_CurPos - Beg);
					If Parser_CurPos = 0 Then
						Parser_Char = "";
					Else
						Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
					EndIf;
					Comment = True;
				Else
					Parser_Tok = Tokens.Div;
				EndIf;

			ElsIf Prev = "<" Then

				If Parser_Char = ">" Then
					Parser_Tok = Tokens.Neq;
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				ElsIf Parser_Char = "=" Then
					Parser_Tok = Tokens.Leq;
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				Else
					Parser_Tok = Tokens.Lss;
				EndIf;

			ElsIf Prev = ">" Then

				If Parser_Char = "=" Then
					Parser_Tok = Tokens.Geq;
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				Else
					Parser_Tok = Tokens.Gtr;
				EndIf;

			ElsIf Prev = "&" Then

				If AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] <> Alpha Then
					Error("Expected directive", Parser_CurPos, True);
				EndIf;

				// scan ident
				Beg = Parser_CurPos;
				Parser_CurPos = Parser_CurPos + 1;
				While AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] <> Undefined Do
					Parser_CurPos = Parser_CurPos + 1;
				EndDo;
				Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);

				If Not Directives.Property(Parser_Lit) Then
					Error(StrTemplate("Unknown directive: '%1'", Parser_Lit));
				EndIf;

				Parser_Tok = Tokens.Directive;

			ElsIf Prev = "#" Then

				// skip space
				While IsBlankString(Parser_Char) And Parser_Char <> "" Do
					If Parser_Char = Chars_LF Then
						Parser_CurLine = Parser_CurLine + 1;
					EndIf;
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				EndDo;

				If AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] <> Alpha Then
					Error("Expected preprocessor instruction", Parser_CurPos, True);
				EndIf;

				// scan ident
				Beg = Parser_CurPos;
				Parser_CurPos = Parser_CurPos + 1;
				While AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] <> Undefined Do
					Parser_CurPos = Parser_CurPos + 1;
				EndDo;
				Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);

				// match token
				If PrepInstructions.Property(Parser_Lit, Parser_Tok) Then
					Parser_Tok = "_" + Parser_Tok;
				Else
					Error(StrTemplate("Unknown preprocessor instruction: '%1'", Parser_Lit));
				EndIf;

			ElsIf Prev = "~" Then

				// skip space
				While IsBlankString(Parser_Char) And Parser_Char <> "" Do
					If Parser_Char = Chars_LF Then
						Parser_CurLine = Parser_CurLine + 1;
					EndIf;
					Parser_CurPos = Parser_CurPos + 1;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
				EndDo;

				If AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] = Undefined Then
					Parser_Lit = "";
				Else
					// scan ident
					Beg = Parser_CurPos; Parser_CurPos = Parser_CurPos + 1;
					While AlphaDigitMap[Mid(Parser_Source, Parser_CurPos, 1)] <> Undefined Do
						Parser_CurPos = Parser_CurPos + 1;
					EndDo;
					Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);
					Parser_Lit = Mid(Parser_Source, Beg, Parser_CurPos - Beg);
				EndIf;

				Parser_Tok = Tokens.Label;

			Else

				Raise "Unknown char!";

			EndIf;

		Else

			Parser_CurPos = Parser_CurPos + 1;
			Parser_Char = Mid(Parser_Source, Parser_CurPos, 1);

		EndIf;

		If Not Comment Then
			Break;
		EndIf;

	EndDo;

	If Parser_Tok = Tokens.Number Then
		Parser_Val = Number(Parser_Lit);
	ElsIf Parser_Tok = Tokens.True Then
		Parser_Val = True;
	ElsIf Parser_Tok = Tokens.False Then
		Parser_Val = False;
	ElsIf Parser_Tok = Tokens.DateTime Then
		Parser_Val = AsDate(Parser_Lit);
	ElsIf Left(Parser_Tok, 6) = Tokens.String Then
		Parser_Val = Mid(Parser_Lit, 2, StrLen(Parser_Lit) - 2);
	ElsIf Parser_Tok = Tokens.Null Then
		Parser_Val = Null;
	Else
		Parser_Val = Undefined;
	EndIf;

	Return Parser_Tok;

EndFunction // Scan()

Function FindObject(Name)
	Var Scope, Object;
	Scope = Parser_Scope;
	Scope.Objects.Property(Name, Object);
	While Object = Undefined And Scope.Outer <> Undefined Do
		Scope = Scope.Outer;
		Scope.Objects.Property(Name, Object);
	EndDo;
	Return Object;
EndFunction // FindObject()

Function OpenScope()
	Var Scope;
	Scope = Scope(Parser_Scope);
	Parser_Scope = Scope;
	Parser_Vars = Scope.Objects;
	Return Scope;
EndFunction // OpenScope()

Function CloseScope()
	Var Scope;
	Scope = Parser_Scope.Outer;
	Parser_Scope = Scope;
	Parser_Vars = Scope.Objects;
	Return Scope;
EndFunction // CloseScope()

Function ParseModule(Source) Export
	Var Decls, Auto, VarObj, Item, Statements, Module;
	Parser_Source = Source;
	Parser_CurPos = 0;
	Parser_CurLine = 1;
	Parser_EndLine = 1;
	Parser_BegPos = 0;
	Parser_EndPos = 0;
	Parser_Methods = New Structure;
	Parser_Unknown = New Structure;
	Parser_IsFunc = False;
	Parser_AllowVar = True;
	Parser_Interface = New Array;
	Parser_Comments = New Map;
	Parser_Len = StrLen(Source);
	Parser_Lit = "";
	Parser_Char = Undefined;
	OpenScope();
	Scan();
	Decls = ParseModDecls();
	Statements = ParseStatements();
	Auto = New Array;
	For Each VarObj In Parser_Scope.Auto Do
		Auto.Add(VarObj);
	EndDo;
	Module = Module(Decls, Auto, Statements, Parser_Interface, Parser_Comments);
	If Verbose Then
		For Each Item In Parser_Unknown Do
			Message(StrTemplate("Undeclared method `%1`", Item.Key));
		EndDo;
	EndIf;
	Expect(Tokens.Eof);
	Parser_Unknown = Undefined;
	Parser_Methods = Undefined;
	Parser_Directive = Undefined;
	Parser_Interface = Undefined;
	Parser_Comments = Undefined;
	Parser_Scope = Undefined;
	Parser_Vars = Undefined;
	Parser_Source = Undefined;
	Return Module;
EndFunction // ParseModule()

#Region ParseExpr

Function ParseExpression()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParseAndExpr();
	While Parser_Tok = Tokens.Or Do
		Operator = Parser_Tok;
		Scan();
		Expr = BinaryExpr(Expr, Operator, ParseAndExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseExpression()

Function ParseAndExpr()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParseNotExpr();
	While Parser_Tok = Tokens.And Do
		Operator = Parser_Tok;
		Scan();
		Expr = BinaryExpr(Expr, Operator, ParseNotExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseAndExpr()

Function ParseNotExpr()
	Var Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	If Parser_Tok = Tokens.Not Then
		Scan();
		Expr = NotExpr(ParseRelExpr(), Place(Pos, Line));
	Else
		Expr = ParseRelExpr();
	EndIf;
	Return Expr;
EndFunction // ParseNotExpr()

Function ParseRelExpr()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParseAddExpr();
	While RelOperators.Find(Parser_Tok) <> Undefined Do
		Operator = Parser_Tok;
		Scan();
		Expr = BinaryExpr(Expr, Operator, ParseAddExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseRelExpr()

Function ParseAddExpr()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParseMulExpr();
	While AddOperators.Find(Parser_Tok) <> Undefined Do
		Operator = Parser_Tok;
		Scan();
		Expr = BinaryExpr(Expr, Operator, ParseMulExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseAddExpr()

Function ParseMulExpr()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParseUnaryExpr();
	While MulOperators.Find(Parser_Tok) <> Undefined Do
		Operator = Parser_Tok;
		Scan();
		Expr = BinaryExpr(Expr, Operator, ParseUnaryExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParseMulExpr()

Function ParseUnaryExpr()
	Var Operator, Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Operator = Parser_Tok;
	If AddOperators.Find(Parser_Tok) <> Undefined Then
		Scan();
		Expr = UnaryExpr(Operator, ParseOperand(), Place(Pos, Line));
	ElsIf Parser_Tok = Tokens.Eof Then
		Expr = Undefined;
	Else
		Expr = ParseOperand();
	EndIf;
	Return Expr;
EndFunction // ParseUnaryExpr()

Function ParseOperand()
	Var Tok, Operand;
	Tok = Parser_Tok;
	If Tok = Tokens.String Or Tok = Tokens.StringBeg Then
		Operand = ParseStringExpr();
	ElsIf BasicLitNoString.Find(Tok) <> Undefined Then
		Operand = BasicLitExpr(Tok, Parser_Val, Place());
		Scan();
	ElsIf Tok = Tokens.Ident Then
		Operand = ParseIdentExpr();
	ElsIf Tok = Tokens.Lparen Then
		Operand = ParseParenExpr();
	ElsIf Tok = Tokens.New Then
		Operand = ParseNewExpr();
	ElsIf Tok = Tokens.Ternary Then
		Operand = ParseTernaryExpr();
	Else
		Error("Expected operand",, True);
	EndIf;
	Return Operand;
EndFunction // ParseOperand()

Function ParseStringExpr()
	Var Tok, ExprList, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Tok = Parser_Tok;
	ExprList = New Array;
	While True Do
		If Tok = Tokens.String Then
			ExprList.Add(BasicLitExpr(Tok, Parser_Val, Place()));
			Tok = Scan();
			While Tok = Tokens.String Do
				ExprList.Add(BasicLitExpr(Tok, Parser_Val, Place()));
				Tok = Scan();
			EndDo;
		ElsIf Tok = Tokens.StringBeg Then
			ExprList.Add(BasicLitExpr(Tok, Parser_Val, Place()));
			Tok = Scan();
			While Tok = Tokens.StringMid Do
				ExprList.Add(BasicLitExpr(Tok, Parser_Val, Place()));
				Tok = Scan();
			EndDo;
			If Tok <> Tokens.StringEnd Then
				Error("Expected """,, True);
			EndIf;
			ExprList.Add(BasicLitExpr(Tok, Parser_Val, Place()));
			Tok = Scan();
		Else
			Break;
		EndIf;
	EndDo;
	Return StringExpr(ExprList, Place(Pos, Line));
EndFunction // ParseStringExpr()

Function ParseNewExpr()
	Var Tok, Name, Args, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Tok = Scan();
	If Tok = Tokens.Ident Then
		Name = Parser_Lit;
		Args = EmptyArray;
		Tok = Scan();
	EndIf;
	If Tok = Tokens.Lparen Then
		Tok = Scan();
		If Tok <> Tokens.Rparen Then
			Args = ParseArguments();
			Expect(Tokens.Rparen);
		EndIf;
		Scan();
	EndIf;
	If Name = Undefined And Args = Undefined Then
		Error("Expected constructor", Parser_EndPos, True);
	EndIf;
	Return NewExpr(Name, Args, Place(Pos, Line));
EndFunction // ParseNewExpr()

Function ParseIdentExpr(Val AllowNewVar = False, NewVar = Undefined, Call = Undefined)
	Var Name, Object, Tail, Args, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Name = Parser_Lit;
	AutoPlace = Place();
	Scan();
	If Parser_Tok = Tokens.Lparen Then
		If Scan() = Tokens.Rparen Then
			Args = EmptyArray;
		Else
			Args = ParseArguments();
		EndIf;
		Expect(Tokens.Rparen);
		Scan();
		If Not Parser_Methods.Property(Name, Object) Then
			If Not Parser_Unknown.Property(Name, Object) Then
				Object = Object(Name);
				Parser_Unknown.Insert(Name, Object);
			EndIf;
		EndIf;
		Call = True;
	Else
		Object = FindObject(Name);
		If Object = Undefined Then
			If AllowNewVar Then
				Object = Object(Name, AutoDecl(AutoPlace));
				NewVar = Object;
			Else
				Object = Object(Name);
				If Verbose Then
					Error(StrTemplate("Undeclared identifier `%1`", Name), Pos);
				EndIf;
			EndIf;
		EndIf;
		Call = False;
	EndIf;
	Tail = ParseIdentTail(Call);
	Return IdentExpr(Object, Tail, Args, Place(Pos, Line));
EndFunction // ParseIdentExpr()

Function ParseIdentTail(Call = Undefined)
	Var Tail, Name, Args, Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Tail = New Array;
	While True Do
		If Parser_Tok = Tokens.Period Then
			Scan();
			If AlphaDigitMap[Left(Parser_Lit, 1)] <> Alpha Or Not Keywords.Property(Parser_Lit) Then
				Expect(Tokens.Ident);
			EndIf;
			Name = Parser_Lit;
			If Scan() = Tokens.Lparen Then
				If Scan() = Tokens.Rparen Then
					Args = EmptyArray;
				Else
					Args = ParseArguments();
				EndIf;
				Expect(Tokens.Rparen);
				Scan();
				Call = True;
			Else
				Args = Undefined;
				Call = False;
			EndIf;
			Tail.Add(FieldExpr(Name, Args, Place(Pos, Line)));
		ElsIf Parser_Tok = Tokens.Lbrack Then
			Call = False;
			If Scan() = Tokens.Rbrack Then
				Error("Expected expression", Pos, True);
			EndIf;
			Expr = ParseExpression();
			Expect(Tokens.Rbrack);
			Scan();
			Tail.Add(IndexExpr(Expr, Place(Pos, Line)));
		Else
			Break;
		EndIf;
	EndDo;
	Return Tail;
EndFunction // ParseIdentTail()

Function ParseArguments()
	Var ExprList;
	ExprList = New Array;
	While True Do
		If InitOfExpression.Find(Parser_Tok) <> Undefined Then
			ExprList.Add(ParseExpression());
		Else
			ExprList.Add(Undefined);
		EndIf;
		If Parser_Tok = Tokens.Comma Then
			Scan();
		Else
			Break;
		EndIf;
	EndDo;
	Return ExprList;
EndFunction // ParseArguments()

Function ParseTernaryExpr()
	Var Cond, ThenPart, ElsePart, Tail, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Expect(Tokens.Lparen);
	Scan();
	Cond = ParseExpression();
	Expect(Tokens.Comma);
	Scan();
	ThenPart = ParseExpression();
	Expect(Tokens.Comma);
	Scan();
	ElsePart = ParseExpression();
	Expect(Tokens.Rparen);
	If Scan() = Tokens.Period Then
		Tail = ParseIdentTail();
	Else
		Tail = EmptyArray;
	EndIf;
	Return TernaryExpr(Cond, ThenPart, ElsePart, Tail, Place(Pos, Line));
EndFunction // ParseTernaryExpr()

Function ParseParenExpr()
	Var Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Expr = ParseExpression();
	Expect(Tokens.Rparen);
	Scan();
	Return ParenExpr(Expr, Place(Pos, Line));
EndFunction // ParseParenExpr()

#EndRegion // ParseExpr

#Region ParseDecl

Function ParseModDecls()
	Var Decls;
	Decls = New Array;
	While Parser_Tok = Tokens.Directive Do
		Parser_Directive = Directives[Parser_Lit];
		Parser_Tok = Scan();
	EndDo;
	While True Do
		If Parser_Tok = Tokens.Var And Parser_AllowVar Then
			Decls.Add(ParseVarModListDecl());
		ElsIf Parser_Tok = Tokens.Function Then
			Parser_IsFunc = True;
			Decls.Add(ParseMethodDecl());
			Parser_IsFunc = False;
			Parser_AllowVar = False;
		ElsIf Parser_Tok = Tokens.Procedure Then
			Decls.Add(ParseMethodDecl());
			Parser_AllowVar = False;
		ElsIf Parser_Tok = Tokens._Region Then
			Decls.Add(ParsePrepRegionInst());
			Scan();
		ElsIf Parser_Tok = Tokens._EndRegion Then
			Decls.Add(ParsePrepEndRegionInst());
			Scan();
		ElsIf Parser_Tok = Tokens._If Then
			Decls.Add(ParsePrepIfInst());
			Scan();
		ElsIf Parser_Tok = Tokens._ElsIf Then
			Decls.Add(ParsePrepElsIfInst());
			Scan();
		ElsIf Parser_Tok = Tokens._Else Then
			Decls.Add(ParsePrepElseInst());
			Scan();
		ElsIf Parser_Tok = Tokens._EndIf Then
			Decls.Add(ParsePrepEndIfInst());
			Scan();
		ElsIf Parser_Tok = Tokens._Use Then
			Decls.Add(ParsePrepUseInst());
		Else
			Break;
		EndIf;
		Parser_Directive = Undefined;
		While Parser_Tok = Tokens.Directive Do
			Parser_Directive = Directives[Parser_Lit];
			Scan();
		EndDo;
	EndDo;
	Return Decls;
EndFunction // ParseModDecls()

Function ParseVarModListDecl()
	Var VarList, Decl, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	VarList = New Array;
	VarList.Add(ParseVarModDecl());
	While Parser_Tok = Tokens.Comma Do
		Scan();
		VarList.Add(ParseVarModDecl());
	EndDo;
	Decl = VarModListDecl(Parser_Directive, VarList, Place(Pos, Line));
	Expect(Tokens.Semicolon);
	Scan();
	While Parser_Tok = Tokens.Semicolon Do
		Scan();
	EndDo;
	Return Decl;
EndFunction // ParseVarModListDecl()

Function ParseVarModDecl()
	Var Name, VarModDecl, Object, Exported, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expect(Tokens.Ident);
	Name = Parser_Lit;
	If Scan() = Tokens.Export Then
		Exported = True;
		Scan();
	Else
		Exported = False;
	EndIf;
	VarModDecl = VarModDecl(Name, Parser_Directive, Exported, Place(Pos, Line));
	If Parser_Vars.Property(Name) Then
		Error("Identifier already declared", Pos, True);
	EndIf;
	Object = Object(Name, VarModDecl);
	Parser_Vars.Insert(Name, Object);
	If Exported Then
		Parser_Interface.Add(Object);
	EndIf;
	Return VarModDecl;
EndFunction // ParseVarModDecl()

Function ParseVars()
	Var Tok, Decls;
	Decls = New Array;
	Tok = Parser_Tok;
	While Tok = Tokens.Var Do
		Scan();
		Decls.Add(ParseVarLocDecl());
		While Parser_Tok = Tokens.Comma Do
			Scan();
			Decls.Add(ParseVarLocDecl());
		EndDo;
		Expect(Tokens.Semicolon);
		Tok = Scan();
	EndDo;
	Return Decls;
EndFunction // ParseVars()

Function ParseVarLocDecl()
	Var Name, VarLocDecl, Pos;
	Pos = Parser_BegPos;
	Expect(Tokens.Ident);
	Name = Parser_Lit;
	VarLocDecl = VarLocDecl(Name, Place());
	If Parser_Vars.Property(Name) Then
		Error("Identifier already declared", Pos, True);
	EndIf;
	Parser_Vars.Insert(Name, Object(Name, VarLocDecl));
	Scan();
	Return VarLocDecl;
EndFunction // ParseVarLocDecl()

Function ParseMethodDecl()
	Var Sign, Object, Name, Vars, Params, Exported, Body, Auto, VarObj, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Exported = False;
	Scan();
	Expect(Tokens.Ident);
	Name = Parser_Lit;
	Scan();
	OpenScope();
	Params = ParseParams();
	If Parser_Tok = Tokens.Export Then
		Exported = True;
		Scan();
	EndIf;
	If Parser_IsFunc Then
		Sign = FuncSign(Name, Parser_Directive, Params, Exported, Place(Pos, Line));
	Else
		Sign = ProcSign(Name, Parser_Directive, Params, Exported, Place(Pos, Line));
	EndIf;
	If Parser_Unknown.Property(Name, Object) Then
		Parser_Unknown.Delete(Name);
		Object.Decl = Sign;
	Else
		Object = Object(Name, Sign);
	EndIf;
	If Parser_Methods.Property(Name) Then
		Error("Method already declared", Pos, True);
	EndIf;
	Parser_Methods.Insert(Name, Object);
	If Exported Then
		Parser_Interface.Add(Object);
	EndIf;
	Vars = ParseVars();
	Body = ParseStatements();
	If Parser_IsFunc Then
		Expect(Tokens.EndFunction);
	Else
		Expect(Tokens.EndProcedure);
	EndIf;
	Auto = New Array;
	For Each VarObj In Parser_Scope.Auto Do
		Auto.Add(VarObj);
	EndDo;
	CloseScope();
	Scan();
	Return MethodDecl(Sign, Vars, Auto, Body, Place(Pos, Line));
EndFunction // ParseMethodDecl()

Function ParseParams()
	Var Params;
	Expect(Tokens.Lparen);
	Scan();
	If Parser_Tok = Tokens.Rparen Then
		Params = EmptyArray;
	Else
		Params = New Array;
		Params.Add(ParseParamDecl());
		While Parser_Tok = Tokens.Comma Do
			Scan();
			Params.Add(ParseParamDecl());
		EndDo;
	EndIf;
	Expect(Tokens.Rparen);
	Scan();
	Return Params;
EndFunction // ParseParams()

Function ParseParamDecl()
	Var Name, ParamDecl, ByVal, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	If Parser_Tok = Tokens.Val Then
		ByVal = True;
		Scan();
	Else
		ByVal = False;
	EndIf;
	Expect(Tokens.Ident);
	Name = Parser_Lit;
	If Scan() = Tokens.Eql Then
		Scan();
		ParamDecl = ParamDecl(Name, ByVal, ParseUnaryExpr(), Place(Pos, Line));
	Else
		ParamDecl = ParamDecl(Name, ByVal,, Place(Pos, Line));
	EndIf;
	If Parser_Vars.Property(Name) Then
		Error("Identifier already declared", Pos, True);
	EndIf;
	Parser_Vars.Insert(Name, Object(Name, ParamDecl));
	Return ParamDecl;
EndFunction // ParseParamDecl()

#EndRegion // ParseDecl

#Region ParseStmt

Function ParseStatements()
	Var Statements, Stmt;
	Statements = New Array;
	Stmt = ParseStmt();
	If Stmt <> Undefined Then
		Statements.Add(Stmt);
	EndIf;
	While True Do
		If Parser_Tok = Tokens.Semicolon Then
			Scan();
		ElsIf Left(Parser_Tok, 1) <> "_" Then
			Break;
		EndIf;
		Stmt = ParseStmt();
		If Stmt <> Undefined Then
			Statements.Add(Stmt);
		EndIf;
	EndDo;
	Return Statements;
EndFunction // ParseStatements()

Function ParseStmt()
	Var Tok, Stmt;
	Tok = Parser_Tok;
	If Tok = Tokens.Ident Then
		Stmt = ParseAssignOrCallStmt();
	ElsIf Tok = Tokens.If Then
		Stmt = ParseIfStmt();
	ElsIf Tok = Tokens.Try Then
		Stmt = ParseTryStmt();
	ElsIf Tok = Tokens.While Then
		Stmt = ParseWhileStmt();
	ElsIf Tok = Tokens.For Then
		If Scan() = Tokens.Each Then
			Stmt = ParseForEachStmt();
		Else
			Stmt = ParseForStmt();
		EndIf;
	ElsIf Tok = Tokens.Return Then
		Stmt = ParseReturnStmt();
	ElsIf Tok = Tokens.Break Then
		Stmt = ParseBreakStmt();
	ElsIf Tok = Tokens.Continue Then
		Stmt = ParseContinueStmt();
	ElsIf Tok = Tokens.Raise Then
		Stmt = ParseRaiseStmt();
	ElsIf Tok = Tokens.Execute Then
		Stmt = ParseExecuteStmt();
	ElsIf Tok = Tokens.Goto Then
		Stmt = ParseGotoStmt();
	ElsIf Tok = Tokens.Label Then
		Stmt = ParseLabelStmt();
	ElsIf Tok = Tokens._Region Then
		Stmt = ParsePrepRegionInst();
	ElsIf Tok = Tokens._EndRegion Then
		Stmt = ParsePrepEndRegionInst();
	ElsIf Tok = Tokens._If Then
		Stmt = ParsePrepIfInst();
	ElsIf Tok = Tokens._ElsIf Then
		Stmt = ParsePrepElsIfInst();
	ElsIf Tok = Tokens._Else Then
		Stmt = ParsePrepElseInst();
	ElsIf Tok = Tokens._EndIf Then
		Stmt = ParsePrepEndIfInst();
	ElsIf Tok = Tokens.Semicolon Then
		// NOP
	EndIf;
	Return Stmt;
EndFunction // ParseStmt()

Function ParseRaiseStmt()
	Var Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	If InitOfExpression.Find(Scan()) <> Undefined Then
		Expr = ParseExpression();
	EndIf;
	Return RaiseStmt(Expr, Place(Pos, Line));
EndFunction // ParseRaiseStmt()

Function ParseExecuteStmt()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Return ExecuteStmt(ParseExpression(), Place(Pos, Line));
EndFunction // ParseExecuteStmt()

Function ParseAssignOrCallStmt()
	Var Left, Call, Right, Stmt, NewVar, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Left = ParseIdentExpr(True, NewVar, Call);
	If Call Then
		Stmt = CallStmt(Left, Place(Pos, Line));
	Else
		Expect(Tokens.Eql);
		Scan();
		Right = ParseExpression();
		If NewVar <> Undefined Then
			Parser_Vars.Insert(NewVar.Name, NewVar);
			Parser_Scope.Auto.Add(NewVar);
		EndIf;
		Stmt = AssignStmt(Left, Right, Place(Pos, Line));
	EndIf;
	Return Stmt;
EndFunction // ParseAssignOrCallStmt()

Function ParseIfStmt()
	Var Cond, ThenPart, ElsePart, ElsIfPart, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Cond = ParseExpression();
	Expect(Tokens.Then);
	Scan();
	ThenPart = ParseStatements();
	If Parser_Tok = Tokens.ElsIf Then
		ElsIfPart = New Array;
		While Parser_Tok = Tokens.ElsIf Do
			ElsIfPart.Add(ParseElsIfStmt());
		EndDo;
	EndIf;
	If Parser_Tok = Tokens.Else Then
		ElsePart = ParseElseStmt();
	EndIf;
	Expect(Tokens.EndIf);
	Scan();
	Return IfStmt(Cond, ThenPart, ElsIfPart, ElsePart, Place(Pos, Line));
EndFunction // ParseIfStmt()

Function ParseElsIfStmt()
	Var ElsIfCond, ElsIfThen, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	ElsIfCond = ParseExpression();
	Expect(Tokens.Then);
	Scan();
	ElsIfThen = ParseStatements();
	Return ElsIfStmt(ElsIfCond, ElsIfThen, Place(Pos, Line));
EndFunction // ParseElsIfStmt()

Function ParseElseStmt()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Return ElseStmt(ParseStatements(), Place(Pos, Line));
EndFunction // ParseElseStmt()

Function ParseTryStmt()
	Var TryPart, ExceptPart, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	TryPart = ParseStatements();
	Expect(Tokens.Except);
	ExceptPart = ParseExceptStmt();
	Expect(Tokens.EndTry);
	Scan();
	Return TryStmt(TryPart, ExceptPart, Place(Pos, Line));
EndFunction // ParseTryStmt()

Function ParseExceptStmt()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Return ExceptStmt(ParseStatements(), Place(Pos, Line));
EndFunction // ParseExceptStmt()

Function ParseWhileStmt()
	Var Cond, Body, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Cond = ParseExpression();
	Expect(Tokens.Do);
	Scan();
	Body = ParseStatements();
	Expect(Tokens.EndDo);
	Scan();
	Return WhileStmt(Cond, Body, Place(Pos, Line));
EndFunction // ParseWhileStmt()

Function ParseForStmt()
	Var IdentExpr, Call, From, Until, Body, VarPos, NewVar, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expect(Tokens.Ident);
	VarPos = Parser_BegPos;
	IdentExpr = ParseIdentExpr(True, NewVar, Call);
	If Call Then
		Error("Expected variable", VarPos, True);
	EndIf;
	Expect(Tokens.Eql);
	Scan();
	From = ParseExpression();
	Expect(Tokens.To);
	Scan();
	Until = ParseExpression();
	If NewVar <> Undefined Then
		Parser_Vars.Insert(NewVar.Name, NewVar);
		Parser_Scope.Auto.Add(NewVar);
	EndIf;
	Expect(Tokens.Do);
	Scan();
	Body = ParseStatements();
	Expect(Tokens.EndDo);
	Scan();
	Return ForStmt(IdentExpr, From, Until, Body, Place(Pos, Line));
EndFunction // ParseForStmt()

Function ParseForEachStmt()
	Var IdentExpr, Call, Collection, Body, VarPos, NewVar, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Expect(Tokens.Ident);
	VarPos = Parser_BegPos;
	IdentExpr = ParseIdentExpr(True, NewVar, Call);
	If Call Then
		Error("Expected variable", VarPos, True);
	EndIf;
	Expect(Tokens.In);
	Scan();
	Collection = ParseExpression();
	If NewVar <> Undefined Then
		Parser_Vars.Insert(NewVar.Name, NewVar);
		Parser_Scope.Auto.Add(NewVar);
	EndIf;
	Expect(Tokens.Do);
	Scan();
	Body = ParseStatements();
	Expect(Tokens.EndDo);
	Scan();
	Return ForEachStmt(IdentExpr, Collection, Body, Place(Pos, Line));
EndFunction // ParseForEachStmt()

Function ParseGotoStmt()
	Var Label, Pos, Line;
	Scan();
	Expect(Tokens.Label);
	Label = Parser_Lit;
	Scan();
	Return GotoStmt(Label, Place(Pos, Line));
EndFunction // ParseGotoStmt()

Function ParseReturnStmt()
	Var Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	If Parser_IsFunc Then
		Expr = ParseExpression();
	EndIf;
	Return ReturnStmt(Expr, Place(Pos, Line));
EndFunction // ParseReturnStmt()

Function ParseBreakStmt()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Return BreakStmt(Place(Pos, Line));
EndFunction // ParseBreakStmt()

Function ParseContinueStmt()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Return ContinueStmt(Place(Pos, Line));
EndFunction // ParseContinueStmt()

Function ParseLabelStmt()
	Var Pos, Line, Label;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Label = Parser_Lit;
	Scan();
	Expect(Tokens.Colon);
	Parser_Tok = Tokens.Semicolon; // cheat code
	Return LabelStmt(Label, Place(Pos, Line));
EndFunction // ParseLabelStmt()

#EndRegion // ParseStmt

#Region ParsePrep

// Expr

Function ParsePrepExpression()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParsePrepAndExpr();
	While Parser_Tok = Tokens.Or Do
		Operator = Parser_Tok;
		Scan();
		Expr = PrepBinaryExpr(Expr, Operator, ParsePrepAndExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParsePrepExpression()

Function ParsePrepAndExpr()
	Var Expr, Operator, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Expr = ParsePrepNotExpr();
	While Parser_Tok = Tokens.And Do
		Operator = Parser_Tok;
		Scan();
		Expr = PrepBinaryExpr(Expr, Operator, ParsePrepNotExpr(), Place(Pos, Line));
	EndDo;
	Return Expr;
EndFunction // ParsePrepAndExpr()

Function ParsePrepNotExpr()
	Var Expr, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	If Parser_Tok = Tokens.Not Then
		Scan();
		Expr = PrepNotExpr(ParsePrepSymExpr(), Place(Pos, Line));
	Else
		Expr = ParsePrepSymExpr();
	EndIf;
	Return Expr;
EndFunction // ParsePrepNotExpr()

Function ParsePrepSymExpr()
	Var Operand, SymbolExist;
	If Parser_Tok = Tokens.Ident Then
		SymbolExist = PrepSymbols.Property(Parser_Lit);
		Operand = PrepSymExpr(Parser_Lit, SymbolExist, Place());
		Scan();
	Else
		Error("Expected preprocessor symbol",, True);
	EndIf;
	Return Operand;
EndFunction // ParsePrepSymExpr()

// Inst

Function ParsePrepUseInst()
	Var Path, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	If Line <> Parser_CurLine Then
		Error("Expected string or identifier", Parser_EndPos, True);
	EndIf;
	If Parser_Tok = Tokens.Number Then
		Path = Parser_Lit;
		If AlphaDigitMap[Parser_Char] = Alpha Then // can be a keyword
			Scan();
			Path = Path + Parser_Lit;
		EndIf;
	ElsIf Parser_Tok = Tokens.Ident
		Or Parser_Tok = Tokens.String Then
		Path = Parser_Lit;
	Else
		Error("Expected string or identifier", Parser_EndPos, True);
	EndIf;
	Scan();
	Return PrepUseInst(Path, Place(Pos, Line));
EndFunction // ParsePrepUseInst()

Function ParsePrepIfInst()
	Var Cond, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Cond = ParsePrepExpression();
	Expect(Tokens.Then);
	Parser_Tok = Tokens.Semicolon; // cheat code
	Return PrepIfInst(Cond, Place(Pos, Line, Parser_CurPos - Pos));
EndFunction // ParsePrepIfInst()

Function ParsePrepElsIfInst()
	Var Cond, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Cond = ParsePrepExpression();
	Expect(Tokens.Then);
	Parser_Tok = Tokens.Semicolon; // cheat code
	Return PrepElsIfInst(Cond, Place(Pos, Line, Parser_CurPos - Pos));
EndFunction // ParsePrepElsIfInst()

Function ParsePrepElseInst()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Parser_Tok = Tokens.Semicolon; // cheat code
	Parser_EndLine = Parser_CurLine; // cheat code
	Return PrepElseInst(Place(Pos, Line, Parser_CurPos - Pos));
EndFunction // ParsePrepElseInst()

Function ParsePrepEndIfInst()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Parser_Tok = Tokens.Semicolon; // cheat code
	Parser_EndLine = Parser_CurLine; // cheat code
	Return PrepEndIfInst(Place(Pos, Line, Parser_CurPos - Pos));
EndFunction // ParsePrepEndIfInst()

Function ParsePrepRegionInst()
	Var Name, Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Scan();
	Expect(Tokens.Ident);
	Name = Parser_Lit;
	Parser_Tok = Tokens.Semicolon; // cheat code
	Return PrepRegionInst(Name, Place(Pos, Line, Parser_CurPos - Pos));
EndFunction // ParsePrepRegionInst()

Function ParsePrepEndRegionInst()
	Var Pos, Line;
	Pos = Parser_BegPos;
	Line = Parser_CurLine;
	Parser_Tok = Tokens.Semicolon; // cheat code
	Parser_EndLine = Parser_CurLine; // cheat code
	Return PrepEndRegionInst(Place(Pos, Line, Parser_CurPos - Pos));
EndFunction // ParsePrepEndRegionInst()

#EndRegion // ParsePrep

#EndRegion // Parser

#Region Auxiliary

Function Place(Pos = Undefined, Line = Undefined, Len = Undefined)
	Var Place;
	If Location Then
		If Pos = Undefined Then
			Len = StrLen(Parser_Lit);
			Pos = Parser_CurPos - Len;
		ElsIf Len = Undefined Then
			Len = Parser_EndPos - Pos;
		EndIf;
		If Line = Undefined Then
			Line = Parser_CurLine;
		EndIf;
		Place = New Structure(
			"Pos,"     // number
			"Len,"     // number
			"BegLine," // number
			"EndLine", // number
			Pos, Len, Line, Parser_EndLine);
		If Debug Then
			Place.Insert("Str", Mid(Parser_Source, Pos, Len));
		EndIf;
	Else
		Place = Line;
	EndIf;
	Return Place;
EndFunction // Place()

Function AsDate(DateLit)
	Var List, Char, Num, DateString;
	List = New Array;
	For Num = 1 To StrLen(DateLit) Do
		Char = Mid(DateLit, Num, 1);
		If AlphaDigitMap[Char] = Digit Then
			List.Add(Char);
		EndIf;
	EndDo;
	DateString = StrConcat(List);
	If DateString = "00000000"
		Or DateString = "000000000000"
		Or DateString = "00000000000000" Then
		Return '00010101';
	EndIf;
	Return Date(DateString);
EndFunction // AsDate()

Procedure Expect(Tok)
	If Parser_Tok <> Tok Then
		Error("Expected " + Tok,, True);
	EndIf;
EndProcedure // Expect()

Function StringToken(Lit)
	Var Tok;
	If Left(Lit, 1) = """" Then
		If Right(Lit, 1) = """" Then
			Tok = Tokens.String;
		Else
			Tok = Tokens.StringBeg;
		EndIf;
	Else // |
		If Right(Lit, 1) = """" Then
			Tok = Tokens.StringEnd;
		Else
			Tok = Tokens.StringMid;
		EndIf;
	EndIf;
	Return Tok;
EndFunction // StringToken()

Procedure Error(Note, Pos = Undefined, Stop = False)
	Var ErrorText;
	If Pos = Undefined Then
		Pos = Min(Parser_CurPos - StrLen(Parser_Lit), Parser_Len);
	EndIf;
	ErrorText = StrTemplate("[ Ln: %1; Col: %2 ] %3",
		StrOccurrenceCount(Mid(Parser_Source, 1, Pos), Chars_LF) + 1,
		Pos - ?(Pos = 0, 0, StrFind(Parser_Source, Chars_LF, SearchDirection.FromEnd, Pos)),
		Note
	);
	If Stop Then
		Raise ErrorText;
	Else
		Message(ErrorText);
	EndIf;
EndProcedure // Error()

#EndRegion // Auxiliary

#Region Visitor

Procedure HookUp(Val Plugins) Export
	Var Plugin, List, MethodName;
	If TypeOf(Plugins) <> Type("Array") Then
		Plugin = Plugins;
		Plugins = New Array;
		Plugins.Add(Plugin);
	EndIf;
	Visitor_Plugins = Plugins;
	Visitor_Hooks = Hooks();
	For Each Plugin In Plugins Do
		List = Undefined;
		For Each MethodName In Plugin.Interface() Do
			If Visitor_Hooks.Property(MethodName, List) Then
				List.Add(Plugin);
			EndIf;
		EndDo;
	EndDo;
EndProcedure // HookUp()

Procedure PushInfo(Parent)
	Var NodeType;
	Visitor_Stack = New FixedStructure("Outer, Parent", Visitor_Stack, Parent);
	NodeType = Parent.Type;
	Visitor_Counters[NodeType] = Visitor_Counters[NodeType] + 1;
EndProcedure // PushInfo()

Procedure PopInfo()
	Var NodeType;
	NodeType = Visitor_Stack.Parent.Type;
	Visitor_Counters[NodeType] = Visitor_Counters[NodeType] - 1;
	Visitor_Stack = Visitor_Stack.Outer;
EndProcedure // PopInfo()

Function Hooks()
	Var Hooks, Item;

	Hooks = New Structure(
		"VisitModule,         AfterVisitModule,"
		"VisitDeclarations,   AfterVisitDeclarations,"
		"VisitStatements,     AfterVisitStatements,"
		"VisitDecl,           AfterVisitDecl,"
		"VisitVarModListDecl, AfterVisitVarModListDecl,"
		"VisitVarModDecl,     AfterVisitVarModDecl,"
		"VisitVarLocDecl,     AfterVisitVarLocDecl,"
		"VisitParamDecl,      AfterVisitParamDecl,"
		"VisitMethodDecl,     AfterVisitMethodDecl,"
		"VisitSignature,      AfterVisitSignature,"
		"VisitExpr,           AfterVisitExpr,"
		"VisitBasicLitExpr,   AfterVisitBasicLitExpr,"
		"VisitIdentExpr,      AfterVisitIdentExpr,"
		"VisitUnaryExpr,      AfterVisitUnaryExpr,"
		"VisitBinaryExpr,     AfterVisitBinaryExpr,"
		"VisitNewExpr,        AfterVisitNewExpr,"
		"VisitTernaryExpr,    AfterVisitTernaryExpr,"
		"VisitParenExpr,      AfterVisitParenExpr,"
		"VisitNotExpr,        AfterVisitNotExpr,"
		"VisitStringExpr,     AfterVisitStringExpr,"
		"VisitStmt,           AfterVisitStmt,"
		"VisitAssignStmt,     AfterVisitAssignStmt,"
		"VisitReturnStmt,     AfterVisitReturnStmt,"
		"VisitBreakStmt,      AfterVisitBreakStmt,"
		"VisitContinueStmt,   AfterVisitContinueStmt,"
		"VisitRaiseStmt,      AfterVisitRaiseStmt,"
		"VisitExecuteStmt,    AfterVisitExecuteStmt,"
		"VisitCallStmt,       AfterVisitCallStmt,"
		"VisitIfStmt,         AfterVisitIfStmt,"
		"VisitElsIfStmt,      AfterVisitElsIfStmt,"
		"VisitElseStmt,       AfterVisitElseStmt,"
		"VisitWhileStmt,      AfterVisitWhileStmt,"
		"VisitForStmt,        AfterVisitForStmt,"
		"VisitForEachStmt,    AfterVisitForEachStmt,"
		"VisitTryStmt,        AfterVisitTryStmt,"
		"VisitExceptStmt,     AfterVisitExceptStmt,"
		"VisitGotoStmt,       AfterVisitGotoStmt,"
		"VisitLabelStmt,      AfterVisitLabelStmt,"
		"VisitPrepInst,       AfterVisitPrepInst,"
		"VisitPrepExpr,       AfterVisitPrepExpr,"
		"VisitPrepBinaryExpr, AfterVisitPrepBinaryExpr,"
		"VisitPrepNotExpr,    AfterVisitPrepNotExpr,"
		"VisitPrepSymExpr,    AfterVisitPrepSymExpr"
	);
	For Each Item In Hooks Do
		Hooks[Item.Key] = New Array;
	EndDo;

	Return Hooks;

EndFunction // Hooks()

Procedure VisitModule(Module) Export
	Var Plugin, Hook, Item;
	For Each Plugin In Visitor_Plugins Do
		Plugin.Init(ThisObject);
	EndDo;
	Visitor_Stack = New FixedStructure("Outer, Parent", Undefined, Undefined);
	Visitor_Counters = New Structure;
	For Each Item In Nodes Do
		Visitor_Counters.Insert(Item.Key, 0);
	EndDo;
	For Each Hook In Visitor_Hooks.VisitModule Do
		Hook.VisitModule(Module, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(Module);
	VisitDeclarations(Module.Decls);
	VisitStatements(Module.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitModule Do
		Hook.AfterVisitModule(Module, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitModule()

Procedure VisitDeclarations(Declarations)
	Var Decl, Hook;
	For Each Hook In Visitor_Hooks.VisitDeclarations Do
		Hook.VisitDeclarations(Declarations, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Decl In Declarations Do
		VisitDecl(Decl);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitDeclarations Do
		Hook.AfterVisitDeclarations(Declarations, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitDeclarations()

Procedure VisitStatements(Statements)
	Var Stmt, Hook;
	For Each Hook In Visitor_Hooks.VisitStatements Do
		Hook.VisitStatements(Statements, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Stmt In Statements Do
		VisitStmt(Stmt);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitStatements Do
		Hook.AfterVisitStatements(Statements, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitStatements()

#Region VisitDecl

Procedure VisitDecl(Decl)
	Var Type, Hook;
	For Each Hook In Visitor_Hooks.VisitDecl Do
		Hook.VisitDecl(Decl, Visitor_Stack, Visitor_Counters);
	EndDo;
	Type = Decl.Type;
	If Type = Nodes.VarModListDecl Then
		VisitVarModListDecl(Decl);
	ElsIf Type = Nodes.VarModDecl Then
		VisitVarModDecl(Decl);
	ElsIf Type = Nodes.VarLocDecl Then
		VisitVarLocDecl(Decl);
	ElsIf Type = Nodes.MethodDecl Then
		VisitMethodDecl(Decl);
	ElsIf Type = Nodes.PrepRegionInst
		Or Type = Nodes.PrepEndRegionInst
		Or Type = Nodes.PrepIfInst
		Or Type = Nodes.PrepElsIfInst
		Or Type = Nodes.PrepElseInst
		Or Type = Nodes.PrepEndIfInst
		Or Type = Nodes.PrepUseInst Then
		VisitPrepInst(Decl);
	EndIf;
	For Each Hook In Visitor_Hooks.AfterVisitDecl Do
		Hook.AfterVisitDecl(Decl, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitDecl()

Procedure VisitVarModListDecl(VarModListDecl)
	Var Hook, VarModDecl;
	For Each Hook In Visitor_Hooks.VisitVarModListDecl Do
		Hook.VisitVarModListDecl(VarModListDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(VarModListDecl);
	For Each VarModDecl In VarModListDecl.List Do
		VisitVarModDecl(VarModDecl);
	EndDo;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitVarModListDecl Do
		Hook.AfterVisitVarModListDecl(VarModListDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitVarModListDecl()

Procedure VisitVarModDecl(VarModDecl)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitVarModDecl Do
		Hook.VisitVarModDecl(VarModDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitVarModDecl Do
		Hook.AfterVisitVarModDecl(VarModDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitVarModDecl()

Procedure VisitVarLocDecl(VarLocDecl)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitVarLocDecl Do
		Hook.VisitVarLocDecl(VarLocDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitVarLocDecl Do
		Hook.AfterVisitVarLocDecl(VarLocDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitVarLocDecl()

Procedure VisitParamDecl(ParamDecl)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitParamDecl Do
		Hook.VisitParamDecl(ParamDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitParamDecl Do
		Hook.AfterVisitParamDecl(ParamDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitParamDecl()

Procedure VisitMethodDecl(MethodDecl)
	Var Hook, VarLocDecl;
	For Each Hook In Visitor_Hooks.VisitMethodDecl Do
		Hook.VisitMethodDecl(MethodDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(MethodDecl);
	VisitSignature(MethodDecl.Sign);
	For Each VarLocDecl In MethodDecl.Vars Do
		VisitVarLocDecl(VarLocDecl);
	EndDo;
	VisitStatements(MethodDecl.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitMethodDecl Do
		Hook.AfterVisitMethodDecl(MethodDecl, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitMethodDecl()

Procedure VisitSignature(Sign)
	Var Hook, ParamDecl;
	For Each Hook In Visitor_Hooks.VisitSignature Do
		Hook.VisitSignature(Sign, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(Sign);
	For Each ParamDecl In Sign.Params Do
		VisitParamDecl(ParamDecl);
	EndDo;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitSignature Do
		Hook.AfterVisitSignature(Sign, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitSignature()

#EndRegion // VisitDecl

#Region VisitExpr

Procedure VisitExpr(Expr)
	Var Type, Hook;
	For Each Hook In Visitor_Hooks.VisitExpr Do
		Hook.VisitExpr(Expr, Visitor_Stack, Visitor_Counters);
	EndDo;
	Type = Expr.Type;
	If Type = Nodes.BasicLitExpr Then
		VisitBasicLitExpr(Expr);
	ElsIf Type = Nodes.IdentExpr Then
		VisitIdentExpr(Expr);
	ElsIf Type = Nodes.UnaryExpr Then
		VisitUnaryExpr(Expr);
	ElsIf Type = Nodes.BinaryExpr Then
		VisitBinaryExpr(Expr);
	ElsIf Type = Nodes.NewExpr Then
		VisitNewExpr(Expr);
	ElsIf Type = Nodes.TernaryExpr Then
		VisitTernaryExpr(Expr);
	ElsIf Type = Nodes.ParenExpr Then
		VisitParenExpr(Expr);
	ElsIf Type = Nodes.NotExpr Then
		VisitNotExpr(Expr);
	ElsIf Type = Nodes.StringExpr Then
		VisitStringExpr(Expr);
	EndIf;
	For Each Hook In Visitor_Hooks.AfterVisitExpr Do
		Hook.AfterVisitExpr(Expr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitExpr()

Procedure VisitBasicLitExpr(BasicLitExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitBasicLitExpr Do
		Hook.VisitBasicLitExpr(BasicLitExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitBasicLitExpr Do
		Hook.AfterVisitBasicLitExpr(BasicLitExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitBasicLitExpr()

Procedure VisitIdentExpr(IdentExpr)
	Var Item, Expr, Hook;
	For Each Hook In Visitor_Hooks.VisitIdentExpr Do
		Hook.VisitIdentExpr(IdentExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(IdentExpr);
	If IdentExpr.Args <> Undefined Then
		For Each Expr In IdentExpr.Args Do
			If Expr <> Undefined Then
				VisitExpr(Expr);
			EndIf;
		EndDo;
	EndIf;
	For Each Item In IdentExpr.Tail Do
		If Item.Type = Nodes.FieldExpr Then
			If Item.Args <> Undefined Then
				For Each Expr In Item.Args Do
					If Expr <> Undefined Then
						VisitExpr(Expr);
					EndIf;
				EndDo;
			EndIf;
		ElsIf Item.Type = Nodes.IndexExpr Then
			VisitExpr(Item.Expr);
		Else
			Raise "Call in violation of protocol";
		EndIf;
	EndDo;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitIdentExpr Do
		Hook.AfterVisitIdentExpr(IdentExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitIdentExpr()

Procedure VisitUnaryExpr(UnaryExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitUnaryExpr Do
		Hook.VisitUnaryExpr(UnaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(UnaryExpr);
	VisitExpr(UnaryExpr.Operand);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitUnaryExpr Do
		Hook.AfterVisitUnaryExpr(UnaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitUnaryExpr()

Procedure VisitBinaryExpr(BinaryExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitBinaryExpr Do
		Hook.VisitBinaryExpr(BinaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(BinaryExpr);
	VisitExpr(BinaryExpr.Left);
	VisitExpr(BinaryExpr.Right);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitBinaryExpr Do
		Hook.AfterVisitBinaryExpr(BinaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitBinaryExpr()

Procedure VisitNewExpr(NewExpr)
	Var Expr, Hook;
	For Each Hook In Visitor_Hooks.VisitNewExpr Do
		Hook.VisitNewExpr(NewExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(NewExpr);
	For Each Expr In NewExpr.Args Do
		If Expr <> Undefined Then
			VisitExpr(Expr);
		EndIf;
	EndDo;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitNewExpr Do
		Hook.AfterVisitNewExpr(NewExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitNewExpr()

Procedure VisitTernaryExpr(TernaryExpr)
	Var Item, Expr, Hook;
	For Each Hook In Visitor_Hooks.VisitTernaryExpr Do
		Hook.VisitTernaryExpr(TernaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(TernaryExpr);
	VisitExpr(TernaryExpr.Cond);
	VisitExpr(TernaryExpr.Then);
	VisitExpr(TernaryExpr.Else);
	For Each Item In TernaryExpr.Tail Do
		If Item.Type = Nodes.FieldExpr Then
			If Item.Args <> Undefined Then
				For Each Expr In Item.Args Do
					If Expr <> Undefined Then
						VisitExpr(Expr);
					EndIf;
				EndDo;
			EndIf;
		ElsIf Item.Type = Nodes.IndexExpr Then
			VisitExpr(Item.Expr);
		Else
			Raise "Call in violation of protocol";
		EndIf;
	EndDo;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitTernaryExpr Do
		Hook.AfterVisitTernaryExpr(TernaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitTernaryExpr()

Procedure VisitParenExpr(ParenExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitParenExpr Do
		Hook.VisitParenExpr(ParenExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ParenExpr);
	VisitExpr(ParenExpr.Expr);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitParenExpr Do
		Hook.AfterVisitParenExpr(ParenExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitParenExpr()

Procedure VisitNotExpr(NotExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitNotExpr Do
		Hook.VisitNotExpr(NotExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(NotExpr);
	VisitExpr(NotExpr.Expr);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitNotExpr Do
		Hook.AfterVisitNotExpr(NotExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitNotExpr()

Procedure VisitStringExpr(StringExpr)
	Var Expr, Hook;
	For Each Hook In Visitor_Hooks.VisitStringExpr Do
		Hook.VisitStringExpr(StringExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(StringExpr);
	For Each Expr In StringExpr.List Do
		VisitBasicLitExpr(Expr);
	EndDo;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitStringExpr Do
		Hook.AfterVisitStringExpr(StringExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitStringExpr()

#EndRegion // VisitExpr

#Region VisitStmt

Procedure VisitStmt(Stmt)
	Var Type, Hook;
	For Each Hook In Visitor_Hooks.VisitStmt Do
		Hook.VisitStmt(Stmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	Type = Stmt.Type;
	If Type = Nodes.AssignStmt Then
		VisitAssignStmt(Stmt);
	ElsIf Type = Nodes.ReturnStmt Then
		VisitReturnStmt(Stmt);
	ElsIf Type = Nodes.BreakStmt Then
		VisitBreakStmt(Stmt);
	ElsIf Type = Nodes.ContinueStmt Then
		VisitContinueStmt(Stmt);
	ElsIf Type = Nodes.RaiseStmt Then
		VisitRaiseStmt(Stmt);
	ElsIf Type = Nodes.ExecuteStmt Then
		VisitExecuteStmt(Stmt);
	ElsIf Type = Nodes.CallStmt Then
		VisitCallStmt(Stmt);
	ElsIf Type = Nodes.IfStmt Then
		VisitIfStmt(Stmt);
	ElsIf Type = Nodes.WhileStmt Then
		VisitWhileStmt(Stmt);
	ElsIf Type = Nodes.ForStmt Then
		VisitForStmt(Stmt);
	ElsIf Type = Nodes.ForEachStmt Then
		VisitForEachStmt(Stmt);
	ElsIf Type = Nodes.TryStmt Then
		VisitTryStmt(Stmt);
	ElsIf Type = Nodes.GotoStmt Then
		VisitGotoStmt(Stmt);
	ElsIf Type = Nodes.LabelStmt Then
		VisitLabelStmt(Stmt);
	ElsIf Type = Nodes.PrepRegionInst
		Or Type = Nodes.PrepEndRegionInst
		Or Type = Nodes.PrepIfInst
		Or Type = Nodes.PrepElsIfInst
		Or Type = Nodes.PrepElseInst
		Or Type = Nodes.PrepEndIfInst Then
		VisitPrepInst(Stmt);
	EndIf;
	For Each Hook In Visitor_Hooks.AfterVisitStmt Do
		Hook.AfterVisitStmt(Stmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitStmt()

Procedure VisitAssignStmt(AssignStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitAssignStmt Do
		Hook.VisitAssignStmt(AssignStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(AssignStmt);
	VisitIdentExpr(AssignStmt.Left);
	VisitExpr(AssignStmt.Right);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitAssignStmt Do
		Hook.AfterVisitAssignStmt(AssignStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitAssignStmt()

Procedure VisitReturnStmt(ReturnStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitReturnStmt Do
		Hook.VisitReturnStmt(ReturnStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ReturnStmt);
	If ReturnStmt.Expr <> Undefined Then
		VisitExpr(ReturnStmt.Expr);
	EndIf;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitReturnStmt Do
		Hook.AfterVisitReturnStmt(ReturnStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitReturnStmt()

Procedure VisitBreakStmt(BreakStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitBreakStmt Do
		Hook.VisitBreakStmt(BreakStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitBreakStmt Do
		Hook.AfterVisitBreakStmt(BreakStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitBreakStmt()

Procedure VisitContinueStmt(ContinueStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitContinueStmt Do
		Hook.VisitContinueStmt(ContinueStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitContinueStmt Do
		Hook.AfterVisitContinueStmt(ContinueStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitContinueStmt()

Procedure VisitRaiseStmt(RaiseStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitRaiseStmt Do
		Hook.VisitRaiseStmt(RaiseStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(RaiseStmt);
	If RaiseStmt.Expr <> Undefined Then
		VisitExpr(RaiseStmt.Expr);
	EndIf;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitRaiseStmt Do
		Hook.AfterVisitRaiseStmt(RaiseStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitRaiseStmt()

Procedure VisitExecuteStmt(ExecuteStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitExecuteStmt Do
		Hook.VisitExecuteStmt(ExecuteStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ExecuteStmt);
	VisitExpr(ExecuteStmt.Expr);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitExecuteStmt Do
		Hook.AfterVisitExecuteStmt(ExecuteStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitExecuteStmt()

Procedure VisitCallStmt(CallStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitCallStmt Do
		Hook.VisitCallStmt(CallStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(CallStmt);
	VisitIdentExpr(CallStmt.Ident);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitCallStmt Do
		Hook.AfterVisitCallStmt(CallStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitCallStmt()

Procedure VisitIfStmt(IfStmt)
	Var ElsIfStmt, Hook;
	For Each Hook In Visitor_Hooks.VisitIfStmt Do
		Hook.VisitIfStmt(IfStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(IfStmt);
	VisitExpr(IfStmt.Cond);
	VisitStatements(IfStmt.Then);
	If IfStmt.ElsIf <> Undefined Then
		For Each ElsIfStmt In IfStmt.ElsIf Do
			VisitElsIfStmt(ElsIfStmt);
		EndDo;
	EndIf;
	If IfStmt.Else <> Undefined Then
		VisitElseStmt(IfStmt.Else);
	EndIf;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitIfStmt Do
		Hook.AfterVisitIfStmt(IfStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitIfStmt()

Procedure VisitElsIfStmt(ElsIfStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitElsIfStmt Do
		Hook.VisitElsIfStmt(ElsIfStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ElsIfStmt);
	VisitExpr(ElsIfStmt.Cond);
	VisitStatements(ElsIfStmt.Then);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitElsIfStmt Do
		Hook.AfterVisitElsIfStmt(ElsIfStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitElsIfStmt()

Procedure VisitElseStmt(ElseStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitElseStmt Do
		Hook.VisitElseStmt(ElseStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ElseStmt);
	VisitStatements(ElseStmt.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitElseStmt Do
		Hook.AfterVisitElseStmt(ElseStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitElseStmt()

Procedure VisitWhileStmt(WhileStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitWhileStmt Do
		Hook.VisitWhileStmt(WhileStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(WhileStmt);
	VisitExpr(WhileStmt.Cond);
	VisitStatements(WhileStmt.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitWhileStmt Do
		Hook.AfterVisitWhileStmt(WhileStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitWhileStmt()

Procedure VisitForStmt(ForStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitForStmt Do
		Hook.VisitForStmt(ForStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ForStmt);
	VisitIdentExpr(ForStmt.Ident);
	VisitExpr(ForStmt.From);
	VisitExpr(ForStmt.To);
	VisitStatements(ForStmt.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitForStmt Do
		Hook.AfterVisitForStmt(ForStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(ForEachStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitForEachStmt Do
		Hook.VisitForEachStmt(ForEachStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ForEachStmt);
	VisitIdentExpr(ForEachStmt.Ident);
	VisitExpr(ForEachStmt.In);
	VisitStatements(ForEachStmt.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitForEachStmt Do
		Hook.AfterVisitForEachStmt(ForEachStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitForEachStmt()

Procedure VisitTryStmt(TryStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitTryStmt Do
		Hook.VisitTryStmt(TryStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(TryStmt);
	VisitStatements(TryStmt.Try);
	VisitExceptStmt(TryStmt.Except);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitTryStmt Do
		Hook.AfterVisitTryStmt(TryStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitTryStmt()

Procedure VisitExceptStmt(ExceptStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitExceptStmt Do
		Hook.VisitExceptStmt(ExceptStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(ExceptStmt);
	VisitStatements(ExceptStmt.Body);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitExceptStmt Do
		Hook.AfterVisitExceptStmt(ExceptStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitExceptStmt()

Procedure VisitGotoStmt(GotoStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitGotoStmt Do
		Hook.VisitGotoStmt(GotoStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitGotoStmt Do
		Hook.AfterVisitGotoStmt(GotoStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitGotoStmt()

Procedure VisitLabelStmt(LabelStmt)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitLabelStmt Do
		Hook.VisitLabelStmt(LabelStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitLabelStmt Do
		Hook.AfterVisitLabelStmt(LabelStmt, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitLabelStmt()

#EndRegion // VisitStmt

#Region VisitPrep

Procedure VisitPrepExpr(PrepExpr)
	Var Type, Hook;
	For Each Hook In Visitor_Hooks.VisitPrepExpr Do
		Hook.VisitPrepExpr(PrepExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	Type = PrepExpr.Type;
	If Type = Nodes.PrepSymExpr Then
		VisitPrepSymExpr(PrepExpr);
	ElsIf Type = Nodes.PrepBinaryExpr Then
		VisitPrepBinaryExpr(PrepExpr);
	ElsIf Type = Nodes.PrepNotExpr Then
		VisitPrepNotExpr(PrepExpr);
	EndIf;
	For Each Hook In Visitor_Hooks.AfterVisitPrepExpr Do
		Hook.AfterVisitPrepExpr(PrepExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitPrepExpr()

Procedure VisitPrepSymExpr(PrepSymExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitPrepSymExpr Do
		Hook.VisitPrepSymExpr(PrepSymExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	For Each Hook In Visitor_Hooks.AfterVisitPrepSymExpr Do
		Hook.AfterVisitPrepSymExpr(PrepSymExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitPrepSymExpr()

Procedure VisitPrepBinaryExpr(PrepBinaryExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitPrepBinaryExpr Do
		Hook.VisitPrepBinaryExpr(PrepBinaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(PrepBinaryExpr);
	VisitPrepExpr(PrepBinaryExpr.Left);
	VisitPrepExpr(PrepBinaryExpr.Right);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitPrepBinaryExpr Do
		Hook.AfterVisitPrepBinaryExpr(PrepBinaryExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitPrepBinaryExpr()

Procedure VisitPrepNotExpr(PrepNotExpr)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitPrepNotExpr Do
		Hook.VisitPrepNotExpr(PrepNotExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(PrepNotExpr);
	VisitPrepExpr(PrepNotExpr.Expr);
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitPrepNotExpr Do
		Hook.AfterVisitPrepNotExpr(PrepNotExpr, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitPrepNotExpr()

Procedure VisitPrepInst(PrepInst)
	Var Hook;
	For Each Hook In Visitor_Hooks.VisitPrepInst Do
		Hook.VisitPrepInst(PrepInst, Visitor_Stack, Visitor_Counters);
	EndDo;
	PushInfo(PrepInst);
	If PrepInst.Property("Cond") Then
		VisitPrepExpr(PrepInst.Cond);
	EndIf;
	PopInfo();
	For Each Hook In Visitor_Hooks.AfterVisitPrepInst Do
		Hook.AfterVisitPrepInst(PrepInst, Visitor_Stack, Visitor_Counters);
	EndDo;
EndProcedure // VisitPrepInst()

#EndRegion // VisitPrep

#EndRegion // Visitor

Init();
