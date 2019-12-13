
// Плагин - демонстрация возможного подхода к правке исходника.
// После каждого (почти) присваивания переменной <x> вставляется Message(<x>);

Var Source; // Анализируемый исходный код
Var Tokens; // Перечисление - токены
Var Nodes;  // Перечисление - узлы

Var Result; // Массив, для промежуточного хранения результатов
Var IgnoredCounters; // Массив для хранения типов узлов, которые не влияют на отступ
Var BegPos; // Позиция с которой копируется текст из исходника перед вставкой

Procedure Init(BSParser) Export

	Source = BSParser.Source();
	Tokens = BSParser.Tokens();
	Nodes = BSParser.Nodes();

	Result = New Array;
	BegPos = 1;

	IgnoredCounters = New Array;
	IgnoredCounters.Add("Module");
	IgnoredCounters.Add("ElsIfStmt");
	IgnoredCounters.Add("ElseStmt");

EndProcedure

Function Result() Export

	// добавление в буфер оставшегося хвоста из исходника
	Result.Add(Mid(Source, BegPos, StrLen(Source) - BegPos));

	Return StrConcat(Result);

EndFunction

Function Hooks() Export

	// Регистрация подписки на присваивания

	Hooks = New Array;
	Hooks.Add("VisitAssignStmt");

	Return Hooks;

EndFunction

Procedure VisitAssignStmt(AssignStmt, Stack, Counters) Export

	// https://lead-tools.github.io/BSParser/#AssignStmt

	Ident = IdentExprToString(AssignStmt.Left); // превращаем переменную слева в текст

	If Ident <> Undefined Then

		Indentation = 0;
		For Each Counter In Counters Do
			If IgnoredCounters.Find(Counter.Key) = Undefined Then
				Indentation = Indentation + Counter.Value; // подсчет отступа, как количества родительских узлов
			EndIf;
		EndDo;

		EndPos = AssignStmt.Place.Pos + AssignStmt.Place.Len + 1; // позиция конца инструкции + 1 точка с запятой
		Result.Add(Mid(Source, BegPos, EndPos - BegPos)); // добавление в буфер диапазона текста из исходника от BegPos до EndPos
		Result.Add(Chars.LF); // перенос перед нашей вставкой
		For Index = 1 To Indentation Do
			Result.Add(Chars.Tab); // добаляем отступ
		EndDo;
		Result.Add(StrTemplate("Message(%1); // Это вставлено плагином!!!", Ident));
		Result.Add(Chars.LF); // перенос послей нашей вставки

		BegPos = EndPos; // текущий конец становится началом для следующего копирования текста из исходника

	EndIf;

EndProcedure

Function IdentExprToString(IdentExpr)

	// предусловие: IdentExpr.Args = Undefined

	// https://lead-tools.github.io/BSParser/#IdentExpr

	Buffer = New Array;

	Buffer.Add(IdentExpr.Head.Name);
	For Each Item In IdentExpr.Tail Do
		If Item.Type = Nodes.FieldExpr Then
			Buffer.Add(".");
			Buffer.Add(Item.Name);
			If Item.Args <> Undefined Then
				If Item.Args.Count() > 0 Then
					Return Undefined; // вызовы с параметрами будем игнорировать
				Else
					Buffer.Add("()");
				EndIf;
			EndIf;
		ElsIf Item.Type = Nodes.IndexExpr Then
			Buffer.Add("[");
			If Item.Expr.Type = Nodes.BasicLitExpr Then
				If Item.Expr.Kind = Tokens.Number Then
				    Buffer.Add(Item.Expr.Value);
				ElsIf Item.Expr.Kind = Tokens.String Then
					Buffer.Add(StrTemplate("""%1""", Item.Expr.Value));
				Else
					Return Undefined; // остальные виды литералов будем игнорировать
				EndIf;
			Else
				Return Undefined; // обращения по индексу со сложным выражением будем игнорировать
			EndIf;
			Buffer.Add("]");
		Else
			Raise "Неизвестный тип";
		EndIf;
	EndDo;

	Return StrConcat(Buffer);

EndFunction
