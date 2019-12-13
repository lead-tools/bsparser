// Пример скрипта выполняющего проверку исходного кода

// Сначала подключаем обработку парсера

AttachScript("..\src\BSParser\Ext\ObjectModule.bsl", "BSParser");

// и обработки необходимых плагинов (набор просто для демонстрации работы с несколькими плагинами).

// Плагин, проверяющий наличие неиспользуемых переменных:
AttachScript("..\plugins\TestVars\src\TestVars\Ext\ObjectModule.bsl", "PluginTestVars");
// Плагин, проверяющий что в окончаниях инструкций стоит актуальны комментарий:
AttachScript("..\plugins\TestEnd\src\TestEnd\Ext\ObjectModule.bsl", "PluginTestEnd");
// Плагин, проверяющий наличие возвратов в конце функций:
AttachScript("..\plugins\ReturnCheck\src\ReturnCheck\Ext\ObjectModule.bsl", "PluginReturnCheck");
// Плагин, проверяющий наличие авто-переменных (необъявленных переменных):
AttachScript("..\plugins\AutoVarsCheck\src\AutoVarsCheck\Ext\ObjectModule.bsl", "PluginAutoVarsCheck");

// Далее читаем исходный код, который хотим проверить.
TextReader = New TextReader("..\src\BSParser\Ext\ObjectModule.bsl");
Source = TextReader.Read();
TextReader.Close();

// собираем нужные плагины в массив
Plugins = New Array;
Plugins.Add(New PluginTestVars);
Plugins.Add(New PluginTestEnd);
Plugins.Add(New PluginReturnCheck);
Plugins.Add(New PluginAutoVarsCheck);

// Запуск проверки на данном исходном коде (Source) с желаемым набором плагинов (Plugins).
BSParser = New BSParser;
BSParser.Go(Source, Plugins);

// Собираем и выводим результаты работы плагинов.
For Each Plugin In Plugins Do
	Message(Plugin.Result());
EndDo;
