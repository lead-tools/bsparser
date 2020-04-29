
$1CPath = "C:\Program Files\1cv8\8.3.13.1809\bin\1cv8.exe"
if (-not (Test-Path $1CPath)) {
    $1CPath = 'C:\Program Files\1cv8\common\1cestart.exe'
}
if (-not (Test-Path $1CPath)) {
    $1CPath = 'C:\Program Files (x86)\1cv8\common\1cestart.exe'
}

if (-not (Test-Path '.\temp')) {
    $ArgList = @('CREATEINFOBASE', 'File=".\temp\"')
    Start-Process $1CPath -ArgumentList $ArgList
}

function complete($percent, $activity) {
    Write-Progress -Activity $activity -Status "Прогресс:" -PercentComplete $percent
}

$list = @{
    ПарсерВстроенногоЯзыка                         = "", "\src"
    Консоль                                        = "", "\gui\src"
    ГлобальноеОкружение                            = "", "\scope\global\src"
    Тестер                                         = "", "\tests\Тестер\src"
    Компилятор                                     = "Бакенды", "\backends\Компилятор\src"
    Визажист                                       = "Плагины - генераторы текста", "\plugins\Визажист\src"
    ГенераторДокументацииДляПарсера                = "Плагины - генераторы текста", "\plugins\ГенераторДокументацииДляПарсера\src"
    РекурсивныйПодсчетСерверныхВызововВМодуляхФорм = "Плагины - генераторы текста", "\plugins\РекурсивныйПодсчетСерверныхВызововВМодуляхФорм\src"
    ПодсчетКогнитивнойСложностиМетодов             = "Плагины - генераторы ошибок", "\plugins\ПодсчетКогнитивнойСложностиМетодов\src"
    ДетекторФункцийБезВозвратаВКонце               = "Плагины - генераторы ошибок", "\plugins\ДетекторФункцийБезВозвратаВКонце\src"
    ДетекторНеиспользуемыхПеременных               = "Плагины - генераторы ошибок", "\plugins\ДетекторНеиспользуемыхПеременных\src"
    ПроверкаКаноничностиКлючевыхСлов               = "Плагины - генераторы ошибок", "\plugins\ПроверкаКаноничностиКлючевыхСлов\src"
    ДетекторПропущенныхТочекСЗапятой               = "Плагины - генераторы ошибок", "\plugins\ДетекторПропущенныхТочекСЗапятой\src"
    ДетекторВложенныхТернарныхОператоров           = "Плагины - генераторы ошибок", "\plugins\ДетекторВложенныхТернарныхОператоров\src"
    РасстановкаПропущенныхТочекСЗапятой            = "Плагины - генераторы замен", "\plugins\РасстановкаПропущенныхТочекСЗапятой\src"
    ЗаменаНеканоничныхКлючевыхСлов                 = "Плагины - генераторы замен", "\plugins\ЗаменаНеканоничныхКлючевыхСлов\src"
    ПереименованиеПеременных                       = "Плагины - генераторы ошибок и замен", "\plugins\ПереименованиеПеременных\src"
    ДетекторОшибочныхЗамыкающихКомментариев        = "Плагины - генераторы ошибок и замен", "\plugins\ДетекторОшибочныхЗамыкающихКомментариев\src"
    ДетекторКонструкторовСтруктур                  = "Плагины - генераторы ошибок и замен", "\plugins\ДетекторКонструкторовСтруктур\src"
}

function run ($command, $description){

    complete 0 $description

    $x = 0; $dx = 100 / $list.Count
    foreach ($item in $list.GetEnumerator()) {
        $ArgList =  "DESIGNER", "/DumpResult designer_result.txt", "/Out designer_out.txt",
                    "/DisableStartupDialogs",
                    "/F .\temp\",
                    "/$command `".$($item.Value[1])\$($item.Name).xml`" `".\build\$($item.Value[0])\$($item.Name).epf`""
        Start-Process $1CPath -ArgumentList $ArgList -Wait
        if ((Get-Content .\designer_result.txt) -ne '0') {
            Get-Content .\designer_out.txt
        }
        $x += $dx
        complete $x $description
    }

}