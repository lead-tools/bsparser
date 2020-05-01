
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
    Консоль                                        = "", "\console\src"
    ГлобальноеОкружение                            = "", "\scope\global\src"
    Компилятор                                     = "Примеры\Бакенды", "\examples\backends\Компилятор\src"
    Визажист                                       = "Примеры\Плагины - генераторы текста", "\examples\plugins\Визажист\src"
    ГенераторДокументацииДляПарсера                = "Примеры\Плагины - генераторы текста", "\examples\plugins\ГенераторДокументацииДляПарсера\src"
    РекурсивныйПодсчетСерверныхВызововВМодуляхФорм = "Примеры\Плагины - генераторы текста", "\examples\plugins\РекурсивныйПодсчетСерверныхВызововВМодуляхФорм\src"
    ПодсчетКогнитивнойСложностиМетодов             = "Примеры\Плагины - генераторы ошибок", "\examples\plugins\ПодсчетКогнитивнойСложностиМетодов\src"
    ДетекторФункцийБезВозвратаВКонце               = "Примеры\Плагины - генераторы ошибок", "\examples\plugins\ДетекторФункцийБезВозвратаВКонце\src"
    ДетекторНеиспользуемыхПеременных               = "Примеры\Плагины - генераторы ошибок", "\examples\plugins\ДетекторНеиспользуемыхПеременных\src"
    ПроверкаКаноничностиКлючевыхСлов               = "Примеры\Плагины - генераторы ошибок", "\examples\plugins\ПроверкаКаноничностиКлючевыхСлов\src"
    ДетекторПропущенныхТочекСЗапятой               = "Примеры\Плагины - генераторы ошибок", "\examples\plugins\ДетекторПропущенныхТочекСЗапятой\src"
    ДетекторВложенныхТернарныхОператоров           = "Примеры\Плагины - генераторы ошибок", "\examples\plugins\ДетекторВложенныхТернарныхОператоров\src"
    РасстановкаПропущенныхТочекСЗапятой            = "Примеры\Плагины - генераторы замен", "\examples\plugins\РасстановкаПропущенныхТочекСЗапятой\src"
    ЗаменаНеканоничныхКлючевыхСлов                 = "Примеры\Плагины - генераторы замен", "\examples\plugins\ЗаменаНеканоничныхКлючевыхСлов\src"
    ПереименованиеПеременных                       = "Примеры\Плагины - генераторы ошибок и замен", "\examples\plugins\ПереименованиеПеременных\src"
    ДетекторОшибочныхЗамыкающихКомментариев        = "Примеры\Плагины - генераторы ошибок и замен", "\examples\plugins\ДетекторОшибочныхЗамыкающихКомментариев\src"
    ДетекторКонструкторовСтруктур                  = "Примеры\Плагины - генераторы ошибок и замен", "\examples\plugins\ДетекторКонструкторовСтруктур\src"
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