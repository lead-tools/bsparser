
$1CPath = "C:\Program Files\1cv8\8.3.13.1809\bin\1cv8.exe"
if (-not (Test-Path $1CPath)) {
    $1CPath = 'C:\Program Files\1cv8\common\1cestart.exe'
}
if (-not (Test-Path $1CPath)) {
    $1CPath = 'C:\Program Files (x86)\1cv8\common\1cestart.exe'
}

function complete($percent, $activity) {
    Write-Progress -Activity $activity -Status "Прогресс:" -PercentComplete $percent
}

$list = @{
    ПарсерВстроенногоЯзыка                         = "\src"
    Консоль                                        = "\gui\src"
    Тестер                                         = "\tests\Тестер\src"
    Компилятор                                     = "\backends\Компилятор\src"
    ГенераторИсходногоКодаПоАСД                    = "\backends\ГенераторИсходногоКодаПоАСД\src"
    ГенераторДокументацииДляПарсера                = "\plugins\ГенераторДокументацииДляПарсера\src"
    ДетекторФункцийБезВозвратаВКонце               = "\plugins\ДетекторФункцийБезВозвратаВКонце\src"
    ДетекторНеиспользуемыхПеременных               = "\plugins\ДетекторНеиспользуемыхПеременных\src"
    ДетекторОшибочныхЗамыкающихКомментариев        = "\plugins\ДетекторОшибочныхЗамыкающихКомментариев\src"
    РекурсивныйПодсчетСерверныхВызововВМодуляхФорм = "\plugins\РекурсивныйПодсчетСерверныхВызововВМодуляхФорм\src"
    ПодсчетКогнитивнойСложностиМетодов             = "\plugins\ПодсчетКогнитивнойСложностиМетодов\src"
    ПримерМодификацииИсходногоКода                 = "\plugins\ПримерМодификацииИсходногоКода\src"
}

function run ($command, $description){

    complete 0 $description

    $x = 0; $dx = 100 / $list.Count
    foreach ($item in $list.GetEnumerator()) {
        $ArgList =  "DESIGNER", "/DumpResult designer_result.txt", "/Out designer_out.txt",
                    "/DisableStartupDialogs",
                    "/F .\temp\",
                    "/N Admin",
                    "/$command `".$($item.Value)\$($item.Name).xml`" `".\build\$($item.Name).epf`""
        Start-Process $1CPath -ArgumentList $ArgList -Wait
        if ((Get-Content .\designer_result.txt) -ne '0') {
            Get-Content .\designer_out.txt
        }
        $x += $dx
        complete $x $description
    }

}