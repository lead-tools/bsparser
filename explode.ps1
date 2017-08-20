Write-Progress -Activity "Extract" -Status "$I% Complete:" -PercentComplete 0

Remove-Item .\src -recurse
$ArgList = @('DESIGNER', '/F .\temp\', '/DumpExternalDataProcessorOrReportToFiles .\src\BSL-Parser.xml .\build\BSL-Parser.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Extract" -Status "$I% Complete:" -PercentComplete 25

Remove-Item .\gui\src -recurse
$ArgList = @('DESIGNER', '/F .\temp\', '/DumpExternalDataProcessorOrReportToFiles .\gui\src\gui.xml .\build\gui.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Extract" -Status "$I% Complete:" -PercentComplete 50

Remove-Item .\backends\BSL\src -recurse
$ArgList = @('DESIGNER', '/F .\temp\', '/DumpExternalDataProcessorOrReportToFiles .\backends\BSL\src\BSL.xml .\build\BSL.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Extract" -Status "$I% Complete:" -PercentComplete 75

Remove-Item .\plugins\ReturnCheck\src -recurse
$ArgList = @('DESIGNER', '/F .\temp\', '/DumpExternalDataProcessorOrReportToFiles .\plugins\ReturnCheck\src\ReturnCheck.xml .\build\ReturnCheck.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Extract" -Status "$I% Complete:" -PercentComplete 100