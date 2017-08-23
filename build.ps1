Write-Progress -Activity "Build" -Status "Progress:" -PercentComplete 0

If(!(test-path .\build)) { (New-Item -ItemType Directory -Force -Path .\build) | Out-Null }
Remove-Item .\build\* -recurse

$ArgList = @('DESIGNER', '/F .\temp\', '/LoadExternalDataProcessorOrReportFromFiles .\src\BSL-Parser.xml .\build\BSL-Parser.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Build" -Status "Progress:" -PercentComplete 25

$ArgList = @('DESIGNER', '/F .\temp\', '/LoadExternalDataProcessorOrReportFromFiles .\gui\src\gui.xml .\build\gui.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Build" -Status "Progress:" -PercentComplete 50

$ArgList = @('DESIGNER', '/F .\temp\', '/LoadExternalDataProcessorOrReportFromFiles .\backends\BSL\src\BSL.xml .\build\BSL.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Build" -Status "Progress:" -PercentComplete 75

$ArgList = @('DESIGNER', '/F .\temp\', '/LoadExternalDataProcessorOrReportFromFiles .\plugins\ReturnCheck\src\ReturnCheck.xml .\build\ReturnCheck.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Build" -Status "Progress:" -PercentComplete 100