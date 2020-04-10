
. .\common.ps1

Write-Host "Создание пустой базы ..."

$ArgList = @('CREATEINFOBASE', 'File=".\temp\"', '/UseTemplate temp.dt')
Start-Process $1CPath -ArgumentList $ArgList

Write-Host "Подготовка завершена!"