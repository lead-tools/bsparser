
Write-Host "Установка модуля SSH ..."

Find-Module Posh-SSH | Install-Module -Scope CurrentUser

Get-SSHTrustedHost | Remove-SSHTrustedHost # фикс ошибки: https://github.com/darkoperator/Posh-SSH/issues/216

Write-Host "Создание пустой базы ..."

$ArgList = @('CREATEINFOBASE', 'File=".\temp\"', '/UseTemplate temp.dt')
$1cpath = 'C:\Program Files (x86)\1cv8\common\1cestart.exe'
if (-not (Test-Path $1cpath)) {
    $1cpath = 'C:\Program Files\1cv8\common\1cestart.exe'
}
Start-Process $1cpath -ArgumentList $ArgList

Write-Host "Подготовка завершена!"