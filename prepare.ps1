Find-Module Posh-SSH | Install-Module -Scope CurrentUser

$ArgList = @('CREATEINFOBASE', 'File=".\temp\"', '/UseTemplate temp.dt')
$1cpath = 'C:\Program Files (x86)\1cv8\common\1cestart.exe'
if (-not (Test-Path $1cpath)) {
    $1cpath = 'C:\Program Files\1cv8\common\1cestart.exe'
}
Start-Process $1cpath -ArgumentList $ArgList