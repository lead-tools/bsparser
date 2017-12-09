Find-Module Posh-SSH | Install-Module -Scope CurrentUser

$ArgList = @('CREATEINFOBASE', 'File=".\temp\"', '/UseTemplate temp.dt')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList