
. .\common.ps1

complete 0 "Explode"

"#explode.ps1 START $(Get-Date)`n`n" >> log.txt

$1c, $ssh = connect

$x = 0; $dx = 100 / $list.Count
foreach ($item in $list.GetEnumerator()) {
    Remove-Item ".$($item.Value)src\*" -recurse -force
    send $ssh "config dump-ext-files --file=""..$($item.Value)src\$($item.Name).xml"" --ext-file=""..\build\$($item.Name).epf"""
    $x += $dx
    complete $x "Explode"
}

disconnect $1c $ssh

"#explode.ps1 END $(Get-Date)`n`n" >> log.txt