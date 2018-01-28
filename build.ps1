
. .\common.ps1

complete 0 "Build"

"#build.ps1 START $(Get-Date)`n`n" >> log.txt

$1c, $ssh = connect

$x = 0; $dx = 100 / $list.Count
foreach ($item in $list.GetEnumerator()) {
    send $ssh "config load-ext-files --file=""..$($item.Value)src\$($item.Name).xml"" --ext-file=""..\build\$($item.Name).epf"""
    $x += $dx
    complete $x "Build"
}

disconnect $1c $ssh

"#build.ps1 END $(Get-Date)`n`n" >> log.txt