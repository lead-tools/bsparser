
function wait($ssh) {
    while (-not $ssh.DataAvailable) {
        Start-Sleep -m 100
    }
}

function send($ssh, $cmd) {
    "#cmd: $cmd" >> log.txt
    $ssh.WriteLine($cmd)
    wait($ssh)
    $res = ConvertFrom-Json $ssh.Read()
    $res | Format-Table | Out-File log.txt -Append
    if (-not ($res | where {$_.type -eq 'success'})) {
        [System.Windows.Forms.MessageBox]::Show('An error has occured. Please see log file') | Out-Null
        $res | where {$_.type -eq 'error'} | Write-Host
        throw
    }
}

function complete($percent) {
    Write-Progress -Activity "Build" -Status "Progress:" -PercentComplete $percent
}

complete 0

"#build.ps1 START $(Get-Date)`n`n" >> log.txt

$UserName = 'admin'
$EmptyPassword = New-object System.Security.SecureString
$PSCredential = New-Object System.Management.Automation.PSCredential($UserName, $EmptyPassword)

$ArgList = @('DESIGNER', '/F .\temp\', '/AgentMode', '/AgentSSHHostKeyAuto', '/AgentBaseDir .\')
#$ArgList += '/Visible'
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList

Start-Sleep 1

$1c = New-SSHSession 127.0.0.1 -Port 1543 -Credential $PSCredential
$ssh = $1c | New-SSHShellStream
wait($ssh)
$ssh.Read() | Out-Null

send $ssh 'options set --output-format=json --show-prompt=no'
send $ssh 'common connect-ib'
complete 20
send $ssh 'config load-ext-files --file="..\src\BSLParser.xml" --ext-file="..\build\BSLParser.epf"'
complete 40
send $ssh 'config load-ext-files --file="..\gui\src\gui.xml" --ext-file="..\build\gui.epf"'
complete 60
send $ssh 'config load-ext-files --file="..\backends\BSL\src\BSL.xml" --ext-file="..\build\BSL.epf"'
complete 80
send $ssh 'config load-ext-files --file="..\plugins\ReturnCheck\src\ReturnCheck.xml" --ext-file="..\build\ReturnCheck.epf"'
complete 100
send $ssh 'common disconnect-ib'
$ssh.WriteLine('common shutdown')
wait($ssh)

$1c | Remove-SSHSession | Out-Null
"#build.ps1 END $(Get-Date)`n`n" >> log.txt