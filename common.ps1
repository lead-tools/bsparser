
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

function complete($percent, $activity) {
    Write-Progress -Activity $activity -Status "Progress:" -PercentComplete $percent
}

function connect() {
    $UserName = 'admin'
    $EmptyPassword = New-object System.Security.SecureString
    $PSCredential = New-Object System.Management.Automation.PSCredential($UserName, $EmptyPassword)

    $ArgList = @('DESIGNER', '/F .\temp\', '/AgentMode', '/AgentSSHHostKeyAuto', '/AgentBaseDir .\')
    $ArgList += '/Visible'
    $1cpath = 'C:\Program Files (x86)\1cv8\common\1cestart.exe'
    if (-not (Test-Path $1cpath)) {
        $1cpath = 'C:\Program Files\1cv8\common\1cestart.exe'
    }
    Start-Process $1cpath -ArgumentList $ArgList

    Start-Sleep 1

    $1c = New-SSHSession 127.0.0.1 -Port 1543 -Credential $PSCredential
    $ssh = $1c | New-SSHShellStream
    wait($ssh)
    $ssh.Read() | Out-Null

    send $ssh 'options set --output-format=json --show-prompt=no'
    send $ssh 'common connect-ib'

    return $1c, $ssh
}

function disconnect($1c, $ssh) {
    send $ssh 'common disconnect-ib'
    $ssh.WriteLine('common shutdown')
    #wait($ssh)

    $1c | Remove-SSHSession | Out-Null
}

$list = @{
    BSLParser      = "\"
    gui            = "\gui\"
    DocGen         = "\plugins\DocGen\"
    BSL            = "\plugins\BSL\"
    ReturnCheck    = "\plugins\ReturnCheck\"
    AutoVarsCheck  = "\plugins\AutoVarsCheck\"
    TestCheck      = "\plugins\TestCheck\"
    TestStat       = "\plugins\TestStat\"
    TestVars       = "\plugins\TestVars\"
    TestEnd        = "\plugins\TestEnd\"
    TestServerCall = "\plugins\TestServerCall\"
}