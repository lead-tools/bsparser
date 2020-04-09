# [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") | Out-Null

Remove-Variable * -ErrorAction SilentlyContinue; Remove-Module *; $error.Clear() #; Clear-Host

function wait($ssh) {
    # похоже ожидание не всегда работает на 15 платформе
    # как минимум точно не работает при распаковке (explode.ps1)
    # гипотетически надо не просто ждать данные, а еще и success в этих данных
    while (-not $ssh.DataAvailable) {
        Start-Sleep -m 100
    }
}

function send($ssh, $cmd) {
    "#cmd: $cmd" >> log.txt
    $ssh.WriteLine($cmd)
    wait($ssh)
    $data = $ssh.Read()
    $data2 = $data -replace "\]\[", "," # фикс ошибки в json от платформы
    $res = ConvertFrom-Json $data2 -WarningAction SilentlyContinue
    $res | Format-Table | Out-File log.txt -Append
    $err = $res | Where-Object {$_.type -eq 'error'}
    if ($err) {
        $data | Write-Host
        # [System.Windows.Forms.MessageBox]::Show('An error has occured. Please see log file') | Out-Null
    }
}

function complete($percent, $activity) {
    Write-Progress -Activity $activity -Status "Прогресс:" -PercentComplete $percent
}

function connect() {
    $UserName = 'admin'
    $EmptyPassword = New-object System.Security.SecureString
    $PSCredential = New-Object System.Management.Automation.PSCredential($UserName, $EmptyPassword)

    $ArgList = @('DESIGNER', '/F .\temp\', '/AgentMode', '/AgentSSHHostKeyAuto', '/AgentBaseDir .\')
    # $ArgList += '/Visible'
    $1cpath = 'C:\Program Files (x86)\1cv8\common\1cestart.exe'
    if (-not (Test-Path $1cpath)) {
        $1cpath = 'C:\Program Files\1cv8\common\1cestart.exe'
    }
    Start-Process $1cpath -ArgumentList $ArgList

    Start-Sleep 1

    $1c = New-SSHSession 127.0.0.1 -Port 1543 -Credential $PSCredential -AcceptKey
    $ssh = $1c | New-SSHShellStream
    wait($ssh)
    $ssh.Read() | Out-Null

    send $ssh 'options set --output-format=json --show-prompt=no'
    send $ssh 'common connect-ib'

    return $1c, $ssh
}

function disconnect($1c, $ssh) {
    send $ssh 'common disconnect-ib' #TODO: эта команда похоже ничего не возвращает (надо убрать wait в итоге)
    Start-Sleep 1
    send $ssh 'common shutdown'
    $1c | Remove-SSHSession | Out-Null
}

$list = @{
    ПарсерВстроенногоЯзыка = "\"
    gui                    = "\gui\"
    DocGen                 = "\plugins\DocGen\"
    BSL                    = "\plugins\BSL\"
    ReturnCheck            = "\plugins\ReturnCheck\"
    TestVars               = "\plugins\TestVars\"
    TestEnd                = "\plugins\TestEnd\"
    TestServerCall         = "\plugins\TestServerCall\"
    CognitiveComplexity    = "\plugins\CognitiveComplexity\"
    CodeModification       = "\plugins\CodeModification\"
    TestRunner             = "\tests\TestRunner\"
}