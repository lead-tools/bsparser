@chcp 65001

call vrunner init-dev %*

@call vrunner restore --db-user ""%*

@rem собрать внешние обработчики и отчеты в каталоге build
call vrunner compileepf src/ПарсерВстроенногоЯзыка .tempdb %*
call vrunner compileepf console/src/Консоль .tempdb %*
call vrunner compileepf testa/async .tempdb %*
                               
