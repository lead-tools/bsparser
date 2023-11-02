@chcp 65001

call vrunner init-dev %*

@call vrunner restore --db-user ""%*

@rem собрать внешние обработчики и отчеты в каталоге .tempdb
call vrunner compileepf src/ПарсерВстроенногоЯзыка .tempdb/ПарсерВстроенногоЯзыка.epf %*
call vrunner compileepf console/src/Консоль .tempdb/Консоль.epf %*
call vrunner compileepf tests/async .tempdb/tests/async.epf %*
                               
