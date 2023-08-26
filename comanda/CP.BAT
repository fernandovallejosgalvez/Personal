ECHO "1 " %1 "2 " %2 "3 " %3 "4 " %4 "5 " %5
basic %1 [UFM(B)]
IF ERRORLEVEL 1 GOTO ERRORES

REM Lib86 %1 [I]
REM IF ERRORLEVEL 1 GOTO ERRORES

GOTO SALIDA

:ERRORES
ECHO OFF
ECHO [-ERROR EN COMPILACION-]
GOTO FIN

:SALIDA
ECHO OFF
if exist %1.inl DEL %1.INL
if exist %1.sym del %1.sym
if exist %1.map del %1.map
if exist %1.lin del %1.lin
rem if exist %1.lst del %1.lst
if exist *.bak  del  *.bak
ECHO [***PROCESO DE COMPILACION OK***]
GOTO FIN

:FIN