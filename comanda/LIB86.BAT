Lib86 %1 [I]
IF ERRORLEVEL 1 GOTO ERRORES

copy COMANDA.L86 ..\v6r4s001g3\

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