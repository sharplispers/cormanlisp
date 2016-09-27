set OldDir=%cd%

cd "%~dp0\"
del "%~dp0\CormanLisp.img"
"%~dp0\clconsole" -execute "%~dp0\sys\compile-sys.lisp" -image ""

cd "%OldDir%"
