@echo off
goto START

-------------------------------------------------------
 @BASEFILENAME@



 Created @DATE@

-------------------------------------------------------

:START
SETLOCAL
if  _%1==_ goto USAGE

set arg1=%1
set arg1short=%~nx1
set arg2=
shift
if _%1==_ goto MAIN
set arg2=%1
goto MAIN


-------------------------------------------------------
:MAIN

@DOT@
call :BACKTICK hostname c:\Windows\system32\hostname.exe

echo %hostname%

goto ALL_DONE

-------------------------------------------------------


--------------------------------------------
:BACKTICK
    for /f "usebackq delims==" %%I in (`%2`) do set %1=%%I
    goto :EOF

--------------------------------------------



--------------------------------------------
:USAGE
  echo usage:   @BASEFILENAMELESSEXTENSION@ ^<arg^> [^<optionalarg^>]
  echo blah blah blah
  goto ALL_DONE

--------------------------------------------


:ALL_DONE
ENDLOCAL
