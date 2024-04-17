#*** Obtain macro definitions from external file ***
!INCLUDE ..\makefile.defs
# ==================================================================

WINFX_PATH=c:\progra~1\refere~1\Microsoft\Framework\v3.0

# Add required libs here to include into all CSC compiles:
_BASE_IMPORTS=/R:System.dll /R:System.Data.dll

default: euh.exe

all:    whatever

WpfAnimation1.exe: WpfAnimation1.cs makefile
        $(_CSC) $(_CS_EXE_FLAGS) /r:$(WINFX_PATH)\PresentationCore.dll  /r:$(WINFX_PATH)\PresentationFramework.dll  /r:$(WINFX_PATH)\WindowsBase.dll  /out:WpfAnimation1.exe WpfAnimation1.cs 

TestExistingZip.exe: TestExistingZip.cs
        $(_CSC) $(_CS_EXE_FLAGS)  /r:c:\dinoch\dev\dotnet\zip\DotNetZip\Library\bin\Debug\Ionic.Zip.dll /out:TestExistingZip.exe TestExistingZip.cs

