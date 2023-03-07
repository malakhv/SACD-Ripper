::
:: Build app
::
:: Author: Mikhail.Malakhov
::

IF EXIST build\ (rd /s /q build\) ELSE (echo The build directory not found.)

mkdir build

fpc.bat ./src/App.pas -FEbuild -Fu./src/app -Fu./src/util -Fu./src/rainbow -osacd.exe
