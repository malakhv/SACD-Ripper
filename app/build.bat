::
:: Build app
::
:: Author: Mikhail.Malakhov
::

IF EXIST build\ (rd /s /q build\) ELSE (echo The build directory not found.)

mkdir build

SET PASCAL_KIT="./../../PascalKit/src/util"

fpc.bat ./src/App.pas -FEbuild ^
    -Fu./src/app -Fu./src/util ^
    -Fu./src/rainbow ^
    -Fu%PASCAL_KIT% ^
    -osacd.exe
