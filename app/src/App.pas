// Copyright (C) 2012 Mikhail Malakhov <malakhv@live.ru>
//
// Confidential and Proprietary. All Rights Reserved.
// Unauthorized copying of this file, via any medium is strictly prohibited.

{
    The simple programm to test Pascal lenguage.
    Author: Mikhail.Malakhov
}
program test;

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, AppLogs, Scarlet, SACD;

{ Global scope }
var
    FName: String;
    isSacd: Boolean;
    F: File;
    Sector: RSector;
    AppLogs: TAppLogs;
    i: integer;

procedure ParseParams();
var 
    count: integer;
begin
    count := paramCount();
    if count > 0 then
        FName := paramStr(1);
end;

procedure CheckFile(name: String);
var
    f: File;
    buf: ShortInt;
    pos: Integer;
begin
    AssignFile(f, name);
    Reset(f, 1);
    buf := 0;
    pos := 0;
    while not EOF(f) do
    begin
        BlockRead(f, buf, SizeOf(buf));
        if buf > 0 then
        begin
            Writeln(IntToHex(pos));
            Writeln(IntToHex(buf));
            break;
        end;
        Inc(pos);
    end;
    CloseFile(f);
end;

//
// Program entry point
//
begin
    ParseParams();
    AppLogs := TAppLogs.Create('SACD');

    AppLogs.D(FName);
    isSacd := isSacdImage(FName);
    AppLogs.I('isSacd=' + BoolToStr(isSacd));
    
    Writeln('-----');
    
    AssignFile(F, FName);
    ReadSector(F, 510, Sector);
    Writeln('-----');
    ReadSector(F, 511, Sector);
    Writeln(Sector.ToString(0, 20));


    Writeln('-----');
    Writeln('MaxInt is ', MaxInt);

    i := MaxInt;
    //Inc(i);
    //Inc(i);
    i := i + 2;
    Writeln('NewInt is ', i);

end.
