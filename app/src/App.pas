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
    DiskArea: TSACDArea;
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;

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
    Writeln(Sector.ToString());
    Writeln('-----');
    for i := 511 to 550 do
    begin
        ReadSector(F, i, Sector);
        //Writeln(Sector.ToString(0, -1));
    end;


    //Writeln('-----');
    DiskArea := TSACDArea.Create(510, 1);
    DiskArea.Load(F);
    if DiskArea.HasData() then
    begin
        WriteLn(DiskArea[0].ToString());
    end;

    Writeln('-----');
    MasterToc := TMasterTocArea.Create();
    MasterToc.Load(F);
    Writeln(MasterToc.Header);

    Writeln('-----');
    TextToc := TMasterTextArea.Create();
    TextToc.Load(F);
    Writeln(TextToc.Header);
    Writeln('Album: ', TextToc.AlbumTitle);
    Writeln('Artist: ', TextToc.AlbumArtist);

end.
