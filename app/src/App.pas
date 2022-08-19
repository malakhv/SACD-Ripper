{----------------------------------------------------------------}
{                                                                }
{                     SACD-Ripper project                        }
{                                                                }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>       }
{                                                                }
{  Unauthorized copying of this file, via any medium is          }
{  strictly prohibited.                                          }
{                                                                }
{       Confidential and Proprietary. All Rights Reserved.       }
{                                                                }
{----------------------------------------------------------------}

{
    The simple programm to test Pascal lenguage.
    Author: Mikhail.Malakhov
}
program test;

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, Mikhan.Util.AppArgs, Mikhan.Util.AppLogs, Mikhan.SACD.Scarlet, Mikhan.SACD;

{ Global scope }
var
    FName: String;
    isSacd: Boolean;
    F: File;
    Sector: RSector;
    AppLogs: TAppLogs;
    i,j,k: integer;
    DiskArea: TSACDArea;
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;
    AppArgs: Mikhan.Util.AppArgs.TAppArgs;
    Test1: Array of ShortString;
    Test2: Array of ShortString;
    //AppOpt: TAppOpt;


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
    
    // Testing programm args
    Writeln('ParamCount: ', ParamCount());
    for i := 0 to ParamCount() do
    begin
        WriteLn(i, ': ', ParamStr(i));
    end;

    Writeln('-----');
    //AppOpt := TAppOpt.Create;
    //AppOpt.ParseArgs();
    //AppOpt.PrintAll();

    //Writeln('-----');
    AppArgs := TAppArgs.Create();
    AppArgs.ParseArgs();
    AppArgs.PrintAll();
    Writeln('-----');
    
    {ParseParams();
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
    }
end.
