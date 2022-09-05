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
    The simple program to test Pascal lenguage.
    Author: Mikhail.Malakhov
}
program test;

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, Mikhan.Util.AppArgs, Mikhan.Util.AppLogs, Mikhan.SACD.Scarlet, Mikhan.SACD,
    Mikhan.Util.AppVersion, Mikhan.Util.StrUtils;

{ Global scope }
var
    FName: String;
    isSacd: Boolean;
    F: File;
    Sector: TSACDSector;
    AppLogs: TAppLogs;
    i,j,k: integer;
    DiskArea: TSACDArea;
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;
    AppArgs: Mikhan.Util.AppArgs.TAppArgs;
    Test1: Array of ShortString;
    Test2: Array of ShortString;
    //AppOpt: TAppOpt;
    AppVer: TSemVer;


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
            Writeln(IntToHex(pos, 2));
            Writeln(IntToHex(buf, 2));
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
    
    // Testing program args
    //Writeln('ParamCount: ', ParamCount());
    for i := 0 to ParamCount() do
    begin
        //WriteLn(i, ': ', ParamStr(i));
    end;

    //Writeln('-----');
    //AppOpt := TAppOpt.Create;
    //AppOpt.ParseArgs();
    //AppOpt.PrintAll();

    //Writeln('-----');
    AppArgs := TAppArgs.Create();
    AppArgs.ParseArgs();
    if AppArgs.HasOption('-f', '--file') then
    begin
        FName := AppArgs.GetValue('-f', '--file');
    end;
    //AppArgs.PrintAll();
    //Writeln('-----');

    AppVer := TSemVer.Create(True);
    if AppArgs.HasVersion() then
    begin
        AppVer.LoadFromFile();
        WriteLn(AppVer.ToString());
        Exit;
    end;

    AppLogs := TAppLogs.Create('SACD');

    AppLogs.D(FName);
    //AppLogs.D(['FName=', 1, ', c=', 2]);

    //isSacd := isSacdImage(FName);
    //AppLogs.I('isSacd=' + BoolToStr(isSacd));
    
    //Writeln('-----');
    
    {ReadSector(F, 510, Sector);
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
    end;}

    if not Mikhan.Util.StrUtils.IsEmpty(FName) then
    begin
        AssignFile(F, FName);

        Writeln('-----');
        MasterToc := TMasterTocArea.Create();
        MasterToc.Load(F);
        Writeln(MasterToc.Header);

        Writeln('-----');
        TextToc := TMasterTextArea.Create();
        TextToc.Load(F);
        Writeln(TextToc.Header);
        Writeln('Album Title: ', TextToc.AlbumTitle);
        Writeln('Album Artist: ', TextToc.AlbumArtist);
        Writeln('Album Publisher: ', TextToc.AlbumPublisher);
        Writeln('Album Copyright: ', TextToc.AlbumCopyright);
        Writeln('Disc Title: ', TextToc.DiscTitle);
        Writeln('Disc Artist: ', TextToc.DiscArtist);
        Writeln('Disc Publisher: ', TextToc.DiscPublisher);
        Writeln('Disc Copyright: ', TextToc.DiscCopyright);
        //PrintArray(TextToc[0]^.RawData, 0, True);
        Writeln('-----');
        TAppLogs.Dump(TextToc[0]^.RawData, 128);
        //Writeln(TextToc[0]^.ToString());
    end;

end.
