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

const

    { The name of this program. }
    APP_NAME = 'SACD-Ripper';

    { The common debug flag. }
    DEBUG = False;

    { The default log separator. }
    LOG_SEP = '---------------------------------------------------------';

{ Program commands }
const

    { Program command: Print information about SACD disc. }
    CMD_INFO = 'info';

{ Global scope }

var
    AppVer: TSemVer;        // Program version
    AppArgs: TAppArgs;      // Program command line arguments
    AppLogs: TAppLogs;      // Program logs
    Command: TArgString;    // The current command
    InputFile: TFileName;   // Input file path
    OutputFile: TFileName;   // Outpot file path

{ Just for test }
var
    Sector: TSACDSector;
    i,j,k: integer;

{
    Print an information about SACD disc.
}
procedure PrintInfo(AFile: TFileName);
var
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;
    F: File;
begin
    AssignFile(F, AFile);
    Writeln(LOG_SEP);
    MasterToc := TMasterTocArea.Create();
    MasterToc.Load(F);
    Writeln(MasterToc.Header);
    Writeln(LOG_SEP);
    TextToc := TMasterTextArea.Create();
    TextToc.Load(F);
    Writeln(TextToc.Header);
    Writeln('Disc Title: ', TextToc.DiscTitle);
    Writeln('Disc Artist: ', TextToc.DiscArtist);
    Writeln('Disc Publisher: ', TextToc.DiscPublisher);
    Writeln('Disc Copyright: ', TextToc.DiscCopyright);
    Writeln('Album Title: ', TextToc.AlbumTitle);
    Writeln('Album Artist: ', TextToc.AlbumArtist);
    Writeln('Album Publisher: ', TextToc.AlbumPublisher);
    Writeln('Album Copyright: ', TextToc.AlbumCopyright);
    Writeln(LOG_SEP);
    // Just for test
    TAppLogs.Dump(TextToc[0]^.RawData, 128);
    //Writeln(TextToc[0]^.ToString();
    //PrintArray(TextToc[0]^.RawData, 0, True);
end;

//
// Program entry point
//
begin

    // Parse input arguments
    AppArgs := TAppArgs.Create();
    AppArgs.ParseArgs();
    if AppArgs.HasOption('-f', '--file') then
    begin
        InputFile := AppArgs.GetValue('-f', '--file');
    end;

    // Program version
    if AppArgs.HasVersion() then
    begin
        AppVer := TSemVer.Create(True);
        AppVer.LoadFromFile();
        WriteLn(APP_NAME);
        WriteLn(AppVer.ToString());
        Exit;
    end;

    // Program Logs
    AppLogs := TAppLogs.Create('SACD');
    AppLogs.D(InputFile);

    if not Mikhan.Util.StrUtils.IsEmpty(InputFile) then
    begin
        PrintInfo(InputFile);
    end;

end.
