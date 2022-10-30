{-----------------------------------------------------------------}
{                                                                 }
{                     SACD-Ripper project                         }
{                                                                 }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>        }
{                                                                 }
{  Unauthorized copying of this file, via any medium is           }
{  strictly prohibited.                                           }
{                                                                 }
{       Confidential and Proprietary. All Rights Reserved.        }
{                                                                 }
{-----------------------------------------------------------------}

{
    The simple program to test Pascal lenguage.
    Author: Mikhail.Malakhov
}
program test;

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, Mikhan.Util.AppArgs, Mikhan.Util.AppLogs, Mikhan.Rainbow.Scarlet,
    Mikhan.Util.AppVersion, Mikhan.Util.StrUtils;

const

    { The name of this program. }
    APP_NAME = 'SACD-Ripper';

    { The common debug flag. }
    DEBUG = True;

    { The default log separator. }
    LOG_SEP = '---------------------------------------------------------';

{ Program commands }
const

    { Program command: Print information about SACD disc. }
    CMD_INFO = 'info';

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{ Any stuff for debug and testing }
const

    { An input file to testing. }
    TEST_FILE_NAME_WIN = 'D:\Media\DireStraits.iso';

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
procedure PrintInfo(AFile: TFileName; Debug: Boolean);
const INDENT = '   - ';
var
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;
    F: File;
begin
    WriteLn();
    WriteLn('SACD: ', AFile);
    AssignFile(F, AFile);
    MasterToc := TMasterTocArea.Create();
    MasterToc.Load(F);
    TextToc := TMasterTextArea.Create();
    TextToc.Load(F);
    if Debug then
        Writeln(INDENT, 'Area Header: ', TextToc.Header);
    Writeln(INDENT, 'Format Version: ', MasterToc.SpecVersion.ToString);
    Writeln(INDENT, 'Disc Title: ', TextToc.DiscTitle);
    Writeln(INDENT, 'Disc Artist: ', TextToc.DiscArtist);
    Writeln(INDENT, 'Disc Publisher: ', TextToc.DiscPublisher);
    Writeln(INDENT, 'Disc Copyright: ', TextToc.DiscCopyright);
    Writeln(INDENT, 'Album Title: ', TextToc.AlbumTitle);
    Writeln(INDENT, 'Album Artist: ', TextToc.AlbumArtist);
    Writeln(INDENT, 'Album Publisher: ', TextToc.AlbumPublisher);
    Writeln(INDENT, 'Album Copyright: ', TextToc.AlbumCopyright);

    // Just for testing and debug
    if Debug then
    begin
        Writeln();
        Writeln('TMasterTocArea dump:');
        TAppLogs.Dump(MasterToc[0]^.RawData, 128);
        Writeln();
        Writeln('MasterTextArea[0] dump:');
        TAppLogs.Dump(TextToc[0]^.RawData, 128);
    end;
end;

//
// Program entry point
//
begin

    // Program Logs
    AppLogs := TAppLogs.Create('SACD');

    // Parse input arguments
    AppArgs := TAppArgs.Create();
    AppArgs.ParseArgs();
    if DEBUG then AppArgs.PrintAll();

    // Any actios for testing
    if AppArgs.Has(CMD_TEST) then
    begin
        // TODO Need to add Linux variant
        InputFile := TFileName(TEST_FILE_NAME_WIN);
        PrintInfo(InputFile, True);
        Exit;
    end;

    // Program command: version
    if AppArgs.HasVersion() then
    begin
        AppVer := TSemVer.Create(True);
        AppVer.LoadFromFile();
        WriteLn(APP_NAME);
        WriteLn(AppVer.ToString());
        Exit;
    end;

    // Program command: info
    if AppArgs.Has(CMD_INFO) then
    begin
        // In this option we expect only SACD file path as
        // second argument
        if AppArgs.Count >= 2 then
        begin
            InputFile := TFileName(AppArgs.Arguments[1].Key);
            PrintInfo(InputFile, DEBUG);
        end else
            AppLogs.W('Please specify SACD image file path...');
        Exit;
    end;

end.
