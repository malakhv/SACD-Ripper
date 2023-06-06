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
program sacd;

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, Mikhan.Util.AppArgs, Mikhan.Util.AppLogs, Mikhan.Rainbow.Scarlet,
    Mikhan.Util.AppVersion, Mikhan.Util.StrUtils;

const

    { The common debug flag. }
    DEBUG = True;

    { The name of this program. }
    APP_NAME = 'SACD-Ripper';

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
    //Command: TArgString;    // The current command
    InputFile: TFileName;   // Input file path
    //OutputFile: TFileName;  // Outpot file path

{ Just for test }
//var
    //Sector: TSACDSector;
    //i,j,k: integer;

procedure PrintHelp();
const INDENT = '   ';
begin
    WriteLn(APP_NAME, ' command options:');
    WriteLn(INDENT, 'info SACD_FILE_NAME - print information abour SACD disc');
    WriteLn(INDENT, '-v or --version - print program version');
end;

procedure PrintVersion();
begin
    WriteLn(AppVer.ToString());
end;

{
    Print an information about SACD disc.
}
procedure PrintInfo(AFile: TFileName; Debug: Boolean);
const INDENT = '   - ';
var
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;
    Album: TMasterTocAlbum;
    Disc: TMasterTocDisc;
    Manuf: TMasterTocManuf;
    F: File;
begin
    WriteLn();
    WriteLn('SACD: ', AFile);
    AssignFile(F, AFile);
    MasterToc := TMasterTocArea.Create();
    MasterToc.Load(F);
    TextToc := TMasterTextArea.Create();
    TextToc.Load(F);
    Manuf := TMasterTocManuf.Create;
    Manuf.Load(F);

    // Master TOC Album Info
    Album := MasterToc.GetAlbumInfo();
    Writeln('Master TOC Album Info:');
    Writeln(INDENT, 'Format Version: ', MasterToc.SpecVersion.ToString);
    Writeln(INDENT, 'Album Number: ', Album.SequenceNumber,' (from ', Album.SetSize,')');
    Writeln(INDENT, 'Album Catalog Number: ', Album.CatalogNumber);
    Writeln(INDENT, 'Album Genre: ', Album.Genres[1].Genre);
    WriteLn();

    // Master TOC Disc Info
    Disc := MasterToc.GetDiscInfo();
    Writeln('Master TOC Disc Info:');
    Writeln(INDENT, 'Creation: ', Disc.Date.ToString());
    Writeln(INDENT, 'Hybrid Disc: ', Disc.IsHybrid());
    Writeln(INDENT, 'SChTocAddress1: ', Disc.SChTocAddress1);
    Writeln(INDENT, 'SChTocAddress2: ', Disc.SChTocAddress2);
    Writeln(INDENT, 'MChTocAddress1: ', Disc.MChTocAddress1);
    Writeln(INDENT, 'MChTocAddress2: ', Disc.MChTocAddress2);
    Writeln(INDENT, 'SChTocLength: ', Disc.SChTocLength);
    Writeln(INDENT, 'MChTocLength: ', Disc.MChTocLength);
    Writeln(INDENT, 'Disc Genre: ', Disc.Genres[1].Genre);
    Writeln(INDENT, 'Disc Catalog Number: ', Disc.CatalogNumber);
    Writeln(INDENT, 'Disc Web Link: ', MasterToc.DiscWebLink);
    WriteLn();

    // Master Text TOC Info
    Writeln('Master Text TOC Info:');
    Writeln(INDENT, 'Disc Title: ', TextToc.DiscTitle);
    Writeln(INDENT, 'Disc Artist: ', TextToc.DiscArtist);
    Writeln(INDENT, 'Disc Publisher: ', TextToc.DiscPublisher);
    Writeln(INDENT, 'Disc Copyright: ', TextToc.DiscCopyright);
    Writeln(INDENT, 'Album Title: ', TextToc.AlbumTitle);
    Writeln(INDENT, 'Album Artist: ', TextToc.AlbumArtist);
    Writeln(INDENT, 'Album Publisher: ', TextToc.AlbumPublisher);
    Writeln(INDENT, 'Album Copyright: ', TextToc.AlbumCopyright);

    // Master TOC Manuf Info
    Writeln('Master TOC Manuf Info:');
    Writeln(INDENT, 'Header: ', Manuf.Header);
    WriteLn();


    // Just for testing and debug
    if Debug then
    begin
        Writeln();
        Writeln('TMasterTocArea dump:');
        TAppLogs.Dump(MasterToc[0]^.RawData, 256);
        Writeln();
        Writeln('MasterTextArea[0] dump:');
        TAppLogs.Dump(TextToc[0]^.RawData, 256);
        Writeln();
        Writeln('MasterManufArea[0] dump:');
        TAppLogs.Dump(Manuf[0]^.RawData, 256);
    end;
end;

//
// Program entry point
//
begin
    // Program Logs
    AppLogs := TAppLogs.Create('SACD');

    // Program Version
    AppVer := TSemVer.Create(0, 1, 0, DEBUG);

    // Parse input arguments
    AppArgs := TAppArgs.Create();
    AppArgs.Parse();
    if DEBUG then AppArgs.Print();

    // Any actios for testing
    if AppArgs.Has(CMD_TEST) then
    begin
        // TODO Need to add Linux variant
        //InputFile := TFileName(TEST_FILE_NAME_WIN);
        //PrintInfo(InputFile, True);
        InputFile := TFileName(TEST_FILE_NAME_WIN);
        WriteLn(Low(TLSData));
        WriteLn(High(TLSData));

        Exit;
    end;

    // Program command: version
    if AppArgs.HasVersion() then
    begin
        PrintVersion(); Exit;
    end;

    // Program command: help
    if AppArgs.HasHelp() then
    begin
        PrintHelp(); Exit;
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
