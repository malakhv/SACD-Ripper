{-------------------------------------------------------------------------}
{                                                                         }
{                          SACD-Ripper project                            }
{                                                                         }
{  Copyright (C) 1996-2022 Mikhail Malakhov <malakhv@gmail.com>           }
{                                                                         }
{  Licensed under the Apache License, Version 2.0 (the "License").        }
{  You may not use this file except in compliance with the License.       }
{  You may obtain a copy of the License at                                }
{                                                                         }
{     http://www.apache.org/licenses/LICENSE-2.0                          }
{                                                                         }
{  Unless required by applicable law or agreed to in writing, software    }
{  distributed under the License is distributed on an "AS IS" BASIS,      }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or        }
{  implied.                                                               }
{                                                                         }
{  See the License for the specific language governing permissions and    }
{  limitations under the License.                                         }
{                                                                         }
{-------------------------------------------------------------------------}

{-------------------------------------------------------------------------}
{ The SACD-Ripper utility to ripping audio content from SACD image files. }
{                                                                         }
{ Created: 14.08.2022                                                     }
{ Author: Mikhail.Malakhov [malakhv@gmail.com|http://mikhan.me/]          }
{-------------------------------------------------------------------------}

program sacd;                                                   { Program }

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, Classes, ProgVer, Mikhan.Util.AppArgs, Mikhan.Util.AppLogs,
    Mikhan.Rainbow.Scarlet, Mikhan.Util.AppVersion, Mikhan.Util.StrUtils,
    Mikhan.Util.Dump;

const

    { The name of this program. }
    PROG_NAME = 'SACD-Ripper';

    { The author of this program. }
    PROG_AUTHOR = 'Mikhail.Malakhov';

    { The copyright string. }
    PROG_COPYRIGHT = 'Copyright (C) 1996-2023 Mikhail Malakhov ' +
        '<malakhv@gmail.com>';

    { The common debug flag. }
    DEBUG = False;

{ Program command line arguments (commands and options) }
const

    { Program command: Print information about SACD disc. }
    CMD_INFO = 'info';

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{ Any stuff for debug and testing }
const

    { An input file to testing. }
    TEST_FILE_NAME_WIN = 'D:\Media\DireStraits.iso';

{ Global Scope }
var
    AppArgs: TAppArgs;      // Program command line arguments
    AppLogs: TAppLogs;      // Program logs
    InputFile: TFileName;   // Input file path

procedure PrintHelp();
const INDENT = '   ';
begin
    WriteLn(PROG_NAME, ' command options:');
    WriteLn(INDENT, 'info SACD_FILE_NAME - print info abour SACD disc');
    WriteLn(INDENT, '-v or --version - print program version');
end;

{
    Prints program version.
}
procedure PrintVersion();
begin
    WriteLn(PROG_NAME);
    WriteLn(ProgVer.GetVersion(DEBUG));
    WriteLn(PROG_COPYRIGHT);
end;

{
    Print an information about SACD disc.
}
procedure PrintInfo(AFile: TFileName; Debug: Boolean);
const INDENT = '   - ';
var
    InStream: TStream;
    MasterToc: TMasterTocArea;
    TextToc: TMasterTextArea;
    Album: TMasterTocAlbum;
    Disc: TMasterTocDisc;
    Manuf: TMasterTocManuf;
begin
    WriteLn();
    WriteLn('SACD: ', AFile);

    MasterToc := TMasterTocArea.Create();
    TextToc := TMasterTextArea.Create();
    Manuf := TMasterTocManuf.Create;

    try
        try
            InStream := TFileStream.Create(AFile, fmOpenRead);
            MasterToc.Load(InStream);
            TextToc.Load(InStream);
            Manuf.Load(InStream);
        except
            WriteLn('Cannot create file stream');
            Exit;
        end;
    finally
        try
            InStream.Free();
        except
            WriteLn('Close ERROR!');
        end;
        WriteLn('Close OK!');
    end;

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
        Dump(MasterToc[0]^.RawData, 256);
        Dump(MasterToc[1]^.RawData, 256);
        Writeln();
        Writeln('MasterTextArea[0] dump:');
        Dump(TextToc[0]^.RawData, 256);
        Writeln();
        Writeln('MasterManufArea[0] dump:');
        Dump(Manuf[0]^.RawData, 256);
    end;
end;

begin                                               { Program Entry Point }

    // Program Logs
    AppLogs := TAppLogs.Create('SACD');

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
