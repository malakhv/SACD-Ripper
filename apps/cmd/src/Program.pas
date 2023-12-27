{------------------------------------------------------------------------------}
{                                                                              }
{                             SACD-Ripper project                              }
{                                                                              }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>                }
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License"). You may     }
{  not use this file except in compliance with the License. You may obtain     }
{  a copy of the License at                                                    }
{                                                                              }
{     http://www.apache.org/licenses/LICENSE-2.0                               }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT   }
{  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.            }
{                                                                              }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ The SACD-Ripper utility is a small program to retrieve information and       }
{ ripping audio content from from SACD image files.                            }
{                                                                              }
{ Project: SACD-Ripper                                                         }
{ Created: 14.08.2022                                                          }
{ Author: Mikhail.Malakhov                                                     }
{------------------------------------------------------------------------------}

program sacd;                                                        { PROGRAM }

{$mode delphi}
{$h+}

uses
    SysUtils, Classes, ProgVer, ProgMsg, Reports, Mikhan.Util.StrUtils,
    Mikhan.Util.AppArgs, Mikhan.Util.AppLogs, Mikhan.Rainbow.Scarlet;

const

    { The name of this program. }
    PROG_NAME = 'SACD-Ripper';

    { The author of this program. }
    PROG_AUTHOR = 'Mikhail.Malakhov';

    { The copyright string. }
    PROG_COPYRIGHT = 'Copyright (C) 1996-2023 Mikhail Malakhov ' +
        '<malakhv@gmail.com>';

    { The common debug flag. }
    DEBUG = True;

{ Program command line arguments (commands and options) }
const

    { Program command: Print information about SACD disc. }
    CMD_INFO = 'info';

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{ Any stuff for debug and testing }
const

    { An input file to testing, windows. }
    TEST_FILE_NAME_WIN = 'D:\Media\DireStraits.iso';

    { An input file to testing, linux. }
    TEST_FILE_NAME_NIX = '/home/mikhan/Music/Blackout/Scorpions.iso';

{ Global Scope }
var
    AppArgs: TAppArgs;      // Program command line arguments
    AppLogs: TAppLogs;      // Program logs
    InputFile: TFileName;   // Input file path

{
    Prints program version.
}
procedure PrintVersion();
begin
    WriteLn(PROG_NAME);
    WriteLn(ProgVer.GetVersion(DEBUG));
    WriteLn(PROG_COPYRIGHT);
end;

procedure OnLoad(Sender: TObject; const FileName: TFileName;
    const Success: Boolean);
begin
    WriteLn('Load - ', FileName, ' - ', Success);
end;

{
    Print information about SACD disc.
}
procedure PrintInfo(AFile: TFileName; Debug: Boolean);
var SACDImg: TSACDImage;
begin
    WriteLn();
    SACDImg := TSACDImage.Create();
    SACDImg.OnLoad := OnLoad;
    if SACDImg.LoadFromFile(AFile) then
        Reports.PrintSACD(SACDImg, piAll, True);
    WriteLn();
    // Just for testing and debug
    if Debug then
    begin
        SACDImg.Dump();
    end;
end;

begin                                                            { ENTRY POINT }

    // Program Logs
    AppLogs := TAppLogs.Create('SACD');

    // Parse input arguments
    AppArgs := TAppArgs.Create();
    AppArgs.Parse();
    if DEBUG then AppArgs.Print();

    // Any actios for testing?
    if AppArgs.Has(CMD_TEST) then
    begin
        // TODO Need to add Linux variant
        InputFile := TFileName(TEST_FILE_NAME_WIN);
        PrintInfo(InputFile, DEBUG);
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
        PrintHelp(PROG_NAME); Exit;
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

end.                                                                     { END }

{------------------------------------------------------------------------------}
