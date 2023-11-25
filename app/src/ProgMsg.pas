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
{ The Unit contains program messages.                                          }
{                                                                              }
{ Project: SACD-Ripper                                                         }
{ Created: 11.11.2023                                                          }
{ Author: Mikhail.Malakhov                                                     }
{------------------------------------------------------------------------------}

unit ProgMsg;                                                           { UNIT }

{$mode delphi}
{$h+}

interface                                                          { INTERFACE }

{ Program messages }
const

    { Program message: No input file. }
    MSG_NO_INPUT = 'Please, specify source file...';

    { Program message: Input file not found. }
    MSG_INPUT_NOT_FOUND = 'The source file not found...';

    { Program message: Cannot read data from data source. }
    MSG_CANNOT_READ_DATA = 'Cannot read data...';

{
  Prints program help message.
}
procedure PrintHelp(ProgName: String);

implementation                                                { IMPLEMENTETION }

const
    DEF_INDENT = '  ';

procedure PrintHelp(ProgName: String);
begin
    WriteLn(ProgName, ' command line options:');
    WriteLn(DEF_INDENT, ' info SACD_FILE_NAME      ' +
        '- The information about SACD disc.');
    WriteLn(DEF_INDENT, ' -v or --version          ' +
        '- The program version.');
    WriteLn(DEF_INDENT, ' -h (--help)              ' +
        '- Display this information.');
end;

end.                                                                     { END }

{------------------------------------------------------------------------------}
