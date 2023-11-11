{-------------------------------------------------------------------------}
{                                                                         }
{                          SACD-Ripper project                            }
{                                                                         }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>           }
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
{ The Unit includes some definitions from Scarlet Book Specification      }
{ (part of Rainbow Books) exactly Basic Types definitions.                }
{                                                                         }
{ For more details about Scarlet Book Specification and Super Audio CD,   }
{ please see Mikhan.Rainbow.Scarlet Unit.                                 }
{                                                                         }
{ Package: Mikhan.Rainbow                                                 }
{                                                                         }
{ Created: 26.03.2023                                                     }
{ Author: Mikhail.Malakhov [malakhv@gmail.com|http://mikhan.me/]          }
{-------------------------------------------------------------------------}

{-------------------------------------------------------------------------}
{                              Scarlet Book                               }
{                                                                         }
{ The Scarlet Book Standard describes Super Audio Compact Disc format and }
{ represents a set of documents called Super Audio CD System Description. }
{                                                                         }
{ The Super Audio CD System Description has three parts:                  }
{    - Part 1, Physical Specification.                                    }
{    - Part 2, Audio Specification.                                       }
{    - Part 3, Copy Protection Specification.                             }
{                                                                         }
{ This Unit was develop folloving Part 2 of Super Audio CD System         }
{ Description.                                                            }
{-------------------------------------------------------------------------}

unit Mikhan.Rainbow.Types;

{$mode delphi}                                   { Compiler mode - Delphi }
{$h+}

interface                                             { Interface section }

type

    {
        Album or Disc Catalog Number. This string is padded at the
        end with space characters ($20). If a Catalog Number is not
        used, all bytes must be set to zero.
    }
    TCatalogNumber = String[15];

    {
        The link to a web page with information about this SACD disc.
        This string is a sequence of char followed by a zero byte
        (null-terminated). A zero length is allowed, which contains
        one zero byte. The maximum length is 128 bytes.
    }
    //TDiscWebLink = String[128];

type

    {
        The SACD format specification version.
    }
    TSACDSpecVersion = packed record
        { The major version number of SACD specification. }
        Major: Byte;
        { The minor version number of SACD specification. }
        Minor: Byte;
        { Represents this version as a human readable string. }
        function ToString(): String;
    end;
    PSACDSpecVersion = ^TSACDSpecVersion;

type

    {
        A date format that used in SACD specification. The value zero
        for Year, Month and Day is only allowed if a valid Date is
        not available.
    }
    TDiscDate = packed record
        { The value of the Year field has a range of 0..65535. }
        Year: Word;
        { The value of the Month field has a range of 0..12,
          with 1..12 meaning January. }
        Month: Byte;
        { The value of the Day field has a range of 0..31. }
        Day: Byte;
        { Represents a creation date of SACD disc as a human readable
          string. }
        function ToString(): String;
    end;

implementation                                   { Implementation section }

uses SysUtils;

{-------------------------------------------------------------------------}
{ Common things                                                           }
{-------------------------------------------------------------------------}

// Empty

{-------------------------------------------------------------------------}
{ TSACDSpecVersion staff                                                  }
{-------------------------------------------------------------------------}

function TSACDSpecVersion.ToString(): String;
begin
    Result := IntToStr(Self.Major) + '.' + IntToStr(Self.Minor);
end;

{-------------------------------------------------------------------------}
{ TDiscDate staff                                                         }
{-------------------------------------------------------------------------}

function TDiscDate.ToString(): String;
begin
    Result := IntToStr(Self.Year) + '-';
    if Self.Month < 10 then Result := Result + '0';
    Result := Result + IntToStr(Month) + '-';
    if Self.Day < 10 then Result := Result + '0';
    Result := Result + IntToStr(Day);
end;

end.

{-------------------------------------------------------------------------}
{ END                                                                     }
{-------------------------------------------------------------------------}
