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
{ The Unit includes some definitions from Scarlet Book Specification (part of  }
{ Rainbow Books) exactly Basic Types definitions.                              }
{                                                                              }
{ For more details about Scarlet Book Specification and Super Audio CD, please }
{ see Mikhan.Rainbow.Scarlet Unit.                                             }
{                                                                              }
{ Project: SACD-Ripper                                                         }
{ Package: Mikhan.Rainbow                                                      }
{ Types: TSACDDate, TSACDVersion, TSACDGenre, TSACDGenres                      }
{                                                                              }
{ Created: 26.03.2023                                                          }
{ Authors: Mikhail.Malakhov                                                    }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{                              Scarlet Book                                    }
{                                                                              }
{ The Scarlet Book Standard describes Super Audio Compact Disc format and      }
{ represents a set of documents called Super Audio CD System Description.      }
{                                                                              }
{ The Super Audio CD System Description has three parts:                       }
{    - Part 1, Physical Specification.                                         }
{    - Part 2, Audio Specification.                                            }
{    - Part 3, Copy Protection Specification.                                  }
{                                                                              }
{ This Unit was develop folloving Part 1 and Part 2 of Super Audio CD System   }
{ Description.                                                                 }
{------------------------------------------------------------------------------}

unit Mikhan.Rainbow.Types;                                              { UNIT }

{$MODE DELPHI}
{$H+}

interface                                                          { INTERFACE }

type

    {
        Album or Disc Catalog Number. This string is padded at the end with
        space characters ($20). If a Catalog Number is not used, all bytes
        must be set to zero.
    }
    TSACDCatalogNumber = String[15]; // 16 bytes

    {
        The link to a web page with information about this SACD disc. This
        string is a sequence of char followed by a zero byte (null-terminated).
        A zero length is allowed, which contains one zero byte. The maximum
        length is 128 bytes.
    }
    //TDiscWebLink = String[128];

type

    {
        Date: A date format that used in SACD specification. The value zero for
        Year, Month and Day is only allowed if a valid Date is not available.
    }
    TSACDDate = packed record  // 4 bytes in total
        { Year: The value of the Year field has a range of 0..65535. }
        Year: Word;  // 2 bytes
        { Month: The value of the Month field has a range of 0..12, with 1..12
            meaning January. }
        Month: Byte;  // 1 byte
        { Day: The value of the Day field has a range of 0..31. }
        Day: Byte;  // 1 byte
        { Represents a creation date of SACD disc as a human readable string. }
        function ToString(): String;
    end;

type

    {
        Spec_Version: The SACD format specification version.
    }
    TSACDVersion = packed record // 2 bytes in total
        { Major_Version: The major version number of SACD specification. }
        Major: Byte;  // 1 byte
        { Minor_Version: The minor version number of SACD specification. }
        Minor: Byte;  // 1 byte
        { Represents this version as a human readable string. }
        function ToString(): String;
    end;
    PSACDVersion = ^TSACDVersion;

{------------------------------------------------------------------------------}
{                                 Music Genre                                  }
{                                                                              }
{ A music genre is a conventional category that identifies some pieces of      }
{ music as belonging to a shared tradition or set of conventions.              }
{                                                                              }
{ For more details about SACD Genre format, please see Part 2 of Super Audio   }
{ CD System Description (section 1.7.2.2 and Annex B).                         }
{------------------------------------------------------------------------------}

type

    {
        Genre_Code: The information about Genre.
    }
    TSACDGenre = packed record // 4 bytes in total
        { Genre_Table: The Genre's table. }
        Table: Byte;  // 1 byte
        { Reserved data. }
        Reserved: Byte;  // 1 byte
        { Genre_Index: The Genre's index in table. }
        Index: Word;  // 2 bytes
        { Returns Genre as a human readable string. }
        function GetGenre(): String;
        { The Genre represented as a human readable string. }
        property Genre: String read GetGenre;
    end;

    {
        Genre4: The Album or Disc Genres.
    }
    TSACDGenres = Array [1..4] of TSACDGenre;  // 16 bytes
    PSACDGenres = ^TSACDGenres;

{------------------------------------------------------------------------------}
{                                Text Channel                                  }
{                                                                              }
{ Text Channels contains the definition of the Text Channels used in the TOC.  }
{ All text in Text (in TOC) must be according to the definitions in Text       }
{ Channels. Text Channels contains language / character set combination for    }
{ each Text in TOC.                                                            }
{                                                                              }
{ For more details about Text Channels, Text Channel, please see Part 2 of     }
{ Super Audio CD System Description (sections 1.7.2.8 and 3.1.1.4).            }
{------------------------------------------------------------------------------}

type

    {
        Language_Code: The ISO 639 Language Code that is used with Text
        Channel. All text in Text Channel must be according to this Language
        Code. The value $0000 is not allowed.
    }
    TSACDLangCode = packed record // 2 bytes in total
        RawData: Array[1..2] of Char;
        { Represents a Language Code as a human readable string. }
        function ToString(): String;
    end;

    {
        The Text Channel's data. Text in the TOC using one language / character
        set combination. One Text Channel can only use one character set. See
        also TSACDTextChannels.
    }
    TSACDTextChannel = packed record // 4 bytes in total

        { Language_Code: The ISO 639 Language Code that is used with
            appropriate Text Channel. The value $0000 is not allowed. }
        LangCode: TSACDLangCode;

        { Character_Set_Code: The character set used for appropriate Text
            Channel. }
        CharSet: Byte;  // 1 byte

        { Reserved: Just reserved to future using. }
        Reserved: Byte;  // 1 byte

        { Represents a Text Channel data as a human readable string. }
        function ToString(): String;

    end;

    {
        Text_Channels: The definition of the Text Channels used in the Master
        TOC and the Area TOC. All Master_Text must be according to the
        definitions in Text_Channels. See also TSACDTextChannel.
    }
    TSACDTextChannels = packed record  // 40 bytes in total

        { N_Text_Channels: The number of Text Channels used. The maximum
            allowed value is 8. A value of zero is allowed. This value must be
            equal to the number of used Text Channels. }
        Count: Byte;  // 1 byte

        { Reserved: Just reserved to future using. }
        Reserved: Array [1..7] of Byte;  // 7 bytes

        { Array of Text Channels. See TSACDTextChannel. }
        Channels: Array [1..8] of TSACDTextChannel; // 32 bytes (8 * 4)

    end;
    PSACDTextChannels = ^TSACDTextChannels;

implementation                                                { IMPLEMENTETION }

uses SysUtils;

{------------------------------------------------------------------------------}
{ Common                                                                       }
{------------------------------------------------------------------------------}

// Empty

{------------------------------------------------------------------------------}
{ TSACDDate                                                                    }
{------------------------------------------------------------------------------}

function TSACDDate.ToString(): String;
begin
    Result := IntToStr(Self.Year) + '-';
    if Self.Month < 10 then Result := Result + '0';
    Result := Result + IntToStr(Month) + '-';
    if Self.Day < 10 then Result := Result + '0';
    Result := Result + IntToStr(Day);
end;

{------------------------------------------------------------------------------}
{ TSACDVersion                                                                 }
{------------------------------------------------------------------------------}

function TSACDVersion.ToString(): String;
begin
    Result := IntToStr(Self.Major) + '.' + IntToStr(Self.Minor);
end;

{------------------------------------------------------------------------------}
{ TSACDGenre                                                                   }
{------------------------------------------------------------------------------}

const

    { The index of Unknown genre. }
    INDEX_UNKNOWN = 0;

    {
        General Genre Table according to the Super Audio CD System Description.
    }
    GENERAL_TABLE: Array[INDEX_UNKNOWN..29] of String = (
        'Unknown', 'Not defined', 'Adult Contemporary', 'Alternative Rock',
        'Children’s Music', 'Classical', 'Contemporary Christian', 'Country',
        'Dance', 'Easy Listening', 'Erotic', 'Folk', 'Gospel', 'Hip Hop',
        'Jazz', 'Latin', 'Musical', 'New Age', 'Opera', 'Operetta',
        'Pop Music', 'RAP', 'Reggae', 'Rock Music', 'Rhythm & Blues',
        'Sound Effects', 'Sound Track', 'Spoken Word', 'World Music', 'Blues'
    );

    // TODO Need to find information about this table
    //JAPANESE_TABLE: Array[INDEX_UNKNOWN..29] of String = ();

function TSACDGenre.GetGenre(): String;
begin
    // Right now, we support only General Genre Table
    if Self.Table <> 1 then
    begin
        Result := GENERAL_TABLE[INDEX_UNKNOWN];
        Exit;
    end;

    // Check Genre Index
    if (Self.Index <= Low(GENERAL_TABLE))
        or (Self.Index > High(GENERAL_TABLE)) then
    begin
        Result := GENERAL_TABLE[INDEX_UNKNOWN];
        Exit;
    end;

    // All is OK, let's return real genre
    Result := GENERAL_TABLE[Self.Index];
end;

{------------------------------------------------------------------------------}
{ TSACDLangCode                                                                }
{------------------------------------------------------------------------------}

function TSACDLangCode.ToString(): String;
begin
    Result := Self.RawData[1] + Self.RawData[2];
end;

{------------------------------------------------------------------------------}
{ TSACDTextChannel                                                             }
{------------------------------------------------------------------------------}

function TSACDTextChannel.ToString(): String;
begin
    Result := Self.LangCode.ToString() + ' (' + IntToStr(Self.CharSet) + ')';
end;

end.                                                                     { END }

{------------------------------------------------------------------------------}
