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
{ Rainbow Books) exactly Genres Specifications.                                }
{                                                                              }
{ For more details about Scarlet Book Specification and Super Audio CD, please }
{ see Mikhan.Rainbow.Scarlet Unit.                                             }
{                                                                              }
{ Project: SACD-Ripper                                                         }
{ Package: Mikhan.Rainbow                                                      }
{ Types: TSACDGenre, TSACDGenres                                               }
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

unit Mikhan.Rainbow.Genres;                                             { UNIT }

{$mode delphi}
{$h+}

interface                                                          { INTERFACE }

{------------------------------------------------------------------------------}
{                                Music Genre                                   }
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
        Table: Byte;
        { Reserved data. }
        Reserved: Byte;
        { Genre_Index: The Genre's index in table. }
        Index: Word;
        { Returns Genre as a human readable string. }
        function GetGenre(): String;
        { The Genre represented as a human readable string. }
        property Genre: String read GetGenre;
    end;

    {
        Genre4: The Album or Disc Genres.
    }
    TSACDGenres = Array [1..4] of TSACDGenre;
    PSACDGenres = ^TSACDGenres;

implementation                                                { IMPLEMENTETION }

{------------------------------------------------------------------------------}
{ Common                                                                       }
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
    //JAPANESE_TABLE: Array[INDEX_UNKNOWN..29] of String = (
    //)

{ Converts bytes in Word. }
function ReverseBytes(const Value: Word): Word;
begin
    Result :=  (((Value and $FF00) shr 8)
        or ((Value and $00FF) shl 8));
end;

{------------------------------------------------------------------------------}
{ TSACDGenre                                                                   }
{------------------------------------------------------------------------------}

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

end.                                                                     { END }

{------------------------------------------------------------------------------}
