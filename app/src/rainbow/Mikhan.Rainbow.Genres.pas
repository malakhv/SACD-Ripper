{--------------------------------------------------------------------}
{                                                                    }
{                       SACD-Ripper project                          }
{                                                                    }
{  Copyright (C) 1996-2023 Mikhail.Malakhov <malakhv@gmail.com>      }
{                                                                    }
{  Unauthorized copying of this file, via any medium is              }
{  strictly prohibited.                                              }
{                                                                    }
{         Confidential and Proprietary. All Rights Reserved.         }
{                                                                    }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{ The Unit includes some definitions from Scarlet Book Specification }
{ (part of Rainbow Books) exactly Genres Specifications.             }
{                                                                    }
{ For more details about Scarlet Book Specification and Super Audio  }
{ CD, please see Mikhan.Rainbow.Scarlet Unit.                        }
{                                                                    }
{ Package: Mikhan.Rainbow                                            }
{                                                                    }
{ Created: 26.03.2023                                                }
{ Author: Mikhail.Malakhov [malakhv@gmail.com|http://mikhan.me/]     }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{                       Scarlet Book                                 }
{                                                                    }
{ The Scarlet Book Standard describes Super Audio Compact Disc       }
{ format and represents a set of documents called Super Audio CD     }
{ System Description.                                                }
{                                                                    }
{ The Super Audio CD System Description has three parts:             }
{    - Part 1, Physical Specification.                               }
{    - Part 2, Audio Specification.                                  }
{    - Part 3, Copy Protection Specification.                        }
{                                                                    }
{ This Unit was develop folloving Part 2 of Super Audio CD System    }
{ Description.                                                       }
{--------------------------------------------------------------------}

unit Mikhan.Rainbow.Genres;

{$mode delphi}
{$h+}

interface

{--------------------------------------------------------------------}
{                           Music Genre                              }
{ A music genre is a conventional category that identifies some      }
{ pieces of music as belonging to a shared tradition or set of       }
{ conventions.                                                       }
{                                                                    }
{ For more details about SACD Genre format, please see Part 2 of     }
{ Super Audio CD System Description (section 1.7.2.2 and Annex B).   }
{--------------------------------------------------------------------}
type

    {
        The information about Genre of Album. 
    }
    TSACDGenreCode = record
        { The Genre's table. }
        Table: Byte;
        { The Genre's index in table. }
        Index: Word;
        { Returns Genre as a human readable string. }
        function GetGenre(): String;
        { The Genre represented as a human readable string. }
        property Genre: String read GetGenre;
    end;

implementation

{--------------------------------------------------------------------}
{ Common things                                                      }
{--------------------------------------------------------------------}

const

    { The index of Unknown genre. }
    INDEX_UNKNOWN = 0;

    {
        General Genre Table according to the Super Audio CD System
        Description.
    }
    GENERAL_TABLE: Array[INDEX_UNKNOWN..29] of String = (
        'Unknown', 'Not defined', 'Adult Contemporary',
        'Alternative Rock', 'Children’s Music', 'Classical',
        'Contemporary Christian', 'Country', 'Dance',
        'Easy Listening', 'Erotic', 'Folk', 'Gospel', 'Hip Hop',
        'Jazz', 'Latin', 'Musical', 'New Age', 'Opera',
        'Operetta', 'Pop Music', 'RAP', 'Reggae', 'Rock Music',
        'Rhythm & Blues', 'Sound Effects', 'Sound Track',
        'Spoken Word', 'World Music', 'Blues'
    );

    // TODO Need to find information about thid table
    //JAPANESE_TABLE: Array[INDEX_UNKNOWN..29] of String = (
    //)

{--------------------------------------------------------------------}
{ TSACDGenreCode staff                                               }
{--------------------------------------------------------------------}

function TSACDGenreCode.GetGenre(): String;
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

end.