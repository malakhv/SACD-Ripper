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
{ (part of Rainbow Books) exactly Basic Types definitions.           }
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

unit Mikhan.Rainbow.Types;

{$mode delphi}
{$h+}

interface

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
        
        { Represents a date of SACD disc as a human readable string. }
        function ToString(): String;

    end;

implementation

uses SysUtils;

{--------------------------------------------------------------------}
{ Common things                                                      }
{--------------------------------------------------------------------}


{--------------------------------------------------------------------}
{ TDiscDate staff                                                    }
{--------------------------------------------------------------------}

function TDiscDate.ToString(): String;
begin
    Result := IntToStr(Self.Year) + '-';
    if Self.Month < 10 then Result := Result + '0';
    Result := Result + IntToStr(Month) + '-';
    if Self.Day < 10 then Result := Result + '0';
    Result := Result + IntToStr(Day);
end;

end.
