{----------------------------------------------------------------}
{                                                                }
{                  Pascal Util Library (PUL)                     }
{                                                                }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>       }
{                                                                }
{  Unauthorized copying of this file, via any medium is          }
{  strictly prohibited.                                          }
{                                                                }
{       Confidential and Proprietary. All Rights Reserved.       }
{                                                                }
{----------------------------------------------------------------}

{----------------------------------------------------------------}
{ The Unit contains constants and methods to working             }
{ with strings.                                                  }
{                                                                }
{ Package: Mikhan.Util                                           }
{ Types: No                                                      }
{ Dependencies: No                                               }
{                                                                }
{ Created: 15.08.2022                                            }
{ Author: Mikhail.Malakhov                                       }
{----------------------------------------------------------------}

unit Mikhan.Util.StrUtils;

{$mode delphi}
{$h+}

Interface

const

    { The empty string. }
    EMPTY = '';

    { The space character. }
    CHAR_SPACE = ' ';

    { The dot character. }
    CHAR_DOT = '.';

    { The comma character. }
    CHAR_COMMA = ',';

    { The equal sign. }
    CHAR_EQUAL = '=';

    { The special char: end of the line. }
    CHAR_NEW_LINE = '\n';

    { The special char: empty line. }
    CHAR_EMPTY_LINE = '\n\n';

    { The special char: Slash }
    CHAR_SLASH = '/';

{
  Converts an array of bytes to string.
}
function BytesToStr(Source: Array of Byte): String;

{
  Returns True if the string is null, 0-length, or this string
  contains only whitespaces.
}
function IsEmpty(Source: String): Boolean;

{
  Trims blank characters (spaces and control characters) at
  the beginning and end of the specified string.
}
procedure TrimStr(var Source: String);

Implementation

uses SysUtils;

{
  Converts an array of bytes to string.
}
function BytesToStr(Source: Array of Byte): String;
var i: Integer;
begin
    Result := '';
    for i := Low(Source) to High(Source) do
        Result := Result + Char(Source[i]);
end;

{
  Returns True if the string is null, 0-length, or this string
  contains only whitespaces.
}
function IsEmpty(Source: String): Boolean;
begin
    Result := (Length(Source) <= 0) or (Length(Trim(Source)) <= 0);
end;

{
  Trims blank characters (spaces and control characters) at
  the beginning and end of the specified string.
}
procedure TrimStr(var Source: String);
begin
    Source := Trim(Source);
end;

end.