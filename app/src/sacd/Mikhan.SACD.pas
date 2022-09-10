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

{-----------------------------------------------------------------}
{ The Unit contains constants and methods to working              }
{ with SACD image file.                                           }
{                                                                 }
{ Package: Mikhan.Util                                            }
{ Types: TODO                                                     }
{ Dependencies: Mikhan.SACD                                       }
{                                                                 }
{ Created: 14.08.2022                                             }
{ Author: Mikhail.Malakhov                                        }
{-----------------------------------------------------------------}

Unit Mikhan.SACD;

{$mode delphi}
{$h+}

Interface

uses Mikhan.SACD.Scarlet;

const

    { The SACD image file magic data represents as a string }
    SACD_MAGIC_CHAR = 'SACDMTOC';

    { The SACD image file magic data offset }
    SACD_MAGIC_OFFSET = $FF000;

{ Checks that specified file is SACD image file. }
function isSacdImage(Name: String): Boolean;

procedure ReadSector(var AFile: File; Number: Integer; var Sector: TSACDSector);

procedure PrintArray(Source: Array of Byte; Limit: Integer; InHex: Boolean);

Implementation

uses
    SysUtils, Mikhan.Util.StrUtils;

const
    DEBUG = False;

{ Converts an array of bytes to string. }
procedure PrintArray(Source: Array of Byte; Limit: Integer; InHex: Boolean);
const COLUMN_LIMIT = $F;
var i, col: Integer;
    val: Byte;
begin
    col := COLUMN_LIMIT;
    if Limit <= 0 then Limit := MaxInt;
    for i := Low(Source) to High(Source) do
    begin
        val := Source[i];
        if (InHex) then
            Write(IntToHex(val, 2))
        else
            Write(val);
        Write(' ');
        Dec(Limit);
        if Limit <= 0 then break;
        Dec(col);
        if col < 0 then
        begin
            Writeln('');
            col := COLUMN_LIMIT;
        end;
    end;
    Writeln('');
end;

{ Checks that specified file is SACD image file. }
function isSacdImage(Name: String): Boolean;
var
    f: File;
    buf: Array [1..SizeOf(SACD_MAGIC_CHAR)] of Byte;
begin
    // Open inpurt file read and setting up
    // size of read chunk to 1 byte
    AssignFile(f, Name);
    Reset(f, 1);
    try
        Seek(f, SACD_MAGIC_OFFSET);
        BlockRead(f, buf, SizeOf(buf));
    finally
        CloseFile(f);
    end;

    if DEBUG then
    begin
        Write('isSacdImage: ');
        PrintArray(buf, 0, True);
    end;
    // Check magic data
    Result := SACD_MAGIC_CHAR = BytesToStr(buf);
end;

procedure ReadSector(var AFile: File; Number: Integer; var Sector: TSACDSector);
begin
    // Configure sector record
    Sector.ClearData();
    Sector.Number := Number;
    // Open inpurt file read and setting up
    // size of read chunk to 1 byte
    Reset(AFile, 1);
    try
        Seek(AFile,Sector.GetOffset());
        BlockRead(AFile, Sector.RawData, SizeOf(Sector.RawData));
    finally
        CloseFile(AFile);
    end;
    if DEBUG then
    begin
        Writeln('ReadSector ', Sector.Number, ':');
        PrintArray(Sector.RawData, 8, True);
    end;
end;

end.
