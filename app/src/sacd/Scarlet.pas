//
// Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>
//
// Confidential and Proprietary. All Rights Reserved.
// Unauthorized copying of this file, via any medium is strictly prohibited.
//

{
    The Unit includes some difinitions from Scarlet Book specification.
    Author: Mikhail.Malakhov
}

unit Scarlet;

// Compiler options
{$mode delphi}
{$h+}

Interface

const
    { The length of one sector on disk in bytes. }
    SACD_SECTOR_LENGTH = 2048;

type
    { The raw data of a disk sector represents as byte array. }
    TSectorData = Array [0..SACD_SECTOR_LENGTH - 1] of Byte;

type
    { The abstract sector with its number and data. }
    RSector = record
        Number: Integer;
        Data: TSectorData;
        { Returns offset for current sector in bytes. }
        function GetOffset(): Integer;
        { Returns part of sector data from Start position as string. }
        function ToString(Start, Count: Integer): String;
        { Clear all sector data in this record. }
        procedure ClearData();
    end;

//type
//    TSacdFile = class(Tancestor)

Implementation

{ Converts an array of bytes to string. }
function ByteArrayToStr(Source: Array of Byte): String;
var i: Integer;
begin
    Result := '';
    for i := Low(Source) to High(Source) do
        Result := Result + Char(Source[i]);
end;

{ Returns offset for current sector in bytes. }
function RSector.GetOffset(): Integer;
begin
    Result := Number * SACD_SECTOR_LENGTH;
end;

{ Returns part of sector data from PosStart to PosEnd as string. }
function RSector.ToString(Start, Count: Integer): String;
var pos: Integer;
begin
    // Check start position
    if Start > 0 then
        pos := Start
    else
        pos := Low(Data); // In any case let's start from begining
    
    // Should convert all data?
    if Count < 0 then
        Count := High(Data);
    // Make a string
    Result := '';
    while count > 0 do
    begin
        Result := Result + Char(Data[pos]);
        Dec(count); Inc(pos);
    end;
end;

{ Clear all sector data in this record. }
procedure RSector.ClearData();
var i: Integer;
begin
    // TODO Need to optimization
    for i := Low(Data) to High(Data) do
        Data[i] := 0;
end;

end.