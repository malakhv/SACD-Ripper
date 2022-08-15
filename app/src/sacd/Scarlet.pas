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

    SACD_MAX_SECTOR_COUNT = 123456; // TODO Need to specify

    { The length of one sector on disk in bytes. }
    SACD_SECTOR_LENGTH = 2048;

type
    { The raw data of a disk sector represents as a byte array. }
    TSectorData = Array [0..SACD_SECTOR_LENGTH - 1] of Byte;

type
    TSectorNumber = 0..High(ShortInt);

type
    { The abstract sector with its number and data. }
    RSector = record
        Number: Integer;
        Data: TSectorData;
        function GetByte(Index: Integer): Byte;
        property RawData[Index: Integer]: Byte read GetByte; default;
        { Returns offset for current sector in bytes. }
        function GetOffset(): Integer;
        { Returns sector as a string. }
        function ToString(): String; overload;
        { Returns part of sector data from Start position as a string. }
        function ToString(Start, Count: Integer): String; overload;
        { Clear all sector data in this record. }
        procedure ClearData();
    end;

type

    TSACDArea = class (TObject)
    private
        FFirstSector: Integer;
        FSectorCount: Integer;
        RawData: Array of RSector;
    protected
        function DoGetData(Index : Integer): RSector;
    public
        property Data[Index : Integer]: RSector Read DoGetData; default;
        property SectorCount: Integer read FSectorCount;
        property FirstSector: Integer read FFirstSector;
        { Returns true, if current object has a data. }
        function HasData(): Boolean;
        { Clear all sectos data. }
        procedure ClearData();
        { Load Aread data from file. }
        procedure Load(var AFile: File);
        { Construct a new instance of TAppLogs class with specified parameters. }
        constructor Create(FirstSector, SectorCount: Integer);
        {Free all related resources. }
        destructor Destroy; override;
    end;

    TTocArea = class (TSACDArea)
    protected
        function DoGetHeader: String; virtual;
    public
        property Header: String read DoGetHeader;
        constructor Create(FirstSector: Integer);
    end;


    TMasterTocArea = class (TTocArea)
    public
        constructor Create();
    end;

    TMasterTextArea = class (TTocArea)
    protected
        function DoGetAlbumTitle(): String;
        function DoGetAlbumArtist(): String;
        function GetPtr(From: Integer): Integer;
    public
        property AlbumTitle: String read DoGetAlbumTitle;
        property AlbumArtist: String read DoGetAlbumArtist;
        constructor Create();
    end;


//type
//    TSacdFile = class(Tancestor)

Implementation

uses MyStrUtils;

{
    Common things
}

{ Returns offset for specified disk sector number. }
function GetSectorOffset(SectorNumber: Integer): Integer;
begin
    Result := SectorNumber * SACD_SECTOR_LENGTH;
end;

{ Converts an array of bytes to string. }
function ByteArrayToStr(Source: Array of Byte): String;
var i: Integer;
begin
    Result := '';
    for i := Low(Source) to High(Source) do
        Result := Result + Char(Source[i]);
end;

function BytesToInt(First, Second: Byte): Integer;
begin
    Result := (Integer(First) shl 8) or Second;
end;
{
    RSector
}

function RSector.GetByte(Index: Integer): Byte;
begin
    Result := Data[Index];
end;

{ Returns offset for current sector in bytes. }
function RSector.GetOffset(): Integer;
begin
    Result := GetSectorOffset(Number);
end;

{ Returns sector as a string. }
function RSector.ToString(): String;
begin
    Result := ToString(0, -1);
end;

{ Returns part of sector data from Start position as a string. }
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

{
    TSACDArea
}

{ Construct a new instance of TAppLogs class with specified parameters. }
constructor TSACDArea.Create(FirstSector, SectorCount: Integer);
begin
    FFirstSector := FirstSector;
    FSectorCount := SectorCount;
end;

{ Free all related resources. }
destructor TSACDArea.Destroy();
begin
    ClearData();
end;

function TSACDArea.DoGetData(Index : Integer): RSector;
begin
    Result := RawData[Index];
end;

{ Returns true, if current object has a data. }
function TSACDArea.HasData(): Boolean;
begin
    Result := Length(RawData) > 0;
end;

{ Clear all sectos data. }
procedure TSACDArea.ClearData();
begin
    if not HasData() then SetLength(RawData, 0);
end;

{ Load Aread data from file. }
procedure TSACDArea.Load(var AFile: File);
var i, sector, offset: integer;
begin
    // Clear current data
    ClearData();
    SetLength(RawData, SectorCount);

    // Open inpurt file read and setting up
    // size of read chunk to 1 byte
    Reset(AFile, 1);

    // Read data
    sector := FirstSector;
    offset := GetSectorOffset(FirstSector);
    try
        Seek(AFile, offset);
        for i:= Low(RawData) to High(RawData) do
        begin
            RawData[i].Number := sector;
            BlockRead(AFile, RawData[i].Data, SizeOf(RawData[i].Data));
            Inc(sector);
        end;
    finally
        CloseFile(AFile);
    end;
end;

{
    TTocArea
}

constructor TTocArea.Create(FirstSector: Integer);
begin
    inherited Create(FirstSector, 1);
end;

function TTocArea.DoGetHeader: String;
begin
    if HasData() then
        Result := Self[0].ToString(0, 8)
    else
        Result := MyStrUtils.EMPTY;
end;

{
    TMasterTocArea
}

constructor TMasterTocArea.Create();
begin
    inherited Create(510);
end;

{
    TMasterTextArea
}

constructor TMasterTextArea.Create();
begin
    inherited Create(511);
end;

function TMasterTextArea.GetPtr(From: Integer): Integer;
begin
    if HasData() then
        Result := BytesToInt(Self[0][From], Self[0][From + 1])
    else
        Result := -1;
end;

function TMasterTextArea.DoGetAlbumArtist(): String;
const
    ALBUM_ARTIST_PTR = 18;
var s_pos, e_pos: Integer;
begin
    if not HasData() then
    begin
        Result := '';
        Exit;
    end;
    s_pos := GetPtr(ALBUM_ARTIST_PTR);
    e_pos := GetPtr(ALBUM_ARTIST_PTR + 2); // The next data is end of the current data
    //WriteLn(s_pos, ' - ', e_pos);
    Result := Self[0].ToString(s_pos, e_pos - s_pos);
end;

function TMasterTextArea.DoGetAlbumTitle(): String;
const
    ALBUM_TITLE_PTR = 16;
var s_pos, e_pos: Integer;
begin
    if not HasData() then
    begin
        Result := '';
        Exit;
    end;
    s_pos := GetPtr(ALBUM_TITLE_PTR);
    e_pos := GetPtr(ALBUM_TITLE_PTR + 2); // The next data is end of the current data
    //WriteLn(s_pos, ' - ', e_pos);
    Result := Self[0].ToString(s_pos, e_pos - s_pos);
end;

end.