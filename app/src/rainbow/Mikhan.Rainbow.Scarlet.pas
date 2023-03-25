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
{ The Unit includes some difinitions from Scarlet Book Specification }
{ (part of Rainbow Books).                                           }
{                                                                    }
{ Package: Mikhan.Rainbow                                            }
{                                                                    }
{ Created: 14.08.2022                                                }
{ Author: Mikhail.Malakhov [malakhv@gmail.com|http://mikhan.me/]     }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{                       Super Audio CD                               }
{                                                                    }
{ Super Audio CD (SACD) is an optical disc format for audio storage  }
{ introduced in 1999. It was developed jointly by Sony and Philips   }
{ Electronics and intended to be the successor to the Compact Disc   }
{ (CD) format.                                                       }
{                                                                    }
{ The SACD format allows multiple audio channels (i.e. surround      }
{ sound or multichannel sound). It also provides a higher bit rate   }
{ and longer playing time than a conventional CD.                    }
{                                                                    }
{ An SACD is designed to be played on an SACD player. A hybrid SACD  }
{ contains a Compact Disc Digital Audio (CDDA) layer and can also be }
{ played on a standard CD player.                                    }
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
{ This Unit was develop folloving Part 1 and Part 2 of Super Audio   }
{ CD System Description.                                             }
{--------------------------------------------------------------------}

unit Mikhan.Rainbow.Scarlet;

{$mode delphi}
{$h+}

interface

const

    { The max number of sectors in SACD disc. }
    SACD_MAX_SECTOR_COUNT = 123456; // TODO Need to specify

{--------------------------------------------------------------------}
{                The SACD disc Logical Sector (LS).                  }
{                                                                    }
{ The length of a Logical Sector must be 2048 bytes, which is equal  }
{ to the length of a Physical Sector (PS). Each Logical Sector of a  }
{ volume is identified by a unique Logical Sector Number (LSN).      }
{                                                                    }
{ Logical Sector Numbers must be consecutive integers assigned in    }
{ ascending order to the Physical Sectors on the disc. The Logical   }
{ Sector Number 0 must be assigned to Sector Start PSN of Physical   }
{ Layer 0.                                                           }
{                                                                    }
{ For more details, please see "Super Audio CD Part 2, Disc Layout"  }
{ document.                                                          }
{--------------------------------------------------------------------}
const

    { The length of one Logical Sector (LS) on SACD disc in bytes. }
    SACD_SECTOR_LENGTH = 2048;

type

    { The sequential number of a SACD disc sector ("LSN"). }
    TSectorNumber = 0..SACD_MAX_SECTOR_COUNT - 1;

    {
        The raw data of a disc sector represents as a byte array. This
        is a "Main Data" in a "Data Frame", see SACD Physical
        Specification for more details.
    }
    TSectorData = Array [0..SACD_SECTOR_LENGTH - 1] of Byte;

    { The abstract Logical Sector (LS) with its number and data. }
    TSACDSector = record
        { Logical Sector Number (LSN), used to address the Sectors on the disc. }
        Number: TSectorNumber;
        { The raw data of a disc sector represents as a byte array. }
        RawData: TSectorData;

        { Returns a single byte by index. See RawData property. }
        function GetByte(Index: Integer): Byte;
        { Array property to quick access to the single bytes by index. }
        property Data[Index: Integer]: Byte read GetByte; default;

        { Returns offset for current sector in bytes. }
        function GetOffset(): Integer;

        { Returns string from Start position to zero terminated char. }
        function GetString(Start: Integer): String;
        { Returns sector as a string (as is with all special character). }
        function ToString(): String; overload;
        { Returns part of sector data from Start position as a string (as is
          with all special character). }
        function ToString(Start, Count: Integer): String; overload;

        { Clear all sector data in this record. }
        procedure ClearData();
    end;
    PSACDSector = ^TSACDSector;
    TSACDSectors = array of TSACDSector;


{--------------------------------------------------------------------}
{ SACD Area.                                                         }
{--------------------------------------------------------------------}
type

    { The abstract area (a group of sequential sectors) on a SACD disc. }
    TSACDArea = class (TObject)
    private
        FFirstSector: Integer;      // See FirstSector property
        FSectorCount: Integer;      // See SectorCount property
        FSectors: TSACDSectors;     // See Sectors property
    protected
        { The default length of area header in bytes. }
        const AREA_HEADER_LENGTH = 8;

        { See Header property. }
        function GetHeader(): String; virtual;
        { See Sectors property. }
        function GetSector(Index : Integer): PSACDSector;
    public
        { The header of this area. }
        property Header: String read GetHeader;

        { The first sector number for this area. }
        property FirstSector: Integer read FFirstSector;
        { The number of sectors in this area. }
        property SectorCount: Integer read FSectorCount;
        { The array of sectors in this area. }
        property Sectors[Index : Integer]: PSACDSector read GetSector; default;

        { Returns true, if this object has a data. }
        function HasData(): Boolean;

        { Clear all sectos data. }
        procedure ClearData(); virtual;

        { Load area data from file. }
        procedure Load(var AFile: File); virtual;

        { Construct a new instance of TSACDArea class with specified parameters. }
        constructor Create(FirstSector: Integer); virtual; overload;
        { Construct a new instance of TSACDArea class with specified parameters. }
        constructor Create(FirstSector, SectorCount: Integer); virtual; overload;

        {Free all related resources. }
        destructor Destroy; override;
    end;

type

    { The SACD format specification version. }
    TSACDSpecVersion = record
        Major, Minor: Byte;
        { Represents SACD specification version as a human readable string. }
        function ToString(): String;
    end;

    TMasterTocAlbum = record
        SetSize: Word;
        Number: Word;
        CatalogNumber: String;
        Genre: Integer;
    end;

    {
        The Master TOC area (Master_TOC_0) contains general information on the disc, such as
        the size and location of the Audio Areas, album information, disc catalog number, disc
        genre and disc date. This area has 'SACDMTOC' signature.
    }
    TMasterTocArea = class (TSACDArea)
    protected
        { The lenght of Master TOC in sectors. }
        const MASTER_TOC_LENGTH = 10;

        { The offset of SACD format specification version in this area. }
        const SPEC_VERSION_OFFSET = 8;

        { See SpecVersion property. }
        function GetSpecVersion(): TSACDSpecVersion;
    public
        { The SACD format specification version. }
        property SpecVersion: TSACDSpecVersion read GetSpecVersion;
        {}
        function GetAlbumInfo(): TMasterTocAlbum;

        constructor Create();
    end;

    {
        The Master Text area (Master_Text) contains all general text information that is related
        with the Album and with the Disc. The size of this area is one SACD sector. This area
        has 'SACDText' signature. This area is a part of Master TOC.
    }
    TMasterTextArea = class (TSACDArea)
    protected
        { The offset of significant data in this area. }
        const AREA_DATA_OFFSET = 16;

        { The offset of Album data in this area. }
        const ALBUM_DATA_OFFSET = AREA_DATA_OFFSET;
        { The offset of pointer (2 bytes) to Album Title string in this area. }
        const ALBUM_TITLE_PTR_OFFSET = ALBUM_DATA_OFFSET;
        { The offset of pointer (2 bytes) to Album Artist string in this area. }
        const ALBUM_ARTIST_PTR_OFFSET = ALBUM_DATA_OFFSET + 2;
        { The offset of pointer (2 bytes) to Album Publisher string in this area. }
        const ALBUM_PUBLISHER_PTR_OFFSET = ALBUM_DATA_OFFSET + 4;
        { The offset of pointer (2 bytes) to Album Copyright string in this area. }
        const ALBUM_COPYRIGHT_PTR_OFFSET = ALBUM_DATA_OFFSET + 6;

        { The offset of Disc data in this area. }
        const DISC_DATA_OFFSET = AREA_DATA_OFFSET + 14;
        { The offset of pointer (2 bytes) to Disc Title string in this area. }
        const DISC_TITLE_PTR_OFFSET = DISC_DATA_OFFSET + 2;
        { The offset of pointer (2 bytes) to Disc Artist string in this area. }
        const DISC_ARTIST_PTR_OFFSET = DISC_DATA_OFFSET + 4;
        { The offset of pointer (2 bytes) to Disc Publisher string in this area. }
        const DISC_PUBLISHER_PTR_OFFSET = DISC_DATA_OFFSET + 6;
        { The offset of pointer (2 bytes) to Disc Copyright string in this area. }
        const DISC_COPYRIGHT_PTR_OFFSET = DISC_DATA_OFFSET + 8;

        function DoGetAlbumTitle(): String;
        function DoGetAlbumArtist(): String;
        function DoGetAlbumPublisher(): String;
        function DoGetAlbumCopyright(): String;

        function DoGetDiscTitle(): String;
        function DoGetDiscArtist(): String;
        function DoGetDiscPublisher(): String;
        function DoGetDiscCopyright(): String;

        function GetStringByPtr(PtrOffset: Integer): String;
    public
        property DiscTitle: String read DoGetDiscTitle;
        property DiscArtist: String read DoGetDiscArtist;
        property DiscPublisher: String read DoGetDiscPublisher;
        property DiscCopyright: String read DoGetDiscCopyright;
        property AlbumTitle: String read DoGetAlbumTitle;
        property AlbumArtist: String read DoGetAlbumArtist;
        property AlbumPublisher: String read DoGetAlbumPublisher;
        property AlbumCopyright: String read DoGetAlbumCopyright;
        constructor Create();
    end;


//type
//    TSacdFile = class(Tancestor)

implementation

uses SysUtils, Mikhan.Util.StrUtils;

{
    Common things
}

{ Returns offset for specified disc sector number. }
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

{ Converts pair of bytes to integer. }
function BytesToInt(First, Second: Byte): Integer;
begin
    Result := (Integer(First) shl 8) or Second;
end;

{ Converts pair of bytes to word. }
function BytesToWord(First, Second: Byte): Word;
begin
    Result := (Word(First) shl 8) or Second;
end;

{
    TSACDSector
}

function TSACDSector.GetByte(Index: Integer): Byte;
begin
    Result := RawData[Index];
end;

function TSACDSector.GetOffset(): Integer;
begin
    Result := GetSectorOffset(Number);
end;

function TSACDSector.GetString(Start: Integer): String;
begin
    Result := String(PAnsiChar(@RawData[Start]));
end;

function TSACDSector.ToString(): String;
begin
    Result := ToString(0, -1);
end;

function TSACDSector.ToString(Start, Count: Integer): String;
var pos: Integer;
begin
    // Check start position
    if Start > 0 then
        pos := Start
    else
        pos := Low(RawData); // In any case let's start from begining
    
    // Should convert all data?
    if Count < 0 then Count := High(RawData);
    // Make a string
    Result := '';
    while Count > 0 do
    begin
        Result := Result + Char(RawData[pos]);
        Dec(count); Inc(pos);
    end;
end;

procedure TSACDSector.ClearData();
var i: Integer;
begin
    // TODO Need to optimization
    for i := Low(RawData) to High(RawData) do
        RawData[i] := 0;
end;

{
    TSACDArea
}

constructor TSACDArea.Create(FirstSector: Integer);
begin
    // By default, the length of area is 1 sector;
    Create(FirstSector, 1);
end;

constructor TSACDArea.Create(FirstSector, SectorCount: Integer);
begin
    FFirstSector := FirstSector;
    FSectorCount := SectorCount;
end;

destructor TSACDArea.Destroy();
begin
    ClearData();
end;

procedure TSACDArea.ClearData();
begin
    SetLength(FSectors, 0);
end;

function TSACDArea.GetHeader(): String;
begin
    if HasData() then
        // TODO May be we can use TSACDSector.GetString here?
        Result := Self[0].ToString(0, AREA_HEADER_LENGTH)
    else
        Result := Mikhan.Util.StrUtils.EMPTY;
end;

function TSACDArea.GetSector(Index : Integer): PSACDSector;
begin
    Result := @FSectors[Index];
end;

function TSACDArea.HasData(): Boolean;
begin
    Result := Length(FSectors) > 0;
end;

procedure TSACDArea.Load(var AFile: File);
var i, sector, offset, size: integer;
begin
    // Clear current data
    ClearData();
    SetLength(FSectors, SectorCount);

    // Open inpurt file read and setting up size of read chunk
    // to 1 byte
    Reset(AFile, 1);

    // Read data
    sector := FirstSector;
    offset := GetSectorOffset(FirstSector);
    size := SizeOf(TSectorData);
    try
        Seek(AFile, offset);
        for i:= Low(FSectors) to High(FSectors) do
        begin
            FSectors[i].Number := sector;
            BlockRead(AFile, FSectors[i].RawData, size);
            Inc(sector);
        end;
    finally
        CloseFile(AFile);
    end;
end;

{
    TSACDSpecVersion
}

function TSACDSpecVersion.ToString(): String;
begin
    Result := IntToStr(Self.Major) + '.' + IntToStr(Self.Minor);
end;

{
    TMasterTocArea
}

constructor TMasterTocArea.Create();
begin
    inherited Create(510);
end;

function TMasterTocArea.GetSpecVersion(): TSACDSpecVersion;
var Ver: TSACDSpecVersion;
begin
    Ver.Major := 0;
    Ver.Minor := 0;
    if HasData() then
    begin
        Ver.Major := Self[0]^.GetByte(SPEC_VERSION_OFFSET);
        Ver.Minor := Self[0]^.GetByte(SPEC_VERSION_OFFSET + 1);
    end;
    Result := Ver;
end;

function TMasterTocArea.GetAlbumInfo(): TMasterTocAlbum;
var info: TMasterTocAlbum;
begin
    Result := info;
    if not HasData() then Exit;
    info.SetSize := BytesToInt(Self[0]^[16], Self[0]^[17]);
    info.Number := BytesToInt(Self[0]^[18], Self[0]^[19]);
    Result := info;
end;

{
    TMasterTextArea
}

constructor TMasterTextArea.Create();
begin
    inherited Create(511);
end;

function TMasterTextArea.GetStringByPtr(PtrOffset: Integer): String;

    function GetPtr(Offset: Integer): Integer;
    begin
        if Offset < SACD_SECTOR_LENGTH - 1 then
            Result := BytesToInt(Self[0]^[Offset], Self[0]^[Offset + 1])
        else
            Result := -1;
    end;

var start: Integer;
begin
    Result := '';
    if not HasData() then Exit;
    start := GetPtr(PtrOffset);
    if start = 0 then Exit;
    Result := Self[0].GetString(start);
end;

function TMasterTextArea.DoGetAlbumArtist(): String;
begin
    Result := Self.GetStringByPtr(ALBUM_ARTIST_PTR_OFFSET);
end;

function TMasterTextArea.DoGetAlbumTitle(): String;
begin
    Result := Self.GetStringByPtr(ALBUM_TITLE_PTR_OFFSET);
end;

function TMasterTextArea.DoGetAlbumPublisher(): String;
begin
    Result := Self.GetStringByPtr(ALBUM_PUBLISHER_PTR_OFFSET);
end;

function TMasterTextArea.DoGetAlbumCopyright(): String;
begin
    Result := Self.GetStringByPtr(ALBUM_COPYRIGHT_PTR_OFFSET);
end;

function TMasterTextArea.DoGetDiscTitle(): String;
begin
    Result := Self.GetStringByPtr(DISC_TITLE_PTR_OFFSET);
end;

function TMasterTextArea.DoGetDiscArtist(): String;
begin
    Result := Self.GetStringByPtr(DISC_ARTIST_PTR_OFFSET);
end;

function TMasterTextArea.DoGetDiscPublisher(): String;
begin
    Result := Self.GetStringByPtr(DISC_PUBLISHER_PTR_OFFSET);
end;

function TMasterTextArea.DoGetDiscCopyright(): String;
begin
    Result := Self.GetStringByPtr(DISC_COPYRIGHT_PTR_OFFSET);
end;

end.
