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
{$H+}
{$T+}

{--------------------------------------------------------------------}
{                       Definitions                                  }
{ Album - An Album consists of one or more discs. All discs in an    }
{         Album must have the same Album Catalog Number.             }
{--------------------------------------------------------------------}

interface

uses Mikhan.Rainbow.Types, Mikhan.Rainbow.Genres;

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
{ For more details, please see Part 2 of Super Audio CD System       }
{ Description.                                                       }
{--------------------------------------------------------------------}
const

    { The length of one Logical Sector (LS) on SACD disc, in bytes. }
    SACD_LOGICAL_SECTOR_LENGTH = 2048;

    { The maximum number of Logical Sectors in SACD disc. }
    SACD_LOGICAL_SECTOR_COUNT = 196608;

type

    {
        The Logical Sector Number (LSN) - sequential number of a SACD
        disc logical sector.
    }
    TLSNumber = 0..SACD_LOGICAL_SECTOR_COUNT - 1;

    {
        The raw data of a disc sector represents as a byte array. This
        is a "Main Data" in a "Data Frame". For more details, please
        see Part 1 of Super Audio CD System Description (section 4.2.2).
    }
    TLSData = Array [0..SACD_LOGICAL_SECTOR_LENGTH - 1] of Byte;
    PLSData = ^TLSData;

    { The abstract Logical Sector (LS) with its number and data. }
    TSACDSector = record

        { Logical Sector Number (LSN), used to address the Sectors on
          the disc. }
        Number: TLSNumber;

        { The raw data of a disc sector represents as a byte array. }
        RawData: TLSData;

        { Returns a single byte by index. See Data property. }
        function GetByte(Index: Integer): Byte;

        { Returns offset of current sector in bytes. }
        function GetOffset(): Integer;

        { Array property to quick access to the single bytes by index. }
        property Data[Index: Integer]: Byte read GetByte; default;

        { Offset of current sector, in bytes. }
        property Offset: Integer read GetOffset;

        { Returns string from Start position to zero terminated char. }
        function GetString(Start: Integer): String;

        { Returns sector as a string (as is, with all special character). }
        function ToString(): String; overload;

        { Returns part of sector data from Start position as a string
          (as is, with all special character). }
        function ToString(Start, Count: Integer): String; overload;

        { Clear all sector data in this record. }
        procedure Clear();
    end;
    PSACDSector = ^TSACDSector;
    TSACDSectors = array of TSACDSector;

{--------------------------------------------------------------------}
{                       The SACD disc Area                           }
{                                                                    }
{ The SACD Volume Space of a disc is split into: File System Area,   }
{ DTCP Area, EKB1 Area, Master TOC Area, Rev TOC Area, Audio Areas,  }
{ Extension Area, EKB2 Area, Revocation Data Area and Extra Data     }
{ Area.                                                              }
{                                                                    }
{ In discs according to the Super Audio CD Specification Version 1.3 }
{ or lower, the EKB1 Area, the Rev TOC Area, the Extension Area,     }
{ the EKB2 Area and the Revocation Data Area do not exist.           }
{                                                                    }
{ For more details, please see Part 2 of Super Audio CD System       }
{ Description (section 2.2).                                         }
{--------------------------------------------------------------------}
const

    { The default length of SACD Area's header, in bytes. }
    SACD_AREA_HEADER_LENGTH = 8;

type

    {
        The abstract Area (a group of sequential sectors) on a SACD
        disc.
    }
    TSACDArea = class (TObject)
    private
        FFirst: TLSNumber;          // See First property
        FSize: TLSNumber;           // See Size property
        FSectors: TSACDSectors;     // See Sectors property
    protected
        { See Header property. }
        function GetHeader(): String; virtual;
        { See Sectors property. }
        function GetSector(Index : TLSNumber): PSACDSector;
    public

        { The header of this Area. }
        property Header: String read GetHeader;

        { The first sector number of this Area. }
        property First: TLSNumber read FFirst;

        { The array of sectors in this area. }
        property Sectors[Index : TLSNumber]: PSACDSector read GetSector; default;

        { The size of this Area, in sectors. }
        property Size: TLSNumber read FSize;

        { Returns true, if this object has a data. }
        function HasData(): Boolean;

        { Clear all sectos data. }
        procedure Clear(); virtual;

        { Load area data from file. }
        procedure Load(var AFile: File); virtual;

        { Construct a new instance of TSACDArea class with specified
          parameters. }
        constructor Create(First: TLSNumber); virtual; overload;

        { Construct a new instance of TSACDArea class with specified
          parameters. }
        constructor Create(First, Size: TLSNumber); virtual; overload;

        { Free all related resources. }
        destructor Destroy; override;
    end;

{--------------------------------------------------------------------}
{                   The Table of Contents (TOC)                      }
{                                                                    }
{ There are two types of Table of Contents (TOC), the highest level  }
{ is the Master TOC, and the several Area TOC for audio data.        }
{                                                                    }
{ The Master TOC contains Album and Disc information. The Area TOC   }
{ contains Track information. The Extra Data Area does not           }
{ contain an Area TOC.                                               }
{                                                                    }
{ For more details, please see Part 2 of Super Audio CD System       }
{ Description (sections 3.1 and 3.2).                                }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{                          Master TOC Area                           }
{                                                                    }
{ The Master TOC Area contains three identical copies of the Master  }
{ TOC. The Master TOC has a fixed size of 10 Sectors. The three      }
{ instances of the Master TOC are stored starting at LSN 510, 520    }
{ and 530. The structure of the Master TOC shows on following        }
{ figure:                                                            }
{                                                                    }
{   +--------------+--------------------------------+------------+   }
{   | Master TOC 0 | 8 Text Channels (Master Texts) | Manuf Info |   }
{   +--------------+--------------------------------+------------+   }
{                                                                    }
{--------------------------------------------------------------------}
type

    {
        Album or Disc Catalog Number. This string is padded at the
        end with space characters ($20). If a Catalog Number is not
        used, all bytes must be set to zero.
    }
    TCatalogNumber = String[15];

    { A date format that used in SACD specification. }
    { TDate = packed record
        Year: Word;
        Month, Day: Byte;
        function ToString(): String;
    end; }

    { The SACD format specification version. }
    TSACDSpecVersion = packed record
        Major, Minor: Byte;
        { Represents this version as a human readable string. }
        function ToString(): String;
    end;
    PSACDSpecVersion = ^TSACDSpecVersion;

    { The information about Album in Master TOC Area. }
    TMasterTocAlbum = packed record     // 48 bytes in total
        SetSize: Word;                  // 2 bytes
        SequenceNumber: Word;           // 2 bytes
        Reserved: DWord;                // 4 bytes
        CatalogNumber: TCatalogNumber;  // 16 bytes
        Genres: TSACDGenres;            // 16 bytes
    end;
    PMasterTocAlbum = ^TMasterTocAlbum;

    { BytesPair = packed record
        First: Byte;
        Second: Byte;
        procedure Reverse();
    end;

    BytesTrio = packed record
        First: Byte;
        Second: Byte;
        Third: Byte;
    end; }

    { The information about SACD disc in Master TOC Area. }
    TMasterTocDisc = packed record

        { The LSN of the first Sector of Area TOC-1 in the 2-Channel
          Stereo Area. If the 2-Channel Stereo Area is not present,
          this value must be zero. }
        SChTocAddress1: DWord;

        { The LSN of the first Sector of Area TOC-2 in the 2-Channel
          Stereo Area. If the 2-Channel Stereo Area is not present,
          this value must be zero. }
        SChTocAddress2: DWord;

        { The LSN of the first Sector of Area TOC-1 in the Multi
          Channel Area. If the Multi Channel Area is not present,
          this value must be zero. }
        MChTocAddress1: DWord;

        { The LSN of the first Sector of Area TOC-2 in the Multi
          Channel Area. If the Multi Channel Area is not present,
          this value must be zero. }
        MChTocAddress2: DWord;

        { The information about SACD disc, Hybrid or not, for
          example. }
        DiscFlags: Byte;

        { Just reserved to future using. }
        Reserved1, Reserved2, Reserved3: Byte;

        { The length in Sectors of Area TOC-A in the 2-Channel
          Stereo Area. If the 2-Channel Stereo Area is not present,
          this value must be zero. }
        ChTocLength: Word;

        { The length in Sectors of Area TOC-A in the Multi Channel
          Area. If the Multi Channel Area is not present, this
          value must be zero. }
        McTocLength: Word;

        { The catalog number of SACD disc that uniquely identifies
          each disc in an Album. }
        CatalogNumber: TCatalogNumber;

        { The SACD disc genres. }
        Genres: TSACDGenres;

        { The creation date of the SACD disc. }
        Date: TDiscDate;

        { Returns true, if this disc is Hybrid SACD. }
        function IsHybrid(): Boolean;

    end;
    PMasterTocDisc = ^TMasterTocDisc;

type

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
        const SPEC_VERSION_OFFSET = SACD_AREA_HEADER_LENGTH;

        { The offset of SACD Album information in this area. }
        const ALBUM_INFO_OFFSET = 16;

        { The offset of SACD Album Catalog Number in this area. }
        const ALBUM_CATALOG_NUMBER_OFFSET = ALBUM_INFO_OFFSET + 8;

        { The offset of SACD Disc information in this area. }
        const DISC_INFO_OFFSET = ALBUM_INFO_OFFSET + 48;

        { The offset of SACD Album Catalog Number in this area. }
        const DISC_CATALOG_NUMBER_OFFSET = DISC_INFO_OFFSET + 24;

        { See SpecVersion property. }
        function GetSpecVersion(): TSACDSpecVersion;
    public
        { The SACD format specification version. }
        property SpecVersion: TSACDSpecVersion read GetSpecVersion;
        {}
        function GetAlbumInfo(): TMasterTocAlbum;
        function GetDiscInfo(): TMasterTocDisc;

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

{--------------------------------------------------------------------}
{ Common things                                                      }
{--------------------------------------------------------------------}

{ Returns offset for specified disc LSN. }
function GetSectorOffset(LSNumber: TLSNumber): Integer;
begin
    Result := LSNumber * SACD_LOGICAL_SECTOR_LENGTH;
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

{ Reverse bytes in Word. }
function ReverseBytes(const Value: Word): Word;
begin
    Result :=  (((Value and $FF00) shr 8)
        or ((Value and $00FF) shl 8));
end;

{--------------------------------------------------------------------}
{ TSACDSector staff                                                  }
{--------------------------------------------------------------------}

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
begin
    // If Start is incorrect, let's start from beginning
    if (Start < 0) or (Start >= High(TLSData)) then
        Start := Low(TLSData);
    
    // Should convert all data?
    if Count <= 0 then Count := High(TLSData);

    // Make a string
    Result := '';
    while Count > 0 do
    begin
        Result := Result + Char(RawData[Start]);
        Dec(Count); Inc(Start);
    end;
end;

procedure TSACDSector.Clear();
var i: Integer;
begin
    // TODO Need to optimization
    for i := Low(RawData) to High(RawData) do
        RawData[i] := 0;
end;

{--------------------------------------------------------------------}
{ TSACDArea staff                                                    }
{--------------------------------------------------------------------}

constructor TSACDArea.Create(First: TLSNumber);
begin
    // By default, the length of area is 1 sector;
    Create(First, 1);
end;

constructor TSACDArea.Create(First, Size: TLSNumber);
begin
    FFirst := First;
    FSize := Size;
end;

destructor TSACDArea.Destroy();
begin
    Clear();
end;

procedure TSACDArea.Clear();
begin
    SetLength(FSectors, 0);
end;

function TSACDArea.GetHeader(): String;
begin
    if HasData() then
        // TODO May be we can use TSACDSector.GetString here?
        Result := Self[0].ToString(0, SACD_AREA_HEADER_LENGTH)
    else
        Result := Mikhan.Util.StrUtils.EMPTY;
end;

function TSACDArea.GetSector(Index : TLSNumber): PSACDSector;
begin
    Result := @FSectors[Index];
end;

function TSACDArea.HasData(): Boolean;
begin
    Result := Length(FSectors) > 0;
end;

procedure TSACDArea.Load(var AFile: File);
var i, sector, offset, sz: integer;
begin
    // Clear current data
    Clear();
    SetLength(FSectors, Size);

    // Open inpurt file read and setting up size of read chunk
    // to 1 byte
    Reset(AFile, 1);

    // Read data
    sector := First;
    offset := GetSectorOffset(First);
    sz := SizeOf(TLSData);
    try
        Seek(AFile, offset);
        for i:= Low(FSectors) to High(FSectors) do
        begin
            FSectors[i].Number := sector;
            BlockRead(AFile, FSectors[i].RawData, sz);
            Inc(sector);
        end;
    finally
        CloseFile(AFile);
    end;
end;

{--------------------------------------------------------------------}
{ TSACDSpecVersion staff                                             }
{--------------------------------------------------------------------}

function TSACDSpecVersion.ToString(): String;
begin
    Result := IntToStr(Self.Major) + '.' + IntToStr(Self.Minor);
end;

{--------------------------------------------------------------------}
{ TMasterTocArea staff                                               }
{--------------------------------------------------------------------}

constructor TMasterTocArea.Create();
begin
    inherited Create(510);
end;

function TMasterTocArea.GetSpecVersion(): TSACDSpecVersion;
var PVer: PSACDSpecVersion;
begin
    if HasData() then
    begin
        PVer := PSACDSpecVersion(PByte(@(Self[0]^.RawData))
            + SPEC_VERSION_OFFSET);
    end;
    Result := PVer^;
end;

function TMasterTocArea.GetAlbumInfo(): TMasterTocAlbum;
var I: Integer;
    PAlbum: PMasterTocAlbum;
begin
    if not HasData() then Exit;
    PAlbum := PMasterTocAlbum((PByte(@(Self[0]^.RawData))
            + ALBUM_INFO_OFFSET));
    Result := PAlbum^;
    // We should convert some pieces of data from
    // big-endian to little-endian
    Result.SetSize := SwapEndian(Result.SetSize);
    Result.SequenceNumber := SwapEndian(Result.SequenceNumber);
    for I := Low(Result.Genres) to High(Result.Genres) do
    begin
        Result.Genres[I].Index :=
            SwapEndian(Result.Genres[I].Index);
    end;
    // Fix CatalogNumber string
    Result.CatalogNumber := Trim(Self[0]^.ToString(
        ALBUM_CATALOG_NUMBER_OFFSET, 16));
end;

function TMasterTocArea.GetDiscInfo(): TMasterTocDisc;
var I: Integer;
    PDisc: PMasterTocDisc;
begin
    if not HasData() then Exit;
    PDisc := PMasterTocDisc((PByte(@(Self[0]^.RawData))
            + DISC_INFO_OFFSET));
    Result := PDisc^;
    // We should convert some pieces of data from
    // big-endian to little-endian
    Result.SChTocAddress1 := SwapEndian(Result.SChTocAddress1);
    Result.SChTocAddress2 := SwapEndian(Result.SChTocAddress2);
    Result.MChTocAddress1 := SwapEndian(Result.MChTocAddress1);
    Result.MChTocAddress2 := SwapEndian(Result.MChTocAddress2);
    Result.ChTocLength := SwapEndian(Result.ChTocLength);
    Result.McTocLength := SwapEndian(Result.McTocLength);
    Result.Date.Year := SwapEndian(Result.Date.Year);
    for I := Low(Result.Genres) to High(Result.Genres) do
    begin
        Result.Genres[I].Index :=
            ReverseBytes(Result.Genres[I].Index);
    end;
    // Fix CatalogNumber string
    Result.CatalogNumber := Trim(Self[0]^.ToString(
        DISC_CATALOG_NUMBER_OFFSET, 16));
end;

{--------------------------------------------------------------------}
{ TMasterTocDisc staff                                               }
{--------------------------------------------------------------------}

function TMasterTocDisc.IsHybrid(): Boolean;
const HYBRID_BIT = 7;
begin
    Result := ((Self.DiscFlags shr HYBRID_BIT) and 1) = 1;
end;

{--------------------------------------------------------------------}
{ TMasterTextArea staff                                              }
{--------------------------------------------------------------------}

constructor TMasterTextArea.Create();
begin
    inherited Create(511);
end;

function TMasterTextArea.GetStringByPtr(PtrOffset: Integer): String;

    function GetPtr(Offset: Integer): Integer;
    begin
        if Offset < SACD_LOGICAL_SECTOR_LENGTH - 1 then
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
