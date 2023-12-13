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
{ Rainbow Books).                                                              }
{                                                                              }
{ Project: SACD-Ripper                                                         }
{ Package: Mikhan.Rainbow                                                      }
{ Types: TLSNumber, TSACDSector, TMasterTocArea                                }
{                                                                              }
{ Dependencies: Mikhan.Util.StrUtils, Mikhan.Rainbow.Types                     }
{                                                                              }
{ Created: 14.08.2022                                                          }
{ Authors: Mikhail.Malakhov                                                    }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{                             Super Audio CD                                   }
{                                                                              }
{ Super Audio CD (SACD) is an optical disc format for audio storage introduced }
{ in 1999. It was developed jointly by Sony and Philips Electronics and        }
{ intended to be the successor to the Compact Disc (CD) format.                }
{                                                                              }
{ The SACD format allows multiple audio channels (i.e. surround sound or       }
{ multichannel sound). It also provides a higher bit rate and longer playing   }
{ time than a conventional CD.                                                 }
{                                                                              }
{ An SACD is designed to be played on an SACD player. A hybrid SACD contains a }
{ Compact Disc Digital Audio (CDDA) layer and can also be played on a standard }
{ CD player.                                                                   }
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

{------------------------------------------------------------------------------}
{                                 Definitions                                  }
{                                                                              }
{ Album    - An Album consists of one or more discs. All discs in an Album     }
{            must have the same Album Catalog Number.                          }
{                                                                              }
{ DSD      - Direct Stream Digital, the one bit audio signal. A DSD signal can }
{            either be DST Coded DSD, or Plain DSD.                            }
{                                                                              }
{ EOS      - End of Sector.                                                    }
{                                                                              }
{ Frame    - A block of data belonging to a certain Time Code. The data can be }
{            either Audio Data, Supplementary Data, Padding or it can be       }
{            Multiplexed. The playing time of a Frame is 1/75 Sec.             }
{                                                                              }
{ Reserved - All fields labeled Reserved are reserved for future               }
{            standardization. All Reserved fields must be set to zero.         }
{                                                                              }
{ Sector   - The 2048 bytes of Main Data in a Data Frame (for more details see }
{            Part 1 of Super Audio CD System Description (chapters 4.2.2 and   }
{            4.2.7).                                                           }
{                                                                              }
{ TNO      - Track Number. A Track Number is the sequence number of a Track.   }
{            The first Track Number in an Audio Area is one. The maximum       }
{            number of Tracks in an Audio Area is 255.                         }
{                                                                              }
{ TOC      - Table Of Contents. Album and disc related information is stored   }
{            in the Master TOC. Area and track related information is stored   }
{            in the Area TOC.                                                  }
{                                                                              }
{ Track    - A Track is a contiguous area on the disc with audio information   }
{            and with one and the same Track Number.                           }
{------------------------------------------------------------------------------}

unit Mikhan.Rainbow.Scarlet;                                            { UNIT }

{$MODE DELPHI}
{$H+}
{$T+}

interface                                                          { INTERFACE }

uses Classes, Mikhan.Rainbow.Types;

{------------------------------------------------------------------------------}
{                      The SACD disc Logical Sector (LS)                       }
{                                                                              }
{ The length of a Logical Sector must be 2048 bytes, which is equal to the     }
{ length of a Physical Sector (PS). Each Logical Sector of a volume is         }
{ identified by a unique Logical Sector Number (LSN).                          }
{                                                                              }
{ Logical Sector Numbers must be consecutive integers assigned in ascending    }
{ order to the Physical Sectors on the disc. The Logical Sector Number 0 must  }
{ be assigned to Sector Start PSN of Physical Layer 0.                         }
{                                                                              }
{ For more details, please see Part 2 of Super Audio CD System Description.    }
{------------------------------------------------------------------------------}

const

    { The length of one Logical Sector (LS) on SACD disc, in bytes. }
    SACD_LOGICAL_SECTOR_LENGTH = 2048;

    { The maximum number of Logical Sectors in SACD disc. }
    SACD_LOGICAL_SECTOR_COUNT = 196608;

type

    {
        The Logical Sector Number (LSN) - sequential number of a SACD disc
        logical sector.
    }
    TLSNumber = 0..SACD_LOGICAL_SECTOR_COUNT - 1;

    {
        The raw data of a disc sector represents as a byte array. This is a
        "Main Data" in a "Data Frame". For more details, please see Part 1 of
        Super Audio CD System Description (section 4.2.2).
    }
    TLSData = Array [0..SACD_LOGICAL_SECTOR_LENGTH - 1] of Byte;
    PLSData = ^TLSData;

    {
        The abstract Logical Sector (LS) with its number and data.
    }
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

{------------------------------------------------------------------------------}
{                            The SACD disc Area                                }
{                                                                              }
{ The SACD Volume Space of a disc is split into: File System Area, DTCP Area,  }
{ EKB1 Area, Master TOC Area, Rev TOC Area, Audio Areas, Extension Area, EKB2  }
{ Area, Revocation Data Area and Extra Data Area.                              }
{                                                                              }
{ In discs according to the Super Audio CD Specification Version 1.3 or lower, }
{ the EKB1 Area, the Rev TOC Area, the Extension Area, the EKB2 Area and the   }
{ Revocation Data Area do not exist.                                           }
{                                                                              }
{ The general structure of the Volume Space shows on following figur:          }
{                                                                              }
{   0                          510          540       544                      }
{   +-------------+------+------+------------+---------+------------------+    }
{   | File System | DTCP | EKB1 | Master TOC | Rev TOC | 2-Channel Stereo |    }
{   |     Area    | Area | Area |    Area    |   Area  |       Area       |    }
{   +-------------+----------------------------------+--------------------+    }
{                                                                              }
{   +---------------+-----------+------+------------+---------+-----------+    }
{   | Multi Channel | Extension | EKB2 | Revocation |     Extra Data      |    }
{   |      Area     |    Area   | Area |  Data Area |        Area         |    }
{   +---------------+-----------+------+------------+---------------------+    }
{                                                                              }
{ If a 2-Channel Stereo Area is present, the 2-Channel Stereo Area must start  }
{ at LSN 544. If a 2-Channel Stereo Area is not present, the Multi Channel     }
{ Area must start at LSN 544. Note that the 2-Channel Stereo Area must be      }
{ present on discs according to the Super Audio CD Specification Version 2.0   }
{ or higher, and that the 2-Channel Stereo Area optionally is present on discs }
{ according to the Super Audio CD Specification Version 1.3 or lower.          }
{                                                                              }
{ For more details, please see Part 2 of Super Audio CD System Description     }
{ (section 2.2).                                                               }
{------------------------------------------------------------------------------}

const

    { The default length of SACD Area's header, in bytes. }
    SACD_AREA_HEADER_LENGTH = 8;

    { The offset of significant data in this area. }
    SACD_AREA_DATA_OFFSET = 16;

type

    {
        The abstract Area (a group of sequential sectors) on a SACD disc.
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

        { The header of this Area. In current implementation this is
            header of first sector. }
        property Header: String read GetHeader;

        { The first sector number of this Area. }
        property First: TLSNumber read FFirst;

        { The array of sectors in this area. }
        property Sectors[Index : TLSNumber]: PSACDSector read GetSector;
            default;

        { The size of this Area, in sectors. }
        property Size: TLSNumber read FSize;

        { Returns true, if this object has a data. }
        function HasData(): Boolean;

        { Clear all sectos data. }
        procedure Clear(); virtual;

        { Load area data from a file. }
        procedure Load(var AFile: File); overload; virtual;

        { Load area data from a stream. }
        procedure Load(const AStream: TStream); overload; virtual;

        { Construct a new instance of TSACDArea class with specified
            parameters. }
        constructor Create(First: TLSNumber); virtual; overload;

        { Construct a new instance of TSACDArea class with specified
            parameters. }
        constructor Create(First, Size: TLSNumber); virtual; overload;

        { Free all related resources. }
        destructor Destroy; override;
    end;

{------------------------------------------------------------------------------}
{                         The Table of Contents (TOC)                          }
{                                                                              }
{ There are two types of Table of Contents (TOC), the highest level is the     }
{ Master TOC, and the several Area TOC for audio data.                         }
{                                                                              }
{ The Master TOC contains Album and Disc information. The Area TOC contains    }
{ Track information. The Extra Data Area does not contain an Area TOC.         }
{                                                                              }
{ For more details, please see Part 2 of Super Audio CD System Description     }
{ (sections 3.1 and 3.2).                                                      }
{                                                                              }
{                               Master TOC Area                                }
{                                                                              }
{ The Master TOC Area contains three identical copies of the Master TOC. The   }
{ Master TOC has a fixed size of 10 Sectors. The three instances of the Master }
{ TOC are stored starting at LSN 510, 520, 530. The structure of the Master    }
{ TOC shows on following figure:                                               }
{                                                                              }
{       +--------------+----------------------------------+------------+       }
{       | Master TOC 0 | 8 x Text Channels (Master Texts) | Manuf Info |       }
{       +--------------+----------------------------------+------------+       }
{                                                                              }
{------------------------------------------------------------------------------}
type

    {
        Album_Info: The information about SACD Album in Master TOC Area (48
        bytes in total).
    }
    TMasterTocAlbum = packed record  // 48 bytes in total

        { Album_Set_Size: The total number of discs in this album. The minimum
            allowed value is one. All discs in one Album must have the same
            value. }
        SetSize: Word;  // 2 bytes

        { Album_Sequence_Number: The sequence number of this disc within the
            album. This must be numbered consecutively over all discs in an
            Album. The first disc from an Album must have set this value to 1.
            The maximum allowed value is equals Album_Set_Size. }
        SequenceNumber: Word;  // 2 bytes

        { Reserved: Just reserved to future using. }
        Reserved: DWord; // 4 bytes

        { Album_Catalog_Number: The catalog number of this album. All discs in
            one Album must have the same catalog number. This fields must be
            padded at the end with bytes with space characters (#20). }
        CatalogNumber: TSACDCatalogNumber; // 16 bytes

        { Album_Genre: The genres associated with this Super Audio CD Album
            (min 0 and max 4). It is recommended that all discs in one Album
            have the same genres. }
        Genres: TSACDGenres; // 16 bytes

    end;
    PMasterTocAlbum = ^TMasterTocAlbum;

    {
        Disc_Info: The information about SACD Disc in Master TOC Area (64
        bytes in total).
    }
    TMasterTocDisc = packed record  // 64 bytes in total

        { 2CH_TOC_1_Address: The LSN of the first Sector of Area TOC-1 in the
            2-Channel Stereo Area. If the 2-Channel Stereo Area is not present,
            this value must be zero. }
        SChTocAddress1: DWord;  // 4 bytes

        { 2CH_TOC_2_Address: The LSN of the first Sector of Area TOC-2 in the
            2-Channel Stereo Area. If the 2-Channel Stereo Area is not present,
            this value must be zero. }
        SChTocAddress2: DWord; // 4 bytes

        { MC_TOC_1_Address: The LSN of the first Sector of Area TOC-1 in the
            Multi Channel Area. If the Multi Channel Area is not present, this
            value must be zero. }
        MChTocAddress1: DWord;  // 4 bytes

        { MC_TOC_2_Address: The LSN of the first Sector of Area TOC-2 in the
            Multi Channel Area. If the Multi Channel Area is not present, this
            value must be zero. }
        MChTocAddress2: DWord;  // 4 bytes

        { Disc_Flags: The information about SACD disc, Hybrid or not, for
            example. }
        DiscFlags: Byte;  // 1 byte

        { Reserved: Just reserved to future using. }
        Reserved1, Reserved2, Reserved3: Byte;  // 3 bytes

        { 2CH_TOC_Length: The length in Sectors of Area TOC-A in the 2-Channel
            Stereo Area. If the 2-Channel Stereo Area is not present, this
            value must be zero. }
        SChTocLength: Word; // 2 bytes

        { MC_TOC_Length: The length in Sectors of Area TOC-A in the Multi
            Channel Area. If the Multi Channel Area is not present, this value
            must be zero. }
        MChTocLength: Word;  // 2 bytes

        { Disc_Catalog_Number: The catalog number of SACD disc that uniquely
            identifies each disc in an Album. }
        CatalogNumber: TSACDCatalogNumber;  // 16 bytes

        { Disc_Genre: The genres associated with this Super Audio CD Album
            (min 0 and max 4). It is recommended that all discs in one Album
            have the same genres. }
        Genres: TSACDGenres;  // 16 bytes

        { Disc_Date: The creation date of the SACD disc. }
        Date: TSACDDate;  // 4 bytes

        { Reserved: Just reserved to future using. }
        Reserved4, Reserved5, Reserved6, Reserved7: Byte;  // 4 bytes

        { Returns true, if this disc is Hybrid SACD. }
        function IsHybrid(): Boolean;

    end;
    PMasterTocDisc = ^TMasterTocDisc;

type

    {
        The Master TOC area (Master_TOC_0) contains general information on the
        disc, such as the size and location of the Audio Areas, album info,
        disc catalog number, disc genre and disc date. This area has 'SACDMTOC'
        signature.
    }
    TMasterTocArea = class (TSACDArea)
    protected

        { See DiscWebLink property. }
        function GetDiscWebLink(): String;

        { See SpecVersion property. }
        function GetSpecVersion(): TSACDVersion;

        { See TextChannelsCount property. }
        //function GetTextChannelsCount(): Byte;

    public

        { The link to a web page with information about SACD disc. }
        property DiscWebLink: String read GetDiscWebLink;

        { The SACD format specification version. }
        property SpecVersion: TSACDVersion read GetSpecVersion;

        { The numbers of Text Channels in this Area. }
        //property TextChannelsCount: Byte read GetTextChannelsCount;

        { Returns the information about SACD Album which stored in Master
            TOC Area. }
        function GetAlbumInfo(): TMasterTocAlbum;

        { Returns the information about SACD Disc which stored in Master
            TOC Area. }
        function GetDiscInfo(): TMasterTocDisc;

        { Returns the definitions of Text Channels in this Area. }
        function GetTextChannels(): TSACDTextChannels;

        { Construct a new instance with default parameters. }
        constructor Create();

    end;

    {
        The Master Text area (Master_Text) contains all general text info that
        is related with the Album and with the Disc. The size of this area is
        one SACD sector. This area has 'SACDText' signature. This area is a
        part of Master TOC.
    }
    TMasterTextArea = class (TSACDArea)
    private
        function GetStringByPtr(PtrOffset: Integer): String;
    protected
        function GetAlbumTitle(): String;
        function GetAlbumArtist(): String;
        function GetAlbumPublisher(): String;
        function GetAlbumCopyright(): String;
        function GetDiscTitle(): String;
        function GetDiscArtist(): String;
        function GetDiscPublisher(): String;
        function GetDiscCopyright(): String;
    public
        property AlbumTitle: String read GetAlbumTitle;
        property AlbumArtist: String read GetAlbumArtist;
        property AlbumPublisher: String read GetAlbumPublisher;
        property AlbumCopyright: String read GetAlbumCopyright;
        property DiscTitle: String read GetDiscTitle;
        property DiscArtist: String read GetDiscArtist;
        property DiscPublisher: String read GetDiscPublisher;
        property DiscCopyright: String read GetDiscCopyright;
        constructor Create();
    end;

    TMasterTocManuf = class (TSACDArea)
    private
        { The offset of significant data in this area. }
        const AREA_DATA_OFFSET = SACD_AREA_HEADER_LENGTH;

    protected

    public

        { Construct a new instance with default parameters. }
        constructor Create();

    end;


//type
//    TSacdFile = class(Tancestor)

implementation                                                { IMPLEMENTETION }

uses SysUtils, Mikhan.Util.StrUtils;

{------------------------------------------------------------------------------}
{ Common                                                                       }
{------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------}
{ TSACDSector                                                                  }
{------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------}
{ TSACDArea                                                                    }
{------------------------------------------------------------------------------}

constructor TSACDArea.Create(First: TLSNumber);
begin
    // By default, the size of area is 1 sector;
    Create(First, 1);
end;

constructor TSACDArea.Create(First, Size: TLSNumber);
begin
    FFirst := First; FSize := Size;
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

    // Open input file read and setting up size of read chunk to 1 byte
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

procedure TSACDArea.Load(const AStream: TStream);
var Offset, Current, i: Integer;
begin
    // Clear current data
    Clear();
    SetLength(FSectors, Self.Size);

    // Empty input?
    if AStream.Size <= 0 then Exit;

    // Read raw data
    Current := First;
    Offset := GetSectorOffset(Self.First);
    try
        if Offset > 0 then AStream.Seek(Offset, soBeginning);
        for i:= Low(FSectors) to High(FSectors) do
        begin
            FSectors[i].Number := Current;
            AStream.ReadBuffer(FSectors[i].RawData, SizeOf(TLSData));
            Inc(Current);
        end;
    except
        Clear(); Raise;
    end;
end;

{------------------------------------------------------------------------------}
{ TMasterTocDisc                                                               }
{------------------------------------------------------------------------------}

const

    { The Hybr bit must be set to 1 on a Hybrid Disc and 0 on a not-Hybrid
        Disc. }
    MASTER_TOC_DISC_HYBRID_BIT = 7;

function TMasterTocDisc.IsHybrid(): Boolean;
begin
    Result := ((Self.DiscFlags shr MASTER_TOC_DISC_HYBRID_BIT) and 1) = 1;
end;

{------------------------------------------------------------------------------}
{ TMasterTocArea                                                               }
{------------------------------------------------------------------------------}

const

    { The lenght of Master TOC, in sectors. }
    MASTER_TOC_LENGTH = 10;

    { The offset of SACD format specification version in this area. }
    MASTER_TOC_SPEC_VERSION_OFFSET = SACD_AREA_HEADER_LENGTH;

    { The offset of SACD Album information in this area. }
    MASTER_TOC_ALBUM_INFO_OFFSET = SACD_AREA_DATA_OFFSET;

    { The offset of SACD Album Catalog Number in this area. }
    MASTER_TOC_ALBUM_CATALOG_NUMBER_OFFSET = MASTER_TOC_ALBUM_INFO_OFFSET + 8;

    { The offset of SACD Disc information in this area. }
    MASTER_TOC_DISC_INFO_OFFSET = MASTER_TOC_ALBUM_INFO_OFFSET + 48;

    { The offset of SACD Album Catalog Number in this area. }
    MASTER_TOC_DISC_CATALOG_NUMBER_OFFSET = MASTER_TOC_DISC_INFO_OFFSET + 24;

    { The offset of Text_Channels definitions into Master TOC. }
    MASTER_TOC_TEXT_CHANNELS_OFFSET = 128;

    { The offset of SACD Disc Web Link Info this area. }
    MASTER_TOC_DISC_WEB_LINK_OFFSET = 168;

constructor TMasterTocArea.Create();
begin
    inherited Create(510);
end;

function TMasterTocArea.GetSpecVersion(): TSACDVersion;
var PVer: PSACDVersion;
begin
    if HasData() then
    begin
        PVer := PSACDVersion(PByte(@(Self[0]^.RawData))
            + MASTER_TOC_SPEC_VERSION_OFFSET);
    end;
    Result := PVer^;
end;

function TMasterTocArea.GetAlbumInfo(): TMasterTocAlbum;
var I: Integer;
    PAlbum: PMasterTocAlbum;
begin
    if not HasData() then Exit;
    PAlbum := PMasterTocAlbum((PByte(@(Self[0]^.RawData))
        + MASTER_TOC_ALBUM_INFO_OFFSET));
    Result := PAlbum^;
    // We should convert some pieces of data from big-endian to little-endian
    Result.SetSize := SwapEndian(Result.SetSize);
    Result.SequenceNumber := SwapEndian(Result.SequenceNumber);
    for I := Low(Result.Genres) to High(Result.Genres) do
    begin
        Result.Genres[I].Index := SwapEndian(Result.Genres[I].Index);
    end;
    // Fix CatalogNumber string
    Result.CatalogNumber := Trim(Self[0]^.ToString(
        MASTER_TOC_ALBUM_CATALOG_NUMBER_OFFSET, 16));
end;

function TMasterTocArea.GetDiscInfo(): TMasterTocDisc;
var I: Integer;
    PDisc: PMasterTocDisc;
begin
    if not HasData() then Exit;
    PDisc := PMasterTocDisc((PByte(@(Self[0]^.RawData))
        + MASTER_TOC_DISC_INFO_OFFSET));
    Result := PDisc^;
    // We should convert some pieces of data from big-endian to little-endian
    Result.SChTocAddress1 := SwapEndian(Result.SChTocAddress1);
    Result.SChTocAddress2 := SwapEndian(Result.SChTocAddress2);
    Result.MChTocAddress1 := SwapEndian(Result.MChTocAddress1);
    Result.MChTocAddress2 := SwapEndian(Result.MChTocAddress2);
    Result.SChTocLength := SwapEndian(Result.SChTocLength);
    Result.MChTocLength := SwapEndian(Result.MChTocLength);
    Result.Date.Year := SwapEndian(Result.Date.Year);
    for I := Low(Result.Genres) to High(Result.Genres) do
    begin
        Result.Genres[I].Index := ReverseBytes(Result.Genres[I].Index);
    end;
    // Fix CatalogNumber string
    Result.CatalogNumber := Trim(Self[0]^.ToString(
        MASTER_TOC_DISC_CATALOG_NUMBER_OFFSET, 16));
end;

function TMasterTocArea.GetTextChannels(): TSACDTextChannels;
var PChannels: PSACDTextChannels;
begin
    if not HasData() then Exit;
    PChannels := PSACDTextChannels((PByte(@(Self[0]^.RawData))
        + MASTER_TOC_TEXT_CHANNELS_OFFSET));
    Result := PChannels^;
end;

function TMasterTocArea.GetDiscWebLink(): String;
begin
    Result := Self[0].GetString(MASTER_TOC_DISC_WEB_LINK_OFFSET);
end;

{------------------------------------------------------------------------------}
{ TMasterTextArea                                                              }
{------------------------------------------------------------------------------}

const

    { The offset of Album data in this area. }
    TEXT_ALBUM_DATA_OFFSET = SACD_AREA_DATA_OFFSET;

    { The offset of pointer (2 bytes) to Album Title string in this area. }
    TEXT_ALBUM_TITLE_PTR_OFFSET = TEXT_ALBUM_DATA_OFFSET;

    { The offset of pointer (2 bytes) to Album Artist string in this area. }
    TEXT_ALBUM_ARTIST_PTR_OFFSET = TEXT_ALBUM_DATA_OFFSET + 2;

    { The offset of pointer (2 bytes) to Album Publisher string in this area. }
    TEXT_ALBUM_PUBLISHER_PTR_OFFSET = TEXT_ALBUM_DATA_OFFSET + 4;

    { The offset of pointer (2 bytes) to Album Copyright string in this area. }
    TEXT_ALBUM_COPYRIGHT_PTR_OFFSET = TEXT_ALBUM_DATA_OFFSET + 6;

    { The offset of Disc data in this area. }
    TEXT_DISC_DATA_OFFSET = SACD_AREA_DATA_OFFSET + 14;

    { The offset of pointer (2 bytes) to Disc Title string in this area. }
    TEXT_DISC_TITLE_PTR_OFFSET = TEXT_DISC_DATA_OFFSET + 2;

    { The offset of pointer (2 bytes) to Disc Artist string in this area. }
    TEXT_DISC_ARTIST_PTR_OFFSET = TEXT_DISC_DATA_OFFSET + 4;

    { The offset of pointer (2 bytes) to Disc Publisher string in this area. }
    TEXT_DISC_PUBLISHER_PTR_OFFSET = TEXT_DISC_DATA_OFFSET + 6;

    { The offset of pointer (2 bytes) to Disc Copyright string in this area. }
    TEXT_DISC_COPYRIGHT_PTR_OFFSET = TEXT_DISC_DATA_OFFSET + 8;

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

function TMasterTextArea.GetAlbumArtist(): String;
begin
    Result := Self.GetStringByPtr(TEXT_ALBUM_ARTIST_PTR_OFFSET);
end;

function TMasterTextArea.GetAlbumTitle(): String;
begin
    Result := Self.GetStringByPtr(TEXT_ALBUM_TITLE_PTR_OFFSET);
end;

function TMasterTextArea.GetAlbumPublisher(): String;
begin
    Result := Self.GetStringByPtr(TEXT_ALBUM_PUBLISHER_PTR_OFFSET);
end;

function TMasterTextArea.GetAlbumCopyright(): String;
begin
    Result := Self.GetStringByPtr(TEXT_ALBUM_COPYRIGHT_PTR_OFFSET);
end;

function TMasterTextArea.GetDiscTitle(): String;
begin
    Result := Self.GetStringByPtr(TEXT_DISC_TITLE_PTR_OFFSET);
end;

function TMasterTextArea.GetDiscArtist(): String;
begin
    Result := Self.GetStringByPtr(TEXT_DISC_ARTIST_PTR_OFFSET);
end;

function TMasterTextArea.GetDiscPublisher(): String;
begin
    Result := Self.GetStringByPtr(TEXT_DISC_PUBLISHER_PTR_OFFSET);
end;

function TMasterTextArea.GetDiscCopyright(): String;
begin
    Result := Self.GetStringByPtr(TEXT_DISC_COPYRIGHT_PTR_OFFSET);
end;

{------------------------------------------------------------------------------}
{ TMasterTocManuf                                                              }
{------------------------------------------------------------------------------}

constructor TMasterTocManuf.Create();
begin
    inherited Create(519);
end;

end.                                                                     { END }

{------------------------------------------------------------------------------}
