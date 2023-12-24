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
{ The Unit contains stuff to print and export information about SACD disc.     }
{                                                                              }
{ Project: SACD-Ripper                                                         }
{ Created: 11.11.2023                                                          }
{ Author: Mikhail.Malakhov                                                     }
{------------------------------------------------------------------------------}

unit Reports;                                                           { UNIT }

{$mode delphi}
{$h+}

interface                                                          { INTERFACE }

uses Mikhan.Rainbow.Scarlet;

type 
    TPrintedInfo = (piAlbum, piDisc, piAll);

{
    Prints information about SACD.
}
procedure PrintSACD(const SACDImg: TSACDImage; PrintInfo: TPrintedInfo;
    PrintTechInfo: Boolean);

implementation                                                { IMPLEMENTETION }

uses Mikhan.Rainbow.Types;

const
    TAB = '    ';
    HEADER =   '+-------------------------------------------------------+';
    PRNT_END = '---------------------------------------------------------';

procedure PrintAlbum(const SACDImg: TSACDImage; PrintTechInfo: Boolean);
var
    MasterToc: TMasterTocArea;
    MasterText: TMasterTextArea;
    Album: TMasterTocAlbum;
    I: Integer;
begin
    MasterToc := SACDImg.MasterToc;
    MasterText := SACDImg.MasterText;
    Album := MasterToc.GetAlbumInfo();

    Writeln(HEADER);
    Writeln('| Album Info',
        '                                            |');
    Writeln(HEADER); Writeln();

    Writeln(TAB, 'Album Number:            ',
    Album.SequenceNumber, ' (from ', Album.SetSize,')');

    for I := Low(Album.Genres) to High(Album.Genres) do
        Writeln(TAB, 'Album Genre ', I, ':           ', Album.Genres[I].Genre);
    Writeln(TAB, 'Album Title:             ', MasterText.AlbumTitle);
    Writeln(TAB, 'Album Artist:            ', MasterText.AlbumArtist);
    Writeln(TAB, 'Album Publisher:         ', MasterText.AlbumPublisher);
    Writeln(TAB, 'Album Copyright:         ', MasterText.AlbumCopyright);
    Writeln(TAB, 'Album Catalog Number:    ', Album.CatalogNumber);
    Writeln();
end;

procedure PrintDisc(const SACDImg: TSACDImage; PrintTechInfo: Boolean);
var
    MasterToc: TMasterTocArea;
    MasterText: TMasterTextArea;
    Disc: TMasterTocDisc;
    Disc2: TMasterTocDiscTwo;
    Channels: TSACDTextChannels;
    I: Integer;
begin
    MasterToc := SACDImg.MasterToc;
    MasterText := SACDImg.MasterText;
    Disc := MasterToc.GetDiscInfo();
    Disc2 := MasterToc.GetDiscInfoTwo();

    Writeln(HEADER);
    Writeln('| Disc Info',
        '                                             |');
    Writeln(HEADER); Writeln();

    Writeln(TAB, 'Creation:                ', Disc.Date.ToString());
    Writeln(TAB, 'Hybrid Disc:             ', Disc.IsHybrid());
    for I := Low(Disc.Genres) to High(Disc.Genres) do
        Writeln(TAB, 'Disc Genre ', I, ':            ', Disc.Genres[1].Genre);

    Writeln(TAB, 'Disc Title:              ', MasterText.DiscTitle);
    Writeln(TAB, 'Disc Artist:             ', MasterText.DiscArtist);
    Writeln(TAB, 'Disc Publisher:          ', MasterText.DiscPublisher);
    Writeln(TAB, 'Disc Copyright:          ', MasterText.DiscCopyright);

    Writeln(TAB, 'Disc Catalog Number:     ', Disc.CatalogNumber);
    Writeln(TAB, 'Disc Web Link:           ', MasterToc.DiscWebLink);
    Writeln();

    if PrintTechInfo then
    begin
        Writeln(TAB, '2CH_TOC_1_Address:       ', Disc.SChToc1);
        Writeln(TAB, '2CH_TOC_2_Address:       ', Disc.SChToc2);
        Writeln(TAB, 'MC_TOC_1_Address:        ', Disc.MChToc1);
        Writeln(TAB, 'MC_TOC_2_Address:        ', Disc.MChToc2);
        Writeln(TAB, '2CH_TOC_Length:          ', Disc.SChTocLength);
        Writeln(TAB, 'MC_TOC_Length:           ', Disc.MChTocLength);
        Writeln(TAB, '2CH_TOC_3_Address:       ', Disc2.SChToc3);
        Writeln(TAB, '2CH_TOC_4_Address:       ', Disc2.SChToc4);
        Writeln(TAB, 'MC_TOC_3_Address:        ', Disc2.MChToc3);
        Writeln(TAB, 'MC_TOC_4_Address:        ', Disc2.MChToc4);
        Writeln(TAB, '2CH_TOC_B_Length:        ', Disc2.SChTocBLength);
        Writeln(TAB, 'MC_TOC_B_Length:         ', Disc2.MChTocBLength);
        Writeln(TAB, 'E_TOC_Address:           ', Disc2.ETocAddress);
        Writeln(TAB, 'E_TOC_Length:            ', Disc2.ETocLength);
        Writeln(TAB, 'E_Data_Start_Address:    ', Disc2.EDataStart);
        Writeln(TAB, 'E_Data_End_Address:      ', Disc2.EDataEnd);
        Writeln(TAB, 'EKB1_Area_Address:       ', Disc2.EKB1Area);
        Writeln(TAB, 'EKB2_Area_Address:       ', Disc2.EKB2Area);
        Writeln(TAB, 'Rev_Area_Start_Address:  ', Disc2.RevAreaStart);
        Writeln(TAB, 'Rev_Area_End_Address:    ', Disc2.RevAreaEnd);
        Writeln();
        Channels := MasterToc.GetTextChannels();
        Writeln(TAB, 'TextChannels:            ', Channels.Count);
        for I := 1 to Channels.Count do
        begin
            Write(TAB, TAB, 'Channel ', I,':           ');
            Writeln(Channels.Channels[I].ToString());
        end;
        Writeln();
        Writeln(TAB, 'Spec Version:            ',
            MasterToc.SpecVersion.ToString);
        Writeln();
    end;
end;

procedure PrintSACD(const SACDImg: TSACDImage; PrintInfo: TPrintedInfo;
    PrintTechInfo: Boolean);
begin
    if (PrintInfo = piAlbum) or (PrintInfo = piAll) then
        PrintAlbum(SACDImg, PrintTechInfo);

    if (PrintInfo = piDisc) or (PrintInfo = piAll) then
        PrintDisc(SACDImg, PrintTechInfo);
    Writeln(HEADER);
end;

end.                                                                     { END }

{------------------------------------------------------------------------------}
