{-------------------------------------------------------------------------}
{                                                                         }
{                          SACD-Ripper project                            }
{                                                                         }
{  Copyright (C) 1996-2022 Mikhail Malakhov <malakhv@gmail.com>           }
{                                                                         }
{  Licensed under the Apache License, Version 2.0 (the "License").        }
{  You may not use this file except in compliance with the License.       }
{  You may obtain a copy of the License at                                }
{                                                                         }
{     http://www.apache.org/licenses/LICENSE-2.0                          }
{                                                                         }
{  Unless required by applicable law or agreed to in writing, software    }
{  distributed under the License is distributed on an "AS IS" BASIS,      }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or        }
{  implied.                                                               }
{                                                                         }
{  See the License for the specific language governing permissions and    }
{  limitations under the License.                                         }
{                                                                         }
{-------------------------------------------------------------------------}

{-------------------------------------------------------------------------}
{ The Unit contains stuff to print and export information about SACD      }
{ disc.                                                                   }
{                                                                         }
{ Created: 11.11.2023                                                     }
{ Author: Mikhail.Malakhov                                                }
{-------------------------------------------------------------------------}

unit Reports;

{$mode delphi}
{$h+}

interface

uses Mikhan.Rainbow.Scarlet;

type 
    TPrintedInfo = (piAlbum, piDisc, piAll);

procedure PrintDiskInfo(const MasterToc: TMasterTocArea;
    const MasterText: TMasterTextArea; PrintInfo: TPrintedInfo;
    PrintTechInfo: Boolean);

implementation

uses Mikhan.Rainbow.Types;

const
    TAB = '    ';
    HEADER =   '+-------------------------------------------------------+';
    PRNT_END = '---------------------------------------------------------';

procedure PrintAlbum(const MasterToc: TMasterTocArea;
    const MasterText: TMasterTextArea; PrintTechInfo: Boolean);
var Album: TMasterTocAlbum;
begin
    Album := MasterToc.GetAlbumInfo();

    Writeln(HEADER);
    Writeln('| Album Info',
        '                                            |');
    Writeln(HEADER); Writeln();

    if (PrintTechInfo) then
        Writeln(TAB, 'Format Version:          ',
            MasterToc.SpecVersion.ToString);

    Writeln(TAB, 'Album Number:            ',
    Album.SequenceNumber, ' (from ', Album.SetSize,')');
    Writeln(TAB, 'Album Genre:             ', Album.Genres[1].Genre);
    Writeln(TAB, 'Album Title:             ', MasterText.AlbumTitle);
    Writeln(TAB, 'Album Artist:            ', MasterText.AlbumArtist);
    Writeln(TAB, 'Album Publisher:         ', MasterText.AlbumPublisher);
    Writeln(TAB, 'Album Copyright:         ', MasterText.AlbumCopyright);
    Writeln(TAB, 'Album Catalog Number:    ', Album.CatalogNumber);
    Writeln();
end;

procedure PrintDisc(const MasterToc: TMasterTocArea;
    const MasterText: TMasterTextArea; PrintTechInfo: Boolean);
var Disc: TMasterTocDisc;
    Channels: TSACDTextChannels;
    I: Integer;
begin
    Disc := MasterToc.GetDiscInfo();

    Writeln(HEADER);
    Writeln('| Disc Info',
        '                                             |');
    Writeln(HEADER); Writeln();

    Writeln(TAB, 'Creation:                ', Disc.Date.ToString());
    Writeln(TAB, 'Hybrid Disc:             ', Disc.IsHybrid());
    Writeln(TAB, 'Disc Genre:              ', Disc.Genres[1].Genre);

    Writeln(TAB, 'Disc Title:              ', MasterText.DiscTitle);
    Writeln(TAB, 'Disc Artist:             ', MasterText.DiscArtist);
    Writeln(TAB, 'Disc Publisher:          ', MasterText.DiscPublisher);
    Writeln(TAB, 'Disc Copyright:          ', MasterText.DiscCopyright);

    Writeln(TAB, 'Disc Catalog Number:     ', Disc.CatalogNumber);
    Writeln(TAB, 'Disc Web Link:           ', MasterToc.DiscWebLink);
    Writeln();

    if PrintTechInfo then
    begin
        Channels := MasterToc.GetTextChannels();
        Writeln(TAB, 'SChTocAddress1:          ', Disc.SChTocAddress1);
        Writeln(TAB, 'SChTocAddress2:          ', Disc.SChTocAddress2);
        Writeln(TAB, 'MChTocAddress1:          ', Disc.MChTocAddress1);
        Writeln(TAB, 'MChTocAddress2:          ', Disc.MChTocAddress2);
        Writeln(TAB, 'SChTocLength:            ', Disc.SChTocLength);
        Writeln(TAB, 'MChTocLength:            ', Disc.MChTocLength);
        Writeln();
        Writeln(TAB, 'TextChannels:            ', Channels.Count);
        for I := 1 to Channels.Count do
        begin
        Writeln(TAB, TAB, 'Channel:             ', I);
        Writeln(TAB, TAB, TAB, 'LangCode:        ', Channels.Channels[I].LangCode);
        Writeln(TAB, TAB, TAB, 'CharSet:         ', Channels.Channels[I].CharSetCode);
        end;
    end;
end;

procedure PrintDiskInfo(const MasterToc: TMasterTocArea;
    const MasterText: TMasterTextArea; PrintInfo: TPrintedInfo;
    PrintTechInfo: Boolean);
begin
    if (PrintInfo = piAlbum) or (PrintInfo = piAll) then
        PrintAlbum(MasterToc, MasterText, PrintTechInfo);

    if (PrintInfo = piDisc) or (PrintInfo = piAll) then
        PrintDisc(MasterToc, MasterText, PrintTechInfo);
    Writeln(HEADER);
end;

end.

{-------------------------------------------------------------------------}
{ END                                                                     }
{-------------------------------------------------------------------------}
