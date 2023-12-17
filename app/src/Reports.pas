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

    if (PrintTechInfo) then
        Writeln(TAB, 'Format Version:          ',
            MasterToc.SpecVersion.ToString);

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
    Channels: TSACDTextChannels;
    I: Integer;
begin
    MasterToc := SACDImg.MasterToc;
    MasterText := SACDImg.MasterText;
    Disc := MasterToc.GetDiscInfo();

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
            Write(TAB, TAB, 'Channel ', I,':           ');
            Writeln(Channels.Channels[I].ToString());
        end;
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
