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
    TPrintedInfo = (piAll, piAlbum, piDisc);
    TOutType = (otAll, otMain);

procedure PrintMasterToc(const Source: TMasterTocArea;
    const Print: TPrintedInfo);

implementation

const
    IND = '   ';
    HEADER =   '+-------------------------------------------------------+';
    PRNT_END = '---------------------------------------------------------';

procedure PrintMasterToc(const Source: TMasterTocArea;
    const Print: TPrintedInfo);

    procedure PrintAlbum();
    var Album: TMasterTocAlbum;
    begin
        Album := Source.GetAlbumInfo();
        
        Writeln(HEADER);
        Writeln('| TOC Album Info',
            '                                        |');
        Writeln(HEADER); Writeln();

        {Writeln(IND, 'Format Version:            ',
            Source.SpecVersion.ToString); }
        Writeln(IND, 'Album Number:              ',
            Album.SequenceNumber, ' (from ', Album.SetSize,')');
        Writeln(IND, 'Album Genre:               ',
            Album.Genres[1].Genre);
        Writeln(IND, 'Album Catalog Number:      ',
            Album.CatalogNumber);
    end;

    procedure PrintDisc();
    var Disc: TMasterTocDisc;
    begin
        Disc := Source.GetDiscInfo();
        
        Writeln(HEADER);
        Writeln('| TOC Disc Info',
            '                                         |');
        Writeln(HEADER); Writeln();

        Writeln(IND, 'Creation:                  ', Disc.Date.ToString());
        Writeln(IND, 'Hybrid Disc:               ', Disc.IsHybrid());
        Writeln(IND, 'Disc Genre:                ', Disc.Genres[1].Genre);
        Writeln(IND, 'Disc Catalog Number:       ', Disc.CatalogNumber);
        Writeln(IND, 'Disc Web Link:             ', Source.DiscWebLink);
        Writeln();
        Writeln(IND, 'SChTocAddress1:            ', Disc.SChTocAddress1);
        Writeln(IND, 'SChTocAddress2:            ', Disc.SChTocAddress2);
        Writeln(IND, 'MChTocAddress1:            ', Disc.MChTocAddress1);
        Writeln(IND, 'MChTocAddress2:            ', Disc.MChTocAddress2);
        Writeln(IND, 'SChTocLength:              ', Disc.SChTocLength);
        Writeln(IND, 'MChTocLength:              ', Disc.MChTocLength);
    end;

begin

    if Print = TPrintedInfo.piAlbum then
    begin
        PrintAlbum(); Writeln(HEADER);
    end;

    if Print = TPrintedInfo.piDisc then
    begin
        PrintDisc(); Writeln(HEADER);
    end;
    
    if Print = piAll then
    begin
        PrintAlbum(); WriteLn(); PrintDisc(); WriteLn();
        Writeln(HEADER);
    end;

end;

end.

{-------------------------------------------------------------------------}
{ END                                                                     }
{-------------------------------------------------------------------------}
