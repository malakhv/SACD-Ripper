{----------------------------------------------------------------}
{                                                                }
{                  Pascal Util Library (PUL)                     }
{                                                                }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>       }
{                                                                }
{  Unauthorized copying of this file, via any medium is          }
{  strictly prohibited.                                          }
{                                                                }
{       Confidential and Proprietary. All Rights Reserved.       }
{                                                                }
{----------------------------------------------------------------}

{----------------------------------------------------------------}
{ The Unit contains types, methods and classes to working with   }
{ program version.                                               }
{                                                                }
{ Package: Mikhan.Util                                           }
{ Types: TAppParams                                              }
{ Dependencies: Mikhan.Util.StrUtils                             }
{                                                                }
{ Created: 17.08.2022                                            }
{ Author: Mikhail.Malakhov                                       }
{----------------------------------------------------------------}

{----------------------------------------------------------------}
{ This unit uses "Semantic Versioning 2.0.0" conception, for     }
{ more information about it, please see http://semver.org/.      }
{----------------------------------------------------------------}

unit Mikhan.Util.AppParams;

{$mode delphi}
{$h+}

Interface

const

    ARG_LIST = '--list';
    ARG_LIST_SHORT = '-l';
    ARG_STATUS = '--status';
    ARG_STATUS_SHORT = '-s';
    ARG_INFO = '--info';
    ARG_INFO_SHORT = '-i'
    ARG_FILE = '--file';
    ARG_FILE_SHORT = '-f';

    ARG_HELP = '--help';
    ARG_HELP_SHORT = '-h';

    ARG_VERSION = '--version';
    ARG_VERSION_SHORT = '-v';

type



