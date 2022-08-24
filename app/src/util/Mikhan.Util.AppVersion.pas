{----------------------------------------------------------------}
{                                                                }
{                  Pascal Utils Library (PUL)                    }
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
{                                                                }
{ The version name represents in format:                         }
{   XX.YY.ZZZ[.DEBUG][-BRANCH_NAME][-COMMIT_ID], where:          }
{                                                                }
{   - XX - major version (max value is 99)                       }
{   - YY - minor version (max value is 99)                       }
{   - ZZZ - number of patch (max value is 999)                   }
{   - DEBUG - the debug flag (only for debug build), may be null }
{   - BRANCH_NAME - the git branch name, for example "master"    }
{   - COMMIT_ID - the first seven letters of git commit id,      }
{     for example "94604e3"                                      }
{                                                                }
{ Below, you could see several examples:                         }
{                                                                }
{   - 0.1.0.debug-master - initial (default) version for all new }
{     project (without any commits and on default branch)        }
{                                                                }
{   - 1.2.15.debug-master-94604e3 - debug build of version       }
{     1.2.15, where latest commit id is 94604e3                  }
{                                                                }
{   - 1.3.0.master-94604e3 - release build of version 1.3.0      }
{     where latest commit id is 94604e3                          }
{                                                                }
{ The initial (default) value of version is:                     }
{                                                                }
{   0.1.0.debug-master                                           }
{                                                                }
{ The version code calculate from version name by                }
{ following rule:                                                }
{                                                                }
{   XX * 100000 + YY * 1000 + ZZZ                                }
{                                                                }
{ Several examples:                                              }
{                                                                }
{   - Name 0.1.0.debug-master-0000000 = Code 1000                }
{   - Name 0.1.8.debug-develop-0000000 = Code 1008               }
{   - Name 2.5.17.master-94604e3 = Code 205017                   }
{   - Name 99.88.777.debug-qa-0000000 = Code 9988777             }
{                                                                }
{ The program version can be stored in separate file.            }
{----------------------------------------------------------------}

unit Mikhan.Util.AppParams;

{$mode delphi}
{$h+}

Interface

const



type



