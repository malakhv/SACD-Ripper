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
{ program input parameters.                                      }
{                                                                }
{ Package: Mikhan.Util                                           }
{ Types: TAppParams                                              }
{ Dependencies: Mikhan.Util.StrUtils                             }
{                                                                }
{ Created: 17.08.2022                                            }
{ Author: Mikhail.Malakhov                                       }
{----------------------------------------------------------------}

{----------------------------------------------------------------}
{ There are three types of programm input parameters:            }
{   - simple option or flag (short or long format) without any   }
{     data, for example: -l, --help                              }
{   - option (short or long format) with value (key-value pair), }
{     for example: -t "Text", -file ./file1.txt                  }
{   - programm argument or command, for example: clone, status   }
{----------------------------------------------------------------}

unit Mikhan.Util.AppArgs;

{$mode delphi}
{$h+}

Interface

const

    { Option prefix: short format. }
    OPTION_PREFIX_SHORT = '-';
    { Option prefix: long format. }
    OPTION_PREFIX_LONG = '--';

const

    { Programm option: List, short format. }
    OPTION_LIST_SHORT = '-l';
    { Programm option: List, long format. }
    OPTION_LIST_LONG = '--list';

    { Programm option: Status, short format. }
    OPTION_STATUS_SHORT = '-s';
    { Programm option: Status, long format. }
    OPTION_STATUS_LONG = '--status';
    
    { Programm option: Info, short format. }
    OPTION_INFO_SHORT = '-i';
    { Programm option: Info, long format. }
    OPTION_INFO_LONG = '--info';

    { Programm option: File, short format. }
    OPTION_FILE_SHORT = '-f';
    { Programm option: File, long format. }
    OPTION_FILE_LONG = '--file';

    { Programm option: Help, short format. }
    OPTION_HELP_SHORT = '-h';
    { Programm option: Help, long format. }
    OPTION_HELP_LONG = '--help';

    { Programm option: Version, short format. }
    OPTION_VERSION_SHORT = '-v';
    { Programm option: Status, long format. }
    OPTION_VERSION_LONG = '--version';

type

    { Class to retreive programm input parameters.  }
    TAppArgs = class(TObject)
    private
        FPWD: ShortString;
        { Program options, with prefix. }
        Options: Array of ShortString;
        { Program parameters (key-value pairs). }
        Parameters: Array of ShortString;
        { Program arguments. }
        Arguments: Array of ShortString;
    protected
        { Returns true, if specified string is option (has option prefix). }
        function IsOption(const Arg: ShortString): Boolean;
        function HasHelp(): Boolean;
        function HasVersion(): Boolean;
        function HasOption(Opt: ShortString): Boolean;
        function Find(Arg: ShortString; const Target: Array of ShortString): Boolean;
        procedure AddOption(const Opt: ShortString);
        procedure AddArgument(const Arg: ShortString);
        procedure AddParameter(const Key, Value: ShortString);
    public
        { }
        property Pwd: ShortString read FPWD;
        { Clears all stored program parameters. }
        procedure ClearArgs();
        { Persing all program parameters. }
        procedure ParseArgs();
        { Print all known (after parsing) program parameters. }
        procedure PrintAll();
        constructor Create; 
        destructor Destroy; override;
    end;

Implementation

constructor TAppArgs.Create;
begin
    inherited Create();
    ClearArgs();
end;

destructor TAppArgs.Destroy;
begin
    ClearArgs();
    inherited Destroy();
end;

{ Returns true, if specified string is option (has option prefix). }
function TAppArgs.IsOption(const Arg: ShortString): Boolean;
begin
    Result := (pos(OPTION_PREFIX_SHORT, Arg) = 1) or
        (pos(OPTION_PREFIX_LONG, Arg) = 1);
end;

function TAppArgs.Find(Arg: ShortString; const Target: Array of ShortString): Boolean;
var item: ShortString;
begin
    for item in Target do
        if item = Arg then
        begin
            Result := True;
            Exit;
        end;
    Result := False;
end;

{ Clears all stored program parameters. }
procedure TAppArgs.ClearArgs();
begin
    SetLength(Options, 0);
    SetLength(Parameters, 0);
    SetLength(Arguments, 0);
end;

procedure TAppArgs.AddOption(const Opt: ShortString);
var len: integer;
begin
    len := Length(Options);
    SetLength(Options, len + 1);
    Options[len] := Opt;
end;

procedure TAppArgs.AddArgument(const Arg: ShortString);
var len: integer;
begin
    len := Length(Arguments);
    SetLength(Arguments, len + 1);
    Arguments[len] := Arg;
end;

procedure TAppArgs.AddParameter(const Key, Value: ShortString);
var len: integer;
begin
    len := Length(Parameters);
    SetLength(Parameters, len + 2);
    Parameters[len] := Key;
    Parameters[len + 1] := Value;
end;

function TAppArgs.HasHelp(): Boolean;
begin
    Result := HasOption(OPTION_HELP_SHORT) or
        HasOption(OPTION_HELP_LONG);
end;

function TAppArgs.HasVersion(): Boolean;
begin
    Result := HasOption(OPTION_VERSION_SHORT) or
        HasOption(OPTION_VERSION_LONG);
end;

function TAppArgs.HasOption(Opt: ShortString): Boolean;
begin
    Result := Find(Opt, Options);
end;

{ Print all known (after parsing) program parameters. }
procedure TAppArgs.PrintAll();
var i: Integer;
begin
    WriteLn('PWD: ', Self.Pwd);
    WriteLn('Options:');
    for i := Low(Options) to High(Options) do
         WriteLn('  ', Options[i]);
    WriteLn('Arguments:');
    for i := Low(Arguments) to High(Arguments) do
         WriteLn('  ', Arguments[i]);
    WriteLn('Parameters:');
    i := Low(Parameters);
    while i < High(Parameters) do
    begin
        WriteLn('  ', Parameters[i], '=', Parameters[i+1]);
        Inc(i); Inc(i);
    end;
end;

{ Persing all program parameters. }
procedure TAppArgs.ParseArgs();
var
    arg, val: String;
    cur, count, len: Integer;
begin
    ClearArgs();
    count := ParamCount();
    if count <= 0 then Exit;
    FPWD := ParamStr(0);
    cur := 1;
    while cur <= count do
    begin
        arg := ParamStr(cur);
        if IsOption(arg) then
        begin
            if cur < count then
                val := ParamStr(cur + 1)
            else
                val := '';
            if (val <> '') and (not IsOption(val)) then
            begin
                AddParameter(arg, val);
                Inc(cur);
            end else
                AddOption(arg);
        end else
            AddArgument(arg);
        Inc(cur);
    end;
end;

end.


