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
{ program command line (input) arguments.                        }
{                                                                }
{ Package: Mikhan.Util                                           }
{ Types: TAppParams                                              }
{ Dependencies: Mikhan.Util.StrUtils                             }
{                                                                }
{ Created: 17.08.2022                                            }
{ Author: Mikhail.Malakhov                                       }
{----------------------------------------------------------------}

{----------------------------------------------------------------}
{ There are three types of programm command line arguments:      }
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

const

    { Option prefix: short format. }
    OPTION_PREFIX_SHORT = '-';
    { Option prefix: long format. }
    OPTION_PREFIX_LONG = '--';

type

    { The base type of programm commang line arguments data. }
    TArgString = String;

type

    { A programm option in short or long format. }
    TOption = record
        Key: TArgString;
        Value: TArgString;
        function IsShort(): Boolean;
        function HasValue(): Boolean;
    end;

    { The list of programm options (in short or long format). }
    TOptions = Array of TOption;

type

    { Class to retreive programm arguments. }
    TAppArgs = class(TObject)
    private
        { The programm file name. }
        FName: TArgString;
        { Program command line options, with prefix (in short and long format). }
        Options: TOptions;
        { Program command line arguments. }
        Arguments: Array of TArgString;
    protected
        { Returns true, if specified argument is option (has option prefix). }
        function IsOption(const Arg: TArgString): Boolean;

        procedure AddOption(const Key: TArgString); overload;
        procedure AddOption(const Key, Value: TArgString); overload;
        procedure AddArgument(const Argument: TArgString);
    public
        { The programm file name. }
        property Name: TArgString read FName;

        { Returns true if programm has Help option (-h or --help). }
        function HasHelp(): Boolean;
        { Returns true if programm has Version option (-v or --version). }
        function HasVersion(): Boolean;
        
        { Returns true if programm has specified option. }
        function HasOption(const Key: TArgString): Boolean; overload;
        { Returns true if programm has specified option in short or long format. }
        function HasOption(const Short, Long: TArgString): Boolean; overload;

        function GetValue(const Key: TArgString): TArgString; overload;
        function GetValue(const Short, Long: TArgString): TArgString; overload;

        { Clears all stored program parameters. }
        procedure ClearArgs();
        { Persing all program parameters. }
        procedure ParseArgs();
        { Print all known (after parsing) program parameters. }
        procedure PrintAll();

        constructor Create; 
        destructor Destroy; override;
    end;

{----------------------------------------------------------------}
{ Unit implementation section                                    }
{----------------------------------------------------------------}
Implementation

uses Mikhan.Util.StrUtils;

const
    STR_EMPTY = Mikhan.Util.StrUtils.EMPTY;

function IsOption(const Argument: TArgString): Boolean;
begin
    Result := (pos(OPTION_PREFIX_SHORT, Argument) = 1) or
        (pos(OPTION_PREFIX_LONG, Argument) = 1);
end;

function FindOption(Long, Short: TArgString; const Target: TOptions): Integer; overload;
var item: TOption;
var i: Integer;
begin
    for i := Low(Target) to High(Target) do
    begin
        item := Target[i];
        if (item.Key = Long) or (item.Key = Short) then
        begin
            Result := i; Exit;
        end;
    end;
    Result := -1;
end;

function FindOption(Key: TArgString; const Target: TOptions): Integer; overload;
begin
    Result := FindOption(Key, STR_EMPTY, Target);
end;

{----------------------------------------------------------------}
{ TOption implementation                                         }
{----------------------------------------------------------------}

function TOption.IsShort(): Boolean;
begin
    Result := (pos(OPTION_PREFIX_LONG, Self.Key) <> 1);
end;

function TOption.HasValue(): Boolean;
begin
    Result := not Mikhan.Util.StrUtils.IsEmpty(Self.Value);
end;

{----------------------------------------------------------------}
{ TAppArgs implementation                                        }
{----------------------------------------------------------------}

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

function TAppArgs.IsOption(const Arg: TArgString): Boolean;
begin
    Result := (pos(OPTION_PREFIX_SHORT, Arg) = 1) or
        (pos(OPTION_PREFIX_LONG, Arg) = 1);
end;

procedure TAppArgs.ClearArgs();
begin
    SetLength(Options, 0);
    SetLength(Arguments, 0);
end;

procedure TAppArgs.AddOption(const Key: TArgString);
begin
    AddOption(Key, STR_EMPTY);
end;

procedure TAppArgs.AddOption(const Key, Value: TArgString);
var len: integer;
begin
    len := Length(Options);
    SetLength(Options, len + 1);
    Options[len].Key := Key;
    Options[len].Value := Value;
end;

procedure TAppArgs.AddArgument(const Argument: TArgString);
var len: integer;
begin
    len := Length(Arguments);
    SetLength(Arguments, len + 1);
    Arguments[len] := Argument;
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

function TAppArgs.HasOption(const Key: TArgString): Boolean;
begin
    Result := HasOption(Key, STR_EMPTY);
end;

function TAppArgs.HasOption(const Short, Long: TArgString): Boolean;
begin
    Result := FindOption(Short, Long, Options) <> -1;
end;

function TAppArgs.GetValue(const Key: TArgString): TArgString;
begin
    Result := Self.GetValue(Key, STR_EMPTY);
end;

function TAppArgs.GetValue(const Short, Long: TArgString): TArgString;
var opt: Integer;
begin
    opt := FindOption(Short, Long, Options);
    if opt >= 0 then
        Result := Options[opt].Value
    else
        Result := STR_EMPTY;
end;

procedure TAppArgs.PrintAll();
var
    i: Integer;
    item: TOption;
begin
    WriteLn('Name: ', Self.Name);
    WriteLn('Options:');
    
    for item in Options do
    begin
        Write('  ', item.Key);
        if item.HasValue then
            Write('=', item.Value);
        WriteLn();
    end;
    WriteLn('Arguments:');
    for i := Low(Arguments) to High(Arguments) do
         WriteLn('  ', Arguments[i]);
end;

procedure TAppArgs.ParseArgs();
var
    arg, val: String;
    cur, count, len: Integer;
begin
    ClearArgs();
    count := ParamCount();
    if count <= 0 then Exit;
    FName := ParamStr(0);
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
                AddOption(arg, val);
                Inc(cur);
            end else
                AddOption(arg);
        end else
            AddArgument(arg);
        Inc(cur);
    end;
end;

end.


