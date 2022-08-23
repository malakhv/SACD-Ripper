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
{ There are three types of program command line arguments:       }
{   - simple option or flag (short or long format) without any   }
{     data, for example: -l, --help                              }
{   - option (short or long format) with value (key-value pair), }
{     for example: -t "Text", -file ./file1.txt                  }
{   - program argument or command, for example: clone, status    }
{----------------------------------------------------------------}

unit Mikhan.Util.AppArgs;

{$mode delphi}
{$h+}

Interface

const

    { Program option: List, short format. }
    OPTION_LIST_SHORT = '-l';
    { Program option: List, long format. }
    OPTION_LIST_LONG = '--list';

    { Program option: Status, short format. }
    OPTION_STATUS_SHORT = '-s';
    { Program option: Status, long format. }
    OPTION_STATUS_LONG = '--status';
    
    { Program option: Info, short format. }
    OPTION_INFO_SHORT = '-i';
    { Program option: Info, long format. }
    OPTION_INFO_LONG = '--info';

    { Program option: File, short format. }
    OPTION_FILE_SHORT = '-f';
    { Program option: File, long format. }
    OPTION_FILE_LONG = '--file';

    { Program option: Help, short format. }
    OPTION_HELP_SHORT = '-h';
    { Program option: Help, long format. }
    OPTION_HELP_LONG = '--help';

    { Program option: Version, short format. }
    OPTION_VERSION_SHORT = '-v';
    { Program option: Status, long format. }
    OPTION_VERSION_LONG = '--version';

    { Program option: Verbose (turning all log messages), long format. }
    OPTION_VERBOSE_LONG = '--verbose';

const

    { Option prefix: short format. }
    OPTION_PREFIX_SHORT = '-';
    { Option prefix: long format. }
    OPTION_PREFIX_LONG = '--';

type

    { The base type of program commang line arguments. }
    TArgString = String;

type

    { A program option in short or long format. }
    TOption = record
        Key: TArgString;
        Value: TArgString;
        function IsShort(): Boolean;
        function HasValue(): Boolean;
    end;

    { The list of program options (in short or long format). }
    TOptions = array of TOption;

type

    { Class to retreive program arguments. }
    TAppArgs = class(TObject)
    private
        { The program file name. }
        FName: TArgString;
        { Program command line options, with prefix (in short and long format). }
        Options: TOptions;
        { Program command line arguments. }
        Arguments: array of TArgString;
    protected
        procedure AddOption(const Key: TArgString); overload;
        procedure AddOption(const Key, Value: TArgString); overload;
        procedure AddArgument(const Argument: TArgString);
    public
        { The program file name. }
        property Name: TArgString read FName;

        { Returns true if program has specified argument. }
        function HasArgument(Argument: TArgString): Boolean;

        { Returns true if program has Help option (-h or --help). }
        function HasHelp(): Boolean;
        { Returns true if program has Version option (-v or --version). }
        function HasVersion(): Boolean;

        { Returns true if program has Verbose option (--verbose). }
        function HasVerbose(): Boolean;

        { Returns true if program has specified option. }
        function HasOption(const Key: TArgString): Boolean; overload;
        { Returns true if program has specified option in short or long format. }
        function HasOption(const Short, Long: TArgString): Boolean; overload;

        { Returns value for specified option, or empty string. }
        function GetValue(const Key: TArgString): TArgString; overload;
        { Returns value for specified option (in short and long format), or empty string. }
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

function HasShortPrefix(const Argument: TArgString): Boolean;
begin
    Result := Mikhan.Util.StrUtils.StartWith(OPTION_PREFIX_SHORT, Argument);
end;

function HasLongPrefix(const Argument: TArgString): Boolean;
begin
    Result := Mikhan.Util.StrUtils.StartWith(OPTION_PREFIX_LONG, Argument);
end;

function IsOption(const Argument: TArgString): Boolean;
begin
    Result := HasShortPrefix(Argument) or HasLongPrefix(Argument);
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
    //Result := (pos(OPTION_PREFIX_LONG, Self.Key) <> 1);
    Result := not HasLongPrefix(Self.Key) and HasShortPrefix(Self.Key);
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

function TAppArgs.HasArgument(Argument: TArgString): Boolean;
var item: TArgString;
begin
    for item in Arguments do
        if item = Argument then
        begin
            Result := True;
            Exit;
        end;
    Result := False;
end;

function TAppArgs.HasHelp(): Boolean;
begin
    Result := HasOption(OPTION_HELP_SHORT, OPTION_HELP_LONG);
end;

function TAppArgs.HasVersion(): Boolean;
begin
    Result := HasOption(OPTION_VERSION_SHORT, OPTION_VERSION_LONG);
end;

function TAppArgs.HasVerbose(): Boolean;
begin
    Result := HasOption(OPTION_VERBOSE_LONG);
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


