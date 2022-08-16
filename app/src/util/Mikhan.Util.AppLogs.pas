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
{ program logs.                                                  }
{                                                                }
{ Package: Mikhan.Util                                           }
{ Types: TLogLevel, TAppLogs                                     }
{ Dependencies: Mikhan.Util.StrUtils                             }
{                                                                }
{ Created: 14.08.2022                                            }
{ Author: Mikhail.Malakhov                                       }
{----------------------------------------------------------------}

unit Mikhan.Util.AppLogs;

// Compiler options
{$mode delphi}
{$h+}

Interface

type

    { The available logging levels. }
    TLogLevel = (
        { Logging level: Not inportant information only for development time. }
        llVerbose,
        { Logging level: Information only for development, debugging and testing time. }
        llDebug,
        { Logging level: Any important information for release version. }
        llInfo,
        { Logging level: Program warnings. }
        llWarn,
        { Logging level: Program errors and critical messages. }
        llError,
        {
          Logging level: Silent mode, turning off all messages. In normal
          case program shouldn't use it to srite a messages.
        }
        llSilent
    );

type

    { Class implements a program logging mechanism. }
    TAppLogs = class (TObject)
    private
        FAppTag: String;        // See AppTag property
        FLogLevel: TLogLevel;   // See LogLevel property
        IsDebug: Boolean;       // Debug mode?
        HasAppTag: Boolean;
    protected
        { Setter for LogLevel property }
        procedure DoSetLogLevel(LogLevel: TLogLevel); virtual;
        { Low-level logging call. Print a message with specified parameters. }
        procedure Print(Level: TLogLevel; const Message: String); overload;
        { Low-level logging call. Print a message with specified parameters. }
        procedure Print(Level: TLogLevel; const Tag, Message: String); overload;
    public
        { The main program log tag. }
        property AppTag: String read FAppTag;

        { The current logging level. All messages less that level will be ignoring. }
        property LogLevel: TLogLevel read FLogLevel write DoSetLogLevel;
        { Checks to loggable or not messages with specified log level. }
        function IsLoggable(Level: TLogLevel): Boolean; virtual;

        { Sends a debug log message with main program tag. }
        procedure D(const Message: String); overload;
        { Sends a debug log message with main program tag and specified tag. }
        procedure D(const Tag: String; const Message: String); overload;

        { Sends a info log message with main program tag. }
        procedure I(const Message: String); overload;
        { Sends a info log message with main program tag and specified tag. }
        procedure I(const Tag: String; const Message: String); overload;

        { Sends a warning log message with main program tag. }
        procedure W(const Message: String); overload;
        { Sends a warning log message with main program tag and specified tag. }
        procedure W(const Tag: String; const Message: String); overload;

        { Sends a error log message with main program tag. }
        procedure E(const Message: String); overload;
        { Sends a error log message with main program tag and specified tag. }
        procedure E(const Tag: String; const Message: String); overload;

        { Construct a new instance of TAppLogs class with specified parameters. }
        constructor Create(AppTag: String); overload;
        { Construct a new instance of TAppLogs class with specified parameters. }
        constructor Create(AppTag: String; Debug: Boolean); overload;
        destructor Destroy; override;
    end;

//--------------------------------------------------------------------------------------------------
// Implementation Section
//--------------------------------------------------------------------------------------------------
Implementation

uses Mikhan.Util.StrUtils;

const

    { The empty log tag. }
    TAG_EMPTY = Mikhan.Util.StrUtils.EMPTY;

    { String that will use as delimiter for tags in LogCat message. }
    TAG_DELIMITER = ': ';

{ Construct a new instance of TAppLogs class with specified parameters. }
constructor TAppLogs.Create(AppTag: String);
begin
    Self.Create(AppTag, False);
end;

{ Construct a new instance of TAppLogs class with specified parameters. }
constructor TAppLogs.Create(AppTag: String; Debug: Boolean);
begin
    inherited Create();
    FAppTag := AppTag;
    HasAppTag := not Mikhan.Util.StrUtils.isEmpty(FAppTag);
    LogLevel := TLogLevel.llDebug;
    IsDebug := Debug;
end;

destructor TAppLogs.Destroy;
begin
    // Empty
end;

{ Setter for LogLevel property }
procedure TAppLogs.DoSetLogLevel(LogLevel: TLogLevel);
begin
    FLogLevel := LogLevel;
end;

{ Checks to loggable or not messages with specified log level. }
function TAppLogs.IsLoggable(Level: TLogLevel): Boolean;
begin
    Result :=  Ord(Level) >= Ord(LogLevel);
end;

{ Low-level logging call. Print a message with specified parameters. }
procedure TAppLogs.Print(Level: TLogLevel; const Message: String);
begin
    Print(Level, TAG_EMPTY, Message);
end;

{ Low-level logging call. Print a message with specified parameters. }
procedure TAppLogs.Print(Level: TLogLevel; const Tag, Message: String); overload;
var prefix: String;
begin
    if not IsLoggable(Level) then Exit;
    prefix := '';
    if HasAppTag then
        prefix := AppTag + TAG_DELIMITER;
    if not Mikhan.Util.StrUtils.isEmpty(Tag) then
        prefix := prefix + Tag + TAG_DELIMITER;
    Writeln(prefix, Message);
end;

{ Sends a debug log message with main program tag. }
procedure TAppLogs.D(const Message: String);
begin
    D(TAG_EMPTY, Message);
end;

{ Sends a debug log message with main program tag and specified tag. }
procedure TAppLogs.D(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llDebug, Tag, Message);
end;

{ Sends a info log message with main program tag. }
procedure TAppLogs.I(const Message: String);
begin
    I(TAG_EMPTY, Message);
end;

{ Sends a info log message with main program tag and specified tag. }
procedure TAppLogs.I(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llInfo, Tag, Message);
end;

{ Sends a warning log message with main program tag. }
procedure TAppLogs.W(const Message: String);
begin
    W(TAG_EMPTY, Message);
end;

{ Sends a warning log message with main program tag and specified tag. }
procedure TAppLogs.W(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llWarn, Tag, Message);
end;

{ Sends a error log message with main program tag. }
procedure TAppLogs.E(const Message: String);
begin
    E(TAG_EMPTY, Message);
end;

{ Sends a error log message with main program tag and specified tag. }
procedure TAppLogs.E(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llError, Tag, Message);
end;

end.
//-----------------------------------------------------------------------------------------------------------
