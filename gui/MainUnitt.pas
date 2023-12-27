unit MainUnitt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ExtCtrls, ComCtrls, StdActns;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList: TActionList;
    ActExit: TFileExit;
    ActFileOpen: TFileOpen;
    MainMenu: TMainMenu;
    MenuSep01: TMenuItem;
    MenuSep02: TMenuItem;
    MenuExit: TMenuItem;
    MenuFile: TMenuItem;
    MenuOpen: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    StatusBar: TStatusBar;
    procedure ActFileOpenAccept(Sender: TObject);
    procedure ActFileOpenCancel(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ Common }

procedure ClearStatusBar();
begin
    MainForm.StatusBar.Panels[0].Text := '';
end;

{ TMainForm }

procedure TMainForm.ActFileOpenAccept(Sender: TObject);
var Dialog: TOpenDialog;
begin
    Dialog := (Sender as TFileOpen).Dialog;
    StatusBar.Panels[0].Text := Dialog.FileName;
end;

procedure TMainForm.ActFileOpenCancel(Sender: TObject);
begin
    ClearStatusBar();
end;

end.

