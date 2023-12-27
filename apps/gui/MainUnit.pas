unit MainUnit;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ExtCtrls, ComCtrls, StdActns, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList: TActionList;
    ActExit: TFileExit;
    ActFileOpen: TFileOpen;
    MainMenu: TMainMenu;
    Memo: TMemo;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses Mikhan.Util.StrUtils, Mikhan.Rainbow.Scarlet;

{ Common }

var
  SACDImg: TSACDImage;

procedure ClearStatusBar();
begin
    MainForm.StatusBar.Panels[0].Text := EMPTY;
end;

procedure FillInfo();
var Lines: TStrings;
    Album: TMasterTocAlbum;
    Disc: TMasterTocDisc;
    Text: TMasterTextArea;
begin
    Lines := MainForm.Memo.Lines;

    Album := SACDImg.MasterToc.GetAlbumInfo();
    Disc := SACDImg.MasterToc.GetDiscInfo();
    Text := SACDImg.MasterText;

    Lines.Add(Text.AlbumArtist);
    Lines.Add(Text.AlbumTitle);
    Lines.Add(Album.CatalogNumber);
end;

{ TMainForm }

procedure TMainForm.ActFileOpenAccept(Sender: TObject);
var FileName: TFileName;
begin
    FileName := (Sender as TFileOpen).Dialog.FileName;
    if SACDImg.LoadFromFile(FileName) and SACDImg.IsSACDImage() then
    begin
        StatusBar.Panels[0].Text := FileName;
        FillInfo();
    end else
      StatusBar.Panels[0].Text := 'Wrong file...'
end;

procedure TMainForm.ActFileOpenCancel(Sender: TObject);
begin
    ClearStatusBar();
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SACDImg.Free();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SACDImg := TSACDImage.Create();
end;

end.

