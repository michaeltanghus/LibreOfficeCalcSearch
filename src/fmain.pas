unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls;

type
  { TMainForm }
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    TabLog: TTabSheet;
    TabResults: TTabSheet;

    // Menu items (names must match what you created in the designer)
    mnuFile: TMenuItem;
    mnuFileNewSearch: TMenuItem;
    mnuFileOpenSearch: TMenuItem;
    mnuFileSaveSearch: TMenuItem;
    mnuFileSaveSearchAs: TMenuItem;
    mnuFileExport: TMenuItem;
    mnuFileExit: TMenuItem;

    mnuSettings: TMenuItem;
    mnuSettingsPreferences: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileNewSearchClick(Sender: TObject);
  private
    procedure Log(const AMessage: string);
  public
  end;

var
  TMainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabResults;
  Log('Application started.');
end;

procedure TMainForm.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.mnuFileNewSearchClick(Sender: TObject);
begin
  Log('New Search clicked (dialog not implemented yet).');
  // Next commit: show Search dialog and run scan
end;

procedure TMainForm.Log(const AMessage: string);
var
  TimeStamp: string;
begin
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  MemoLog.Lines.Add(TimeStamp + '  ' + AMessage);
end;

end.

