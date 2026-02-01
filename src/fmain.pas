unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls, fsearch;

type
  { TMainForm }
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MemoLog: TMemo;
    MenuItemExit: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemNewSearch: TMenuItem;
    MenuItemOpenSearch: TMenuItem;
    MenuItemPreferences: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSettings: TMenuItem;
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
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemExportClick(Sender: TObject);
    procedure MenuItemNewSearchClick(Sender: TObject);
    procedure MenuItemOpenSearchClick(Sender: TObject);
    procedure MenuItemPreferencesClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuOpenSearchClick(Sender: TObject);
  private
    procedure Log(const AMessage: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabResults;
  Log('Application started.');
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemExportClick(Sender: TObject);
begin
  Log('Export clicked (dialog not implemented yet).');
  // Next commit: show Search dialog and run scan
end;

procedure TMainForm.MenuItemNewSearchClick(Sender: TObject);
var
  SearchForm: TSearchForm;
begin
  SearchForm := TSearchForm.Create(Self);
  try
    if SearchForm.ShowModal = mrOK then
      Log('Search accepted: "' + SearchForm.edtQuery.Text + '" in ' + SearchForm.edtDirectory.Text)
    else
      Log('Search canceled.');
  finally
    SearchForm.Free;
  end;
end;

procedure TMainForm.MenuItemOpenSearchClick(Sender: TObject);
begin
  Log('Open Search clicked (dialog not implemented yet).');
  // Next commit: show Search dialog and run scan
end;

procedure TMainForm.MenuItemPreferencesClick(Sender: TObject);
begin
  Log('Preferences clicked (dialog not implemented yet).');
  // Next commit: show Search dialog and run scan
end;

procedure TMainForm.MenuItemSaveAsClick(Sender: TObject);
begin
  Log('Save As clicked (dialog not implemented yet).');
  // Next commit: show Search dialog and run scan
end;

procedure TMainForm.MenuItemSaveClick(Sender: TObject);
begin
  Log('Save clicked (dialog not implemented yet).');
  // Next commit: show Search dialog and run scan
end;

procedure TMainForm.MenuOpenSearchClick(Sender: TObject);
begin
  Log('File New Search clicked (dialog not implemented yet).');
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

