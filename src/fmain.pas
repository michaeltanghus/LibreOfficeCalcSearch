unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Grids, ExtCtrls, uModels, uSearchEngine, fsearch;

type
  { TMainForm }
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MemoDetails: TMemo;
    MemoLog: TMemo;
    MenuItemExit: TMenuItem;
    MenuItemExport: TMenuItem;
    // MenuItemFile: TMenuItem;
    // MenuItemNewSearch: TMenuItem;
    MenuItemOpenSearch: TMenuItem;
    MenuItemPreferences: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    // MenuItemSettings: TMenuItem;
    PageControl1: TPageControl;
    GridResults: TStringGrid;
    Splitter1: TSplitter;
    TabLog: TTabSheet;
    TabResults: TTabSheet;

    // Menu items (names must match what you created in the designer)
    MenuItemFile: TMenuItem;
    MenuItemNewSearch: TMenuItem;
    MenuItemFileOpenSearch: TMenuItem;
    MenuItemFileSaveSearch: TMenuItem;
    MenuItemFileSaveSearchAs: TMenuItem;
    MenuItemFileExport: TMenuItem;
    MenuItemFileExit: TMenuItem;

    MenuItemSettings: TMenuItem;
    MenuItemSettingsPreferences: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure GridResultsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
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
    procedure ClearResultsGrid;
    procedure AddHitToGrid(const AHit: TSearchHit);
    procedure AppException(Sender: TObject; E: Exception);
    procedure AutoSizeResultsColumns(const AMaxRowsToScan: Integer);
    function MeasureCellTextWidth(const S: string): Integer;

  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := @AppException;
  PageControl1.ActivePage := TabResults;
  Log('Application started.');
end;

procedure TMainForm.GridResultsSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
  var
    FilePath: string;
    SheetName: string;
    CellA1: string;
    ValueText: string;
    ContextText: string;
  begin
    if aRow <= 0 then
    begin
      MemoDetails.Clear;
      Exit;
    end;

    FilePath := GridResults.Cells[0, aRow];
    SheetName := GridResults.Cells[1, aRow];
    CellA1 := GridResults.Cells[2, aRow];
    ValueText := GridResults.Cells[3, aRow];
    ContextText := GridResults.Cells[4, aRow];

    MemoDetails.Lines.BeginUpdate;
    try
      MemoDetails.Clear;
      MemoDetails.Lines.Add('File:    ' + FilePath);
      MemoDetails.Lines.Add('Sheet:   ' + SheetName);
      MemoDetails.Lines.Add('Cell:    ' + CellA1);
      MemoDetails.Lines.Add('');
      MemoDetails.Lines.Add('Value:');
      MemoDetails.Lines.Add(ValueText);
      if ContextText <> '' then
      begin
        MemoDetails.Lines.Add('');
        MemoDetails.Lines.Add('Context:');
        MemoDetails.Lines.Add(ContextText);
      end;
    finally
      MemoDetails.Lines.EndUpdate;
    end;
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
//  procedure TMainForm.mnuFileNewSearchClick(Sender: TObject);
  var
    SearchForm: TSearchForm;
    Crit: TSearchCriteria;
    Engine: TSearchEngine;
    Results: TList;
    I: Integer;
    HitPtr: ^TSearchHit;
  begin
    SearchForm := TSearchForm.Create(Self);
    try
      if SearchForm.ShowModal <> mrOK then
      begin
        Log('Search canceled.');
        Exit;
      end;

      Crit := SearchForm.BuildCriteria;
    finally
      SearchForm.Free;
    end;

    if (Crit.Directory = '') or (not DirectoryExists(Crit.Directory)) then
    begin
      Log('Invalid directory: ' + Crit.Directory);
      Exit;
    end;

    ClearResultsGrid;
    PageControl1.ActivePage := TabResults;

    Log('Search started: "' + Crit.QueryText + '" in ' + Crit.Directory + ' mask=' + Crit.FileMask);

    Results := TList.Create;
    try
      Engine := TSearchEngine.Create(Crit, @Log);
      try
        Engine.Execute(Results);
      finally
        Engine.Free;
      end;

      Log('Hits: ' + IntToStr(Results.Count));

      for I := 0 to Results.Count - 1 do
      begin
        HitPtr := Results[I];
        AddHitToGrid(HitPtr^);
      end;

      AutoSizeResultsColumns(200);

    finally
      // free hit records
      for I := 0 to Results.Count - 1 do
      begin
        HitPtr := Results[I];
        Dispose(HitPtr);
      end;
      Results.Free;
    end;

    Log('Search finished.');
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

procedure TMainForm.ClearResultsGrid;
begin
  GridResults.ColCount := 5;
  GridResults.FixedRows := 1;
  GridResults.RowCount := 2;

  GridResults.Cells[0,0] := 'File';
  GridResults.Cells[1,0] := 'Sheet';
  GridResults.Cells[2,0] := 'Cell';
  GridResults.Cells[3,0] := 'Value';
  GridResults.Cells[4,0] := 'Context';
end;

procedure TMainForm.AddHitToGrid(const AHit: TSearchHit);
var
  Row: Integer;
begin
  Row := GridResults.RowCount;
  GridResults.RowCount := Row + 1;

  GridResults.Cells[0, Row] := AHit.FilePath;
  GridResults.Cells[1, Row] := AHit.SheetName;
  GridResults.Cells[2, Row] := AHit.CellA1;
  GridResults.Cells[3, Row] := AHit.CellText;
  GridResults.Cells[4, Row] := AHit.ContextText;
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
var
  I: Integer;
begin
  Log('EXCEPTION: ' + E.ClassName + ': ' + E.Message);

  // This prints the call stack. With -gl enabled, it includes unit+line.
  for I := 0 to ExceptFrameCount - 1 do
    Log(BackTraceStrFunc(ExceptFrames[I]));
end;
     function TMainForm.MeasureCellTextWidth(const S: string): Integer;
begin
  // Some padding so text isn't flush
  Result := GridResults.Canvas.TextWidth(S) + 16;
end;

procedure TMainForm.AutoSizeResultsColumns(const AMaxRowsToScan: Integer);
var
  Col: Integer;
  Row: Integer;
  MaxWidth: Integer;
  W: Integer;
  LastRow: Integer;
begin
  // Ensure Canvas is ready
  GridResults.Canvas.Font.Assign(GridResults.Font);

  // Limit scan for performance (e.g. first 200 hits)
  LastRow := GridResults.RowCount - 1;
  if (AMaxRowsToScan > 0) and (LastRow > AMaxRowsToScan) then
    LastRow := AMaxRowsToScan;

  for Col := 0 to GridResults.ColCount - 1 do
  begin
    MaxWidth := MeasureCellTextWidth(GridResults.Cells[Col, 0]); // header

    for Row := 1 to LastRow do
    begin
      W := MeasureCellTextWidth(GridResults.Cells[Col, Row]);
      if W > MaxWidth then
        MaxWidth := W;
    end;

    // Set some reasonable caps so Context doesn't become enormous
    case Col of
      0: if MaxWidth > 600 then MaxWidth := 600; // File
      3: if MaxWidth > 500 then MaxWidth := 500; // Value
      4: if MaxWidth > 800 then MaxWidth := 800; // Context
    else
      if MaxWidth > 250 then MaxWidth := 250;
    end;

    // Set a minimum too
    if MaxWidth < 70 then
      MaxWidth := 70;

    GridResults.ColWidths[Col] := MaxWidth;
  end;
end;

end.

