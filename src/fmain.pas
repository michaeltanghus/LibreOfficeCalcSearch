unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, Grids, ExtCtrls, Types,
  Process, URIParser, FileUtil,
  uModels, usearchengine, fsearch,
  uAppPrefs, fpreferences;

type
  PSearchHit = ^TSearchHit;

type
  { TMainForm }
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;

    MenuItemFile: TMenuItem;
    MenuItemNewSearch: TMenuItem;
    MenuItemOpenSearch: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemExit: TMenuItem;

    MenuItemSettings: TMenuItem;
    MenuItemPreferences: TMenuItem;

    PageControl1: TPageControl;
    TabResults: TTabSheet;
    TabLog: TTabSheet;

    GridResults: TStringGrid;
    MemoLog: TMemo;

    // Detail panel already in your .lfm
    Splitter1: TSplitter;
    MemoDetails: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemNewSearchClick(Sender: TObject);
    procedure MenuItemOpenSearchClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemExportClick(Sender: TObject);
    procedure MenuItemPreferencesClick(Sender: TObject);

    procedure GridResultsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);

  private
    FHits: TList;

    procedure Log(const AMessage: string);

    procedure ClearHits;
    procedure ClearResultsGrid;
    procedure AddHitToGrid(const AHitPtr: PSearchHit);

    procedure AutoSizeResultsColumns(const AMaxRowsToScan: Integer);
    function MeasureCellTextWidth(const S: string): Integer;

    procedure ShowHitInDetails(const AHitPtr: PSearchHit);

    function GridRowHitPtr(const ARow: Integer): PSearchHit;
    procedure SetGridRowHitPtr(const ARow: Integer; const AHitPtr: PSearchHit);

    procedure GridResultsDblClick(Sender: TObject);

    function BuildLibreOfficeTargetUrl(const AHitPtr: PSearchHit): string;
    procedure OpenHitInLibreOffice(const AHitPtr: PSearchHit);
    procedure OpenSearchAsync(Data: PtrInt);

  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FHits := TList.Create;

  // Results grid setup (matches .lfm)
  GridResults.ColCount := 5;
  GridResults.FixedRows := 1;
  GridResults.RowCount := 2;
  GridResults.Options := GridResults.Options + [goRowSelect];

  GridResults.Cells[0, 0] := 'File';
  GridResults.Cells[1, 0] := 'Sheet';
  GridResults.Cells[2, 0] := 'Cell';
  GridResults.Cells[3, 0] := 'Value';
  GridResults.Cells[4, 0] := 'Context';

  // Assign double-click in code (no .lfm change)
  GridResults.OnDblClick := @GridResultsDblClick;

  // Details memo UX (horizontal scroll for long paths/text)
  MemoDetails.ReadOnly := True;
  MemoDetails.ScrollBars := ssAutoBoth;
  MemoDetails.WordWrap := False;

  // Log memo UX
  MemoLog.ReadOnly := True;
  MemoLog.ScrollBars := ssAutoVertical;

  PageControl1.ActivePage := TabResults;

  Log('Application started.');
  if AppPrefs.AutoOpenSearchDialog then
    Application.QueueAsyncCall(@OpenSearchAsync, 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ClearHits;
  FreeAndNil(FHits);
end;

procedure TMainForm.Log(const AMessage: string);
var
  TimeStamp: string;
begin
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  MemoLog.Lines.Add(TimeStamp + '  ' + AMessage);
end;

procedure TMainForm.ClearHits;
var
  I: Integer;
  HitPtr: PSearchHit;
begin
  if FHits = nil then
    Exit;

  for I := 0 to FHits.Count - 1 do
  begin
    HitPtr := PSearchHit(FHits[I]);
    Dispose(HitPtr);
  end;

  FHits.Clear;
end;

procedure TMainForm.ClearResultsGrid;
begin
  GridResults.RowCount := 1;
  SetGridRowHitPtr(1, nil);
  MemoDetails.Clear;
end;

function TMainForm.GridRowHitPtr(const ARow: Integer): PSearchHit;
begin
  if (ARow < 1) or (ARow >= GridResults.RowCount) then
    Exit(nil);

  Result := PSearchHit(PtrUInt(GridResults.Objects[0, ARow]));
end;

procedure TMainForm.SetGridRowHitPtr(const ARow: Integer; const AHitPtr: PSearchHit);
begin
  if (ARow < 1) or (ARow >= GridResults.RowCount) then
    Exit;

  GridResults.Objects[0, ARow] := TObject(PtrUInt(AHitPtr));
end;

procedure TMainForm.AddHitToGrid(const AHitPtr: PSearchHit);
var
  Row: Integer;
begin
  Row := GridResults.RowCount;
  GridResults.RowCount := Row + 1;

  // Show filename only
  GridResults.Cells[0, Row] := ExtractFileName(AHitPtr^.FilePath);
  GridResults.Cells[1, Row] := AHitPtr^.SheetName;
  GridResults.Cells[2, Row] := AHitPtr^.CellA1;
  GridResults.Cells[3, Row] := AHitPtr^.CellText;
  GridResults.Cells[4, Row] := AHitPtr^.ContextText;

  SetGridRowHitPtr(Row, AHitPtr);
end;

function TMainForm.MeasureCellTextWidth(const S: string): Integer;
begin
  GridResults.Canvas.Font.Assign(GridResults.Font);
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

    // Caps to keep UI sane (details panel shows full)
    case Col of
      0: if MaxWidth > 320 then MaxWidth := 320; // File (filename)
      1: if MaxWidth > 200 then MaxWidth := 200; // Sheet
      2: if MaxWidth > 90 then MaxWidth := 90;   // Cell
      3: if MaxWidth > 600 then MaxWidth := 600; // Value
      4: if MaxWidth > 220 then MaxWidth := 220; // Context
    else
      if MaxWidth > 250 then MaxWidth := 250;
    end;

    if MaxWidth < 70 then
      MaxWidth := 70;

    GridResults.ColWidths[Col] := MaxWidth;
  end;
end;

procedure TMainForm.ShowHitInDetails(const AHitPtr: PSearchHit);
begin
  if AHitPtr = nil then
  begin
    MemoDetails.Clear;
    Exit;
  end;

  MemoDetails.Lines.BeginUpdate;
  try
    MemoDetails.Clear;
    MemoDetails.Lines.Add('File:    ' + AHitPtr^.FilePath);
    MemoDetails.Lines.Add('Sheet:   ' + AHitPtr^.SheetName);
    MemoDetails.Lines.Add('Cell:    ' + AHitPtr^.CellA1);
    MemoDetails.Lines.Add('');
    MemoDetails.Lines.Add('Value:');
    MemoDetails.Lines.Add(AHitPtr^.CellText);

    if AHitPtr^.ContextText <> '' then
    begin
      MemoDetails.Lines.Add('');
      MemoDetails.Lines.Add('Context:');
      MemoDetails.Lines.Add(AHitPtr^.ContextText);
    end;
  finally
    MemoDetails.Lines.EndUpdate;
  end;
end;

procedure TMainForm.GridResultsSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  HitPtr: PSearchHit;
begin
  if aRow <= 0 then
  begin
    MemoDetails.Clear;
    Exit;
  end;

  HitPtr := GridRowHitPtr(aRow);
  ShowHitInDetails(HitPtr);

  if HitPtr <> nil then
  begin
    GridResults.Hint := HitPtr^.FilePath;
    GridResults.ShowHint := True;
  end;
end;

function TMainForm.BuildLibreOfficeTargetUrl(const AHitPtr: PSearchHit): string;
var
  FileUrl: string;
  SheetEsc: string;
begin
  // Convert local path (UTF-8, spaces, æøå, etc.) to file:/// URL safely
  FileUrl := FilenameToURI(AHitPtr^.FilePath);

  // Sheet names with spaces must be quoted. LibreOffice uses single quotes.
  // Escape embedded ' by doubling it.
  SheetEsc := StringReplace(AHitPtr^.SheetName, '''', '''''', [rfReplaceAll]);

  // External target syntax commonly uses: file:///path.ods#$'Sheet name'.A1
  Result := FileUrl + '#$''' + SheetEsc + '''.' + AHitPtr^.CellA1;
end;

procedure TMainForm.OpenSearchAsync(Data: PtrInt);
begin
  MenuItemNewSearchClick(Self);  // or nil
end;

procedure TMainForm.OpenHitInLibreOffice(const AHitPtr: PSearchHit);
var
  TargetUrl: string;
  Proc: TProcess;
  ExePath: string;

  procedure TryRun(const AExe: string);
  begin
    Proc.Executable := AExe;
    Proc.Parameters.Clear;
    Proc.Parameters.Add('--calc');
    Proc.Parameters.Add(TargetUrl);
    Proc.Options := [poNoConsole];
    Proc.Execute;
  end;

begin
  if AHitPtr = nil then
    Exit;

  TargetUrl := BuildLibreOfficeTargetUrl(AHitPtr);

  Log('Open in LibreOffice: ' + TargetUrl);

  Proc := TProcess.Create(nil);
  try
    // Prefer 'libreoffice' wrapper if installed; fallback to 'soffice'
    ExePath := FindDefaultExecutablePath('libreoffice');
    if ExePath = '' then
      ExePath := FindDefaultExecutablePath('soffice');

    try
      if ExePath <> '' then
        TryRun(ExePath)
      else
        TryRun('libreoffice');
    except
      on E: Exception do
      begin
        // Fallback attempt
        try
          TryRun('soffice');
        except
          on E2: Exception do
            MessageDlg('Unable to start LibreOffice.' + LineEnding +
              'Tried libreoffice/soffice.' + LineEnding +
              'Error: ' + E2.Message, mtError, [mbOK], 0);
        end;
      end;
    end;

  finally
    Proc.Free;
  end;
end;

procedure TMainForm.GridResultsDblClick(Sender: TObject);
var
  Row: Integer;
  HitPtr: PSearchHit;
begin
  Row := GridResults.Row;
  if Row <= 0 then
    Exit;

  HitPtr := GridRowHitPtr(Row);
  OpenHitInLibreOffice(HitPtr);
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemPreferencesClick(Sender: TObject);
var
  PrefForm: TPreferencesForm;
begin
  PrefForm := TPreferencesForm.Create(Self);
  try
    if PrefForm.ShowModal = mrOK then
      Log('Preferences saved.')
    else
      Log('Preferences canceled.');
  finally
    PrefForm.Free;
  end;
end;

procedure TMainForm.MenuItemOpenSearchClick(Sender: TObject);
begin
  Log('Open Search clicked (not implemented yet).');
end;

procedure TMainForm.MenuItemSaveClick(Sender: TObject);
begin
  Log('Save Search clicked (not implemented yet).');
end;

procedure TMainForm.MenuItemSaveAsClick(Sender: TObject);
begin
  Log('Save Search As clicked (not implemented yet).');
end;

procedure TMainForm.MenuItemExportClick(Sender: TObject);
begin
  Log('Export clicked (not implemented yet).');
end;

procedure TMainForm.MenuItemNewSearchClick(Sender: TObject);
var
  SearchForm: TSearchForm;
  Crit: TSearchCriteria;
  Engine: TSearchEngine;
  I: Integer;
  HitPtr: PSearchHit;
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

  if (Trim(Crit.Directory) = '') or (not DirectoryExists(Crit.Directory)) then
  begin
    Log('Invalid directory: ' + Crit.Directory);
    Exit;
  end;

  PageControl1.ActivePage := TabResults;

  ClearResultsGrid;
  ClearHits;

  Log('Search started: "' + Crit.QueryText + '" in ' + Crit.Directory + ' mask=' + Crit.FileMask);

  Engine := TSearchEngine.Create(Crit, @Log);
  try
    Engine.Execute(FHits);
    Log('Files scanned: ' + IntToStr(Engine.FilesScanned) +
        ', failed: ' + IntToStr(Engine.FilesFailed));
  finally
    Engine.Free;
  end;

  Log('Hits: ' + IntToStr(FHits.Count));

  for I := 0 to FHits.Count - 1 do
  begin
    HitPtr := PSearchHit(FHits[I]);
    AddHitToGrid(HitPtr);
  end;

  AutoSizeResultsColumns(200);

  Log('Search finished.');
end;

end.

