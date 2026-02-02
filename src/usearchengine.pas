unit usearchengine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Masks, RegExpr, LazUTF8,
  fpstypes, fpspreadsheet, fpsallformats, fpsutils,
  uModels;

type
  TLogProc = procedure(const AMsg: string) of object;

  TSearchEngine = class
  private
    FCrit: TSearchCriteria;
    FOnLog: TLogProc;

    FRegex: TRegExpr;
    FMasks: TStringList;

    FFilesScanned: Integer;
    FFilesFailed: Integer;

    procedure LogMsg(const AMsg: string);

    procedure BuildMasks;
    function NormalizeMask(const AMask: string): string;
    function MatchesFileMask(const AFileName: string): Boolean;

    function IsWordChar(const C: Char): Boolean;
    function ContainsWholeWord(const AHaystack, ANeedle: string): Boolean;
    function IsMatch(const AText: string): Boolean;

    function LooksLikeFileLink(const S: string): Boolean;
    function ShouldIgnoreCellText(const S: string): Boolean;

    function BuildContextText(const ASheet: TsWorksheet; const ARow, ACol: Cardinal): string;

    procedure LoadWorkbookRobust(const AFilePath: string; const AWorkbook: TsWorkbook);
    procedure ScanSpreadsheetFile(const AFilePath: string; const AResults: TList);
    procedure ScanDirectory(const ADir: string; const AResults: TList);

  public
    constructor Create(const ACrit: TSearchCriteria; const AOnLog: TLogProc);
    destructor Destroy; override;

    procedure Execute(const AResults: TList);

    property FilesScanned: Integer read FFilesScanned;
    property FilesFailed: Integer read FFilesFailed;
  end;

implementation

procedure TSearchEngine.LogMsg(const AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMsg);
end;

constructor TSearchEngine.Create(const ACrit: TSearchCriteria; const AOnLog: TLogProc);
begin
  inherited Create;

  FCrit := ACrit;
  FOnLog := AOnLog;

  FFilesScanned := 0;
  FFilesFailed := 0;

  FMasks := TStringList.Create;
  FMasks.StrictDelimiter := True;
  FMasks.Delimiter := ';';

  FRegex := TRegExpr.Create;

  BuildMasks;

  if FCrit.UseRegex then
  begin
    FRegex.Expression := FCrit.QueryText;
    FRegex.ModifierI := not FCrit.MatchCase;
  end;
end;

destructor TSearchEngine.Destroy;
begin
  FRegex.Free;
  FMasks.Free;
  inherited Destroy;
end;

function TSearchEngine.NormalizeMask(const AMask: string): string;
var
  M: string;
begin
  M := Trim(AMask);

  if M = '' then
    Exit('*.ods');

  // If user typed ".ods" or "ods" -> "*.ods"
  if (Pos('*', M) = 0) and (Pos('?', M) = 0) then
  begin
    if (M[1] <> '.') then
      M := '.' + M;
    M := '*' + M;
  end;

  Result := M;
end;

procedure TSearchEngine.BuildMasks;
var
  Raw: string;
  I: Integer;
  Part: string;
begin
  FMasks.Clear;

  Raw := Trim(FCrit.FileMask);
  if Raw = '' then
    Raw := '*.ods';

  // Split manually to avoid TStringList.DelimitedText corner cases
  I := 1;
  while I <= Length(Raw) do
  begin
    Part := '';
    while (I <= Length(Raw)) and (Raw[I] <> ';') do
    begin
      Part := Part + Raw[I];
      Inc(I);
    end;

    Part := NormalizeMask(Part);
    if Part <> '' then
      FMasks.Add(Part);

    if (I <= Length(Raw)) and (Raw[I] = ';') then
      Inc(I);
  end;

  if FMasks.Count = 0 then
    FMasks.Add('*.ods');
end;

function TSearchEngine.MatchesFileMask(const AFileName: string): Boolean;
var
  I: Integer;
begin
  if FMasks.Count = 0 then
    Exit(True);

  for I := 0 to FMasks.Count - 1 do
  begin
    if MatchesMask(AFileName, FMasks[I]) then
      Exit(True);
  end;

  Result := False;
end;

function TSearchEngine.IsWordChar(const C: Char): Boolean;
begin
  Result :=
    ((C >= 'A') and (C <= 'Z')) or
    ((C >= 'a') and (C <= 'z')) or
    ((C >= '0') and (C <= '9')) or
    (C = '_') or
    (Ord(C) > 127);
end;

function TSearchEngine.ContainsWholeWord(const AHaystack, ANeedle: string): Boolean;
var
  P: SizeInt;
  LeftOK: Boolean;
  RightOK: Boolean;
  LeftChar: Char;
  RightChar: Char;
begin
  Result := False;
  if (ANeedle = '') or (AHaystack = '') then
    Exit;

  P := Pos(ANeedle, AHaystack);
  while P > 0 do
  begin
    if P = 1 then
      LeftOK := True
    else
    begin
      LeftChar := AHaystack[P - 1];
      LeftOK := not IsWordChar(LeftChar);
    end;

    if (P + Length(ANeedle)) > Length(AHaystack) then
      RightOK := True
    else
    begin
      RightChar := AHaystack[P + Length(ANeedle)];
      RightOK := not IsWordChar(RightChar);
    end;

    if LeftOK and RightOK then
      Exit(True);

    P := PosEx(ANeedle, AHaystack, P + 1);
  end;
end;

function TSearchEngine.IsMatch(const AText: string): Boolean;
var
  Haystack: string;
  Needle: string;
begin
  Result := False;

  if FCrit.QueryText = '' then
    Exit(False);

  if FCrit.UseRegex then
    Exit(FRegex.Exec(AText));

  Haystack := AText;
  Needle := FCrit.QueryText;

  if not FCrit.MatchCase then
  begin
    Haystack := UTF8LowerCase(Haystack);
    Needle := UTF8LowerCase(Needle);
  end;

  if FCrit.WholeWord then
    Result := ContainsWholeWord(Haystack, Needle)
  else
    Result := Pos(Needle, Haystack) > 0;
end;

function TSearchEngine.LooksLikeFileLink(const S: string): Boolean;
var
  L: string;
begin
  L := LowerCase(Trim(S));

  Result :=
    (Pos('file://', L) = 1) or
    (Pos('smb://', L) = 1) or
    (Pos('ssh://', L) = 1) or
    (Pos('ftp://', L) = 1) or
    (Pos('http://', L) = 1) or
    (Pos('https://', L) = 1) or
    (Pos('\\', S) = 1) or                // UNC \\server\share
    ((Length(S) >= 2) and (S[2] = ':'));  // C:\...
end;

function TSearchEngine.ShouldIgnoreCellText(const S: string): Boolean;
begin
  Result := LooksLikeFileLink(S);
end;

function TSearchEngine.BuildContextText(const ASheet: TsWorksheet; const ARow, ACol: Cardinal): string;
var
  Col: Cardinal;
  Row: Cardinal;
  Parts: TStringList;
  CellText: string;
  I: Integer;
begin
  Result := '';

  if FCrit.ResultMode = rmCellOnly then
    Exit('');

  Parts := TStringList.Create;
  try
    if FCrit.ResultMode = rmWholeRow then
    begin
      for Col := 0 to 255 do
      begin
        CellText := ASheet.ReadAsText(ARow, Col);
        if (CellText <> '') and (not ShouldIgnoreCellText(CellText)) then
          Parts.Add(CellText);
      end;
    end
    else if FCrit.ResultMode = rmWholeColumn then
    begin
      for Row := 0 to 1000 do
      begin
        CellText := ASheet.ReadAsText(Row, ACol);
        if (CellText <> '') and (not ShouldIgnoreCellText(CellText)) then
          Parts.Add(CellText);
      end;
    end;

    // Safe join
    for I := 0 to Parts.Count - 1 do
    begin
      if I > 0 then
        Result := Result + ' | ';
      Result := Result + Parts[I];
    end;

  finally
    Parts.Free;
  end;
end;

procedure TSearchEngine.LoadWorkbookRobust(const AFilePath: string; const AWorkbook: TsWorkbook);
var
  Stream: TFileStream;
begin
  // Fast path
  try
    AWorkbook.ReadFromFile(AFilePath);
    Exit;
  except
    // Fallback below
  end;

  // Retry: stream + explicit format (helps with some edge cases)
  Stream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    {$if declared(sfOpenDocument)}
      AWorkbook.ReadFromStream(Stream, sfOpenDocument);
    {$elseif declared(sfODS)}
      AWorkbook.ReadFromStream(Stream, sfODS);
    {$else}
      // If neither is available, try auto-detect from stream
      AWorkbook.ReadFromStream(Stream);
    {$endif}
  finally
    Stream.Free;
  end;
end;

procedure TSearchEngine.ScanSpreadsheetFile(const AFilePath: string; const AResults: TList);
var
  Workbook: TsWorkbook;
  Sheet: TsWorksheet;
  SheetIndex: Integer;
  Cell: PCell;
  Hit: ^TSearchHit;
  TextValue: string;
  ActualFilePath: string;
  HashPos: SizeInt;
begin
  // Defensive: fpspreadsheet treats "file.ods#Sheet" as workbook+sheet selector
  ActualFilePath := AFilePath;
  HashPos := Pos('#', ActualFilePath);
  if HashPos > 0 then
    ActualFilePath := Copy(ActualFilePath, 1, HashPos - 1);

  Workbook := TsWorkbook.Create;
  try
    LoadWorkbookRobust(ActualFilePath, Workbook);

    for SheetIndex := 0 to Workbook.GetWorksheetCount - 1 do
    begin
      Sheet := Workbook.GetWorksheetByIndex(SheetIndex);

      for Cell in Sheet.Cells do
      begin
        TextValue := Sheet.ReadAsText(Cell^.Row, Cell^.Col);
        if TextValue = '' then
          Continue;

        // Ignore link-like content (your requirement)
        if ShouldIgnoreCellText(TextValue) then
          Continue;

        if IsMatch(TextValue) then
        begin
          New(Hit);
          Hit^.FilePath := ActualFilePath;
          Hit^.SheetName := Sheet.Name;
          Hit^.RowIndex := Cell^.Row;
          Hit^.ColIndex := Cell^.Col;
          // Hit^.CellA1 := GetCellString(Cell^.Col, Cell^.Row);
          Hit^.CellA1 := GetCellString(Cell^.Row, Cell^.Col);
          Hit^.CellText := TextValue;
          Hit^.ContextText := BuildContextText(Sheet, Cell^.Row, Cell^.Col);
          AResults.Add(Hit);
        end;
      end;
    end;

  finally
    Workbook.Free;
  end;
end;

procedure TSearchEngine.ScanDirectory(const ADir: string; const AResults: TList);
var
  SR: TSearchRec;
  Path: string;
  FullName: string;
begin
  Path := IncludeTrailingPathDelimiter(ADir);

  if FindFirst(Path + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;

      FullName := Path + SR.Name;

      if (SR.Attr and faDirectory) <> 0 then
      begin
        if FCrit.SearchSubfolders then
          ScanDirectory(FullName, AResults);
        Continue;
      end;

      if not MatchesFileMask(SR.Name) then
        Continue;

      Inc(FFilesScanned);
      try
        ScanSpreadsheetFile(FullName, AResults);
      except
        on E: Exception do
        begin
          Inc(FFilesFailed);
          LogMsg('Failed to read: ' + FullName + '  ' + E.ClassName + ': ' + E.Message);
        end;
      end;

    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure TSearchEngine.Execute(const AResults: TList);
begin
  if (Trim(FCrit.Directory) = '') or (not DirectoryExists(FCrit.Directory)) then
    raise Exception.Create('Invalid directory: ' + FCrit.Directory);

  LogMsg('Masks: ' + FMasks.CommaText);
  ScanDirectory(FCrit.Directory, AResults);
end;

end.

