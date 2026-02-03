unit uAppPrefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg, FileUtil;

type
  TAppPreferences = class
  private
    FAutoOpenSearchDialog: Boolean;
    FDefaultSearchDirectory: string;
    FLastSearchDirectory: string;
    FRecentDirectories: TStringList;
    FConfigFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;
    procedure AddRecentDirectory(const Dir: string);

    property AutoOpenSearchDialog: Boolean
      read FAutoOpenSearchDialog write FAutoOpenSearchDialog;

    property DefaultSearchDirectory: string
      read FDefaultSearchDirectory write FDefaultSearchDirectory;

    property LastSearchDirectory: string
      read FLastSearchDirectory write FLastSearchDirectory;

    property RecentDirectories: TStringList
      read FRecentDirectories;
  end;

function AppPrefs: TAppPreferences;

implementation

var
  GPrefs: TAppPreferences = nil;

function AppPrefs: TAppPreferences;
begin
  if GPrefs = nil then
    GPrefs := TAppPreferences.Create;
  Result := GPrefs;
end;

constructor TAppPreferences.Create;
begin
  inherited Create;

  FConfigFileName := GetAppConfigFile(False);
  FRecentDirectories := TStringList.Create;

  // Defaults
  FAutoOpenSearchDialog := True;
  FDefaultSearchDirectory := GetUserDir;
  FLastSearchDirectory := '';

  Load;
end;

destructor TAppPreferences.Destroy;
begin
  FRecentDirectories.Free;
  inherited Destroy;
end;

procedure TAppPreferences.Load;
var
  Cfg: TXMLConfig;
  Count: Integer;
  I: Integer;
  Key: string;
begin
  Cfg := TXMLConfig.Create(nil);
  try
    Cfg.Filename := FConfigFileName;

    FAutoOpenSearchDialog := Cfg.GetValue('ui/auto_open_search', True);
    FDefaultSearchDirectory := Cfg.GetValue('search/default_directory', GetUserDir);
    FLastSearchDirectory := Cfg.GetValue('search/last_directory', '');

    FRecentDirectories.Clear;
    Count := Cfg.GetValue('search/recent_dirs/count', 0);
    for I := 0 to Count - 1 do
    begin
      Key := Format('search/recent_dirs/dir%d', [I]);
      FRecentDirectories.Add(Cfg.GetValue(Key, ''));
    end;

    // Remove empties
    for I := FRecentDirectories.Count - 1 downto 0 do
      if Trim(FRecentDirectories[I]) = '' then
        FRecentDirectories.Delete(I);

  finally
    Cfg.Free;
  end;
end;

procedure TAppPreferences.Save;
var
  Cfg: TXMLConfig;
  I: Integer;
  MaxCount: Integer;
  Key: string;
begin
  Cfg := TXMLConfig.Create(nil);
  try
    Cfg.Filename := FConfigFileName;

    Cfg.SetValue('ui/auto_open_search', FAutoOpenSearchDialog);
    Cfg.SetValue('search/default_directory', FDefaultSearchDirectory);
    Cfg.SetValue('search/last_directory', FLastSearchDirectory);

    MaxCount := FRecentDirectories.Count;
    if MaxCount > 25 then
      MaxCount := 25;

    Cfg.SetValue('search/recent_dirs/count', MaxCount);
    for I := 0 to MaxCount - 1 do
    begin
      Key := Format('search/recent_dirs/dir%d', [I]);
      Cfg.SetValue(Key, FRecentDirectories[I]);
    end;

    Cfg.Flush;
  finally
    Cfg.Free;
  end;
end;

procedure TAppPreferences.AddRecentDirectory(const Dir: string);
var
  CleanDir: string;
  Index: Integer;
begin
  CleanDir := Trim(Dir);
  if CleanDir = '' then
    Exit;

  Index := FRecentDirectories.IndexOf(CleanDir);
  if Index >= 0 then
    FRecentDirectories.Delete(Index);

  FRecentDirectories.Insert(0, CleanDir);

  while FRecentDirectories.Count > 25 do
    FRecentDirectories.Delete(FRecentDirectories.Count - 1);
end;

finalization
  FreeAndNil(GPrefs);

end.

