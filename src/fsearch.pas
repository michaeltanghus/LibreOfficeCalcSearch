unit fsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uModels, uAppPrefs;

type

  { TSearchForm }

  TSearchForm = class(TForm)
    btnBrowseDir: TButton;
    btnCancel: TButton;
    btnSearch: TButton;
    cbTrim: TCheckBox;
    chkMatchCase: TCheckBox;
    chkWholeWord: TCheckBox;
    cbFileType: TComboBox;
    cbSearchSubfolders: TComboBox;
    cmbDirectory: TComboBox;
    cmbEdit: TComboBox;
    edtMask: TEdit;
    Label1: TLabel;
    rbWholeColumn: TRadioButton;
    rbWholeRow: TRadioButton;
    rbCellOnly: TRadioButton;
    RadioGroupResultMode: TRadioGroup;
    rbRegex: TRadioButton;
    rbNormal: TRadioButton;
    rgSearchMode: TRadioGroup;
    LabelWhatToFind: TLabel;
    LabelDirectory: TLabel;
    LabelFileMask: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnBrowseDirClick(Sender: TObject);
    procedure cmbEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    function BuildCriteria: TSearchCriteria;
  end;

var
  SearchForm: TSearchForm;

implementation

{$R *.lfm}

{ TSearchForm }

procedure TSearchForm.btnBrowseDirClick(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := cmbDirectory.Text;

  if SelectDirectoryDialog1.Execute then
  begin
    cmbDirectory.Text := SelectDirectoryDialog1.FileName;  // REPLACE
    cmbDirectory.SelStart := Length(cmbDirectory.Text);    // caret at end (optional)
    cmbDirectory.SelLength := 0;
  end;
end;

procedure TSearchForm.cmbEditChange(Sender: TObject);
var
  S: string;
begin
  S := cmbEdit.Text;

  // If the query has leading/trailing spaces, do NOT trim.
  // Otherwise default to trim.
  if (S <> Trim(S)) then
    cbTrim.Checked := False
  else
    cbTrim.Checked := True;
end;

procedure TSearchForm.FormCreate(Sender: TObject);
var
  InitialQuery: string;
  InitialDir: string;
begin
  // Query history
  cmbEdit.Items.BeginUpdate;
  try
    cmbEdit.Items.Clear;
    cmbEdit.Items.AddStrings(AppPrefs.RecentQueries);
  finally
    cmbEdit.Items.EndUpdate;
  end;


  InitialQuery := AppPrefs.LastQuery;
  cmbEdit.Text := InitialQuery;

  // If the query has leading/trailing spaces, do NOT trim.
  // Otherwise default to trim.
  if (cmbEdit.Text <> Trim(cmbEdit.Text)) then
    cbTrim.Checked := False
  else
    cbTrim.Checked := True;


  // Directory history
  cmbDirectory.Items.BeginUpdate;
  try
    cmbDirectory.Items.Clear;
    cmbDirectory.Items.AddStrings(AppPrefs.RecentDirectories);
  finally
    cmbDirectory.Items.EndUpdate;
  end;

  InitialDir := AppPrefs.LastSearchDirectory;
  if (InitialDir = '') or (not DirectoryExists(InitialDir)) then
    InitialDir := AppPrefs.DefaultSearchDirectory;

  cmbDirectory.Text := InitialDir;
end;


function TSearchForm.BuildCriteria: TSearchCriteria;
var
  RawQuery: string;
begin
  // Query
  RawQuery := cmbEdit.Text;   // preserve exactly as typed

  // Always prevent "empty" query (only whitespace)
  if Trim(RawQuery) = '' then
    raise Exception.Create('Query is empty.');

  // Apply trim only if user requested it
  if cbTrim.Checked then
    Result.QueryText := Trim(RawQuery)
  else
    Result.QueryText := RawQuery;

//  Result.QueryText := cmbEdit.Text;

  Result.Directory := Trim(cmbDirectory.Text);
  Result.FileMask := (edtMask.Text + '.' + cbFileType.Items[cbFileType.ItemIndex]);

  if (cbSearchSubfolders.Items[cbSearchSubfolders.ItemIndex] = '0') then
    Result.SearchSubfolders := false
  else
    Result.SearchSubfolders := true;

  Result.MatchCase := chkMatchCase.Checked;
  Result.WholeWord := chkWholeWord.Checked;

  if rbWholeRow.Checked then
    Result.ResultMode := rmWholeRow
  else if rbWholeColumn.Checked then
    Result.ResultMode := rmWholeColumn
  else
    Result.ResultMode := rmCellOnly;

  if (rbNormal.Checked) then
    Result.UseRegex :=  false
  else
    Result.UseRegex := true;

end;


end.

