unit fsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uModels;

type

  { TSearchForm }

  TSearchForm = class(TForm)
    btnBrowseDir: TButton;
    btnCancel: TButton;
    btnSearch: TButton;
    chkMatchCase: TCheckBox;
    chkWholeWord: TCheckBox;
    cbFileType: TComboBox;
    cbSearchSubfolders: TComboBox;
    edtMask: TEdit;
    edtQuery: TEdit;
    edtDirectory: TEdit;
    Label1: TLabel;
    rbRegex: TRadioButton;
    rbNormal: TRadioButton;
    rgSearchMode: TRadioGroup;
    LabelWhatToFind: TLabel;
    LabelDirectory: TLabel;
    LabelFileMask: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnBrowseDirClick(Sender: TObject);
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
  SelectDirectoryDialog1.InitialDir := edtDirectory.Text;  // optional
  if SelectDirectoryDialog1.Execute then
    edtDirectory.Text := SelectDirectoryDialog1.FileName;
end;

procedure TSearchForm.FormCreate(Sender: TObject);
begin

end;


function TSearchForm.BuildCriteria: TSearchCriteria;
begin
  Result.QueryText := edtQuery.Text;
  Result.Directory := edtDirectory.Text;
  Result.FileMask := (edtMask.Text + '.' + cbFileType.Items[cbFileType.ItemIndex]);

  if (cbSearchSubfolders.Items[cbSearchSubfolders.ItemIndex] = '0') then
    Result.SearchSubfolders := false
  else
    Result.SearchSubfolders := true;

  Result.MatchCase := chkMatchCase.Checked;
  Result.WholeWord := chkWholeWord.Checked;
  Result.ResultMode := rmCellOnly;

  if (rbNormal.Checked) then
    Result.UseRegex :=  false
  else
    Result.UseRegex := true;

end;

end.

