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
    chkSubfolders: TCheckBox;
    chkMatchCase: TCheckBox;
    chkWholeWord: TCheckBox;
    chkRegex: TCheckBox;
    edtMask: TEdit;
    edtQuery: TEdit;
    edtDirectory: TEdit;
    RadioGroupResultMode: TRadioGroup;
    LabelWhatToFind: TLabel;
    LabelDirectory: TLabel;
    LabelFileMask: TLabel;
    rbCellOnly: TRadioButton;
    rbWholeRow: TRadioButton;
    rbWholeColumn: TRadioButton;
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
  Result.FileMask := edtMask.Text;
  Result.SearchSubfolders := chkSubfolders.Checked;

  Result.MatchCase := chkMatchCase.Checked;
  Result.WholeWord := chkWholeWord.Checked;
  Result.UseRegex := chkRegex.Checked;

  case RadioGroupResultMode.ItemIndex of
    1: Result.ResultMode := rmWholeRow;
    2: Result.ResultMode := rmWholeColumn;
  else
    Result.ResultMode := rmCellOnly;
  end;
end;

end.

