unit fsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

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
    GroupResultMode: TGroupBox;
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

end.

