unit fpreferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  uAppPrefs;

type
  TPreferencesForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnBrowseDir: TButton;
    chkAutoOpenSearch: TCheckBox;
    edtDefaultDir: TEdit;
    LabelDefaultDir: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnBrowseDirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

procedure TPreferencesForm.FormShow(Sender: TObject);
begin
  chkAutoOpenSearch.Checked := AppPrefs.AutoOpenSearchDialog;
  edtDefaultDir.Text := AppPrefs.DefaultSearchDirectory;
end;

procedure TPreferencesForm.btnBrowseDirClick(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := edtDefaultDir.Text;
  if SelectDirectoryDialog1.Execute then
    edtDefaultDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TPreferencesForm.btnOKClick(Sender: TObject);
begin
  AppPrefs.AutoOpenSearchDialog := chkAutoOpenSearch.Checked;
  AppPrefs.DefaultSearchDirectory := Trim(edtDefaultDir.Text);
  AppPrefs.Save;
  ModalResult := mrOK;
end;

end.

