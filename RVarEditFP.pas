unit RVarEditFP;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TFormVarEditFP = class(TDialogTemplate)
    ValueEditLabel: TLabel;
    ValueEdit: TEdit;
    DirectoryButton: TSpeedButton;
    FileButton: TSpeedButton;
    VarsButton: TSpeedButton;
    procedure VarsButtonClick(Sender: TObject);
    procedure DirectoryButtonClick(Sender: TObject);
    procedure FileButtonClick(Sender: TObject);
  private
    fTagsChar: Char;
    fVarList: TStrings;
  public
    { Public declarations }
  end;

function EditStringFPV(const DlgTitle: string; VarList: TStrings; const TagsChar: Char;
  var Value: string): Boolean;

implementation

{$R *.dfm}

uses
  FileCtrl, RVarListSelect;

function EditStringFPV(const DlgTitle: string; VarList: TStrings; const TagsChar: Char;
  var Value: string): Boolean;
begin
  with TFormVarEditFP.Create(Application.MainForm) do
  begin
    try
      if DlgTitle <> EmptyStr then
        Caption := DlgTitle;
      fTagsChar := TagsChar;
      fVarList := VarList;
      ValueEdit.Text := Value;
      Result := ShowModal = mrOk;
      if Result then Value := ValueEdit.Text; 
    finally
      Free;
    end;
  end;
end;

procedure TFormVarEditFP.VarsButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  SelValue := ValueEdit.SelText;
  if (SelValue <> EmptyStr) then
  begin
    while SelValue[1] = fTagsChar do
      Delete(SelValue, 1, 1);
    while SelValue[Length(SelValue)] = fTagsChar do
      Delete(SelValue, Length(SelValue), 1);
  end;
  if SelectVarName(fVarList, SelValue) then
    ValueEdit.SelText := fTagsChar + SelValue + fTagsChar;
end;

procedure TFormVarEditFP.DirectoryButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  SelValue := ValueEdit.Text;
  if SelectDirectory(EmptyStr, EmptyStr, SelValue) then
    ValueEdit.Text := SelValue;
end;

procedure TFormVarEditFP.FileButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  SelValue := ValueEdit.Text;
  if PromptForFileName(SelValue) then
    ValueEdit.Text := SelValue;
end;

end.
