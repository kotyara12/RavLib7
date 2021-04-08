unit RScriptsPrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RScripts;

type
  TFormScriptPrm = class(TDialogTemplate)
    ValueEditLabel: TLabel;
    ValueEdit: TEdit;
    DirectoryButton: TSpeedButton;
    FileButton: TSpeedButton;
    VarsButton: TSpeedButton;
    procedure VarsButtonClick(Sender: TObject);
    procedure DirectoryButtonClick(Sender: TObject);
    procedure FileButtonClick(Sender: TObject);
  private
    fEditor: TRScriptEditor;
  public
  end;

function EditCmdParameter(Editor: TRScriptEditor; var Value: string): Boolean;

implementation

{$R *.dfm}

uses
  FileCtrl;

function EditCmdParameter(Editor: TRScriptEditor; var Value: string): Boolean;
begin
  with TFormScriptPrm.Create(Application.MainForm) do
  begin
    try
      fEditor := Editor;
      ValueEdit.Text := Value;
      Result := ShowModal = mrOk;
      if Result then Value := ValueEdit.Text; 
    finally
      Free;
    end;
  end;
end;

procedure TFormScriptPrm.VarsButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  SelValue := ValueEdit.SelText;
  if fEditor.SelectVariableName(True, SelValue) then
    ValueEdit.SelText := SelValue;
end;

procedure TFormScriptPrm.DirectoryButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  SelValue := ValueEdit.Text;
  if SelectDirectory(EmptyStr, EmptyStr, SelValue) then
    ValueEdit.Text := ExpandUncFileName(SelValue);
end;

procedure TFormScriptPrm.FileButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  SelValue := ValueEdit.Text;
  if PromptForFileName(SelValue) then
    ValueEdit.Text := ExpandUncFileName(SelValue);
end;

end.
