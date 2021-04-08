unit RScriptsVar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RScripts;

type
  TFormScriptVar = class(TDialogTemplate)
    NameEditLabel: TLabel;
    ValueEditLabel: TLabel;
    ValueEdit: TEdit;
    VarListBtn: TSpeedButton;
    ComboBox: TComboBox;
    ComboBoxLabel: TLabel;
    VarNameButton: TSpeedButton;
    NameEdit: TComboBox;
    procedure VarListBtnClick(Sender: TObject);
    procedure VarNameButtonClick(Sender: TObject);
  private
    fEditor: TRScriptEditor;
    function  GetCommand: string;
    procedure SetCommand(const Value: string);
  public
    procedure LoadVariables(Editor: TRScriptEditor);
    property  CommandName: string read GetCommand write SetCommand;
  end;

function EditVarCommand(Editor: TRScriptEditor; var Token, VarName, VarValue: string): Boolean;

implementation

uses
  RVclUtils, StrUtils;

{$R *.dfm}

resourcestring
  SVarListText  = 'Vars - cписок переменных скрипта';

function EditVarCommand(Editor: TRScriptEditor; var Token, VarName, VarValue: string): Boolean;
begin
  with TFormScriptVar.Create(Application.MainForm) do
  begin
    try
      StartWait;
      try
        fEditor := Editor;
        LoadVariables(fEditor);
        CommandName := Token;
        NameEdit.Text := VarName;
        ValueEdit.Text := VarValue;
      finally
        StopWait;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Token := CommandName;
        VarName := NameEdit.Text;
        VarValue := ValueEdit.Text;
      end;
    finally
      Free;
    end;
  end;
end;

function TFormScriptVar.GetCommand: string;
begin
  if ComboBox.ItemIndex > -1
  then Result := Copy(ComboBox.Text, 1, 3)
  else Result := EmptyStr;
end;

procedure TFormScriptVar.SetCommand(const Value: string);
var
  i: Integer;
begin
  ComboBox.ItemIndex := -1;
  if Value <> EmptyStr then
  begin
    for i := 0 to ComboBox.Items.Count - 1 do
      if AnsiStartsText(Value, ComboBox.Items[i]) then
      begin
        ComboBox.ItemIndex := i;
        Break;
      end;
  end
  else begin
    ComboBox.Enabled := False;
    ComboBox.Style := csDropDown;
    ComboBox.Text := SVarListText;
  end;
end;

procedure TFormScriptVar.VarNameButtonClick(Sender: TObject);
var
  VarName: string;
begin
  if NameEdit.SelLength = 0 then
    NameEdit.SelectAll;
  VarName := NameEdit.SelText;
  if fEditor.SelectVariableName(False, VarName) then
    NameEdit.SelText := VarName;
end;

procedure TFormScriptVar.VarListBtnClick(Sender: TObject);
var
  VarName: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  VarName := ValueEdit.SelText;
  if fEditor.SelectVariableName(True, VarName) then
    ValueEdit.SelText := VarName;
end;

procedure TFormScriptVar.LoadVariables(Editor: TRScriptEditor);
var
  i: Integer;
begin
  NameEdit.Items.BeginUpdate;
  try
    NameEdit.Items.Clear;
    for i := 0 to Editor.Variables.Count - 1 do
      NameEdit.Items.Add(Editor.Variables.Names[i]);
  finally
    NameEdit.Items.EndUpdate;
  end;
end;

end.
