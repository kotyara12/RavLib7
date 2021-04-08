unit RScriptsCyc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RScripts;

type
  TFormScriptCyc = class(TDialogTemplate)
    ComboBoxLabel: TLabel;
    CmdComboBox: TComboBox;
    ValueEditLabel: TLabel;
    ListEdit: TEdit;
    DirectoryButton: TSpeedButton;
    FileButton: TSpeedButton;
    VarsButton: TSpeedButton;
    procedure DirectoryButtonClick(Sender: TObject);
    procedure FileButtonClick(Sender: TObject);
    procedure VarsButtonClick(Sender: TObject);
  private
    fEditor: TRScriptEditor;
    function  GetCmdType: TClass;
    procedure SetCmdType(const Value: TClass);
    function  GetListName: string;
    procedure SetListName(const Value: string);
    procedure CmdComboBoxLoad;
  public
    property CommandType: TClass read GetCmdType write SetCmdType;
    property ListName: string read GetListName write SetListName;
  end;

function EditCycCommand(Editor: TRScriptEditor; var CmdClass: TClass; var ListText: string): Boolean;

implementation

{$R *.dfm}

uses
  RVclUtils, RScrCnst, RScrFncs, FileCtrl, StrUtils;

function EditCycCommand(Editor: TRScriptEditor; var CmdClass: TClass; var ListText: string): Boolean;
begin
  with TFormScriptCyc.Create(Application) do
  begin
    try
      fEditor := Editor;
      CmdComboBoxLoad;
      CommandType := CmdClass;
      ListName := ListText;
      Result := ShowModal = mrOk;
      if Result then
      begin
        CmdClass := CommandType;
        ListText := ListName;
      end;
    finally
      Free;
    end;
  end;
end;

{ TFormScriptCyc }

procedure TFormScriptCyc.CmdComboBoxLoad;
var
  i: TRCmdType;
  Cmd: TRCmdCycle;
begin
  CmdComboBox.Items.BeginUpdate;
  try
    CmdComboBox.Items.Clear;
    for i := Low(LRCommandsCyc) to High(LRCommandsCyc) do
    begin
      Cmd := LRCommandsCyc[i].Create(fEditor);
      CmdComboBox.Items.AddObject(Cmd.GetCommandName
        + #32 + Cmd.GetCommandNote, Cmd);
    end;
  finally
    CmdComboBox.Items.EndUpdate;
  end;
end;

function TFormScriptCyc.GetCmdType: TClass;
begin
  if CmdComboBox.ItemIndex > intDisable then
    Result := CmdComboBox.Items.Objects[CmdComboBox.ItemIndex].ClassType
  else Result := nil;
end;

procedure TFormScriptCyc.SetCmdType(const Value: TClass);
var
  i: Integer;
begin
  for i := 0 to CmdComboBox.Items.Count - 1 do
    if CmdComboBox.Items.Objects[i].ClassType = Value then
    begin
      CmdComboBox.ItemIndex := i;
      Break;
    end;
end;

function TFormScriptCyc.GetListName: string;
begin
  Result := ListEdit.Text;
end;

procedure TFormScriptCyc.SetListName(const Value: string);
begin
  ListEdit.Text := Value;
end;

procedure TFormScriptCyc.DirectoryButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ListEdit.SelLength = 0 then
    ListEdit.SelectAll;
  SelValue := ListEdit.Text;
  if SelectDirectory(EmptyStr, EmptyStr, SelValue) then
    ListEdit.Text := ExpandUncFileName(SelValue);
end;

procedure TFormScriptCyc.FileButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ListEdit.SelLength = 0 then
    ListEdit.SelectAll;
  SelValue := ListEdit.Text;
  if PromptForFileName(SelValue) then
    ListEdit.Text := ExpandUncFileName(SelValue);
end;

procedure TFormScriptCyc.VarsButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ListEdit.SelLength = 0 then
    ListEdit.SelectAll;
  SelValue := ListEdit.SelText;
  if fEditor.SelectVariableName(True, SelValue) then
      ListEdit.SelText := SelValue;
end;

end.
