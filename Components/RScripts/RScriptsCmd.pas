unit RScriptsCmd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  RScripts, RScrCnst, RScrCmds,
  ImgList;

type
  TFormScriptCmd = class(TDialogTemplate)
    CmdComboBox: TComboBox;
    CmdComboBoxLabel: TLabel;
    ParamsListBoxLabel: TLabel;
    ParamsList: TListView;
    FlagsListLabel: TLabel;
    FlagsList: TListView;
    NoteEditLabel: TLabel;
    NoteEdit: TEdit;
    ImageList: TImageList;
    DefaultFlagsCheckBox: TCheckBox;
    VarListBtn: TSpeedButton;
    DefaultNotesCheckBox: TCheckBox;
    EditParameter: TSpeedButton;
    ParamsMemo: TMemo;
    procedure CmdComboBoxLoad;
    procedure CmdComboBoxChange(Sender: TObject);
    procedure UpdateButtons(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure VarListBtnClick(Sender: TObject);
    procedure EditParameterClick(Sender: TObject);
    procedure ParamsListDblClick(Sender: TObject);
  private
    fEditor: TRScriptEditor;
    function  GetCmdType: TClass;
    procedure SetCmdType(const Value: TClass);
    function  GetParams: TRCmdParams;
    procedure SetParams(const Params: TRCmdParams);
    function  GetFlags: string;
    procedure SetFlags(const Flags: string);
    function  GetNotes: string;
    procedure SetNotes(const Notes: string);
    function  GetDefFlags: Boolean;
    procedure SetDefFlags(const Value: Boolean);
    function  GetDefNotes: Boolean;
    procedure SetDefNotes(const Value: Boolean);
  public
    property CommandType: TClass read GetCmdType write SetCmdType;
    property Parameters: TRCmdParams read GetParams write SetParams;
    property Attributes: string read GetFlags write SetFlags;
    property Description: string read GetNotes write SetNotes;
    property DefaultFlags: Boolean read GetDefFlags write SetDefFlags;
    property DefaultNotes: Boolean read GetDefNotes write SetDefNotes;
  end;

function EditExeCommand(Editor: TRScriptEditor; var CmdClass: TClass; var CmdData: TRCmdData): Boolean;

implementation

uses
  RVclUtils, RScriptsPrm;

{$R *.dfm}

function EditExeCommand(Editor: TRScriptEditor; var CmdClass: TClass; var CmdData: TRCmdData): Boolean;
begin
  with TFormScriptCmd.Create(Application) do
  begin
    try
      fEditor := Editor;
      CmdComboBoxLoad;
      CommandType := CmdClass;
      Parameters := CmdData.fParams;
      Attributes := CmdData.fFlags;
      Description := CmdData.fNotes;
      DefaultFlags := CmdData.fFlagsDefault;
      DefaultNotes := CmdData.fNotesDefault;
      Result := ShowModal = mrOk;
      if Result then
      begin
        CmdClass := CommandType;
        CmdData.fParams := Parameters;
        CmdData.fFlags := Attributes;
        CmdData.fNotes := Description;
        CmdData.fFlagsDefault := DefaultFlags;
        CmdData.fNotesDefault := DefaultNotes;
      end;
    finally
      Free;
    end;
  end;
end;

{ TFormScriptCmd }

procedure TFormScriptCmd.CmdComboBoxLoad;
var
  i: TRCmdType;
  Cmd: TRCmdExecuted;
begin
  CmdComboBox.Items.BeginUpdate;
  try
    CmdComboBox.Items.Clear;
    for i := Low(LRCommandsExe) to High(LRCommandsExe) do
    begin
      Cmd := LRCommandsExe[i].Create(fEditor);
      CmdComboBox.Items.AddObject(Cmd.GetCommandName
        + #32 + Cmd.GetCommandNote, Cmd);
    end;
  finally
    CmdComboBox.Items.EndUpdate;
  end;
end;

procedure TFormScriptCmd.CmdComboBoxChange(Sender: TObject);
var
  i: Integer;
  FlgList, FlgValues: string;
begin
  ParamsList.Items.BeginUpdate;
  FlagsList.Items.BeginUpdate;
  FlgValues := Attributes;
  try
    FlagsList.Items.Clear;
    if CmdComboBox.ItemIndex > intDisable then
    begin
      with TRCmdExecuted(CmdComboBox.Items.Objects[CmdComboBox.ItemIndex]) do
      begin
        // Изменяем список параметров операции
        ParamsList.Visible := GetParamsLimit > intDisable;
        ParamsMemo.Visible := not ParamsList.Visible;
        if ParamsList.Visible
        then ActiveControl := ParamsList
        else ActiveControl := ParamsMemo;
        if GetParamsLimit > intDisable then
        begin
          ParamsList.Items.BeginUpdate;
          try
            while ParamsList.Items.Count < GetParamsLimit do
              ParamsList.Items.Add;
            while ParamsList.Items.Count > GetParamsLimit do
              ParamsList.Items.Delete(ParamsList.Items.Count - 1);
          finally
            ParamsList.Items.EndUpdate;
          end;
        end;
        // Изменяем список атрибутов операции
        FlgList := GetCommandAttrList;
        for i := 1 to Length(FlgList) do
          with FlagsList.Items.Add do
          begin
            Subitems.Add(FlgList[i]);
            Subitems.Add(GetCommandAttrNote(FlgList[i]));
          end;
      end;
    end;
  finally
    Attributes := FlgValues;
    FlagsList.Items.EndUpdate;
    ParamsList.Items.EndUpdate;
  end;
end;

function TFormScriptCmd.GetCmdType: TClass;
begin
  if CmdComboBox.ItemIndex > intDisable then
    Result := CmdComboBox.Items.Objects[CmdComboBox.ItemIndex].ClassType
  else Result := nil;
end;

procedure TFormScriptCmd.SetCmdType(const Value: TClass);
var
  i: Integer;
begin
  for i := 0 to CmdComboBox.Items.Count - 1 do
    if CmdComboBox.Items.Objects[i].ClassType = Value then
    begin
      CmdComboBox.ItemIndex := i;
      CmdComboBoxChange(nil);
      Break;
    end;
end;

function TFormScriptCmd.GetParams: TRCmdParams;
var
  i: Integer;
begin
  if ParamsList.Visible then
  begin
    SetLength(Result, ParamsList.Items.Count);
    for i := 0 to ParamsList.Items.Count - 1 do
      Result[Low(Result) + i] := ParamsList.Items[i].Caption;
  end
  else begin
    SetLength(Result, ParamsMemo.Lines.Count);
    for i := 0 to ParamsMemo.Lines.Count - 1 do
      Result[Low(Result) + i] := ParamsMemo.Lines[i];
  end;
end;

procedure TFormScriptCmd.SetParams(const Params: TRCmdParams);
var
  i: Integer;
begin
  if ParamsList.Visible then
  begin
    ParamsList.Items.BeginUpdate;
    try
      for i := 0 to ParamsList.Items.Count - 1 do
        if Low(Params) + i <= High(Params) then
        begin
          ParamsList.Items[i].Caption := Params[Low(Params) + i];
          ParamsList.Items[i].ImageIndex := 0;
        end;
    finally
      ParamsList.Items.EndUpdate;
    end;
  end
  else begin
    ParamsMemo.Lines.BeginUpdate;
    try
      ParamsMemo.Lines.Clear;
      for i := Low(Params) to High(Params) do
        ParamsMemo.Lines.Add(Params[i]);
    finally
      ParamsMemo.Lines.EndUpdate;
    end;
  end;
end;

function TFormScriptCmd.GetFlags: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to FlagsList.Items.Count - 1 do
    if FlagsList.Items[i].Checked then
      Result := Result + FlagsList.Items[i].SubItems[0];
end;

procedure TFormScriptCmd.SetFlags(const Flags: string);
var
  i, j: Integer;
begin
  FlagsList.Items.BeginUpdate;
  try
    for i := 0 to FlagsList.Items.Count - 1 do
    begin
      FlagsList.Items[i].Checked := False;
      for j := 1 to Length(Flags) do
        if FlagsList.Items[i].SubItems[0] = Flags[j] then
        begin
          FlagsList.Items[i].Checked := True;
          Break;
        end;
    end;
  finally
    FlagsList.Items.EndUpdate;
  end;
end;

function TFormScriptCmd.GetNotes: string;
begin
  Result := NoteEdit.Text;
end;

procedure TFormScriptCmd.SetNotes(const Notes: string);
begin
  NoteEdit.Text := Notes;
end;

function TFormScriptCmd.GetDefFlags: Boolean;
begin
  Result := DefaultFlagsCheckBox.Checked;
end;

procedure TFormScriptCmd.SetDefFlags(const Value: Boolean);
begin
  DefaultFlagsCheckBox.Checked := Value;
  UpdateButtons(nil);
end;

function TFormScriptCmd.GetDefNotes: Boolean;
begin
  Result := DefaultNotesCheckBox.Checked;
end;

procedure TFormScriptCmd.SetDefNotes(const Value: Boolean);
begin
  DefaultNotesCheckBox.Checked := Value;
  UpdateButtons(nil);
end;

procedure TFormScriptCmd.FormActivate(Sender: TObject);
begin
  inherited;
  UpdateButtons(Sender);
end;

procedure TFormScriptCmd.UpdateButtons(Sender: TObject);
begin
  EditParameter.Enabled := ParamsList.Selected <> nil;
  NoteEdit.Enabled := not DefaultNotesCheckBox.Checked;
  VarListBtn.Enabled := not DefaultNotesCheckBox.Checked;
  FlagsList.Enabled := not DefaultFlagsCheckBox.Checked;
end;

procedure TFormScriptCmd.VarListBtnClick(Sender: TObject);
var
  VarName: string;
begin
  if NoteEdit.SelLength = 0 then
    NoteEdit.SelectAll;
  VarName := NoteEdit.SelText;
  if fEditor.SelectVariableName(True, VarName) then
    NoteEdit.SelText := VarName;
end;

procedure TFormScriptCmd.EditParameterClick(Sender: TObject);
var
  Value: string;
begin
  if ParamsList.Visible then
  begin
    Value := ParamsList.Selected.Caption;
    if EditCmdParameter(fEditor, Value) then
      ParamsList.Selected.Caption := Value;
  end
  else begin
    Value := ParamsMemo.Text;
    if EditCmdParameter(fEditor, Value) then
      ParamsMemo.Text := Value;
  end;
end;

procedure TFormScriptCmd.ParamsListDblClick(Sender: TObject);
begin
  if ParamsList.Selected <> nil then EditParameterClick(Sender);
end;

end.
