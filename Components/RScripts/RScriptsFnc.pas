unit RScriptsFnc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RScripts, RScrCnst, RScrFncs;

type
  TFormScriptFnc = class(TDialogTemplate)
    OperGroupBox: TGroupBox;
    NotCheckBox: TCheckBox;
    NoneRadioButton: TRadioButton;
    AndRadioButton: TRadioButton;
    OrRadioButton: TRadioButton;
    FncComboBoxLabel: TLabel;
    FncComboBox: TComboBox;
    ValueEditLabel: TLabel;
    ValueEdit: TEdit;
    DirectoryButton: TSpeedButton;
    FileButton: TSpeedButton;
    VarsButton: TSpeedButton;
    procedure DirectoryButtonClick(Sender: TObject);
    procedure FileButtonClick(Sender: TObject);
    procedure VarsButtonClick(Sender: TObject);
    procedure FncComboBoxLoad;
    procedure FncComboBoxChange(Sender: TObject);
  private
    fEditor: TRScriptEditor;
    function  GetOperator: TRFncOperator;
    procedure SetOperator(const Value: TRFncOperator);
    function  GetInversed: Boolean;
    procedure SetInversed(const Value: Boolean);
    function  GetFncType: TClass;
    procedure SetFncType(const Value: TClass);
    function  GetParameters: TRCmdParams;
    procedure SetParameters(const Value: TRCmdParams);
    procedure SetFirstNode(const Value: Boolean);
  public
    property FncFirstNode: Boolean write SetFirstNode;
    property FncType: TClass read GetFncType write SetFncType;
    property FncOperator: TRFncOperator read GetOperator write SetOperator;
    property FncInversed: Boolean read GetInversed write SetInversed;
    property FncParameters: TRCmdParams read GetParameters write SetParameters;
  end;

function EditFncCommand(Editor: TRScriptEditor; const FirstNode: Boolean; var FncClass: TClass;
  var Inversed: Boolean; var Operator: TRFncOperator; var Parameters: TRCmdParams): Boolean;

implementation

uses
  FileCtrl, RVclUtils;

{$R *.dfm}

function EditFncCommand(Editor: TRScriptEditor; const FirstNode: Boolean; var FncClass: TClass;
  var Inversed: Boolean; var Operator: TRFncOperator; var Parameters: TRCmdParams): Boolean;
begin
  with TFormScriptFnc.Create(Application.MainForm) do
  begin
    try
      fEditor := Editor;
      FncComboBoxLoad;
      FncFirstNode := FirstNode;
      FncType := FncClass;
      FncOperator := Operator;
      FncInversed := Inversed;
      FncParameters := Parameters;
      Result := ShowModal = mrOk;
      if Result then
      begin
        FncClass := FncType;
        Operator := FncOperator;
        Inversed := FncInversed;
        Parameters := FncParameters;
      end;
    finally
      Free;
    end;
  end;
end;

{ TFormScriptFnc }

procedure TFormScriptFnc.FncComboBoxLoad;
var
  i: TRFncType;
  Fnc: TRFncExecuted;
begin
  FncComboBox.Items.BeginUpdate;
  try
    FncComboBox.Items.Clear;
    for i := Low(LRFunctions) to High(LRFunctions) do
    begin
      Fnc := LRFunctions[i].Create(fEditor, False, coNone);
      FncComboBox.Items.AddObject(Fnc.GetCommandName
        + #32 + Fnc.GetCommandNote, Fnc);
    end;
  finally
    FncComboBox.Items.EndUpdate;
  end;
end;

procedure TFormScriptFnc.SetFirstNode(const Value: Boolean);
begin
  if Value then
  begin
    NoneRadioButton.Checked := True;
    AndRadioButton.Enabled := False;
    OrRadioButton.Enabled := False;
  end
  else begin
    NoneRadioButton.Enabled := False;
    if NoneRadioButton.Checked then AndRadioButton.Checked := True;
  end;
end;

function TFormScriptFnc.GetOperator: TRFncOperator;
begin
  Result := coNone;
  if AndRadioButton.Checked then Result := coAnd;
  if OrRadioButton.Checked then Result := coOr;
end;

procedure TFormScriptFnc.SetOperator(const Value: TRFncOperator);
begin
  case Value of
    coAnd: AndRadioButton.Checked := True;
    coOr: OrRadioButton.Checked := True;
    else NoneRadioButton.Checked := True;
  end;
end;

function TFormScriptFnc.GetInversed: Boolean;
begin
  Result := NotCheckBox.Checked;
end;

procedure TFormScriptFnc.SetInversed(const Value: Boolean);
begin
  NotCheckBox.Checked := Value;
end;

function TFormScriptFnc.GetFncType: TClass;
begin
  if FncComboBox.ItemIndex > intDisable then
    Result := FncComboBox.Items.Objects[FncComboBox.ItemIndex].ClassType
  else Result := nil;
end;

procedure TFormScriptFnc.SetFncType(const Value: TClass);
var
  i: Integer;
begin
  for i := 0 to FncComboBox.Items.Count - 1 do
    if FncComboBox.Items.Objects[i].ClassType = Value then
    begin
      FncComboBox.ItemIndex := i;
      FncComboBoxChange(nil);
      Break;
    end;
end;

procedure TFormScriptFnc.FncComboBoxChange(Sender: TObject);
begin
  if FncComboBox.ItemIndex > intDisable
  then ValueEdit.Enabled := LRFunctions[TRFncType(FncComboBox.ItemIndex)].GetParamsLimit > 0
  else ValueEdit.Enabled := False;
  DirectoryButton.Enabled := ValueEdit.Enabled;
  FileButton.Enabled := ValueEdit.Enabled;
  VarsButton.Enabled := ValueEdit.Enabled;
end;

function TFormScriptFnc.GetParameters: TRCmdParams;
begin
  if ValueEdit.Enabled then
  begin
    SetLength(Result, 1);
    Result[Low(Result)] := ValueEdit.Text;
  end
  else SetLength(Result, 0);
end;

procedure TFormScriptFnc.SetParameters(const Value: TRCmdParams);
begin
  if Length(Value) > 0
  then ValueEdit.Text := Value[Low(Value)]
  else ValueEdit.Text := EmptyStr;
end;

procedure TFormScriptFnc.DirectoryButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  SelValue := ValueEdit.Text;
  if SelectDirectory(EmptyStr, EmptyStr, SelValue) then
    ValueEdit.Text := ExpandUncFileName(SelValue);
end;

procedure TFormScriptFnc.FileButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  SelValue := ValueEdit.Text;
  if PromptForFileName(SelValue) then
    ValueEdit.Text := ExpandUncFileName(SelValue);
end;

procedure TFormScriptFnc.VarsButtonClick(Sender: TObject);
var
  SelValue: string;
begin
  if ValueEdit.SelLength = 0 then
    ValueEdit.SelectAll;
  SelValue := ValueEdit.SelText;
  if fEditor.SelectVariableName(
    not ((FncType = TRFncVarIs) or (FncType = TRFncVarExists)),
    SelValue) then
      ValueEdit.SelText := SelValue;
end;

end.
