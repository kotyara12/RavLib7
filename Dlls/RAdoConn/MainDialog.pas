unit MainDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, Spin, Grids,
  ValEdit, RAdoUtils, Mask, ToolEdit;

type

  TFormParameters = class(TDialogTemplate)
    PageControl: TPageControl;
    MainTabSheet: TTabSheet;
    OptionsTabSheet: TTabSheet;
    OptionsLabel: TLabel;
    DateFormatEditLabel: TLabel;
    DateFormatEdit: TEdit;
    CaseCheckBox: TCheckBox;
    ResqueTabSheet: TTabSheet;
    ResqueLabel: TLabel;
    DefaultOptionsButton: TButton;
    ResqueCheckBox: TCheckBox;
    SpinEditLabel: TLabel;
    SpinEdit: TSpinEdit;
    ConnLabel: TLabel;
    ValueListEditor: TValueListEditor;
    EditButton: TButton;
    CurrentDirRadioButton: TRadioButton;
    SelectedDirRadioButton: TRadioButton;
    ResqueDirEdit: TDirectoryEdit;
    IntervalEdit: TSpinEdit;
    IntervalEditLabel: TLabel;
    lblWatchDogTime: TLabel;
    WatchDogTime: TSpinEdit;
    procedure MainTabSheetShow(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure DefaultOptionsButtonClick(Sender: TObject);
    procedure ResqueCheckBoxClick(Sender: TObject);
  private
    DefParams: RAdoDbParameters;
    Password: string;
    function  GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
    function  GetDbParameters: RAdoDbParameters;
    procedure SetDbParameters(const Value: RAdoDbParameters);
    function  GetDbResqueCopy: RAdoDbResqueCopy;
    procedure SetDbResqueCopy(const Value: RAdoDbResqueCopy);
  public
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property Parameters: RAdoDbParameters read GetDbParameters write SetDbParameters;
    property ResqueCopy: RAdoDbResqueCopy read GetDbResqueCopy write SetDbResqueCopy;
  end;

function ChangeDbParameters(const FileName, DefConnString: string; const DefExtParams: RAdoDbParameters): Boolean;

implementation

{$R *.dfm}

uses
  RVclUtils, IniFiles, AdoDb, RxStrUtils, RDialogs, RMsgRu;

function ChangeDbParameters(const FileName, DefConnString: string; const DefExtParams: RAdoDbParameters): Boolean;
begin
  with TFormParameters.Create(Application) do
  begin
    try
      StartWait;
      try
        DefParams := DefExtParams;
        SetConnectionString(LoadConnectionString(FileName, DefConnString));
        SetDbParameters(LoadConnectionParams(FileName, DefExtParams));
        SetDbResqueCopy(LoadConnectionRqCopy(FileName));
      finally
        StopWait;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        StartWait;
        try
          SaveConnectionString(FileName, GetConnectionString);
          SaveConnectionParams(FileName, GetDbParameters);
          SaveConnectionRqCopy(FileName, GetDbResqueCopy);
        finally
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TFormParameters.GetConnectionString: string;
var
  i: Integer;
begin
  StartWait;
  try
    Result := EmptyStr;
    for i := 0 to ValueListEditor.Strings.Count - 1 do
      if IsPassword(ExtractWord(1, ValueListEditor.Strings[i], [DelimPart]))
      then Result := Result + DelimChar
        + ExtractWord(1, ValueListEditor.Strings[i], [DelimPart])
        + DelimPart + Password
      else Result := Result + DelimChar + ValueListEditor.Strings[i];
    if Result <> EmptyStr then Delete(Result, 1, 1);
  finally
    StopWait;
  end;
end;

procedure TFormParameters.SetConnectionString(const Value: string);
var
  i: Integer;
begin
  StartWait;
  ValueListEditor.Strings.BeginUpdate;
  try
    Password := EmptyStr;
    ValueListEditor.Strings.Clear;
    for i := 1 to WordCount(Value, [DelimChar]) do
      if IsPassword(ExtractWord(i, Value, [DelimChar])) then
      begin
        Password := Trim(ExtractWord(2, ExtractWord(i, Value, [DelimChar]), [DelimPart]));
        ValueListEditor.Strings.Add(ExtractWord(1, ExtractWord(i, Value, [DelimChar]), [DelimPart])
          + DelimPart + PasswordMask);
      end
      else ValueListEditor.Strings.Add(ExtractWord(i, Value, [DelimChar]));
    ResqueTabSheet.TabVisible := IsFileDataSource(Value);
  finally
    ValueListEditor.Strings.EndUpdate;
    ValueListEditor.Refresh;
    StopWait;
  end;
end;

function TFormParameters.GetDbParameters: RAdoDbParameters;
begin
  Result.DateFormat := DateFormatEdit.Text;
  Result.CaseEnabled := CaseCheckBox.Checked;
  Result.WatchDogTime := WatchDogTime.Value;
end;

procedure TFormParameters.SetDbParameters(const Value: RAdoDbParameters);
begin
  if Trim(Value.DateFormat) = EmptyStr
  then DefaultOptionsButtonClick(nil)
  else DateFormatEdit.Text := Value.DateFormat;
  CaseCheckBox.Checked := Value.CaseEnabled;
  WatchDogTime.Value := Value.WatchDogTime;
end;

function TFormParameters.GetDbResqueCopy: RAdoDbResqueCopy;
begin
  Result.ResqueEnabled := ResqueCheckBox.Checked;
  Result.ResqueMaxCopies := SpinEdit.Value;
  Result.ResqueInterval := IntervalEdit.Value;
  Result.UseCurrentDir := CurrentDirRadioButton.Checked;
  Result.UserResqueDir := ResqueDirEdit.Text;
end;

procedure TFormParameters.SetDbResqueCopy(const Value: RAdoDbResqueCopy);
begin
  ResqueCheckBox.Checked := Value.ResqueEnabled;
  SpinEdit.Value := Value.ResqueMaxCopies;
  SpinEdit.Enabled := ResqueCheckBox.Checked;
  IntervalEdit.Value := Value.ResqueInterval;
  IntervalEdit.Enabled := ResqueCheckBox.Checked;
  CurrentDirRadioButton.Checked := Value.UseCurrentDir;
  CurrentDirRadioButton.Enabled := ResqueCheckBox.Checked;
  SelectedDirRadioButton.Checked := not Value.UseCurrentDir;
  SelectedDirRadioButton.Enabled := ResqueCheckBox.Checked;
  ResqueDirEdit.Text := Value.UserResqueDir;
  ResqueDirEdit.Enabled := SelectedDirRadioButton.Checked and ResqueCheckBox.Checked;
end;

procedure TFormParameters.MainTabSheetShow(Sender: TObject);
begin
  EditButton.SetFocus;
end;

procedure TFormParameters.EditButtonClick(Sender: TObject);
begin
  SetConnectionString(PromptDataSource(Handle, GetConnectionString));
end;

procedure TFormParameters.DefaultOptionsButtonClick(Sender: TObject);
begin
  if (Sender = nil) or (QueryBoxStdYN(SQuerySetDefault) = ID_YES) then
  begin
    DateFormatEdit.Text := DefParams.DateFormat;
    CaseCheckBox.Checked := DefParams.CaseEnabled;
    WatchDogTime.Value := DefParams.WatchDogTime;
  end;
end;

procedure TFormParameters.ResqueCheckBoxClick(Sender: TObject);
begin
  SpinEdit.Enabled := ResqueCheckBox.Checked;
  IntervalEdit.Enabled := ResqueCheckBox.Checked;
  CurrentDirRadioButton.Enabled := ResqueCheckBox.Checked;
  SelectedDirRadioButton.Enabled := ResqueCheckBox.Checked;
  ResqueDirEdit.Enabled := SelectedDirRadioButton.Checked and ResqueCheckBox.Checked;
end;

end.
