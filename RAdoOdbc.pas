unit RAdoOdbc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TProvider  = (prMsSql, prOdbc, prMsJet);

  RAdoConnectParams = record
    Provider: TProvider;
    Data_Source: string;
    User_ID: string;
    Password: string;
    Initial_Catalog: string;
  end;

type
  TFormAdoOdbc = class(TDialogTemplate)
    ProviderComboBox: TComboBox;
    ProviderComboBoxLabel: TLabel;
    ParamsGroupBox: TGroupBox;
    SourceEdit: TEdit;
    SourceEditLabel: TLabel;
    DbEdit: TEdit;
    DbEditLabel: TLabel;
    UserEdit: TEdit;
    UserEditLabel: TLabel;
    PwdEdit: TEdit;
    PwdEditLabel: TLabel;
    OpenButton: TSpeedButton;
    OpenDialog: TOpenDialog;
    procedure ProviderComboBoxChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  protected
    procedure InitForm; override;
  public
    procedure SetProvider(const APrv: TProvider);
    procedure GetProvider(var APrv: TProvider);
  end;

{ == Чтение и генерация строки подключения ===================================== }
function  GetConnectionString(const FileName: string): string;
{ == Изменение параметров подключения к базе данных ============================ }
function  ChangeConnectionString(const FileName: string): Boolean;
{ == Чтение настроек из INI файла ============================================== }
function  ReadConnectParams(const FileName: string): RAdoConnectParams;
{ == Запись настроек в INI файл ================================================ }
procedure SaveConnectParams(const FileName: string; const Params: RAdoConnectParams);
{ == Проверка заполнения параметров соединения ================================= }
function  CheckConnectParams(const Params: RAdoConnectParams): Boolean;
{ == Генерация строки соединения =============================================== }
function  GenerateConnectionString(const Params: RAdoConnectParams): string;
{ == Выбор параметров соединения с базой данных ================================ }
function  SelectConnectParams(var Params: RAdoConnectParams): Boolean;

implementation

{$R *.dfm}

uses
  IniFiles, RCrypt;

const
  ProvSysNames: array [TProvider] of string = (
    'SQLOLEDB.1',
    'MSDASQL.1',
    'Microsoft.Jet.OLEDB.4.0');
  ProvExtNames: array [TProvider] of string = (
    'Microsoft OLE DB Provider for SQL Server (SQLOLEDB)',
    'Microsoft OLE DB Provider for ODBC Drivers (MSDASQL)',
    'Microsoft OLE DB Provider Jet 4.0 (MSACCESS)');

  csProvider         = 'Provider=%s';
  csData_Source      = 'Data Source=%s';
  csUser_ID          = 'User ID=%s';
  csPassword         = 'Password=%s';
  csInitial_Catalog  = 'Initial Catalog=%s';
  csPSIFalse         = 'Persist Security Info=False';

  iniDatabase        = 'DATABASE';
  iniProvider        = 'Provider';
  iniData_Source     = 'Data_Source';
  iniUser_ID         = 'User_ID';
  iniPassword        = 'Password';
  iniInitial_Catalog = 'Initial_Catalog';

  StartKey = 567;
  MultKey  = 35469;
  AddKey   = 74351;

{ == Чтение и генерация строки подключения ===================================== }
function GetConnectionString(const FileName: string): string;
var
  Params: RAdoConnectParams;
begin
  Result := EmptyStr;
  Params := ReadConnectParams(FileName);
  if CheckConnectParams(Params) or SelectConnectParams(Params) then
  begin
    Result := GenerateConnectionString(Params);
    SaveConnectParams(FileName, Params);
  end;
end;

{ == Изменение параметров подключения к базе данных ============================ }
function ChangeConnectionString(const FileName: string): Boolean;
var
  Params: RAdoConnectParams;
begin
  Params := ReadConnectParams(FileName);
  Result := SelectConnectParams(Params);
  if Result then SaveConnectParams(FileName, Params);
end;

{ == Чтение настроек из INI файла ============================================== }
function ReadConnectParams(const FileName: string): RAdoConnectParams;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result.Provider := TProvider(Ini.ReadInteger(iniDatabase, iniProvider, Integer(prOdbc)));
    Result.Data_Source := Ini.ReadString(iniDatabase, iniData_Source, EmptyStr);
    Result.Initial_Catalog := Ini.ReadString(iniDatabase, iniInitial_Catalog, EmptyStr);
    Result.User_ID := Ini.ReadString(iniDatabase, iniUser_ID, EmptyStr);
    Result.Password := Decrypt(HexStrToStr(Ini.ReadString(iniDatabase, iniPassword, EmptyStr)),
      StartKey, MultKey, AddKey);
  finally
    Ini.Free;
  end;
end;

{ == Запись настроек в INI файл ================================================ }
procedure SaveConnectParams(const FileName: string; const Params: RAdoConnectParams);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger(iniDatabase, iniProvider, Integer(Params.Provider));
    Ini.WriteString(iniDatabase, iniData_Source, Params.Data_Source);
    Ini.WriteString(iniDatabase, iniInitial_Catalog, Params.Initial_Catalog);
    Ini.WriteString(iniDatabase, iniUser_ID, Params.User_ID);
    Ini.WriteString(iniDatabase, iniPassword, StrToHexStr(Encrypt(Params.Password,
      StartKey, MultKey, AddKey)));
  finally
    Ini.Free;
  end;
end;

{ == Проверка заполнения параметров соединения ================================= }
function CheckConnectParams(const Params: RAdoConnectParams): Boolean;
begin
  Result := (Params.Data_Source <> EmptyStr)
        and (Params.Provider = prMsJet)
        or ((Params.Initial_Catalog <> EmptyStr)
        and (Params.User_ID <> EmptyStr));
end;

{ == Генерация строки соединения =============================================== }
function GenerateConnectionString(const Params: RAdoConnectParams): string;
begin
  case Params.Provider of
    prMsSql, prOdbc:
      Result := Format(csProvider, [ProvSysNames[Params.Provider]]) + ';' +
            Format(csData_Source, [Params.Data_Source]) + ';' +
            Format(csUser_ID, [Params.User_ID]) + ';' +
            Format(csPassword, [Params.Password]) + ';' +
            Format(csInitial_Catalog, [Params.Initial_Catalog]);
    prMsJet:
      Result := Format(csProvider, [ProvSysNames[Params.Provider]]) + ';' +
            Format(csData_Source, [Params.Data_Source]) + ';' + csPSIFalse;
    else Result := EmptyStr;
  end;
end;

{ == Выбор параметров соединения с базой данных ================================ }
function SelectConnectParams(var Params: RAdoConnectParams): Boolean;
begin
  with TFormAdoOdbc.Create(Application) do
  begin
    try
      SetProvider(Params.Provider);
      SourceEdit.Text := Params.Data_Source;
      DbEdit.Text := Params.Initial_Catalog;
      UserEdit.Text := Params.User_ID;
      PwdEdit.Text := Params.Password;
      Result := ShowModal = mrOk;
      if Result then begin
        GetProvider(Params.Provider);
        Params.Data_Source := SourceEdit.Text;
        Params.Initial_Catalog := DbEdit.Text;
        Params.User_ID := UserEdit.Text;
        Params.Password := PwdEdit.Text;
      end;
    finally
      Free;
    end;
  end;
end;

{ ============================================================================== }
{ TFormAdoOdbc                                                                   }
{ ============================================================================== }

{ == Инициализация формы ======================================================= }
procedure TFormAdoOdbc.InitForm;
var
  i: TProvider;
begin
  ProviderComboBox.Items.BeginUpdate;
  try
    for i := Low(TProvider) to High(TProvider) do
      ProviderComboBox.Items.Add(ProvExtNames[i]);
  finally
    ProviderComboBox.Items.EndUpdate;
  end;
end;

{ == Установка и считывание значений из формы ================================== }
procedure TFormAdoOdbc.SetProvider(const APrv: TProvider);
begin
  ProviderComboBox.ItemIndex := Integer(APrv);
end;

procedure TFormAdoOdbc.GetProvider(var APrv: TProvider);
begin
  APrv := TProvider(ProviderComboBox.ItemIndex);
end;

procedure TFormAdoOdbc.ProviderComboBoxChange(Sender: TObject);
begin
  OpenButton.Visible := ProviderComboBox.ItemIndex in [2];
  if OpenButton.Visible then SourceEdit.Width := 221 else SourceEdit.Width := 245;
  DbEdit.Enabled := ProviderComboBox.ItemIndex in [0, 1];
  UserEdit.Enabled := ProviderComboBox.ItemIndex in [0, 1];
  PwdEdit.Enabled := ProviderComboBox.ItemIndex in [0, 1];
end;

procedure TFormAdoOdbc.OpenButtonClick(Sender: TObject);
begin
  OpenDialog.FileName := SourceEdit.Text;
  if OpenDialog.Execute then SourceEdit.Text := OpenDialog.FileName;
end;

end.
