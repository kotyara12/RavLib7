unit LoginForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, AdoDb, Db, RDbLogin;

type
  TFormLogin = class(TDialogTemplate)
    Image: TImage;
    ProductName: TLabel;
    InternalName: TLabel;
    LineBevel: TBevel;
    NameEditLabel: TLabel;
    PwdEditLabel: TLabel;
    NameEdit: TEdit;
    PwdEdit: TEdit;
    UserPanel: TPanel;
    CapsLockText: TStaticText;
    StaticText: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure NameEditEnter(Sender: TObject);
    procedure PwdEditEnter(Sender: TObject);
    procedure ShowKeyboardState(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    fDb: TAdoConnection;
    fState: Byte;
    fUserData: TUserData;
    fUserPwdC: string;
    fCryptKey: string;
    fErrPwdCount: Byte;
    procedure LoadLastUserName(const AppName, DbName: string);
    procedure SaveLastUserName(const AppName, DbName: string);
    procedure ShowMessage(const Msg: string);
    function  FindUser(const sLogin: string): Boolean;
  protected
    procedure InitForm; override;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
  end;


function UserLogin(Db: TAdoConnection; const CryptKey: string; var User: TUserData): Boolean;

implementation

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RxVerInf, RVclUtils, RDialogs, RExHandlers, RMsgRu,
  RDbConst, RDbUtils, RCryptApiEx, Registry;

{$R *.dfm}

const
  SProgramName         = '%s, версия %s';
  SRegistryPath        = '\Software\RavSoft\%s';
  SRegistryKey         = 'LastUser_%s';

function UserLogin(Db: TAdoConnection; const CryptKey: string; var User: TUserData): Boolean;
begin
  Result := False;

  with TFormLogin.Create(Application) do
  begin
    try
      StartWait;
      try
        fDb := Db;
        fCryptKey := CryptKey;
        fUserData := User;
        fErrPwdCount := 0;
        LoadLastUserName(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), fDb.DefaultDatabase);
      finally
        StopWait;
      end;

      ShowModal;

      if fUserData.IsLogined then
      begin
        Result := True;
        User := fUserData;
        SaveLastUserName(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), fDb.DefaultDatabase);
      end;
    finally
      Free;
    end;
  end;
end;

{ == Инициализация формы ======================================================= }
procedure TFormLogin.InitForm;
var
  VerInfo: TVersionInfo;
begin
  inherited;

  EnterTab := False;

  VerInfo := AppVerInfo;
  try
    ProductName.Caption := VerInfo.ProductName;
    InternalName.Caption := Format(SProgramName, [VerInfo.InternalName, VerInfo.ProductVersion]);
  finally
    VerInfo.Free;
  end;
end;

{ == Установка стиля формы ===================================================== }
{$IFDEF STYLES}
procedure TFormLogin.SetStyle;
begin
  inherited;
  FontDataToFontNoSized(ApplicationStyle.DataForm.FormFont, ProductName.Font);
  FontDataToFontNoSized(ApplicationStyle.DataForm.FormFont, InternalName.Font);

  ProductName.Font.Size := ApplicationStyle.DataForm.FormFont.Size + 3;
  InternalName.Font.Size := ApplicationStyle.DataForm.FormFont.Size + 1;
end;
{$ENDIF}

{ == Сохранение имени последнего пользователя ================================== }
procedure TFormLogin.LoadLastUserName(const AppName, DbName: string);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    NameEdit.Text := Reg.ReadString(
      Format(SRegistryPath, [AppName]),
      Format(SRegistryKey, [DbName]),
      EmptyStr);
  finally
    Reg.Free;
  end;
end;

procedure TFormLogin.SaveLastUserName(const AppName, DbName: string);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.WriteString(Format(SRegistryPath, [AppName]),
      Format(SRegistryKey, [DbName]),
      NameEdit.Text);
  finally
    Reg.Free;
  end;
end;

{ == Отображение статуса клавиатуры ============================================ }
procedure TFormLogin.ShowKeyboardState(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Layout: array [0..KL_NAMELENGTH] of Char;
  LayoutStr: string;
  CaseState: Boolean;
begin
  // Определение языка
  LayoutStr := '??';
  GetKeyboardLayoutName(Layout);
  if Layout = '00000409' then LayoutStr := 'en';
  if Layout = '00000419' then LayoutStr := 'ru';
  // Отображение статуса клавиатуры
  CaseState := (Odd(GetKeyState(VK_CAPITAL)) and not (ssShift in Shift)) or
    (not Odd(GetKeyState(VK_CAPITAL)) and (ssShift in Shift));
  if CaseState then
  begin
    CapsLockText.Caption := AnsiUpperCase(LayoutStr);
    CapsLockText.Font.Color := clRed;
  end
  else begin
    CapsLockText.Caption := LayoutStr;
    CapsLockText.Font.Color := Font.Color;
  end;
end;

procedure TFormLogin.FormShow(Sender: TObject);
var
  Key: Word;
begin
  inherited;

  Key := 0;
  ShowKeyboardState(Sender, Key, []);
end;

{ == Отображение сообщения ===================================================== }
procedure TFormLogin.ShowMessage(const Msg: string);
begin
  StaticText.Caption := Msg;
  ShowInStatusBar(Msg);
  Application.ProcessMessages;
end;

{ == Переход к вводу имени пользователя ======================================== }
procedure TFormLogin.NameEditEnter(Sender: TObject);
begin
  fState := 1;
  PwdEdit.Enabled := False;
  OkBtn.Hint := SMsgFindUser;
  ShowMessage(SMsgEnterUsedId);
end;

{ == Переход к вводу пароля ==================================================== }
procedure TFormLogin.PwdEditEnter(Sender: TObject);
begin
  fState := 2;
  OkBtn.Hint := SMsgCheckPwd;
  ShowMessage(SMsgEnterPassword);
end;

{ == Поиск пользователя в базе данных ========================================== }
function TFormLogin.FindUser(const sLogin: string): Boolean;
var
  qryUserData: TAdoQuery;
begin
  Result := False;
  try
    StartWait;
    try
      qryUserData := OpenDynamicQuery(fDb, Format(sqlUserData, [sLogin]));
      try
        if DataSetIsNotEmpty(qryUserData) then
        begin
          Result := True;

          fUserData.UserId := qryUserData.Fields[0].AsInteger;
          fUserData.UserLogin := qryUserData.Fields[1].AsString;
          fUserData.UserName := qryUserData.Fields[2].AsString;
          fUserData.UserType := qryUserData.Fields[4].AsInteger;
          fUserData.IsDeleted := qryUserData.Fields[5].AsBoolean;
          fUserData.IsLogined := False;
          fUserPwdC := Trim(qryUserData.Fields[3].AsString);
          if fUserPwdC <> '' then
          begin
            fUserPwdC := DecryptPwd_Text_RsaShaRc2B64_Ex(fUserPwdC, fCryptKey);
            fUserData.NeedPwdChange := False;
          end
          else begin
            fUserPwdC := SPwdDefault;
            fUserData.NeedPwdChange := True;
          end;
        end;
      finally
        FreeDynamicQuery(qryUserData);
      end;
    finally
      StopWait;
    end;
  except
    on E: Exception do
    begin
      ShowMessage(SErrFindUser);
      HandleSqlExcept(E, nil, Format(sqlUserData, [sLogin]), SErrFindUser);
    end;
  end;
end;

{ == Выполнение регистрации ==================================================== }
procedure TFormLogin.OkBtnClick(Sender: TObject);
begin
  if fState = 1 then
  begin
    if NameEdit.Text <> EmptyStr then
    begin
      InitUserData(fUserData);
      if FindUser(NameEdit.Text)
      then begin
        UserPanel.Caption := Format(SFmtUserInfo, [fUserData.UserName, fErrPwdCount + 1, MaxPwdErrors]);

        if fUserData.IsDeleted then
        begin
          ShowMessage(Format(SErrUserDeleted, [fUserData.UserLogin]));
          ErrorBox(Format(SErrUserDeleted, [fUserData.UserLogin]));
          NameEdit.Clear;
          NameEdit.SetFocus;
        end
        else begin
          PwdEdit.Enabled := True;
          PwdEdit.SetFocus;
          Exit;
        end;
      end
      else begin
        ShowMessage(Format(SErrUserNotFound, [NameEdit.Text]));
        ErrorBox(Format(SErrUserNotFound, [NameEdit.Text]));
        NameEdit.Clear;
        NameEdit.SetFocus;
      end;
    end
    else begin
      ErrorBox(SErrUserNotEnter);
      NameEdit.SetFocus;
    end;
  end;

  if fState = 2 then
  begin
    if PwdEdit.Text = fUserPwdC then
    begin
      fUserData.IsLogined := True;
      Close;
    end
    else begin
      Inc(fErrPwdCount);
      if fErrPwdCount < MaxPwdErrors then
      begin
        UserPanel.Caption := Format(SFmtUserInfo, [fUserData.UserName, fErrPwdCount + 1, MaxPwdErrors]);
        ShowMessage(SErrBadPassword);
        ErrorBox(SErrBadPassword);
        PwdEdit.Clear;
        PwdEdit.SetFocus;
      end
      else begin
        UserPanel.Caption := Format(SFmtCanLogin, [fUserData.UserName]);
        ShowMessage(SErrBadPasswordBkl);
        ErrorBox(SErrBadPasswordBkl);
        Close;
      end;
    end;
  end;
end;

{ == Отмена регистрации ======================================================== }
procedure TFormLogin.CancelBtnClick(Sender: TObject);
begin
  fUserData.IsLogined := False;
  Close;
end;

end.
