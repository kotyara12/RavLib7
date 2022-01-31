unit LoginRssForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RUserRights, AdoDb;

type
  TFormLogin = class(TDialogTemplate)
    Image: TImage;
    ProductName: TLabel;
    ProgramName: TLabel;
    LineBevel: TBevel;
    NameEditLabel: TLabel;
    NameEdit: TEdit;
    PwdEdit: TEdit;
    PwdEditLabel: TLabel;
    UserPanel: TPanel;
    CapsLockText: TStaticText;
    StaticText: TStaticText;
    procedure ShowKeyboardState(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure NameEditEnter(Sender: TObject);
    procedure NameEditExit(Sender: TObject);
    procedure PwdEditEnter(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    FProgramName: string;
    FState: Byte;
    FUserPwd: string;
    FErrPwdCount: Byte;
    procedure ShowMessage(const Msg: string);
    function  FindUser(const Name: string): Boolean;
    function  CheckArmRights: Boolean;
    procedure StorePasswordErrors;
    procedure LoadLastUserName(const Name: string; const ArmId: Integer);
    procedure SaveLastUserName(const Name: string; const ArmId: Integer);
  protected
    procedure InitForm; override;
  public
    FConn: TAdoConnection;
    FUser: TUserRights;
    FAppTag: Integer;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
  end;

{ == ����������� ������������ � ������� ======================================== }
procedure LoginUserRss(Db: TAdoConnection; const AppTag: Integer; var User: TUserRights);

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RxVerInf, RVclUtils, RDialogs, RRssConst, RRssBase, RExHandlers,
  RDbConst, RDbUtils, RDbLog, RCryptApiEx, ChPwdRssForm, Registry;

const
  SProgramName         = '%s, ������ %s';
  SRegistryPath        = '\Software\RavSoft\RLoginDll';
  SRegistryKey         = 'LastUser_%s_%d';

{ == ����������� ������������ � ������� ======================================== }
procedure LoginUserRss(Db: TAdoConnection; const AppTag: Integer; var User: TUserRights);
begin
  with TFormLogin.Create(Application) do
  begin
    try
      LoadLastUserName(Db.DefaultDatabase, Db.Tag);
      FConn := Db;
      FAppTag := AppTag;
      ShowModal;
      User := FUser;
      if User.Registration then
        SaveLastUserName(Db.DefaultDatabase, Db.Tag);
      if User.Registration and User.PwdChange then
      begin
        InfoBox(SMsgChangePassword);
        ChangePasswordRss(Db, User, False);
      end;
    finally
      Free;
    end;
  end;
end;

{ == ������������� ����� ======================================================= }
procedure TFormLogin.InitForm;
var
  VerInfo: TVersionInfo;
begin
  inherited;
  EnterTab := False;
  VerInfo := AppVerInfo;
  try
    FProgramName := VerInfo.InternalName;
    ProductName.Caption := VerInfo.ProductName;
    ProgramName.Caption := Format(SProgramName, [FProgramName, VerInfo.ProductVersion]);
  finally
    VerInfo.Free;
  end;
end;

{ == ��������� ����� ����� ===================================================== }
{$IFDEF STYLES}
procedure TFormLogin.SetStyle;
begin
  inherited;
  FontDataToFontNoSized(ApplicationStyle.DataForm.FormFont, ProductName.Font);
  FontDataToFontNoSized(ApplicationStyle.DataForm.FormFont, ProgramName.Font);

  ProductName.Font.Size := ApplicationStyle.DataForm.FormFont.Size + 3;
  ProgramName.Font.Size := ApplicationStyle.DataForm.FormFont.Size + 1;
end;
{$ENDIF}

{ == ���������� ����� ���������� ������������ ================================== }
procedure TFormLogin.LoadLastUserName(const Name: string; const ArmId: Integer);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    NameEdit.Text := Reg.ReadString(SRegistryPath,
      Format(SRegistryKey, [Name, ArmId]), EmptyStr);
  finally
    Reg.Free;
  end;
end;

procedure TFormLogin.SaveLastUserName(const Name: string; const ArmId: Integer);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.WriteString(SRegistryPath,
      Format(SRegistryKey, [Name, ArmId]), NameEdit.Text);
  finally
    Reg.Free;
  end;
end;

{ == ����������� ������� ���������� ============================================ }
procedure TFormLogin.ShowKeyboardState(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Layout: array [0..KL_NAMELENGTH] of Char;
  LayoutStr: string;
  CaseState: Boolean;
begin
  // ����������� �����
  GetKeyboardLayoutName(Layout);
  if Layout = '00000409' then
  begin
    LayoutStr := 'eng';
    CapsLockText.Font.Style := [];
  end
  else begin
    LayoutStr := '���';
    CapsLockText.Font.Style := [fsBold];
  end;
  // ����������� ������� ����������
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

{ == ����������� ��������� ===================================================== }
procedure TFormLogin.ShowMessage(const Msg: string);
begin
  StaticText.Caption := Msg;
  ShowInStatusBar(Msg);
  Application.ProcessMessages;
end;

{ == ������� � ����� ����� ������������ ======================================== }
procedure TFormLogin.NameEditEnter(Sender: TObject);
begin
  FState := 1;
  // PwdEdit.Enabled := False;
  OkBtn.Hint := SMsgFindUser;
  ShowMessage(SMsgEnterUsedId);
end;

procedure TFormLogin.NameEditExit(Sender: TObject);
begin
  OkBtnClick(Sender);
end;

{ == ������� � ����� ������ ==================================================== }
procedure TFormLogin.PwdEditEnter(Sender: TObject);
begin
  FState := 2;
  OkBtn.Hint := SMsgCheckPwd;
  ShowMessage(SMsgEnterPassword);
end;

{ == ����� ������������ � ���� ������ ========================================== }
function TFormLogin.FindUser(const Name: string): Boolean;
var
  qryUser: TADOQuery;

  function CheckDeleted: Boolean;
  begin
    Result := not qryUser.FieldByName(fnDELETED).AsBoolean;
    if qryUser.FieldByName(fnDELETED).AsBoolean then
    begin
      UserPanel.Caption := Format(SFmtCanLogin, [FUser.FullName]);
      ShowMessage(Format(SErrUserDeleted, [Name]));
      AddToDbLog(tagErrRegistration, Format(SLogDelUserRegistration,
        [FProgramName, FUser.UserName]));
      ErrorBox(Format(SErrUserDeleted, [Name]));
    end;
  end;

  function CheckBlocked: Boolean;
  begin
    Result := not (qryUser.FieldByName(fnBLOCKED).AsBoolean or (FErrPwdCount >= MaxPwdErrors));
    if not Result then
    begin
      UserPanel.Caption := Format(SFmtCanLogin, [FUser.FullName]);
      ShowMessage(Format(SErrUserBlocked, [Name]));
      AddToDbLog(tagErrRegistration, Format(SLogBlkUserRegistration,
         [FProgramName, FUser.UserName]));
      ErrorBox(Format(SErrUserBlocked, [Name]));
    end
  end;

begin
  Result := False;
  StartWait;
  qryUser := nil;
  try
    ShowMessage(SMsgFindUser);
    try
      qryUser := OpenDynamicQuery(FConn, Format(sqlGetUserData, [Name]));
      if DataSetIsNotEmpty(qryUser) then
      begin
        FUser.UserId := qryUser.FieldByName(fnID).AsInteger;
        FUser.Registration := False;
        FUser.UserName := qryUser.FieldByName(fnNAME).AsString;
        FUser.FullName := qryUser.FieldByName(fnFULLNAME).AsString;
        FUserPwd := DecryptPwd_Text_RsaShaRc2B64_Ex(Trim(qryUser.FieldByName(fnPASSWORD).AsString), PwdKey);
        if FUserPwd = EmptyStr then FUserPwd := SDefaultPwd;
        FUser.PwdChange := Rss_CheckChangePasswordNow(FUserPwd, qryUser.FieldByName(fnCHANGED).AsDateTime);
        FErrPwdCount := qryUser.FieldByName(fnERRPWDCOUNT).AsInteger;
        Result := CheckDeleted and CheckBlocked;
      end
      else begin
        ShowMessage(Format(SErrUserNotFound, [Name]));
        AddToDbLogUser(tagErrRegistration, FAppTag, 0,
          Format(SLogErrUserRegistration, [FProgramName, Name]));
        ErrorBox(Format(SErrUserNotFound, [Name]));
      end;
    except
      on E: Exception do
      begin
        ShowMessage(SErrFindUser);
        HandleSqlExcept(E, nil, Format(sqlGetUserData, [Name]), SErrFindUser);
      end;
    end;
  finally
    FreeDynamicQuery(qryUser);
    ExitWait;
  end;
end;

{ == �������� ���� ������� ===================================================== }
function TFormLogin.CheckArmRights: Boolean;
var
  qryWpList: TADOQuery;
  sqlWpList: string;
begin
  Result := False;
  StartWait;
  qryWpList := nil;
  try
    ShowMessage(SMsgSelectUserPwd);
    try
      sqlWpList := Format(sqlUserWorkplases, [Format(sqlFixOpULinks, [FUser.UserId]),
        Format(sqlOpGroupsOpULinks, [FUser.UserId])]);
      qryWpList := OpenDynamicQuery(FConn, sqlWpList);
      if DataSetIsNotEmpty(qryWpList) then
      begin
        qryWpList.First;
        while not qryWpList.EOF and (qryWpList.FieldByName(fnID).AsInteger <> FAppTag) do
          qryWpList.Next;
        Result := qryWpList.FieldByName(fnID).AsInteger = FAppTag;
      end;
      if not Result then
      begin
        ShowMessage(Format(SErrUserBadArm, [FProgramName, FUser.UserName]));
        ErrorBox(Format(SErrUserBadArm, [FProgramName, FUser.UserName]));
        AddToDbLog(tagErrRegistration, Format(SLogWpcUserRegistration,
          [FProgramName, FUser.UserName]));
      end;
    except
      on E: Exception do
      begin
        ShowMessage(Format(SErrSelectUserData, [FUser.UserId]));
        HandleSqlExcept(E, nil, sqlWpList, Format(SErrSelectUserData, [FUser.UserId]));
      end;
    end;
  finally
    FreeDynamicQuery(qryWpList);
    ExitWait;
  end;
end;

{ == ���������� ������ ������ ================================================== }
procedure TFormLogin.StorePasswordErrors;
begin
  StartWait;
  try
    try
      ExecDynamicQuery(FConn, Format(sqlSetErrPwsCount, [FErrPwdCount, FUser.UserId]));
    except
      on E: Exception do
        HandleSqlExcept(E, nil, Format(sqlSetErrPwsCount, [FErrPwdCount, FUser.UserId]),
         Format(SErrStoreUserData, [FUser.UserId]));
    end;
    try
      if FErrPwdCount >= MaxPwdErrors
      then ExecDynamicQuery(FConn, Format(sqlSetBlocked, [FUser.UserId]));
    except
      on E: Exception do
        HandleSqlExcept(E, nil, Format(sqlSetBlocked, [FUser.UserId]),
          Format(SErrStoreUserData, [FUser.UserId]));
    end;
  finally
    ExitWait;
  end;
end;

{ == ���������� ����������� ==================================================== }
procedure TFormLogin.OkBtnClick(Sender: TObject);
begin
  if FState = 1 then
  begin
    if NameEdit.Text <> EmptyStr then
    begin
      InitUserData(FUser);
      if FindUser(NameEdit.Text)
      then begin
        DbLog_UpdateUser(FUser);
        UserPanel.Caption := Format(SFmtUserInfo, [FUser.FullName, FErrPwdCount + 1, MaxPwdErrors]);
        // PwdEdit.Enabled := True;
        PwdEdit.SetFocus;
        Exit;
      end
      else begin
        NameEdit.Clear;
        NameEdit.SetFocus;
      end;
    end
    else CancelBtnClick(Sender);
  end;
  if FState = 2 then
  begin
    if PwdEdit.Text = FUserPwd then
    begin
      FUser.Registration := CheckArmRights;
      if FUser.Registration then
      begin
        FErrPwdCount := 0;
        StorePasswordErrors;
        AddToDbLog(tagRegistration, Format(SLogSucsRegistration,
          [FUser.UserName, FProgramName]));
      end;
      Close;
    end
    else begin
      Inc(FErrPwdCount);
      if FErrPwdCount < MaxPwdErrors then
      begin
        UserPanel.Caption := Format(SFmtUserInfo, [FUser.FullName, FErrPwdCount + 1, MaxPwdErrors]);
        ShowMessage(SErrBadPassword);
        StorePasswordErrors;
        AddToDbLog(tagErrRegistration, Format(SLogBadPassword,
          [FProgramName, FUser.UserName]));
        ErrorBox(SErrBadPassword);
        PwdEdit.Clear;
        PwdEdit.SetFocus;
      end
      else begin
        UserPanel.Caption := Format(SFmtCanLogin, [FUser.FullName]);
        ShowMessage(SErrBadPasswordBkl);
        StorePasswordErrors;
        AddToDbLog(tagErrRegistration, Format(SLogBlockPassword,
          [FUser.UserName, FProgramName]));
        ErrorBox(SErrBadPasswordBkl);
        Close;
      end;
    end;
  end;
end;

{ == ������ ����������� ======================================================== }
procedure TFormLogin.CancelBtnClick(Sender: TObject);
begin
  FUser.Registration := False;
  Close;
end;

end.
