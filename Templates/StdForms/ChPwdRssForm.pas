unit ChPwdRssForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, AdoDb, RUserRights;

type
  EChangePwdException = class (Exception);

  TFormChPwdRss = class(TDialogTemplate)
    OldPwdEditLabel: TLabel;
    OldPwdEdit: TEdit;
    NewPwdEditLabel: TLabel;
    NewPwdEdit: TEdit;
    CnfPwdEditLabel: TLabel;
    CnfPwdEdit: TEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FOldPwd: string;
  public
  end;

{ == Смена пароля пользователем ================================================ }
procedure ChangePasswordRss(Db: TAdoConnection; var User: TUserRights;
  const CancelEnabled: Boolean = True);

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RCryptApiEx, RDbConst, RDbUtils, RDbLog, RExHandlers,
  RRssConst, RRssBase;

function ChangePassword(const SetOldPwd, CancelEnabled: Boolean; var Password: string): Boolean;
begin
  with TFormChPwdRss.Create(Application) do
  begin
    try
      if Trim(Password) <> EmptyStr
      then FOldPwd := Password
      else FOldPwd := SDefaultPwd;
      CancelBtn.Enabled := CancelEnabled;
      if SetOldPwd or (Trim(Password) = EmptyStr) then
      begin
        OldPwdEdit.Text := FOldPwd;
        ActiveControl := NewPwdEdit;
      end;
      Result := ShowModal = mrOk;
      if Result then Password := NewPwdEdit.Text;
    finally
      Free;
    end;
  end;
end;

procedure ChangePasswordRss(Db: TAdoConnection; var User: TUserRights;
  const CancelEnabled: Boolean = True);
var
  Password: string;
  UserData: TADOQuery;
  PwdStored: Boolean;
begin
  UserData := nil;
  try
    StartWait;
    try
      try
        UserData := OpenDynamicQuery(Db, Format(sqlReadUserPwd, [User.UserId]));
      except
        on E: Exception do
          HandleSqlExcept(E, UserData, Format(sqlReadUserPwd, [User.UserId]), SErrLoadUserData);
      end;
    finally
      ExitWait;
    end;
    if DataSetIsNotEmpty(UserData) then
    begin
      Password := DecryptPwd_Text_RsaShaRc2B64_Ex(Trim(UserData.FieldByName(fnPASSWORD).AsString), PwdKey);
      if ChangePassword(False, CancelEnabled, Password) then
      begin
        StartWait;
        try
          Password := EncryptPwd_Text_RsaShaRc2B64_Ex(Password, PwdKey);
          PwdStored := False;
          try
            PwdStored := ExecDynamicQuery(Db, Format(sqlSaveUserPwd, [Password, User.UserId]));
          except
            on E: Exception do
              HandleSqlExcept(E, nil, Format(sqlSaveUserPwd, [Password, User.UserId]), SErrSaveUserData);
          end;
          if PwdStored then
          begin
            User.PwdChange := False;
            AddToDbLog(tagChPassword, Format(SLogChangePassword, [User.UserName]));
            InfoBox(SMsgPwdChanged);
          end;
        finally
          ExitWait;
        end;
      end;
    end;
  finally
    FreeDynamicQuery(UserData);
  end;
end;

procedure TFormChPwdRss.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk
  then CanClose := Rss_CheckNewPassword(FOldPwd, OldPwdEdit.Text, NewPwdEdit.Text, CnfPwdEdit.Text)
  else CanClose := CancelBtn.Enabled;
end;

end.
