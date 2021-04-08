unit ChPwdForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, AdoDb, RDbLogin;

type
  EChangePwdException = class (Exception);

  TFormChPwd = class(TDialogTemplate)
    OldPwdEditLabel: TLabel;
    OldPwdEdit: TEdit;
    NewPwdEditLabel: TLabel;
    NewPwdEdit: TEdit;
    CnfPwdEditLabel: TLabel;
    CnfPwdEdit: TEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fOldPwd: string;
  public
  end;

procedure ChangePassword(Db: TAdoConnection; const CryptKey: string;
  var User: TUserData; const CancelEnabled: Boolean = True);

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RCryptApiEx, RDbConst, RDbUtils, RExHandlers;

function InternalChangePassword(const SetOldPwd, CancelEnabled: Boolean; var Password: string): Boolean;
begin
  with TFormChPwd.Create(Application) do
  begin
    try
      if Trim(Password) <> EmptyStr
      then fOldPwd := Password
      else fOldPwd := SPwdDefault;
      CancelBtn.Enabled := CancelEnabled;
      if SetOldPwd or (Trim(Password) = EmptyStr) then
      begin
        OldPwdEdit.Text := fOldPwd;
        ActiveControl := NewPwdEdit;
      end;
      Result := ShowModal = mrOk;
      if Result then Password := NewPwdEdit.Text;
    finally
      Free;
    end;
  end;
end;

procedure ChangePassword(Db: TAdoConnection; const CryptKey: string;
  var User: TUserData; const CancelEnabled: Boolean = True);
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
          HandleExcept(E, nil, SErrLoadUserData);
      end;
    finally
      ExitWait;
    end;
    if DataSetIsNotEmpty(UserData) then
    begin
      Password := DecryptPwd_Text_RsaShaRc2B64_Ex(Trim(UserData.Fields[0].AsString), CryptKey);
      if InternalChangePassword(False, CancelEnabled, Password) then
      begin
        StartWait;
        try
          Password := EncryptPwd_Text_RsaShaRc2B64_Ex(Password, CryptKey);
          PwdStored := False;
          try
            PwdStored := ExecDynamicQuery(Db, Format(sqlSaveUserPwd, [Password, User.UserId]));
          except
            on E: Exception do
              HandleSqlExcept(E, nil,
                Format(sqlSaveUserPwd, [Password, User.UserId]),
                SErrSaveUserData);
          end;
          if PwdStored then
          begin
            User.NeedPwdChange := False;
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

procedure TFormChPwd.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk
  then CanClose := CheckNewPassword(FOldPwd, OldPwdEdit.Text, NewPwdEdit.Text, CnfPwdEdit.Text)
  else CanClose := CancelBtn.Enabled;
end;

end.
