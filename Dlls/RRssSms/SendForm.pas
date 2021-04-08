unit SendForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, StdCtrls, DBCtrls, RDbText, DB, Buttons, ExtCtrls,
  ADODB, Mask;

type
  TFormSend = class(TDbDialogTemplate)
    lblTitle: TLabel;
    lblAddress: TLabel;
    lblMessage: TLabel;
    deMessage: TDBMemo;
    deTitle: TDBEdit;
    deAddress: TDBEdit;
    AddUserBtn: TSpeedButton;
    AddWpBtn: TSpeedButton;
    sm_messages: TADOQuery;
    sm_messagesid: TIntegerField;
    sm_messagesid_users: TIntegerField;
    sm_messagessended: TDateTimeField;
    sm_messagestitle: TStringField;
    sm_messagesaddress: TStringField;
    sm_messagesmessage: TMemoField;
    procedure AddUserBtnClick(Sender: TObject);
    procedure AddWpBtnClick(Sender: TObject);
  private
    procedure CreateSms(Db: TAdoConnection; const UserId: Integer);
    procedure SendSms;
  public
    { Public declarations }
  end;

procedure SendSms(Db: TAdoConnection; const UserId: Integer);

implementation

{$R *.dfm}

uses
  RVclUtils, RExHandlers, RSmsVars, RDbLog, RDbConst, RDbGetId, RRssConst,
  UsersList, WpsList;

procedure SendSms(Db: TAdoConnection; const UserId: Integer);
begin
  with TFormSend.Create(Application) do
  begin
    try
      CreateSms(Db, UserId);
      if ShowModal = mrOk then SendSms;
    finally
      Free;
    end;
  end;
end;

procedure TFormSend.CreateSms(Db: TAdoConnection; const UserId: Integer);
begin
  StartWait;
  try
    try
      sm_messages.Connection := Db;
      sm_messages.Open;
      if sm_messages.Active then begin
        sm_messages.Append;
        sm_messagesid_users.AsInteger := UserId;
        sm_messagesaddress.AsString := '*';
      end;
    except
      on E: Exception do
        HandleExcept(E, sm_messages, SErrCreateSms);
    end;
  finally
    StopWait;
  end;
end;

procedure TFormSend.SendSms;
begin
  StartWait;
  try
    try
      sm_messagesid.AsInteger := GetNextID(sm_messages.Connection, sm_messages.Name, fnID);
      sm_messagessended.AsDateTime := Now;
      sm_messages.Post;
      AddToDbLog(tagSendSms, Format(SLogSendSms, [sm_messagestitle.AsString, sm_messagesaddress.AsString]));
    except
      on E: Exception do
        HandleExcept(E, sm_messages, SErrCreateSms);
    end;
  finally
    StopWait;
  end;
end;

procedure TFormSend.AddUserBtnClick(Sender: TObject);
begin
  deAddress.Text := GetUserList(sm_messages.Connection, sm_messagesid_users.AsInteger, deAddress.Text);
end;

procedure TFormSend.AddWpBtnClick(Sender: TObject);
begin
  deAddress.Text := GetWpsList(sm_messages.Connection, deAddress.Text);
end;

end.
