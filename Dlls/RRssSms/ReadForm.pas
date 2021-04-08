unit ReadForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, ADODB, StdCtrls, Buttons, ExtCtrls, DBCtrls,
  Mask, RDbText;

type
  TFormRead = class(TDbDialogTemplate)
    sm_messages: TADOQuery;
    sm_messagesid_users: TIntegerField;
    sm_messagesname: TStringField;
    sm_messagesfullname: TStringField;
    sm_messagessended: TDateTimeField;
    sm_messagestitle: TStringField;
    sm_messagesaddress: TStringField;
    sm_messagesmessage: TMemoField;
    lblName: TLabel;
    lblSended: TLabel;
    lblTitle: TLabel;
    lblAddress: TLabel;
    lblMessage: TLabel;
    sm_history: TADOQuery;
    sm_historyid_messages: TIntegerField;
    sm_historyid_users: TIntegerField;
    sm_historyopened: TDateTimeField;
    sm_historyclosed: TDateTimeField;
    deName: TRDbText;
    deFullname: TRDbText;
    deSended: TRDbText;
    deTitle: TRDbText;
    deAddress: TRDbText;
    deMessage: TDBMemo;
    procedure sm_historyPostError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure FormDestroy(Sender: TObject);
  protected
    procedure SetOkBtnState; override;
  public
    procedure LoadMessage(Db: TAdoConnection; const MsgId, UserId: Integer);
    procedure LoadMsgState(Db: TAdoConnection; const MsgId, UserId: Integer);
    procedure SaveMsgState(const MsgId, UserId: Integer);
  end;

procedure ReadSms(Db: TAdoConnection; const MsgId, UserId: Integer);

implementation

{$R *.dfm}

uses
  RVclUtils, RDbLog, RExHandlers, RRssConst, RSmsVars;

procedure ReadSms(Db: TAdoConnection; const MsgId, UserId: Integer);
begin
  with TFormRead.Create(Application) do
  begin
    try
      LoadMessage(Db, MsgId, UserId);
      if ShowModal = mrOk then
        SaveMsgState(MsgId, UserId);
    finally
      Free;
    end;
  end;
end;

procedure TFormRead.LoadMessage(Db: TAdoConnection; const MsgId, UserId: Integer);
begin
  StartWait;
  try
    try
      sm_messages.Connection := Db;
      sm_messages.SQL.Clear;
      sm_messages.SQL.Add(Format(sqlReadSmsId, [MsgId]));
      sm_messages.Open;
      if sm_messages.Active then
      begin
        LoadMsgState(Db, MsgId, UserId);
        AddToDbLog(tagReadSms, Format(SLogReadSms, [sm_messagestitle.AsString,
          sm_messagesname.AsString, sm_messagesfullname.AsString, sm_messagessended.AsString]));
      end;
    except
      on E: Exception do
        HandleExcept(E, sm_messages, Format(SErrLoadSms, [MsgId]));
    end;
  finally
    StopWait;
  end;
end;

procedure TFormRead.LoadMsgState(Db: TAdoConnection; const MsgId, UserId: Integer);
begin
  try
    sm_history.Connection := Db;
    sm_history.SQL.Clear;
    sm_history.SQL.Add(Format(sqlReadSmsState, [MsgId, UserId]));
    sm_history.Open;
    if sm_history.Active and sm_history.IsEmpty then
    begin
      sm_history.Append;
      sm_historyid_messages.AsInteger := MsgId;
      sm_historyid_users.AsInteger := UserId;
      sm_historyopened.AsDateTime := Now;
      sm_historyclosed.Clear;
      sm_history.Post;
    end;
  except
    on E: Exception do
      HandleExcept(E, sm_history, Format(SErrLoadSmsState, [MsgId]));
  end;
end;

procedure TFormRead.SaveMsgState(const MsgId, UserId: Integer);
begin
  if sm_history.Active then
  begin
    if sm_history.IsEmpty then
    begin
      sm_history.Append;
      sm_historyid_messages.AsInteger := MsgId;
      sm_historyid_users.AsInteger := UserId;
      sm_historyopened.AsDateTime := Now;
      sm_historyclosed.AsDateTime := Now;
      sm_history.Post;
    end
    else begin
      sm_history.Edit;
      sm_historyclosed.AsDateTime := Now;
      sm_history.Post;
    end;
  end;
end;

procedure TFormRead.SetOkBtnState;
begin
  OkBtn.Visible := sm_history.Active;
  OkBtn.Enabled := sm_history.Active and sm_historyclosed.IsNull;
end;

procedure TFormRead.sm_historyPostError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
begin
  HandleExcept(E, DataSet, EErrSaveSmsState);
  Action := daAbort;
end;

procedure TFormRead.FormDestroy(Sender: TObject);
begin
  if sm_history.Active then sm_history.Close;
  if sm_messages.Active then sm_messages.Close;
end;

end.
