unit RExHandlersExDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RExHandlers, RExHandlersDlg;

type
  TExDlgExceptChannel = class (TDialogsExceptChannel)
  protected
    procedure RegisterExceptRecord(const ER: RExceptRecord); override;
  end;

  TExtErrorBox = class(TDialogTemplate)
    DescrMemo: TMemo;
    MsgMemoLabel: TLabel;
    MsgMemo: TMemo;
    ObjNameEditLabel: TLabel;
    ObjNameEdit: TEdit;
    ObjClassEdit: TEdit;
    ObjClassEditLabel: TLabel;
    SqlMemo: TMemo;
    SqlMemoLabel: TLabel;
    ExClassEditLabel: TLabel;
    ExClassEdit: TEdit;
    ExCodeEditLabel: TLabel;
    ExCodeEdit: TEdit;
    ExCatgEdit: TEdit;
    Image: TImage;
    procedure CancelBtnClick(Sender: TObject);
  public
  end;

implementation

{$R *.dfm}

uses
  RClipBrd, RVclUtils, RDialogs;

resourcestring
  sClipBrdTitle        = 'Текст скопирован в буфер обмена Windows';

{ == TExDlgExceptChannel ======================================================= }

procedure TExDlgExceptChannel.RegisterExceptRecord(const ER: RExceptRecord);
var
  DecM, DecC, DecQ: Integer;
begin
  if ER.Description = EmptyStr
  then inherited RegisterExceptRecord(ER)
  else with TExtErrorBox.Create(Application) do
         begin
           PauseWait;
           try

             DescrMemo.Lines.Text := ER.Description;
             // ExceptMessage
             DecM := 0;
             MsgMemo.Lines.Text := ER.ExceptMessage;
             if ER.ExceptMessage = EmptyStr then
             begin
               MsgMemo.Visible := False;
               MsgMemoLabel.Visible := False;
               DecM := ObjNameEdit.Top - MsgMemo.Top;
             end;
             // CreatorObject
             DecC := 0;
             ObjNameEdit.Text := ER.CreatorObject;
             ObjClassEdit.Text := ER.CreatorClass;
             if (ER.CreatorObject = EmptyStr) and (ER.CreatorClass = EmptyStr) then
             begin
               ObjNameEdit.Visible := False;
               ObjNameEditLabel.Visible := False;
               ObjClassEdit.Visible := False;
               ObjClassEditLabel.Visible := False;
               DecC := SqlMemo.Top - ObjNameEdit.Top;
             end;
             // SqlCommand
             DecQ := 0;
             SqlMemo.Lines.Text := ER.SqlCommand;
             if ER.SqlCommand = EmptyStr then
             begin
               SqlMemo.Visible := False;
               SqlMemoLabel.Visible := False;
               DecQ := ExClassEdit.Top - SqlMemo.Top;
             end;
             // ER.ExceptClass
             ExClassEdit.Text := ER.ExceptClass;
             ExCodeEdit.Text := IntToStr(ER.ExceptId);
             ExCatgEdit.Text := IntToStr(ER.CategoryId);
             if (ER.ExceptId = 0) and (ER.CategoryId = 0) then
             begin
               ExCodeEdit.Visible := False;
               ExCodeEditLabel.Visible := False;
               ExCatgEdit.Visible := False;
             end;
             // Set Form Positions
             ObjNameEdit.Top := ObjNameEdit.Top - DecM;
             ObjNameEditLabel.Top := ObjNameEditLabel.Top - DecM;
             ObjClassEdit.Top := ObjClassEdit.Top - DecM;
             ObjClassEditLabel.Top := ObjClassEditLabel.Top - DecM;
             SqlMemo.Top := SqlMemo.Top - DecM - DecC;
             SqlMemoLabel.Top := SqlMemoLabel.Top - DecM - DecC;
             ExClassEdit.Top := ExClassEdit.Top - DecM - DecC - DecQ;
             ExClassEditLabel.Top := ExClassEditLabel.Top - DecM - DecC - DecQ;
             ExCodeEdit.Top := ExCodeEdit.Top - DecM - DecC - DecQ;
             ExCodeEditLabel.Top := ExCodeEditLabel.Top - DecM - DecC - DecQ;
             ExCatgEdit.Top := ExCatgEdit.Top - DecM - DecC - DecQ;
             Height := Height - DecM - DecC - DecQ;
             // Show Form;
             ShowModal;
           finally
             Free;
             ContiniueWait;
           end;
         end;
end;

procedure TExtErrorBox.CancelBtnClick(Sender: TObject);
var
  sClipMsg: string;

  procedure PutMessage(const Header, Text, Divider: string);
  begin
    if Text <> EmptyStr then
    begin
      if sClipMsg = EmptyStr then
      begin
        if Header = EmptyStr
        then sClipMsg := Text
        else sClipMsg := Header + #32 + Text;
      end
      else begin
        if Header = EmptyStr
        then sClipMsg := sClipMsg + Divider + Text
        else sClipMsg := sClipMsg + Divider + Header + #32 + Text;
      end;
    end;
  end;

begin
  sClipMsg := EmptyStr;

  if DescrMemo.Visible then
    PutMessage(EmptyStr, DescrMemo.Text, #13#10);
  if MsgMemo.Visible then
    PutMessage(MsgMemoLabel.Caption, MsgMemo.Text, #13#10);
  if ObjNameEdit.Visible then
    PutMessage(ObjNameEditLabel.Caption, ObjNameEdit.Text, #13#10);
  if ObjClassEdit.Visible then
    PutMessage(ObjClassEditLabel.Caption, ObjClassEdit.Text, ', ');
  if SqlMemo.Visible then
    PutMessage(SqlMemoLabel.Caption, SqlMemo.Text, #13#10);
  if ExClassEdit.Visible then
    PutMessage(ExClassEditLabel.Caption, ExClassEdit.Text, #13#10);
  if ExCodeEdit.Visible then
    PutMessage(ExCodeEditLabel.Caption, ExCodeEdit.Text, ', ');
  if ExCatgEdit.Visible then
    PutMessage(EmptyStr, ExCatgEdit.Text, ', ');

  PutStringIntoClipBoard(sClipMsg);
  
  CustomMsgBox(sClipBrdTitle, sClipMsg, MB_ICONINFORMATION + MB_OK);
end;

end.
