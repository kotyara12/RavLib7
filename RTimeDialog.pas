unit RTimeDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TDialogMode = (dmInfo, dmWarning, dmCaution, dmError);

  TTimeDialog = class(TDialogTemplate)
    Image_I: TImage;
    Image_W: TImage;
    Image_E: TImage;
    Image_Q: TImage;
    Timer: TTimer;
    RichEdit: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    fTimeOut: Word;
  end;

procedure ShowInfoBoxEx(const Mode: TDialogMode; const Title, TextMsg: string; const Timeout: Word = 60);
procedure ShowInfoBox(const Mode: TDialogMode; const TextMsg: string; const Timeout: Word = 60);
function  ShowQueryBoxEx(const OkDefault, WarningIcon: Boolean; const Title, TextMsg: string; const Timeout: Word = 60): Boolean;
function  ShowQueryBox(const OkDefault: Boolean; const TextMsg: string; const Timeout: Word = 60): Boolean;

implementation

{$R *.dfm}

resourcestring
  SDlgInfo              = 'Информация';
  SDlgWarning           = 'Предупреждение';
  SDlgError             = 'Ошибка';
  SDlgCaution           = 'Внимание!';
  SDlgQuery             = 'Подтверждение';

  SBtnOk                = 'OK';
  SBtnCancel            = 'Отмена';
  SBtnTime              = ' [%d]';

const
  iSingleBtns           = 124;
  iDoubleBtns           = 222;

procedure ShowInfoBoxEx(const Mode: TDialogMode; const Title, TextMsg: string; const Timeout: Word = 60);
begin
  with TTimeDialog.Create(Application) do
  begin
    try
      fTimeOut := Timeout;
      Caption := Title;
      RichEdit.Lines.Text := TextMsg;
      Image_I.Visible := Mode = dmInfo;
      Image_W.Visible := Mode in [dmWarning, dmCaution];
      Image_E.Visible := Mode = dmError;
      Image_Q.Visible := False;
      ButtonsMovedPanel.Width := iSingleBtns;
      OkBtn.Caption := SBtnOk;
      OkBtn.Default := True;
      CancelBtn.Visible := False;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure ShowInfoBox(const Mode: TDialogMode; const TextMsg: string; const Timeout: Word = 60);
begin
  case Mode of
    dmInfo: ShowInfoBoxEx(Mode, SDlgInfo, TextMsg, Timeout);
    dmWarning: ShowInfoBoxEx(Mode, SDlgWarning, TextMsg, Timeout);
    dmCaution: ShowInfoBoxEx(Mode, SDlgCaution, TextMsg, Timeout);
    dmError: ShowInfoBoxEx(Mode, SDlgError, TextMsg, Timeout);
  end;
end;

function ShowQueryBoxEx(const OkDefault, WarningIcon: Boolean; const Title, TextMsg: string; const Timeout: Word = 60): Boolean;
begin
  with TTimeDialog.Create(Application) do
  begin
    try
      fTimeOut := Timeout;
      Caption := Title;
      RichEdit.Lines.Text := TextMsg;
      Image_I.Visible := False;
      Image_W.Visible := WarningIcon;
      Image_E.Visible := False;
      Image_Q.Visible := not WarningIcon;
      ButtonsMovedPanel.Width := iDoubleBtns;
      OkBtn.Caption := SBtnOk;
      OkBtn.Default := OkDefault;
      CancelBtn.Visible := True;
      CancelBtn.Caption := SBtnCancel;
      CancelBtn.Default := not OkDefault;
      Result := ShowModal = mrOk;
    finally
      Free;
    end;
  end;
end;

function ShowQueryBox(const OkDefault: Boolean; const TextMsg: string; const Timeout: Word = 60): Boolean;
begin
  Result := ShowQueryBoxEx(OkDefault, True, SDlgQuery, TextMsg, Timeout);
end;

procedure TTimeDialog.FormCreate(Sender: TObject);
begin
  inherited;
  BringToFront;
  Timer.Enabled := True;
end;

procedure TTimeDialog.TimerTimer(Sender: TObject);
begin
  inherited;
  if fTimeOut > 0 then
    Dec(fTimeOut);
  if fTimeOut > 0 then
  begin
    if OkBtn.Default
    then OkBtn.Caption := SBtnOk + Format(SBtnTime, [fTimeOut])
    else CancelBtn.Caption := SBtnCancel + Format(SBtnTime, [fTimeOut]);
  end
  else begin
    Timer.Enabled := False;
    if OkBtn.Default
    then OkBtn.Click
    else CancelBtn.Click;
  end;
end;

end.
