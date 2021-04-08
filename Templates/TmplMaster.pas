unit TmplMaster;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplBase, StdCtrls, Buttons, ExtCtrls;

type
  TMasterTemplate = class(TBaseTemplate)
    ButtonsPanel: TPanel;
    HelpBtn: TBitBtn;
    ButtonsMovedPanel: TPanel;
    PrevBtn: TBitBtn;
    CancelBtn: TBitBtn;
    NextBtn: TBitBtn;
    ButtonsBevel: TBevel;
    HeaderPanel: TPanel;
    HeaderBevel: TBevel;
    HeaderLabel: TLabel;
    HeaderImage: TImage;
    procedure FormActivate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    FEnterTab: Boolean;
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
  protected
    procedure InitFormVariables; override;
    procedure UpdateButtons(const BlockButtons: Boolean = False); virtual; abstract;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure SetNext;
    procedure SetComplete;
  end;

implementation

{$R *.dfm}

{$IFDEF STYLES}
uses
  RAppStyles, RFonts;
{$ENDIF}

resourcestring
  SNextCaption     = 'Далее';
  SNextHint        = 'Перейти на следующий шаг';
  SCompleteCaption = 'Готово';
  SCompleteHint    = 'Завершить работу мастера';

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TMasterTemplate.SetStyle;
begin
  inherited;
  FontDataToFontNoStyled(ApplicationStyle.DataForm.FormFont, HeaderLabel.Font);
  ButtonsPanel.Color := ApplicationStyle.DataForm.ButtonsPanelColor;
end;
{$ENDIF}

{ == Инициализация формы ======================================================= }
procedure TMasterTemplate.InitFormVariables;
begin
  inherited;
  FEnterTab := True;
end;

type
  THackMemo = class (TCustomMemo)
  public
    property WantReturns;
  end;

{ == Подмена нажатия Enter на Tab ============================================== }
procedure TMasterTemplate.CMDialogKey(var Msg: TWMKey);
begin
  if FEnterTab and not (ActiveControl is TButton) and
  not ((ActiveControl is TCustomMemo) and THackMemo(ActiveControl).WantReturns) then
    if Msg.Charcode = 13 then
      Msg.Charcode := 9;
  inherited;
end;

{ == Обработка свойств элементов управления ==================================== }
procedure TMasterTemplate.FormActivate(Sender: TObject);
begin
  HelpBtn.Visible := ((HelpFile <> EmptyStr) and FileExists(HelpFile))
                  or FileExists(Application.HelpFile);
  HelpBtn.Enabled := ((HelpType = htKeyword) and (HelpKeyword <> EmptyStr))
                  or ((HelpType = htContext) and (HelpContext <> 0));
  UpdateButtons;
end;

{ == Вызов справочной информации =============================================== }
procedure TMasterTemplate.HelpBtnClick(Sender: TObject);
begin
  if HelpType = htKeyword
  then Application.HelpKeyword(HelpKeyword)
  else Application.HelpContext(HelpContext);
end;

{ == Обработка нажатий на кнопки "Назад" - "Вперед" ============================ }
procedure TMasterTemplate.SetNext;
begin
  NextBtn.Caption := SNextCaption;
  NextBtn.Hint := SNextHint;
  NextBtn.ModalResult := mrNone;
end;

procedure TMasterTemplate.SetComplete;
begin
  NextBtn.Caption := SCompleteCaption;
  NextBtn.Hint := SCompleteHint;
  NextBtn.ModalResult := mrOk;
end;

end.
