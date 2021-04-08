unit TmplDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Buttons, Dialogs, ExtCtrls, StdCtrls, TmplBase, RSysUtils, RVclUtils;

type
  TDialogTemplate = class(TBaseTemplate)
    ButtonsPanel: TPanel;
    ButtonsBevel: TBevel;
    HelpBtn: TBitBtn;
    ButtonsMovedPanel: TPanel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    procedure FormActivate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FEnterTab: Boolean;
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
  protected
    procedure InitFormVariables; override;
    procedure StartForm; override;
    function  CheckOkClose: Boolean; virtual;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    property EnterTab: Boolean read FEnterTab write FEnterTab;
    procedure DisableControls;
    procedure EnableControls;
  end;

implementation

{$R *.dfm}

{$IFDEF STYLES}
uses
  RAppStyles, RFonts, RDialogs;
{$ENDIF}

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TDialogTemplate.SetStyle;
begin
  inherited;
  ButtonsPanel.ControlStyle := ButtonsPanel.ControlStyle - [csParentBackground] + [csOpaque];
  ButtonsPanel.Color := ApplicationStyle.DataForm.ButtonsPanelColor;
end;
{$ENDIF}

{ == Инициализация формы ======================================================= }
procedure TDialogTemplate.InitFormVariables;
begin
  inherited;
  FEnterTab := True;
end;

procedure TDialogTemplate.StartForm;

  // 2013-04-01: установка высоты TSpeedButton при изменении шрифта как у TEdit
  procedure SetSpeedButtonHeight(Control: TControl);
  var
    i: Integer;
  begin
    if Assigned(Control) and (Control is TWinControl) then
    begin
      for i := 0 to TWinControl(Control).ControlCount - 1 do
        if (TWinControl(Control).Controls[i] is TSpeedButton)
        and (TSpeedButton(TWinControl(Control).Controls[i]).Height >= 21) then
          TSpeedButton(TWinControl(Control).Controls[i]).Height := Canvas.TextHeight('Wg') + 8
        else begin
          SetSpeedButtonHeight(TWinControl(Control).Controls[i]);
        end;
    end;
  end;

begin
  inherited;
  // 2013-04-01: установка высоты TSpeedButton при изменении шрифта как у TEdit
  SetSpeedButtonHeight(Self);
end;

{ == Подмена нажатия Enter на Tab ============================================== }
type
  THackMemo = class (TCustomMemo)
  public
    property WantReturns;
  end;

procedure TDialogTemplate.CMDialogKey(var Msg: TWMKey);
begin
  if FEnterTab and not (ActiveControl is TButton) and
  not ((ActiveControl is TCustomMemo) and THackMemo(ActiveControl).WantReturns) then
  begin
    if Msg.Charcode = 13 then
      Msg.Charcode := 9;
  end;
  inherited;
end;

{ == Обработка свойств элементов управления ==================================== }
procedure TDialogTemplate.FormActivate(Sender: TObject);
begin
  HelpBtn.Visible := (HelpKeyword <> EmptyStr)
    and (((HelpFile <> EmptyStr) and FileExists(HelpFile))
     or ((Application.HelpFile <> EmptyStr) and FileExists(Application.HelpFile)));
end;

{ == Вызов справочной информации =============================================== }
procedure TDialogTemplate.HelpBtnClick(Sender: TObject);
begin
  if FileExists(HelpFile)
  then OpenHelp(HelpFile, HelpKeyword)
  else OpenHelp(Application.HelpFile, HelpKeyword);
end;

{ == Блокировка элемнтов управления ============================================ }
procedure TDialogTemplate.DisableControls;
begin
  ToggleControls(Self, False);
end;

procedure TDialogTemplate.EnableControls;
begin
  ToggleControls(Self, True);
end;

function TDialogTemplate.CheckOkClose: Boolean;
begin
  Result := True;
end;

procedure TDialogTemplate.OkBtnClick(Sender: TObject);
begin
  if CheckOkClose then
  begin
    Close;
    ModalResult := mrOk;
  end;
end;

end.
