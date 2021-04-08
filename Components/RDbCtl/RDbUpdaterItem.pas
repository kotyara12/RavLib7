unit RDbUpdaterItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplBase, StdCtrls, Buttons, ExtCtrls;

type
  TFormDbUpdaterItem = class(TBaseTemplate)
    UpdaterPanel: TPanel;
    FieldLabel: TLabel;
    ButtonsBevel: TBevel;
    FieldText: TStaticText;
    ButtonsPanel: TPanel;
    HelpBtn: TBitBtn;
    ButtonsMovedPanel: TPanel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ClearRadioButton: TRadioButton;
    SetRadioButton: TRadioButton;
    procedure FormActivate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  protected
    procedure StartForm; override;
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
  end;

  TFormDbUpdaterItemClass = class of TFormDbUpdaterItem;

implementation

{$R *.dfm}

{$IFDEF STYLES}
uses
  RAppStyles, RFonts;

{ == Установка стиля формы ===================================================== }
procedure TFormDbUpdaterItem.SetStyle;
begin
  inherited;
  ButtonsPanel.ControlStyle := ButtonsPanel.ControlStyle - [csParentBackground] + [csOpaque];
  ButtonsPanel.Color := ApplicationStyle.DataForm.ButtonsPanelColor;
end;
{$ENDIF}

procedure TFormDbUpdaterItem.StartForm;
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
procedure TFormDbUpdaterItem.CMDialogKey(var Msg: TWMKey);
begin
  if not (ActiveControl is TButton) then
  begin
    if Msg.Charcode = 13 then
      Msg.Charcode := 9;
  end;
  inherited;
end;

{ == Вызов справочной информации =============================================== }
procedure TFormDbUpdaterItem.FormActivate(Sender: TObject);
begin
  HelpBtn.Visible := ((HelpFile <> EmptyStr) and FileExists(HelpFile))
                  or FileExists(Application.HelpFile);
  HelpBtn.Enabled := ((HelpType = htKeyword) and (HelpKeyword <> EmptyStr))
                  or ((HelpType = htContext) and (HelpContext <> 0));
end;

procedure TFormDbUpdaterItem.HelpBtnClick(Sender: TObject);
begin
  if HelpType = htKeyword
  then Application.HelpKeyword(HelpKeyword)
  else Application.HelpContext(HelpContext);
end;

end.
