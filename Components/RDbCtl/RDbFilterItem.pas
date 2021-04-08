unit RDbFilterItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, TmplBase;

type
  TFormDbFilterItem = class(TBaseTemplate)
    FilterPanel: TPanel;
    FieldLabel: TLabel;
    FieldText: TStaticText;
    ActiveCheckBox: TCheckBox;
    InvertCheckBox: TCheckBox;
    OperationLabel: TLabel;
    OperationComboBox: TComboBox;
    ButtonsBevel: TBevel;
    ButtonsPanel: TPanel;
    HelpBtn: TBitBtn;
    ButtonsMovedPanel: TPanel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
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

  TFormDbFilterItemClass = class of TFormDbFilterItem;

implementation

{$R *.dfm}

{$IFDEF STYLES}
uses
  RAppStyles, RFonts;

{ == Установка стиля формы ===================================================== }
procedure TFormDbFilterItem.SetStyle;
begin
  inherited;
  ButtonsPanel.ControlStyle := ButtonsPanel.ControlStyle - [csParentBackground] + [csOpaque];
  ButtonsPanel.Color := ApplicationStyle.DataForm.ButtonsPanelColor;
end;
{$ENDIF}

procedure TFormDbFilterItem.StartForm;
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
procedure TFormDbFilterItem.CMDialogKey(var Msg: TWMKey);
begin
  if not (ActiveControl is TButton) then
  begin
    if Msg.Charcode = 13 then
      Msg.Charcode := 9;
  end;
  inherited;
end;

{ == Вызов справочной информации =============================================== }
procedure TFormDbFilterItem.FormActivate(Sender: TObject);
begin
  HelpBtn.Visible := (HelpKeyword <> EmptyStr)
    and (((HelpFile <> EmptyStr) and FileExists(HelpFile))
     or ((Application.HelpFile <> EmptyStr) and FileExists(Application.HelpFile)));
end;

procedure TFormDbFilterItem.HelpBtnClick(Sender: TObject);
begin
  if HelpType = htKeyword
  then Application.HelpKeyword(HelpKeyword)
  else Application.HelpContext(HelpContext);
end;

end.
