unit RDbGridTunerItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplBase, StdCtrls, Buttons, ExtCtrls, Spin;

type
  TFormDbGridTunerItem = class(TBaseTemplate)
    FilterPanel: TPanel;
    ButtonsBevel: TBevel;
    ButtonsPanel: TPanel;
    HelpBtn: TBitBtn;
    ButtonsMovedPanel: TPanel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CaptionEditLabel: TLabel;
    NameComboBox: TComboBox;
    PhysEditLabel: TLabel;
    PhysEdit: TEdit;
    SpinEditLabel: TLabel;
    SpinEdit: TSpinEdit;
    DAComboBoxLabel: TLabel;
    DAComboBox: TComboBox;
    TAComboBoxLabel: TLabel;
    TAComboBox: TComboBox;
    procedure FormActivate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  protected
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
  end;

implementation

{$R *.dfm}

{$IFDEF STYLES}
uses
  RAppStyles, RFonts;

{ == Установка стиля формы ===================================================== }
procedure TFormDbGridTunerItem.SetStyle;
begin
  inherited;
  ButtonsPanel.Color := ApplicationStyle.DataForm.ButtonsPanelColor;
end;
{$ENDIF}

{ == Подмена нажатия Enter на Tab ============================================== }
procedure TFormDbGridTunerItem.CMDialogKey(var Msg: TWMKey);
begin
  if not (ActiveControl is TButton) then
  begin
    if Msg.Charcode = 13 then
      Msg.Charcode := 9;
  end;
  inherited;
end;

{ == Вызов справочной информации =============================================== }
procedure TFormDbGridTunerItem.FormActivate(Sender: TObject);
begin
  HelpBtn.Visible := (HelpKeyword <> EmptyStr)
    and (((HelpFile <> EmptyStr) and FileExists(HelpFile))
     or ((Application.HelpFile <> EmptyStr) and FileExists(Application.HelpFile)));
end;

procedure TFormDbGridTunerItem.HelpBtnClick(Sender: TObject);
begin
  if HelpType = htKeyword
  then Application.HelpKeyword(HelpKeyword)
  else Application.HelpContext(HelpContext);
end;

end.
