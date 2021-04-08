unit RavFloatEdit;

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  Messages,
  Classes,
  Graphics,
  Controls,
  Menus,
  Forms,
  Dialogs,
  StdCtrls;

type
  TRFloatEdit = class(TCustomMemo)
  private
    fValue: Extended;
    fDisplayFormat: string;
    fBeepOnError: Boolean;
    fUndoOnError: Boolean;
    fFormatOnEdit: Boolean;
    fTextChanged: Boolean;
    procedure SetFormat(const aValue: string);
    procedure SetValue(const aValue: Extended);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure DecodeText;
    procedure DisplayValue;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment default taRightJustify;
    property AutoSize default True;
    property BeepOnError: Boolean read fBeepOnError write fBeepOnError default True;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DisplayFormat: string read fDisplayFormat write SetFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEdit: Boolean read fFormatOnEdit write fFormatOnEdit default True;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property Value: Extended read fValue write SetValue;
    property Visible;
    property UndoOnError: Boolean read fUndoOnError write fUndoOnError default True;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  Themes;

constructor TRFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := True;
  Alignment := taRightJustify;
  Width := 121;
  Height := 25;
  fDisplayFormat := ',0.00';
  fValue := 0.0;
  fBeepOnError := True;
  fUndoOnError := True;
  fFormatOnEdit := True;
  AutoSelect := False;
  WantReturns := False;
  WordWrap := False;
  DisplayValue;
end;

procedure TRFloatEdit.SetFormat(const aValue: string);
begin
  if fDisplayFormat <> aValue then
  begin
    fDisplayFormat := aValue;
    DisplayValue;
  end;
end;

procedure TRFloatEdit.SetValue(const aValue: Extended);
begin
  if fValue <> aValue then
  begin
    fValue := aValue;
    DisplayValue;
  end;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TRFloatEdit.DecodeText;
var
  TmpValue: Extended;
  TmpText: string;
  i: Integer;
  IsNeg: Boolean;
begin
  IsNeg := Pos('-', Text) > 0;
  TmpText := '';
  TmpValue := fValue;
  for i := 1 to Length(Text) do
  begin
    if (Text[i] in ['0'..'9'])
    or ((Text[i] = DecimalSeparator) and (Pos(DecimalSeparator, TmpText) = 0)) then
      TmpText := TmpText + Text[i];
  end;
  try
    fValue := StrToFloat(TmpText);
    if IsNeg then fValue := - fValue;
    if Assigned(OnChange) then OnChange(Self);
  except
    if fBeepOnError then MessageBeep(0);
    if not fUndoOnError then fValue := TmpValue;
  end;
end;

procedure TRFloatEdit.DisplayValue;
begin
  fTextChanged := False;
  if (fFormatOnEdit or not Focused) and (fDisplayFormat <> EmptyStr) then
    Text := FormatFloat(fDisplayFormat, fValue)
  else
    Text := FloatToStr(fValue);
end;

procedure TRFloatEdit.CMEnter(var Message: TCMEnter);
begin
  DisplayValue;
  SelectAll;
  inherited;
end;

procedure TRFloatEdit.CMExit(var Message: TCMExit);
begin
  DecodeText;
  DisplayValue;
  inherited;
end;

procedure TRFloatEdit.KeyPress(var Key: Char);
begin
  if Key in ['.', ','] then Key := DecimalSeparator;
  if Key in ['0'..'9', '-', '+', #8, #13, #27] + [DecimalSeparator] then
  begin
    if Key = '+' then Key := #0;
    if (Key = DecimalSeparator) and (Pos(DecimalSeparator, Text) > 0) then Key := #0;
    if (Key = #13) and fFormatOnEdit then
    begin
      if fTextChanged then Key := #0;
      DecodeText;
      DisplayValue;
      SelectAll;
    end;
    if (Key = #27) then
    begin
      if fTextChanged then Key := #0;
      DisplayValue;
      SelectAll;
    end;
  end
  else begin
    if fBeepOnError then MessageBeep(0);
    Key := #0;
  end;
  fTextChanged := Key <> #0;
  inherited KeyPress(Key);
end;

procedure TRFloatEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  case Alignment of
    taLeftJustify: Params.Style := Params.Style or ES_LEFT and not ES_MULTILINE;
    taRightJustify: Params.Style := Params.Style or ES_RIGHT and not ES_MULTILINE;
    taCenter: Params.Style := Params.Style or ES_CENTER and not ES_MULTILINE;
  end;
end;

end.
