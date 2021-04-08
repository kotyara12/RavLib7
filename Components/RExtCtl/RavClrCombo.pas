{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Extended Color Combo Box               }
{                                                       }
{       Copyright (c) 2005-06 Razzhivin Alexandr        }
{                                                       }
{*******************************************************}

unit RavClrCombo;

interface

uses
  Classes, Windows, StdCtrls, Graphics;

const
  ColorsInListBase     = 20;
  ColorsInListExtended = 45;
  ColorsList: array [0..ColorsInListExtended - 1] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal,
     clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clGray, clSilver, clWhite,
     clMoneyGreen, clSkyBlue, clCream, clMedGray,
     clBackground, clAppWorkSpace,
     clActiveCaption, clCaptionText, clActiveBorder,
     clInactiveCaption, clInactiveCaptionText, clInactiveBorder,
     clMenu, clMenuText, clGrayText, clHighlight, clHighlightText,
     clWindow, clWindowFrame, clWindowText, clScrollBar,
     clBtnFace, clBtnText, clBtnShadow, clBtnHighlight,
     cl3DDkShadow, cl3DLight, clInfoText, clInfoBk);

type
  TRCCOption = (ccSystemColors, ccUserDefinedColor, ccUserColorDialog);
  TRCCOptions = set of TRCCOption;

type
  TRColorStorage = class
  private
    fColor: TColor;
  public
    constructor Create(const aColor: TColor);
    property Color: TColor read fColor write fColor;
  end;

  TRColorCombo = class (TCustomComboBox)
  private
    fColorUser: Integer;
    fDisplayNames: Boolean;
    fOptions: TRCCOptions;
    fOnChange: TNotifyEvent;
    function  GetColorValue: TColor;
    procedure SetColorValue(aValue: TColor);
    procedure SetDisplayNames(aValue: Boolean);
    procedure SetOptions(aValue: TRCCOptions);
  protected
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CreateWnd; override;
    procedure PopulateList;
    procedure InsertUserColor(aValue: TColor);
    procedure DefineUserColor;
    procedure Click; override;
    procedure DoChange; dynamic;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property ColorValue: TColor read GetColorValue write SetColorValue default clBlack;
    property DisplayNames: Boolean read fDisplayNames write SetDisplayNames default True;
    property Options: TRCCOptions read fOptions write SetOptions;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    // property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

function GetColorName(const Color: TColor): string;

implementation

uses
  Themes, SysUtils, Dialogs, RavClrStrings;

{ == Utilites ================================================================== }
function GetColorName(const Color: TColor): string;
begin
  case Color of
    clBlack: Result := SBlack;
    clMaroon: Result := SMaroon;
    clGreen: Result := SGreen;
    clOlive: Result := SOlive;
    clNavy: Result := SNavy;
    clPurple: Result := SPurple;
    clTeal: Result := STeal;
    clRed: Result := SRed;
    clLime: Result := SLime;
    clYellow: Result := SYellow;
    clBlue: Result := SBlue;
    clFuchsia: Result := SFuchsia;
    clAqua: Result := SAqua;
    clGray: Result := SGray;
    clSilver: Result := SSilver;
    clWhite: Result := SWhite;
    clMoneyGreen: Result := SMoneyGreen;
    clSkyBlue: Result := SSkyBlue;
    clCream: Result := SCream;
    clMedGray: Result := SMedGray;
    clScrollBar: Result := SScrollBar;
    clBackground: Result := SBackground;
    clActiveCaption: Result := SActiveCaption;
    clInactiveCaption: Result := SInactiveCaption;
    clMenu: Result := SMenu;
    clWindow: Result := SWindow;
    clWindowFrame: Result := SWindowFrame;
    clMenuText: Result := SMenuText;
    clWindowText: Result := SWindowText;
    clCaptionText: Result := SCaptionText;
    clActiveBorder: Result := SActiveBorder;
    clInactiveBorder: Result := SInactiveBorder;
    clAppWorkSpace: Result := SAppWorkSpace;
    clHighlight: Result := SHighlight;
    clHighlightText: Result := SHighlightText;
    clBtnFace: Result := SBtnFace;
    clBtnShadow: Result := SBtnShadow;
    clGrayText: Result := SGrayText;
    clBtnText: Result := SBtnText;
    clInactiveCaptionText: Result := SInactiveCaptionText;
    clBtnHighlight: Result := SBtnHighlight;
    cl3DDkShadow: Result := S3DDkShadow;
    cl3DLight: Result := S3DLight;
    clInfoText: Result := SInfoText;
    clInfoBk: Result := SInfoBk;
    else Result := SUserDefined;
  end;
end;

{ == TRColorStorage ============================================================ }

constructor TRColorStorage.Create(const aColor: TColor);
begin
  inherited Create;
  fColor := aColor;
end;

{ == TRColorCombo ============================================================== }

constructor TRColorCombo.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Style := csOwnerDrawFixed;
  ParentCtl3D := False;
  fDisplayNames := True;
  fColorUser := -1;
  fOptions := [ccSystemColors, ccUserDefinedColor, ccUserColorDialog];
end;

procedure TRColorCombo.SetStyle(Value: TComboBoxStyle);
begin
  inherited SetStyle(csOwnerDrawFixed);
end;

procedure TRColorCombo.SetDisplayNames(aValue: Boolean);
begin
  if fDisplayNames <> aValue then
  begin
    fDisplayNames := aValue;
    Invalidate;
  end;
end;

procedure TRColorCombo.SetOptions(aValue: TRCCOptions);
begin
  if fOptions <> aValue then
  begin
    fOptions := aValue;
    if not (csLoading in ComponentState) then
      RecreateWnd;
  end;
end;

procedure TRColorCombo.CreateWnd;
begin
  inherited CreateWnd;
  PopulateList;
  SetColorValue(ColorValue);
end;

procedure TRColorCombo.PopulateList;
var
  i, MaxColors: Integer;
begin
  Items.BeginUpdate;
  try
    Clear;
    if ccSystemColors in fOptions
    then MaxColors := ColorsInListExtended - 1
    else MaxColors := ColorsInListBase - 1;
    for i := 0 to MaxColors do
      Items.AddObject(GetColorName(ColorsList[i]), TRColorStorage.Create(ColorsList[i]));
    if ccUserDefinedColor in fOptions then
      InsertUserColor(ColorValue);
  finally
    Items.EndUpdate;
  end;
end;

procedure TRColorCombo.InsertUserColor(aValue: TColor);
begin
  Items.BeginUpdate;
  try
    if fColorUser > -1
    then TRColorStorage(Items.Objects[fColorUser]).Color := aValue
    else fColorUser := Items.AddObject(SUserDefined, TRColorStorage.Create(aValue));
  finally
    Items.EndUpdate;
  end;
end;

procedure TRColorCombo.DefineUserColor;
var
  Dialog: TColorDialog;
begin
  Dialog := TColorDialog.Create(Self);
  try
    Dialog.Options := [cdFullOpen];
    if fColorUser > -1
    then Dialog.Color := TRColorStorage(Items.Objects[fColorUser]).Color
    else Dialog.Color := clBlack;
    if Dialog.Execute then InsertUserColor(Dialog.Color);
  finally
    Dialog.Free;
  end;
end;

procedure TRColorCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  ColorWidth = 22;
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  ARect := Rect;
  Inc(ARect.Top, 2);
  Inc(ARect.Left, 2);
  Dec(ARect.Bottom, 2);
  if fDisplayNames then ARect.Right := ARect.Left + ColorWidth
  else Dec(ARect.Right, 2);
  with Canvas do begin
    FillRect(Rect);
    Safer := Brush.Color;
    if (odSelected in State) then Pen.Color := clWhite else Pen.Color := clBlack;
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    Brush.Color := TRColorStorage(Items.Objects[Index]).Color;
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect);
    finally
      Brush.Color := Safer;
    end;
    if fDisplayNames then
    begin
      StrPCopy(Text, Items[Index]);
      Rect.Left := Rect.Left + ColorWidth + 6;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
    end;
  end;
end;

procedure TRColorCombo.Click;
begin
  if (ItemIndex > -1) and (ItemIndex < Items.Count) then
    ColorValue := TRColorStorage(Items.Objects[ItemIndex]).Color;
  inherited Click;
end;

procedure TRColorCombo.DoChange;
begin
  if not (csReading in ComponentState) then
    if Assigned(FOnChange) then FOnChange(Self);
end;

function TRColorCombo.GetColorValue: TColor;
begin
  if (ItemIndex > -1) and (ItemIndex < Items.Count)
  then Result := TRColorStorage(Items.Objects[ItemIndex]).Color
  else Result := clBlack;
end;

procedure TRColorCombo.SetColorValue(aValue: TColor);
var
  i: Integer;
begin
  if (ItemIndex > -1) and (ItemIndex = fColorUser)
  and (ccUserDefinedColor in fOptions) then
  begin
    if not (csDesigning in ComponentState)
    and (ccUserColorDialog in fOptions)
    then DefineUserColor
    else InsertUserColor(aValue);
    ItemIndex := fColorUser;
    DoChange;
  end
  else begin
    for i := 0 to Items.Count - 1 do
    begin
      if TRColorStorage(Items.Objects[i]).Color = aValue then
      begin
        if ItemIndex <> i then ItemIndex := i;
        DoChange;
        Exit;
      end;
    end;
    if ccUserDefinedColor in fOptions then
    begin
      InsertUserColor(aValue);
      ItemIndex := fColorUser;
      DoChange;
    end
    else ItemIndex := 0;
  end;
end;

end.
