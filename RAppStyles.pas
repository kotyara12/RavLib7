unit RAppStyles;

interface

uses
  Forms, Classes, Graphics, RFonts;

type
  TMainFormStyle = record
    TbVisible: Boolean;
    SbVisible: Boolean;
    MdiUnique: Boolean;
  end;

  TDataFormStyle = record
    // Основные элементы окна
    TbVisible: Boolean;
    TbCaptions: Boolean;
    SbVisible: Boolean;
    FastFindPanel: Boolean;
    FastFindGotoData: Boolean;
    FastFindLeadAsterisk: Boolean;
    Ctl3D: Boolean;
    FormColor: TColor;
    ButtonsPanelColor: TColor;
    FormFont: TRFontData;
    // Быстрый поиск
    // Панель управления, навигации (дерево)
    TreeColor: TColor;
    TreeFont: TRFontData;
    // Данные
    DataColor: TColor;
    DataFont: TRFontData;
    DataPositive: TColor;
    DataNegative: TColor;
    CellPositive: TColor;
    CellNegative: TColor;
    // Панель детальной информации
    InfoPanelVisible: Boolean;
    InfoPanelColor: TColor;
    InfoTextColor: TColor;
    InfoLabelFont: TRFontData;
    InfoTextFont: TRFontData;
  end;

  PApplicationStyle = ^TApplicationStyle;
  TApplicationStyle = record
    MainForm: TMainFormStyle;
    DataForm: TDataFormStyle;
  end;

var
  ApplicationStyle: PApplicationStyle;

procedure InitStyles(var Style: PApplicationStyle);

implementation

uses
  RDialogs;

procedure InitMainFormStyle(var Style: TMainFormStyle);
begin
  with Style do
  begin
    TbVisible := False;
    SbVisible := True;
    MdiUnique := True;
  end;
end;

procedure InitDataFormStyle(var Style: TDataFormStyle);
var
  DefFont: TFont;
begin
  if Assigned(Application.MainForm)
  then DefFont := Application.MainForm.Font
  else DefFont := Screen.IconFont;
  with Style do
  begin
    TbVisible := True;
    TbCaptions := True;
    SbVisible := True;
    FastFindPanel := True;
    FastFindGotoData := True;
    FastFindLeadAsterisk := False;
    Ctl3D := True;
    FormColor := clBtnFace;
    ButtonsPanelColor := clWindow;
    FormFont := FontToFontData(DefFont);
    TreeColor := clWindow;
    TreeFont := FontToFontData(DefFont);
    DataColor := clWindow;
    DataFont := FontToFontData(DefFont);
    DataPositive := clWindowText;
    DataNegative := clRed;
    CellPositive := $00EAFFEA;
    CellNegative := $00FDEBFE;
    InfoPanelVisible := True;
    InfoPanelColor := clInfoBk;
    InfoTextColor := clBtnFace;
    InfoLabelFont := FontToFontData(DefFont);
    InfoTextFont := FontToFontData(DefFont);
  end;
end;

procedure InitStyles(var Style: PApplicationStyle);
begin
  InitMainFormStyle(Style^.MainForm);
  InitDataFormStyle(Style^.DataForm);
end;

initialization
  if not IsLibrary then
  begin
    New(ApplicationStyle);
    InitStyles(ApplicationStyle);
  end
  else ApplicationStyle := nil;

finalization
  if not IsLibrary and Assigned(ApplicationStyle) then
  begin
    Dispose(ApplicationStyle);
  end;
  ApplicationStyle := nil;

end.
