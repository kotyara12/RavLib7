unit RFonts;

interface

uses
  Graphics;

type
  TRFontData = record
    Name: ShortString;
    Color: TColor;
    Charset: TFontCharset;
    Size: Integer;
    Pitch: TFontPitch;
    Style: TFontStyles;
  end;

function  FontToFontData(Font: TFont): TRFontData;
procedure FontDataToFont(const Data: TRFontData; Font: TFont);
procedure FontDataToFontNoStyled(const Data: TRFontData; Font: TFont);
procedure FontDataToFontNoSized(const Data: TRFontData; Font: TFont);

function  FontStylesToString(Styles: TFontStyles): string;
function  StringToFontStyles(const Styles: string): TFontStyles;

function  FontDataToString(const Font: TRFontData): string;
procedure StringToFontData(const Str: string; var Font: TRFontData);

implementation

uses
  SysUtils, Classes, RxStrUtils;

type
  THackFont = class(TFont);

const
  tfBold      = 'B';
  tfItalic    = 'I';
  tfUnderline = 'U';
  tfStrikeOut = 'S';

function FontToFontData(Font: TFont): TRFontData;
begin
  Result.Name := Font.Name;
  Result.Color := Font.Color;
  Result.Charset := Font.Charset;
  Result.Size := Font.Size;
  Result.Pitch := Font.Pitch;
  Result.Style := Font.Style;
end;

procedure FontDataToFont(const Data: TRFontData; Font: TFont);
var
  FontChange: TNotifyEvent;
begin
  FontChange := Font.OnChange;
  Font.OnChange := nil;
  try
    Font.Name := Data.Name;
    Font.Color := Data.Color;
    Font.Charset := Data.Charset;
    Font.Size := Data.Size;
    Font.Pitch := Data.Pitch;
    Font.Style := Data.Style;
  finally
    Font.OnChange := FontChange;
    THackFont(Font).Changed;
  end;
end;

procedure FontDataToFontNoStyled(const Data: TRFontData; Font: TFont);
var
  FontChange: TNotifyEvent;
begin
  FontChange := Font.OnChange;
  Font.OnChange := nil;
  try
    Font.Name := Data.Name;
    Font.Charset := Data.Charset;
    Font.Size := Data.Size;
    Font.Pitch := Data.Pitch;
  finally
    Font.OnChange := FontChange;
    THackFont(Font).Changed;
  end;
end;

procedure FontDataToFontNoSized(const Data: TRFontData; Font: TFont);
var
  FontChange: TNotifyEvent;
begin
  FontChange := Font.OnChange;
  Font.OnChange := nil;
  try
    Font.Name := Data.Name;
    Font.Charset := Data.Charset;
    Font.Pitch := Data.Pitch;
  finally
    Font.OnChange := FontChange;
    THackFont(Font).Changed;
  end;
end;

function FontStylesToString(Styles: TFontStyles): string;
begin
  Result := '';
  if fsBold in Styles then Result := Result + tfBold;
  if fsItalic in Styles then Result := Result + tfItalic;
  if fsUnderline in Styles then Result := Result + tfUnderline;
  if fsStrikeOut in Styles then Result := Result + tfStrikeOut;
end;

function StringToFontStyles(const Styles: string): TFontStyles;
begin
  Result := [];
  if Pos(tfBold, UpperCase(Styles)) > 0 then Include(Result, fsBold);
  if Pos(tfItalic, UpperCase(Styles)) > 0 then Include(Result, fsItalic);
  if Pos(tfUnderline, UpperCase(Styles)) > 0 then Include(Result, fsUnderline);
  if Pos(tfStrikeOut, UpperCase(Styles)) > 0 then Include(Result, fsStrikeOut);
end;

function FontDataToString(const Font: TRFontData): string;
begin
  with Font do
    Result := Format('%s,%d,%s,%d,%d,%d', [Name, Size,
      FontStylesToString(Style), Ord(Pitch), Color, Charset]);
end;

procedure StringToFontData(const Str: string; var Font: TRFontData);
const
  Delims = [',', ';'];
var
  Pos: Integer;
  I: Byte;
  S: string;
begin
  Pos := 1;
  I := 0;
  while Pos <= Length(Str) do begin
    Inc(I);
    S := Trim(ExtractSubstr(Str, Pos, Delims));
    case I of
      1: Font.Name := S;
      2: Font.Size := StrToIntDef(S, Font.Size);
      3: Font.Style := StringToFontStyles(S);
      4: Font.Pitch := TFontPitch(StrToIntDef(S, Ord(Font.Pitch)));
      5: Font.Color := StrToIntDef(S, Font.Color);
      6: Font.Charset := TFontCharset(StrToIntDef(S, Font.Charset));
    end;
  end;
end;

end.
