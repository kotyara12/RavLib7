unit RVclUtils;

interface

uses
  Classes, Controls, SysUtils;

type
  RId2 = record
    iId1: Integer;
    iId2: Integer;
  end;

  RKindId = record
    Id: Integer;
    Kind: Integer;
  end;

  TTextStyle = ^RTextStyle;
  RTextStyle = record
    FontStyle: Integer;
    FontColor: Integer;
    BackColor: Integer;
  end;

  TId              = ^Integer;
  TId2             = ^RId2;
  TKindId          = ^RKindId;

  TByteSet         = set of Byte;

  EDllException    = class(Exception);
  EDLLLoadError    = class(Exception);
  EDLLCantFindProc = class(Exception);

const
  intDisable                   = -1;
  intFieldToColumnWidth        = 6;

  // Форматирование полей записей
  SNullText                    = '<NULL>';
  SBlobText                    = '{DATA}';
  SBlobTextCrc32               = '{DATA [%.8x]}';
  SNoneText                    = '???';
  SFieldText                   = '%s="%s"';

  // Расширения файлов
  SAnyFile                     = '*.*';
  SIniExt                      = '.ini';

  // Разделитель списков "по умолчанию"
  chListDivChar                = ',';
  chDefDivChar                 = ';';
  chDivChars                   = [',',';'];

  // Стандартные коды операций
  tagDbUpdate                  = 9995;
  tagError                     = 9998;

  // Коды изображений
  imBm_Properties              = 9;
  imBm_Folder                  = 26;
  imBm_OpenFolder              = 27;

  imOk                         = 0;
  imCancel                     = 1;
  imSave                       = -1;
  imNew                        = 8;
  imEdit                       = 9;
  imDeleted                    = 10;
  imLink                       = 28;
  imFree                       = 29;

  // Цвета
  clPurpleTone                 = 16770790;
  clYellowTone                 = 14286332;
  clYellowTone200              = 11206655;
  clGreenTone                  = 15400938;
  clGreenTone220               = 14024661;
  clGreenTone210               = 12582847;
  clGreenTone200               = 11206610;
  clRedTone                    = 15395583;
  clRedTone220                 = 14013951;
  clRedTone210                 = 12566527;
  clRedTone200                 = 11184895;
  clOrangeTone                 = 15398911;
  clOrangeTone200              = 11195903;
  clBlueTone                   = 16777194;
  clBlueTone200                = 16755370;
  clCyanTone                   = 16771818;
  clCyanTone200                = 16777130;
  clGrayTone                   = 16053492;

  clYellowDark                 = 6242137;
  clRedDark                    = 128;
  clGreenDark                  = 1329169;

  cEOF                         = #13#10;
  cCR                          = #10;
  cLF                          = #13;
  cTAB                         = #9;

{ == Проверка, включено ли значение в динамический список ====================== }
function ValueInList(const Value: Integer; const ValuesList: array of Integer): Boolean;
{ == Смена курсора на время занятости приложения =============================== }
procedure StartWait;
procedure StopWait;
procedure ExitWait;
procedure PauseWait;
procedure ContiniueWait;
function  IsNotWait: Boolean;
{ == Блокировка элементов управления =========================================== }
procedure ToggleControls(Control: TWinControl; const Enabled: Boolean);
{ == Вывод информации в главную строку статуса приложения ====================== }
procedure ShowInStatusBar(const Msg: string);
procedure ShowInStatusBarPanel(const Panel: Integer; Msg: string);
{ == Задержка выполнения программы ============================================= }
procedure Delay(MSecs: Longint);
{ == Генерация записи RKindId ================================================== }
function  KindId(const AId, AKind: Integer): RKindId;
{ == Правильное преобразование Boolean в строку и обратно ====================== }
function RBoolToStr(const aValue: Boolean): string;
function RStrToBool(const aValue: string): Boolean;
{ == Преобразование строки в число (кривые символы игнорируются) =============== }
function RStrToInt(const sValue: string): Integer;
function RStrToIntDef(const sValue: string; const defValue: Integer): Integer;
function RFloatToStr(const aValue: Extended): string;
function RStrToFloat(const sValue: string): Extended;
function RStrToFloatDef(const sValue: string; const defValue: Extended): Extended;
{ == Преобразование вариантов в число ========================================== }
function RVarToInteger(const VarValue: Variant): Integer;

implementation

uses
  Forms, Windows, StdCtrls, StrUtils, ComCtrls, Variants, RMsgRu, RDialogs;

const
  StatusBarName = 'StatusBar';

var
  iWaitCount, tWaitCount: Integer;

{ == Проверка, включено ли значение в динамический список ====================== }
function ValueInList(const Value: Integer; const ValuesList: array of Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(ValuesList) to High(ValuesList) do
  begin
    Result := Value = ValuesList[i];
    if Result then Break;
  end;
end;

{ == Смена курсора на время занятости приложения =============================== }

procedure StartWait;
begin
  Inc(iWaitCount);
  Screen.Cursor := crHourGlass;
end;

procedure StopWait;
begin
  if iWaitCount > 0 then Dec(iWaitCount);
  if iWaitCount = 0 then Screen.Cursor := crDefault;
end;

procedure ExitWait;
begin
  iWaitCount := 0;
  Screen.Cursor := crDefault;
end;

function IsNotWait: Boolean;
begin
  Result := iWaitCount = 0;
end;

procedure PauseWait;
begin
  tWaitCount := iWaitCount;
  ExitWait;
end;

procedure ContiniueWait;
begin
  iWaitCount := tWaitCount;
  if iWaitCount = 0 then Screen.Cursor := crDefault else Screen.Cursor := crHourGlass;
end;

{ == Блокировка элементов управления =========================================== }
procedure ToggleControls(Control: TWinControl; const Enabled: Boolean);
var
  i: Integer;
begin
  for i := 0 to Control.ControlCount - 1 do
  begin
    if Control.Controls[i] is TWinControl
    then ToggleControls(TWinControl(Control.Controls[i]), Enabled);
    if not (Control.Controls[i] is TLabel) then
      Control.Controls[i].Enabled := Enabled;
  end;
end;

{ == Вывод информации в главную строку статуса приложения ====================== }
procedure ShowInStatusBar(const Msg: string);
var
  Sb: TStatusBar;
begin
  if Assigned(Application.MainForm) then
  begin
    Sb := TStatusBar(Application.MainForm.FindComponent(StatusBarName));
    if Assigned(Sb) then
    begin
      if Sb.SimplePanel
      then Sb.SimpleText := Msg
      else Sb.Panels[Sb.Tag].Text := Msg;
    end;
  end;
  Application.ProcessMessages;
end;

procedure ShowInStatusBarPanel(const Panel: Integer; Msg: string);
var
  Sb: TStatusBar;
begin
  Sb := TStatusBar(Application.MainForm.FindComponent(StatusBarName));
  if Assigned(Sb) then Sb.Panels[Panel].Text := Msg;
  Application.ProcessMessages;
end;

{ == Задержка выполнения программы ============================================= }
procedure Delay(MSecs: Longint);
var
  FirstTickCount, Now: Longint;
begin
  FirstTickCount := GetTickCount;
  repeat
    Application.ProcessMessages;
    Now := GetTickCount;
  until (Now - FirstTickCount >= MSecs) or (Now < FirstTickCount);
end;

{ == Генерация записи RKindId ================================================== }
function KindId(const AId, AKind: Integer): RKindId;
begin
  Result.Id := AId;
  Result.Kind := AKind;
end;

{ == Правильное преобразование Boolean в строку и обратно ====================== }
function RBoolToStr(const aValue: Boolean): string;
begin
  if aValue then Result := '1' else Result := '0';
end;

function RStrToBool(const aValue: string): Boolean;
begin
  Result := aValue = '1';
end;

{ == Преобразование строки в число (кривые символы игнорируются) =============== }
function RStrToInt(const sValue: string): Integer;
var
  i, iCount: Integer;
  bufValue: string;
begin
  bufValue := EmptyStr;

  iCount := Length(sValue);
  for i := 1 to iCount do
  begin
    if sValue[i] in ['-','0'..'9'] then
      bufValue := bufValue + sValue[i]
    else begin
      if not (sValue[i] in [#32, ThousandSeparator]) then
        Break;
    end;
  end;

  Result := StrToInt(bufValue);
end;

function RStrToIntDef(const sValue: string; const defValue: Integer): Integer;
var
  i, iCount: Integer;
  bufValue: string;
begin
  bufValue := EmptyStr;

  iCount := Length(sValue);
  for i := 1 to iCount do
  begin
    if sValue[i] in ['-','0'..'9'] then
      bufValue := bufValue + sValue[i]
    else begin
      if not (sValue[i] in [#32,ThousandSeparator]) then
        Break;
    end;
  end;

  Result := StrToIntDef(bufValue, defValue);
end;

function RFloatToStr(const aValue: Extended): string;
begin
  Result := StringReplace(FloatToStr(aValue), DecimalSeparator, '.', [rfReplaceAll]);
end;

function RStrToFloat(const sValue: string): Extended;
var
  i, iCount: Integer;
  bDecimalSeparator: Boolean;
  bufValue: string;
begin
  bufValue := EmptyStr;
  bDecimalSeparator := False;

  iCount := Length(sValue);
  for i := 1 to iCount do
  begin
    if sValue[i] in ['-','0'..'9'] then
      bufValue := bufValue + sValue[i]
    else if (sValue[i] in ['.',',',DecimalSeparator]) then
    begin
      if not bDecimalSeparator then
      begin
        bufValue := bufValue + DecimalSeparator;
        bDecimalSeparator := True;
      end
      else Break;
    end
    else begin
      if not (sValue[i] in [#32,ThousandSeparator]) then
        Break;
    end;
  end;

  Result := StrToFloat(bufValue);
end;

function RStrToFloatDef(const sValue: string; const defValue: Extended): Extended;
var
  i, iCount: Integer;
  bDecimalSeparator: Boolean;
  bufValue: string;
begin
  bufValue := EmptyStr;
  bDecimalSeparator := False;

  iCount := Length(sValue);
  for i := 1 to iCount do
  begin
    if sValue[i] in ['-','0'..'9'] then
      bufValue := bufValue + sValue[i]
    else if (sValue[i] in ['.',',',DecimalSeparator]) then
    begin
      if not bDecimalSeparator then
      begin
        bufValue := bufValue + DecimalSeparator;
        bDecimalSeparator := True;
      end
      else Break;
    end
    else begin
      if not (sValue[i] in [#32,ThousandSeparator]) then
        Break;
    end;
  end;

  Result := StrToFloatDef(bufValue, DefValue);
end;

{ == Преобразование вариантов в число ========================================== }
function RVarToInteger(const VarValue: Variant): Integer;
begin
  if not VarIsNull(VarValue) and VarIsOrdinal(VarValue) then
    Result := VarValue
  else
    Result := -1
end;

initialization
  iWaitCount := 0;
  tWaitCount := 0;

end.
