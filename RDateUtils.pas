unit RDateUtils;

interface

uses
  Controls;

// Преобрабование даты из UTC в локальную
function UTCDateTimeToDateTime(aDate: TDateTime): TDateTime;
// Преобрабование даты из формата PHP в локальную
function PhpToDateTime(const sPhpDate: string): TDate;

// Преобразование строки в дату по формату
function StrToDateFmt(const DateStr, DateFmt: string): TDate;
function StrToDateInt(const DateStr: string): TDate;

// Преобразование строки в дату по формату YYYY-MM-DD HH:NN:SS
function StrToDateTimeDef(const sDate: string): TDateTime;

// Получение первого дня в неделе по дате
function GetMondayOnDate(Date: TDate): TDate;
// Получение первого дня в месяце по дате
function GetFirstDayMonthOnDate(Date: TDate): TDate;
// Получение последнего дня в месяце по дате
function GetLastDayMonthOnDate(Date: TDate): TDate;
// Получение первого дня года по дате
function GetFirstDayYearOnDate(Date: TDate): TDate;
// Получение последнего дня года по дате
function GetLastDayYearOnDate(Date: TDate): TDate;

implementation

uses
  SysUtils, DateUtils, Windows;

// Преобрабование даты из UTC в локальную
function UTCDateTimeToDateTime(aDate: TDateTime): TDateTime;
var
  TZI: TTimeZoneInformation;
  LocalTime, UTCTime: TSystemTime;
begin
  GetTimeZoneInformation(TZI);
  DateTimeToSystemTime(aDate, UTCTime);
  SystemTimeToTzSpecificLocalTime(@TZI, UTCTime,LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;

// Преобрабование даты из формата PHP в локальную
function PhpToDateTime(const sPhpDate: string): TDate;
var
  Y, M, D, H, N, S: Word;
begin
  Result := 0;
  if sPhpDate <> EmptyStr then
  begin
    Y := StrToInt(Copy(sPhpDate, 1, 4));
    M := StrToInt(Copy(sPhpDate, 6, 2));
    D := StrToInt(Copy(sPhpDate, 9, 2));
    H := StrToInt(Copy(sPhpDate, 12, 2));
    N := StrToInt(Copy(sPhpDate, 15, 2));
    S := StrToInt(Copy(sPhpDate, 18, 2));

    Result := UTCDateTimeToDateTime(EncodeDate(Y, M, D) + EncodeTime(H, N, S, 0));
  end;
end;

// Преобразование строки в дату по формату
function StrToDateFmt(const DateStr, DateFmt: string): TDate;
var
  FS: TFormatSettings;
  i: Integer;
begin
  GetLocaleFormatSettings(GetSystemDefaultLCID, FS);
  FS.ShortDateFormat := DateFmt;
  for i := 1 to Length(DateFmt) do
    if not (AnsiUpperCase(DateFmt)[i] in ['D', 'M', 'Y']) then
    begin
      FS.DateSeparator := DateFmt[i];
      Break;
    end;
  Result := StrToDate(DateStr, FS);
end;

function StrToDateInt(const DateStr: string): TDate;
begin
  Result := EncodeDate(
    StrToInt(Copy(DateStr, 1, 4)),
    StrToInt(Copy(DateStr, 5, 2)),
    StrToInt(Copy(DateStr, 7, 2)),
    );
end;


// Преобразование строки в дату по формату YYYY-MM-DD HH:NN:SS
function StrToDateTimeDef(const sDate: string): TDateTime;
begin
  Result := EncodeDateTime(
    StrToInt(Copy(sDate, 1, 4)),
    StrToInt(Copy(sDate, 6, 2)),
    StrToInt(Copy(sDate, 9, 2)),
    StrToInt(Copy(sDate, 12, 2)),
    StrToInt(Copy(sDate, 15, 2)),
    StrToInt(Copy(sDate, 18, 2)),
    0);
end;

// Получение первого дня в неделе по дате
function GetMondayOnDate(Date: TDate): TDate;
var
  W: Byte;
begin
  W := DayOfWeek(Date);
  if W = 1
  then Result := IncDay(Date, -6)
  else Result := IncDay(Date, -(W - 2));
end;

// Получение первого дня в месяце по дате
function GetFirstDayMonthOnDate(Date: TDate): TDate;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  D := 1;
  Result := EncodeDate(Y, M, D);
end;

// Получение последнего дня в месяце по дате
function GetLastDayMonthOnDate(Date: TDate): TDate;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  D := DaysInAMonth(Y, M);
  Result := EncodeDate(Y, M, D);
end;

// Получение первого дня года по дате
function GetFirstDayYearOnDate(Date: TDate): TDate;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  D := 1; M := 1;
  Result := EncodeDate(Y, M, D);
end;

// Получение последнего дня года по дате
function GetLastDayYearOnDate(Date: TDate): TDate;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  D := 31; M := 12;
  Result := EncodeDate(Y, M, D);
end;

end.
