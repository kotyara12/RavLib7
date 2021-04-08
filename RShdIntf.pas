unit RShdIntf;

interface

uses
  Controls, DateUtils;

const
  BitMonday      = 1;
  BitTuesday     = 2;
  BitWednesday   = 4;
  BitThursday    = 8;
  BitFriday      = 16;
  BitSaturday    = 32;
  BitSunday      = 64;


  SWeekDays: array [1..7] of string = ('ом', 'бр', 'яп', 'вр', 'ор', 'яа', 'бя');
  SWeekDaysDiv = ';';
  FullWeek     = BitMonday + BitTuesday + BitWednesday + BitThursday + BitFriday + BitSaturday + BitSunday;
  WorkWeek     = BitMonday + BitTuesday + BitWednesday + BitThursday + BitFriday;

function IntToTime(const IntTime: Integer): TTime;
function TimeToInt(const Time: TTime): Integer;
function IntToDaysStr(const IntDays: Integer): string;
function IntDayIncluded(const IntDays: Integer; const Day: TDateTime): Boolean;

implementation

uses
  SysUtils, RDialogs;

const
  OfsH      = 10000;
  OfsM      = 100;

function IntToTime(const IntTime: Integer): TTime;
var
  H, M, S: Word;
begin
  H := IntTime div OfsH;
  M := (IntTime - H * OfsH) div OfsM;
  S := IntTime - H * OfsH - M * OfsM;
  Result := EncodeTime(H, M, S, 0);
end;

function TimeToInt(const Time: TTime): Integer;
var
  H, M, S, D: Word;
begin
  DecodeTime(Time, H, M, S, D);
  Result := H * OfsH + M * OfsM + S;
end;

function IntToDaysStr(const IntDays: Integer): string;
begin
  Result := '';
  if (BitMonday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[1];
  if (BitTuesday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[2];
  if (BitWednesday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[3];
  if (BitThursday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[4];
  if (BitFriday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[5];
  if (BitSaturday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[6];
  if (BitSunday and IntDays) > 0 then
    Result := Result + SWeekDaysDiv + SWeekDays[7];
  if Result <> '' then
    Delete(Result, 1, 1);
end;

function IntDayIncluded(const IntDays: Integer; const Day: TDateTime): Boolean;
begin
  case DayOfWeek(Day) of
    1: Result := (BitSunday and IntDays) > 0;
    2: Result := (BitMonday and IntDays) > 0;
    3: Result := (BitTuesday and IntDays) > 0;
    4: Result := (BitWednesday and IntDays) > 0;
    5: Result := (BitThursday and IntDays) > 0;
    6: Result := (BitFriday and IntDays) > 0;
    7: Result := (BitSaturday and IntDays) > 0;
    else Result := False;
  end;
end;

end.
