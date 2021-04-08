unit RExHandlersEvLog;

interface

uses
  SvcMgr, Windows, SysUtils, RExHandlers;

type
  TEvLogExceptChannel = class (TCustomExceptChannel)
  protected
    function  GetChannelType: TExceptChannelType; override;
    procedure RegisterExceptRecord(const ER: RExceptRecord); override;
  public
    Service: TService;
  end;

implementation

{ == TDbLogExceptChannel ======================================================= }

function TEvLogExceptChannel.GetChannelType: TExceptChannelType;
begin
  Result := ecEventLog;
end;

procedure TEvLogExceptChannel.RegisterExceptRecord(const ER: RExceptRecord);
var
  Msg: string;
begin
  if Service <> nil then
  begin
    Msg := FormatLine(GetErrorMessage(ER))
         + FormatLine(GetExceptionMessage(ER))
         + FormatLine(GetObjectInfo(ER))
         + FormatLine(GetSqlCommand(ER))
         + GetExceptionClass(ER);
    Msg := Trim(Msg);
    while (Length(Msg) > 0) and (Msg[Length(Msg)] = '.') do
      Msg := Trim(Copy(Msg, 1, Length(Msg) - 1));
    Service.LogMessage(Msg, EVENTLOG_ERROR_TYPE, ER.CategoryId, ER.ExceptId);
  end;
end;

end.
