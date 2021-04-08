unit RMsFileLog;

interface

uses
  SysUtils;

type
  TMsMsgType = (mtNone, mtInfo, mtWarning, mtError);

procedure SaveToMsLogFile(const FileName: string; MsgText: string; const MsgType: TMsMsgType;
  const Component, Context, SourceFile: string; const ThreadId: Cardinal; const TimeStamp: TDateTime);

implementation

const
  fmtMsLogLine = '<![LOG[%s]LOG]!><time="%s" date="%s" component="%s" context="%s" type="%d" thread="%d" file="%s">';

procedure SaveToMsLogFile(const FileName: string; MsgText: string; const MsgType: TMsMsgType;
  const Component, Context, SourceFile: string; const ThreadId: Cardinal; const TimeStamp: TDateTime);
var
  sDate, sTime: string;
  fLog: TextFile;
begin
  DateTimeToString(sTime, 'hh:nn:ss.zzz+-180', TimeStamp);
  DateTimeToString(sDate, 'mm-dd-yyyy', TimeStamp);
  AssignFile(fLog, FileName);
  if FileExists(FileName) then Append(fLog) else Rewrite(fLog);
  try
    WriteLn(fLog, Format(fmtMsLogLine, [MsgText, sTime, sDate,
      Component, Context, Integer(MsgType), ThreadId, SourceFile]));
  finally
    CloseFile(fLog);
  end;
end;

end.
