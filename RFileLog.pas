unit RFileLog;

interface

uses
  SysUtils;

procedure SaveToLogFile(const FileName: string; MsgText: string);

implementation

procedure SaveToLogFile(const FileName: string; MsgText: string);
var
  fLog: TextFile;
begin
  AssignFile(fLog, FileName);
  if FileExists(FileName) then Append(fLog) else Rewrite(fLog);
  try
    WriteLn(fLog, MsgText);
  finally
    CloseFile(fLog);
  end;
end;

end.
