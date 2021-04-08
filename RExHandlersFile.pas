unit RExHandlersFile;

interface

uses
  SysUtils, RExHandlers;

type
  TFileExceptChannel = class (TCustomExceptChannel)
  protected
    function  GetAutoDisabled: Boolean; override;
    function  GetChannelType: TExceptChannelType; override;
    procedure RegisterExceptRecord(const ER: RExceptRecord); override;
  end;

implementation

{ TEvLogExceptChannel }

resourcestring
  LogFmt = '%s: %4.4d.%4.4d - %s';

function TFileExceptChannel.GetAutoDisabled: Boolean;
begin
  Result := False;
end;

function TFileExceptChannel.GetChannelType: TExceptChannelType;
begin
  Result := ecLog;
end;

procedure TFileExceptChannel.RegisterExceptRecord(const ER: RExceptRecord);
var
  N: string;
  F: TextFile;
begin
  N := ChangeFileExt(ParamStr(0), '.err');
  AssignFile(F, N);
  if FileExists(N) then Append(F) else Rewrite(F);
  try
    WriteLn(F, Format(LogFmt, [DateTimeToStr(Now),
      ER.CategoryId, ER.ExceptId, GetErrorLineMessage(ER)]));
  finally
    CloseFile(F);
  end;
end;

end.
