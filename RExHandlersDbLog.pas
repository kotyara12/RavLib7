unit RExHandlersDbLog;

interface

uses
  SysUtils, RExHandlers;

type
  TDbLogExceptChannel = class (TCustomExceptChannel)
  protected
    function  GetAutoDisabled: Boolean; override;
    function  GetChannelType: TExceptChannelType; override;
    procedure RegisterExceptRecord(const ER: RExceptRecord); override;
  end;

implementation

uses
  RVclUtils, RDbLog;

const
  SErrorSignature  = '### %s';
  SErrorText       = '### Ошибка "%s" в приложении "%s".';
  SErrorHandle     = 'Ошибка обработки исключительной ситуации в канале TDbLogExceptChannel!';

{ == TDbLogExceptChannel ======================================================= }

function TDbLogExceptChannel.GetAutoDisabled: Boolean;
begin
  Result := True;
end;

function TDbLogExceptChannel.GetChannelType: TExceptChannelType;
begin
  Result := ecDbLog;
end;

procedure TDbLogExceptChannel.RegisterExceptRecord(const ER: RExceptRecord);
begin
  try
    AddToDbLog(tagError, Format(SErrorText, [GetErrorMessage(ER), GetApplicationName]));
    if GetExceptionMessage(ER) <> EmptyStr then AddToDbLog(tagError, Format(SErrorSignature, [GetExceptionMessage(ER)]));
    if GetObjectInfo(ER) <> EmptyStr then AddToDbLog(tagError, Format(SErrorSignature, [GetObjectInfo(ER)]));
    if GetSqlCommand(ER) <> EmptyStr then AddToDbLog(tagError, Format(SErrorSignature, [GetSqlCommand(ER)]));
    AddToDbLog(tagError, Format(SErrorSignature, [GetExceptionClass(ER)]));
  except
    on E: Exception do
      HandleExcept(E, nil, SErrorHandle, 0, 0, ecExcludeDbLog);
  end;
end;

end.
