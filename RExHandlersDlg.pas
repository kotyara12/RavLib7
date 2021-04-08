unit RExHandlersDlg;

interface

uses
  SysUtils, RExHandlers;

type
  TDialogsExceptChannel = class (TCustomExceptChannel)
  protected
    function  GetChannelType: TExceptChannelType; override;
    procedure RegisterExceptRecord(const ER: RExceptRecord); override;
  end;

implementation

uses
  RDialogs;
  
{ == TDialogsExceptChannel ===================================================== }

function TDialogsExceptChannel.GetChannelType: TExceptChannelType;
begin
  Result := ecMessage;
end;

procedure TDialogsExceptChannel.RegisterExceptRecord(const ER: RExceptRecord);
begin
  ErrorBox(FormatLine(GetErrorMessage(ER))
         + FormatLine(GetExceptionMessage(ER))
         + FormatLine(GetObjectInfo(ER))
         + FormatLine(GetSqlCommand(ER))
         + GetExceptionClass(ER));
end;

end.
