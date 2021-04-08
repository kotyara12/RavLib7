unit RExHandlersSb;

interface

uses
  SysUtils, RExHandlers;

type
  TStatusExceptChannel = class (TCustomExceptChannel)
  protected
    function  GetChannelType: TExceptChannelType; override;
    procedure RegisterExceptRecord(const ER: RExceptRecord); override;
  end;

implementation

uses
  RVclUtils;

resourcestring
  SStatusError          = 'Œÿ»¡ ¿! %s...';

{ == TStatusExceptChannel ====================================================== }

function TStatusExceptChannel.GetChannelType: TExceptChannelType;
begin
  Result := ecHint;
end;

procedure TStatusExceptChannel.RegisterExceptRecord(const ER: RExceptRecord);
begin
  ShowInStatusBar(Format(SStatusError, [GetErrorLineMessage(ER)]));
end;

end.
