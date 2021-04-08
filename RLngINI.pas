unit RLngINI;

interface

uses
  Windows;

function rli_GetLangFile(const iLCID: LCID): string;

implementation

uses
  SysUtils, RLngUtils;

const
  sLangFileMask                = '%s_%s.lng';

function rli_GetLangFile(const iLCID: LCID): string;
begin
  Result := Format(sLangFileMask, [ChangeFileExt(ParamStr(0), EmptyStr), lng_GetLcidAbbr(iLCID)]);
end;

end.
