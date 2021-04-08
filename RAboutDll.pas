unit RAboutDll;

interface

procedure ShowDllAboutBox;

implementation

uses
  SysUtils, Windows, Forms, RMsgRu;

type
  TShowAboutBox     = procedure(const AppHandle: THandle);

const
  SDllName          = 'RAboutBox.dll';
  SShowAboutBox     = 'ShowAboutBox';

procedure ShowDllAboutBox;
var
  DllHandle: Cardinal;
  PShowAboutBox: TShowAboutBox;
begin
  DllHandle := LoadLibrary(SDllName);
  if DllHandle = 0 then
    raise Exception.CreateFmt(SErrLoadLibrary, [SDllName]);
  try
    PShowAboutBox := GetProcAddress(DllHandle, SShowAboutBox);
    if not Assigned(PShowAboutBox) then
        raise Exception.CreateFmt(SErrFindProcedure, [SShowAboutBox, SDllName]);
    PShowAboutBox(Application.Handle);
  finally
    FreeLibrary(DllHandle);
  end;
end;

end.
