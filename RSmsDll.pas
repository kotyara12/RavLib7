unit RSmsDll;

interface

uses
  RAdoUtils, RUserRights;

function  CheckLoadSmsDll: Boolean;
procedure DllReadMessages(const ConnStr: string; const User: TUserRights);
procedure DllSendMessages(const ConnStr: string; const User: TUserRights);
procedure DllViewMessages(const ConnStr: string; const User: TUserRights);

implementation

uses
  SysUtils, Windows, Forms, RVclUtils, RAppStyles, RMsgRu;

type
  TInitDll          = procedure (AppHandle: THandle; const AppStyle: PApplicationStyle;
                        const ConnectionStr: PChar; const ArmTag: Integer; const User: TUserRights);

const
  SDllName          = 'RSms.dll';
  SInitDll          = 'InitDll';
  SReadMessages     = 'ReadMessages';
  SSendMessages     = 'SendMessages';
  SViewMessages     = 'ViewMessages';

var
  DllHandle: THandle;
  PInitDll: TInitDll;
  PReadMessages: procedure;
  PSendMessages: procedure;
  PViewMessages: procedure;

function LoadLibrary_RSmsDll(const ConnStr: string; const User: TUserRights): Boolean;
begin
  DllHandle := LoadLibrary(SDllName);
  if DllHandle = INVALID_HANDLE_VALUE then
    raise EDllException.CreateFmt(SErrLoadLibrary, [SDllName]);
  PInitDll := GetProcAddress(DllHandle, SInitDll);
  if Assigned(PInitDll) then
    PInitDll(Application.Handle, ApplicationStyle, PChar(ConnStr),
    Application.MainForm.Tag, User)
  else
    raise EDllException.CreateFmt(SErrFindProcedure, [SInitDll, SDllName]);
  PReadMessages := GetProcAddress(DllHandle, SReadMessages);
  PSendMessages := GetProcAddress(DllHandle, SSendMessages);
  PViewMessages := GetProcAddress(DllHandle, SViewMessages);
  Result := True;
end;

procedure FreeLibrary_RSmsDll;
begin
  if DllHandle <> INVALID_HANDLE_VALUE then
  begin
    FreeLibrary(DllHandle);
    PInitDll := nil;
    PReadMessages := nil;
    PSendMessages := nil;
    PViewMessages := nil;
  end;
end;

function CheckLoadSmsDll: Boolean;
begin
  DllHandle := LoadLibrary(SDllName);
  Result := DllHandle <> INVALID_HANDLE_VALUE;
  if Result then
    FreeLibrary(DllHandle);
end;

procedure DllReadMessages(const ConnStr: string; const User: TUserRights);
begin
  try
    if LoadLibrary_RSmsDll(ConnStr, User) then
    begin
      if not Assigned(PReadMessages) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SReadMessages, SDllName]);
      PReadMessages;
    end;
  finally
    FreeLibrary_RSmsDll;
  end;
end;

procedure DllSendMessages(const ConnStr: string; const User: TUserRights);
begin
  try
    if LoadLibrary_RSmsDll(ConnStr, User) then
    begin
      if not Assigned(PSendMessages) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SSendMessages, SDllName]);
      PSendMessages;
    end;
  finally
    FreeLibrary_RSmsDll;
  end;
end;

procedure DllViewMessages(const ConnStr: string; const User: TUserRights);
begin
  try
    if LoadLibrary_RSmsDll(ConnStr, User) then
    begin
      if not Assigned(PViewMessages) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SViewMessages, SDllName]);
      PViewMessages;
    end;
  finally
    FreeLibrary_RSmsDll;
  end;
end;

end.
