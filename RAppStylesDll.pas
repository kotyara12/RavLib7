unit RAppStylesDll;

interface

uses
  RAppStyles;

procedure LoadAndSetStyles(const GlobalIniFile: string);
procedure SaveAndSetStyles(const GlobalIniFile: string; Save: Boolean = True);
function  EditAppStyles: Boolean;
function  EditAppStylesSave(const GlobalIniFile: string): Boolean;

implementation

uses
  SysUtils, Windows, Forms, RVclUtils, RMsgRu;

type
  TLoadStyles = procedure (const GlobalIniFile: PChar; const Style: PApplicationStyle);
  TSaveStyles = procedure (const GlobalIniFile: PChar; const Style: PApplicationStyle);
  TSetStyles  =  procedure (const MainWnd: THandle);
  TEditStyles = function (const AppHandle, MainWnd: THandle; const Style: PApplicationStyle): Boolean;
  TEditStylesSave = function (const AppHandle, MainWnd: THandle;
                             const Style: PApplicationStyle;
                             const GlobalIniFile: PChar): Boolean;

const
  SDllName          = 'RStyles.dll';
  SLoadStyles       = 'DllLoadStyles';
  SSaveStyles       = 'DllSaveStyles';
  SSetStyles        = 'DllSetStyles';
  SEditStyles       = 'DllEditStyles';
  SEditStylesSave   = 'DllEditStylesSave';

var
  DllHandle: Cardinal;
  PLoadStyles: TLoadStyles;
  PSaveStyles: TSaveStyles;
  PSetStyles: TSetStyles;
  PEditStyles: TEditStyles;
  PEditStylesSave: TEditStylesSave;

function LoadLibrary_RStylesDll: Boolean;
begin
  DllHandle := LoadLibrary(SDllName);
  Result := DllHandle > 0;
  if not Result then raise EDllException.CreateFmt(SErrLoadLibrary, [SDllName]);
  PLoadStyles := GetProcAddress(DllHandle, SLoadStyles);
  PSaveStyles := GetProcAddress(DllHandle, SSaveStyles);
  PSetStyles := GetProcAddress(DllHandle, SSetStyles);
  PEditStyles := GetProcAddress(DllHandle, SEditStyles);
  PEditStylesSave := GetProcAddress(DllHandle, SEditStylesSave);
end;

procedure FreeLibrary_RStylesDll;
begin
  if DllHandle > 0 then
  begin
    FreeLibrary(DllHandle);
    PLoadStyles := nil;
    PSaveStyles := nil;
    PSetStyles := nil;
    PEditStyles := nil;
    PEditStylesSave := nil;
  end;
end;

procedure LoadAndSetStyles(const GlobalIniFile: string);
begin
  if LoadLibrary_RStylesDll then begin
    try
      if not Assigned(PLoadStyles) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SLoadStyles, SDllName]);
      if not Assigned(PSetStyles) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SSetStyles, SDllName]);
      PLoadStyles(PChar(GlobalIniFile), ApplicationStyle);
      PSetStyles(Application.MainForm.Handle);
    finally
      FreeLibrary_RStylesDll;
    end;
  end;
end;

procedure SaveAndSetStyles(const GlobalIniFile: string; Save: Boolean = True);
begin
  if LoadLibrary_RStylesDll then begin
    try
      if not Assigned(PSaveStyles) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SSaveStyles, SDllName]);
      if not Assigned(PSetStyles) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SSetStyles, SDllName]);
      if Save then PSaveStyles(PChar(GlobalIniFile), ApplicationStyle);
      PSetStyles(Application.MainForm.Handle);
    finally
      FreeLibrary_RStylesDll;
    end;
  end;
end;

function EditAppStyles: Boolean;
begin
  Result := False;
  if LoadLibrary_RStylesDll then begin
    try
      if not Assigned(PEditStyles) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SEditStyles, SDllName]);
      Result := PEditStyles(Application.Handle, Application.MainForm.Handle, ApplicationStyle);
    finally
      FreeLibrary_RStylesDll;
    end;
  end;
end;

function EditAppStylesSave(const GlobalIniFile: string): Boolean;
begin
  Result := False;
  if LoadLibrary_RStylesDll then begin
    try
      if not Assigned(PEditStylesSave) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SEditStylesSave, SDllName]);
      Result := PEditStylesSave(Application.Handle, Application.MainForm.Handle,
        ApplicationStyle, PChar(GlobalIniFile));
    finally
      FreeLibrary_RStylesDll;
    end;
  end;
end;

end.
