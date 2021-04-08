unit RAttachDll;

interface

uses
  RUserRights;

procedure DllRssShowAttachments(const ConnStr: string; {$IFDEF RSS} const User: TUserRights; {$ENDIF}
  const ObjectName, RecordName: string; const ObjectId {$IFDEF RSS}, ViewTag, EditTag {$ENDIF}: Integer;
  const EditEnabled: Boolean);
function DllRssSaveAttachments(const ConnStr: string; {$IFDEF RSS} const User: TUserRights; {$ENDIF}
  const ObjectName, DirName: string; const ObjectId {$IFDEF RSS}, ViewTag, EditTag {$ENDIF}: Integer): Boolean;
function DllRssDeleteAttachments(const ConnStr: string; {$IFDEF RSS} const User: TUserRights; {$ENDIF}
  const ObjectName: string; const ObjectId {$IFDEF RSS}, ViewTag, EditTag {$ENDIF}: Integer): Boolean;

implementation

uses
  SysUtils, Windows, Forms, RVclUtils, RAppStyles, RMsgRu;

type
  TInitDllSmp = function (AppHandle: THandle; const AppStyle: PApplicationStyle;
    const ConnectionStr: PChar): Boolean;
  TInitDllRss = function (AppHandle: THandle; const AppStyle: PApplicationStyle;
    const ConnectionStr: PChar; const ArmTag: Integer; const User: TUserRights): Boolean;
  TShowAttachments = procedure (const ObjectName, RecordName: PChar;
    const ObjectId, ViewTag, EditTag: Integer; const EditEnabled: Boolean);
  TSaveAttachments = function (const ObjectName, DirName: PChar;
    const ObjectId, ViewTag, EditTag: Integer): Boolean;
  TDeleteAttachments = function (const ObjectName: PChar;
    const ObjectId, ViewTag, EditTag: Integer): Boolean;

const
  SDllName           = 'RDbAttachs.dll';
  SInitDllSmp        = 'InitDllSmp';
  SInitDllRss        = 'InitDllRss';
  SShowAttachments   = 'ShowAttachments';
  SSaveAttachments   = 'SaveAttachments';
  SDeleteAttachments = 'DeleteAttachments';

resourcestring
  SMsgShowAttachments = 'Просмотр и редактирование прикрепленных файлов к записи "%s"...';
  SMsgSaveAttachments = 'Сохранение прикрепленных файлов в каталоге "%s"...';

var
  DllHandle: THandle;
  PInitDllSmp: TInitDllSmp;
  PInitDllRss: TInitDllRss;
  PShowAttachments: TShowAttachments;
  PSaveAttachments: TSaveAttachments;
  PDeleteAttachments: TDeleteAttachments;

function LoadLibrary_RDbAttachsDll(const ConnStr: string{$IFDEF RSS}; const User: TUserRights {$ENDIF}): Boolean;
begin
  DllHandle := LoadLibrary(SDllName);
  if DllHandle = INVALID_HANDLE_VALUE then
    raise EDllException.CreateFmt(SErrLoadLibrary, [SDllName]);

  PInitDllSmp := nil;
  PInitDllRss := nil;
  {$IFDEF RSS}
  PInitDllRss := GetProcAddress(DllHandle, SInitDllRss);
  if Assigned(PInitDllRss) then
    PInitDllRss(Application.Handle, ApplicationStyle, PChar(ConnStr), Application.MainForm.Tag, User)
  else
    raise EDllException.CreateFmt(SErrFindProcedure, [SInitDllRss, SDllName]);
  {$ELSE}
  PInitDllSmp := GetProcAddress(DllHandle, SInitDllSmp);
  if Assigned(PInitDllSmp) then
    PInitDllSmp(Application.Handle, ApplicationStyle, PChar(ConnStr))
  else
    raise EDllException.CreateFmt(SErrFindProcedure, [SInitDllSmp, SDllName]);
  {$ENDIF}

  PShowAttachments := GetProcAddress(DllHandle, SShowAttachments);
  PSaveAttachments := GetProcAddress(DllHandle, SSaveAttachments);
  PDeleteAttachments := GetProcAddress(DllHandle, SDeleteAttachments);
  Result := True;
end;

procedure FreeLibrary_RDbAttachsDll;
begin
  if DllHandle <> INVALID_HANDLE_VALUE then
  begin
    FreeLibrary(DllHandle);
    PInitDllSmp := nil;
    PInitDllRss := nil;
    PShowAttachments := nil;
    PSaveAttachments := nil;
    PDeleteAttachments := nil;
  end;
end;

procedure DllRssShowAttachments(const ConnStr: string; {$IFDEF RSS} const User: TUserRights; {$ENDIF}
  const ObjectName, RecordName: string; const ObjectId {$IFDEF RSS}, ViewTag, EditTag {$ENDIF}: Integer;
  const EditEnabled: Boolean);
begin
  ShowInStatusbar(Format(SMsgShowAttachments, [RecordName]));
  try
    if LoadLibrary_RDbAttachsDll(ConnStr {$IFDEF RSS}, User {$ENDIF}) then
    begin
      if not Assigned(PShowAttachments) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SShowAttachments, SDllName]);
      {$IFDEF RSS}
      PShowAttachments(PChar(ObjectName), PChar(RecordName), ObjectId, ViewTag, EditTag, EditEnabled);
      {$ELSE}
      PShowAttachments(PChar(ObjectName), PChar(RecordName), ObjectId, 0, 0, EditEnabled);
      {$ENDIF}
    end;
  finally
    FreeLibrary_RDbAttachsDll;
    ShowInStatusbar(EmptyStr);
  end;
end;

function DllRssSaveAttachments(const ConnStr: string; {$IFDEF RSS} const User: TUserRights; {$ENDIF}
  const ObjectName, DirName: string; const ObjectId {$IFDEF RSS}, ViewTag, EditTag {$ENDIF}: Integer): Boolean;
begin
  Result := False;
  ShowInStatusbar(Format(SMsgSaveAttachments, [DirName]));
  try
    if LoadLibrary_RDbAttachsDll(ConnStr {$IFDEF RSS}, User {$ENDIF}) then
    begin
      if not Assigned(PSaveAttachments) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SSaveAttachments, SDllName]);
      {$IFDEF RSS}
      Result := PSaveAttachments(PChar(ObjectName), PChar(DirName), ObjectId, ViewTag, EditTag);
      {$ELSE}
      Result := PSaveAttachments(PChar(ObjectName), PChar(DirName), ObjectId, 0, 0);
      {$ENDIF}
    end;
  finally
    FreeLibrary_RDbAttachsDll;
    ShowInStatusbar(EmptyStr);
  end;
end;

function DllRssDeleteAttachments(const ConnStr: string; {$IFDEF RSS} const User: TUserRights; {$ENDIF}
  const ObjectName: string; const ObjectId {$IFDEF RSS}, ViewTag, EditTag {$ENDIF}: Integer): Boolean;
begin
  Result := False;
  try
    if LoadLibrary_RDbAttachsDll(ConnStr {$IFDEF RSS}, User {$ENDIF}) then
    begin
      if not Assigned(PDeleteAttachments) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SDeleteAttachments, SDllName]);
      {$IFDEF RSS}
      Result := PDeleteAttachments(PChar(ObjectName), ObjectId, ViewTag, EditTag);
      {$ELSE}
      Result := PDeleteAttachments(PChar(ObjectName), ObjectId, 0, 0);
      {$ENDIF}
    end;
  finally
    FreeLibrary_RDbAttachsDll;
  end;
end;

end.
