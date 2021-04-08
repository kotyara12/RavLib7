unit RMsOutlook;

interface

{ == Получение OLE - имени MsOutlook =========================================== }
function  GetMsOutlookOleName: string;
{ == Открываем MsOutlook через OLE ============================================= }
function  ConnectToMsOutlook: Boolean;
{ == Отключаем MsOutlook через OLE ============================================= }
procedure DisconnectOutlook;
{ == Создать письмо в Outlook ================================================== }
function  CreateMail(const ASubject, AReciptients, ABody: string): Variant;

const
  olMailItem        = 0;
  olAppointmentItem = 1;
  olContactItem     = 2;
  olTaskItem        = 3;
  olJournalItem     = 4;
  olNoteItem        = 5;
  olPostItem        = 6;

  olByValue         = 1;
  olByReference     = 4;
  olEmbeddedItem    = 5;
  olOLE             = 6;

resourcestring
  SErrConnectMsOutlook        = 'Ошибка подключения к Microsoft Outlook!';
  SMsgCreateMailItem          = 'Создание почтового сообщения...';
  SErrCreateMailItem          = 'Ошибка создания почтового сообщения!';

var
  MsOutlook: Variant;

implementation

uses
  Classes, Registry, Windows, Variants, ComObj, SysUtils,
  RExHandlers;

const
  OutlookOleNameDefault       = 'Outlook.Application';
  OutlookOleNameRegistryKey   = '\Outlook.Application\CurVer';

{ == Получение OLE - имени MsOutlook =========================================== }
function GetMsOutlookOleName: string;
var
  RegData: TRegistry;
begin
  Result := OutlookOleNameDefault;
  RegData := TRegistry.Create;
  RegData.RootKey := HKEY_CLASSES_ROOT;
  try
    if RegData.OpenKey(OutlookOleNameRegistryKey, False)
    then
      begin
        Result := RegData.ReadString('');
        RegData.CloseKey;
      end;
  finally
    RegData.Free;
  end;
end;

{ == Открываем MsOutlook через OLE ============================================= }
function ConnectToMsOutlook: Boolean;
begin
  try
    if not VarIsEmpty(MsOutlook) then DisconnectOutlook;
    try
      MsOutlook := CreateOleObject(GetMsOutlookOleName);
    except
      on E: Exception do
        HandleExcept(E, nil, SErrConnectMsOutlook);
    end;
  finally
    Result := not VarIsEmpty(MsOutlook);
  end;
end;

{ == Отключаем MsOutlook через OLE ============================================= }
procedure DisconnectOutlook;
begin
  // MsOutlook.Visible := True;
  VarClear(MsOutlook);
end;

{ == Создать письмо в Outlook ================================================== }
function CreateMail(const ASubject, AReciptients, ABody: string): Variant;
begin
  VarClear(Result);
  try
    if not VarIsEmpty(MsOutlook) or ConnectToMsOutlook then
    begin
      Result := MsOutlook.CreateItem(olMailItem);
      if not VarIsEmpty(Result) then
      begin
        Result.Subject := ASubject;
        if AReciptients <> '' then Result.Recipients.Add(AReciptients);
        if ABody <> '' then Result.Body := ABody;
        try
          Result.Display;
        except
        end;
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, nil, SErrCreateMailItem);
  end;
end;

end.
