unit RMsWord;

interface

uses
  Graphics, RVclUtils;
  
{ == Получение OLE - имени MsWord ============================================== }
function  GetMsWordOleName: string;
{ == Открываем MsWord через OLE ================================================ }
function  ConnectToMsWord: Boolean;
{ == Показать скрытый Word ===================================================== }
procedure ShowMsWord;
{ == Показать скрытый Word и отключить связь =================================== }
procedure ShowMsWordAndDiconnect;

var
  MsWord: Variant;

implementation

uses
  Classes, Registry, Windows, Variants, ComObj, SysUtils,
  WordConst, RExHandlers;

const
  WordOleNameDefault       = 'Word.Application';
  WordOleNameRegistryKey   = '\Word.Application\CurVer';

resourcestring
  SErrConnectMsWord        = 'Ошибка подключения к Microsoft Word!';

{ == Получение OLE - имени MsWord ============================================== }
function GetMsWordOleName: string;
var
  RegData: TRegistry;
begin
  Result := WordOleNameDefault;
  RegData := TRegistry.Create;
  RegData.RootKey := HKEY_CLASSES_ROOT;
  try
    if RegData.OpenKey(WordOleNameRegistryKey, False)
    then
      begin
        Result := RegData.ReadString('');
        RegData.CloseKey;
      end;
  finally
    RegData.Free;
  end;
end;

{ == Открываем MsWord через OLE ================================================ }
function ConnectToMsWord: Boolean;
begin
  try
    if not VarIsEmpty(MsWord) then ShowMsWordAndDiconnect;
    try
      MsWord := CreateOleObject(GetMsWordOleName);
    except
      on E: Exception do
        HandleExcept(E, nil, SErrConnectMsWord);
    end;
  finally
    Result := not VarIsEmpty(MsWord);
  end;
end;

{ == Показать скрытый Word ===================================================== }
procedure ShowMsWord;
begin
  if not VarIsEmpty(MsWord) then
    MsWord.Visible := True;
end;

{ == Показать скрытый Word и отключить связь =================================== }
procedure ShowMsWordAndDiconnect;
begin
  ShowMsWord;
  VarClear(MsWord);
end;

end.
