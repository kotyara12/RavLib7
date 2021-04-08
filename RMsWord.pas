unit RMsWord;

interface

uses
  Graphics, RVclUtils;
  
{ == ��������� OLE - ����� MsWord ============================================== }
function  GetMsWordOleName: string;
{ == ��������� MsWord ����� OLE ================================================ }
function  ConnectToMsWord: Boolean;
{ == �������� ������� Word ===================================================== }
procedure ShowMsWord;
{ == �������� ������� Word � ��������� ����� =================================== }
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
  SErrConnectMsWord        = '������ ����������� � Microsoft Word!';

{ == ��������� OLE - ����� MsWord ============================================== }
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

{ == ��������� MsWord ����� OLE ================================================ }
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

{ == �������� ������� Word ===================================================== }
procedure ShowMsWord;
begin
  if not VarIsEmpty(MsWord) then
    MsWord.Visible := True;
end;

{ == �������� ������� Word � ��������� ����� =================================== }
procedure ShowMsWordAndDiconnect;
begin
  ShowMsWord;
  VarClear(MsWord);
end;

end.
