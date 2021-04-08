unit RRegUtils;

interface

uses
  SysUtils, Windows;

const
  RegDivChar = '\';

procedure ExtractRootKey(const FullPath: string; var RootKey: HKEY; var RelativePath: string);

implementation

uses
  RDialogs;

procedure ExtractRootKey(const FullPath: string; var RootKey: HKEY; var RelativePath: string);
var
  DCP: Integer;
  S1, S2: string;
begin
  RelativePath := FullPath;
  DCP := Pos(RegDivChar, FullPath);
  if DCP > 0 then
  begin
    S1 := AnsiUpperCase(Copy(FullPath, 1, DCP - 1));
    S2 := Copy(FullPath, DCP + 1, Length(FullPath) - DCP);
    if (S1 = 'HKEY_CLASSES_ROOT') or (S1 = 'HKCR') or (S1 = 'HK_CR') then
    begin
      RootKey := HKEY_CLASSES_ROOT;
      RelativePath := S2;
    end;
    if (S1 = 'HKEY_CURRENT_USER') or (S1 = 'HKCU') or (S1 = 'HK_CU') then
    begin
      RootKey := HKEY_CURRENT_USER;
      RelativePath := S2;
    end;
    if (S1 = 'HKEY_LOCAL_MACHINE') or (S1 = 'HKLM') or (S1 = 'HK_LM') then
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      RelativePath := S2;
    end;
    if (S1 = 'HKEY_USERS') or (S1 = 'HKU') or (S1 = 'HK_U') then
    begin
      RootKey := HKEY_USERS;
      RelativePath := S2;
    end;
    if (S1 = 'HKEY_PERFORMANCE_DATA') or (S1 = 'HKPD') or (S1 = 'HK_PD') then
    begin
      RootKey := HKEY_PERFORMANCE_DATA;
      RelativePath := S2;
    end;
    if (S1 = 'HKEY_CURRENT_CONFIG') or (S1 = 'HKCC') or (S1 = 'HK_CC') then
    begin
      RootKey := HKEY_CURRENT_CONFIG;
      RelativePath := S2;
    end;
    if (S1 = 'HKEY_DYN_DATA') or (S1 = 'HKDD') or (S1 = 'HK_DD') then
    begin
      RootKey := HKEY_DYN_DATA;
      RelativePath := S2;
    end;
  end;
end;

end.
