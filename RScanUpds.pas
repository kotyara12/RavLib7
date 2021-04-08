unit RScanUpds;

interface

type
  TUpdateDataEx = packed record
    sId: string;
    sName: string;
    sType: string;
    sDescr: string;
    sVersion: string;
    sProduct: string;
    sUserInst: string;
    dDateInst: TDateTime;
  end;

  TUpdateListEx = array of TUpdateDataEx;

procedure ScanUpdatesEx(var UpdateList: TUpdateListEx);

implementation

uses
  SysUtils, Registry, Windows, Classes, StrUtils,
  RDateUtils, RDialogs;

const
  sNameKey_Updates_x86     = 'SOFTWARE\Microsoft\Updates';
  sNameKey_Updates_x64     = 'SOFTWARE\Wow6432Node\Microsoft\Updates';
  sNameKey_Uninstall_x86   = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall';
  sNameKey_Uninstall_x64   = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall';

{ ** Init Structure ************************************************************ }

procedure InitUpdateData(var UpdateData: TUpdateDataEx);
begin
  with UpdateData do
  begin
    sId := '';
    sName := '';
    sType := '';
    sDescr := '';
    sVersion := '';
    sProduct := '';
    sUserInst := '';
    dDateInst := 0;
  end;
end;

{ ** Get Values **************************************************************** }

function RegReadDate(Reg: TRegistry; const ValueName: string): TDateTime;
var
  sValue: string;
begin
  sValue := Reg.ReadString(ValueName);
  try
    Result := StrToDateInt(sValue);
  except
    try
      Result := StrToDateFmt(sValue, 'dd.mm.yyyy');
    except
      try
        Result := StrToDateFmt(sValue, 'm/d/yyyy');
      except
        Result := 0;
      end;
    end;
  end;
end;

function ExtractKeyPath(const FullName: string): string;
begin
  Result := Copy(FullName, 1, LastDelimiter('\', FullName) - 1);
end;

function ExtractKeyName(const FullName: string): string;
begin
  Result := Copy(FullName, LastDelimiter('\', FullName) + 1, MaxInt);
end;

function CheckUpdateName(const UpdateId, Description: string): string;
var
  iPosS, iPosE: Integer;
begin
  Result := UpdateId;
  if (Trim(UpdateId) = '') or not AnsiStartsText('KB', UpdateId) then
  begin
    iPosS := Pos('(', Description);
    iPosE := Pos(')', Description);
    if (iPosS > 0) and (iPosS < iPosE) then
      Result := Copy(Description, iPosS + 1, iPosE - iPosS - 1);
  end;
end;

function CheckUpdateType(const UpdateType, Description: string): string;
begin
  Result := UpdateType;
  if (Trim(UpdateType) = '') or AnsiStartsText('%', UpdateType) then
  begin
    if AnsiStartsText('Hotfix', Description) then
      Result := 'Hotfix';
    if AnsiStartsText('Update', Description) then
      Result := 'Update';
    if AnsiStartsText('Security Update', Description) then
      Result := 'Security Update';
    if AnsiStartsText('Обновление', Description) then
      Result := 'Update';
    if AnsiStartsText('Обновление безопасности', Description) then
      Result := 'Security Update';
  end;
end;

function CheckUpdateProduct(const UpdateProduct, Description: string): string;
var
  iPosS, iPosE: Integer;
begin
  Result := UpdateProduct;
  if AnsiSameText(Result, 'OperatingSystem') then
  // and not AnsiContainsText(Description, 'Windows Media') then
    Exit;
  iPosS := Pos('для', Description);
  if iPosS = 0 then
    iPosS := Pos('for', Description);
  iPosE := Pos('(', Description);
  if (iPosS > 0) and (iPosE > iPosS) then
    Result := Copy(Description, iPosS + 4, iPosE - iPosS - 5);
  if Pos('-', Trim(Result)) = Length(Result) then
    Result := Copy(Result, 1, Length(Result) - 2);
  // Замены некорректных выражений
  if AnsiSameText(Result, '2007 Microsoft Office System') then
    Result := 'Microsoft Office System 2007';
  if AnsiSameStr(Result, 'Microsoft Office system 2007') then
    Result := 'Microsoft Office System 2007';
  if AnsiContainsText(Result, 'Windows Media') then
    Result := 'Windows Media';
  if AnsiStartsText('Outlook', Result)
  or AnsiStartsText('Excel', Result)
  or AnsiStartsText('Access', Result)
  or AnsiStartsText('Word', Result)
  or AnsiStartsText('PowerPoint', Result)
  or AnsiStartsText('Publisher', Result)
  or AnsiStartsText('Project', Result)
  or AnsiStartsText('Proof', Result)
  or AnsiStartsText('InfoPath', Result)
  or AnsiStartsText('Shared', Result)
  or AnsiStartsText('Visio', Result) then
    Result := 'Microsoft Office ' + Result;
  // if AnsiStartsText('Microsoft', Result)
  // and not AnsiStartsText('Microsoft Office', Result) then
  //   Result := Copy(Result, 11, Length(Result) - 10);
end;

procedure PutUpdateData(const UpdateData: TUpdateDataEx; var UpdateList: TUpdateListEx);
var
  iIndex: Integer;

  function GetIndex(const UpdateData: TUpdateDataEx; const UpdateList: TUpdateListEx): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := Low(UpdateList) to High(UpdateList) do
    begin
      if SameText(UpdateList[i].sId, UpdateData.sId)
      and (SameText(UpdateList[i].sProduct, UpdateData.sProduct)
        or SameText(UpdateList[i].sProduct, 'OperatingSystem')) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

begin
  // Проверяем, есть ли такой в списке
  iIndex := GetIndex(UpdateData, UpdateList);
  if iIndex = -1 then
  begin
    // ... если нет - добавляем
    SetLength(UpdateList, Length(UpdateList) + 1);
    UpdateList[High(UpdateList)] := UpdateData;
  end
  else begin
    // ... если есть - обновляем ранее незаполненные поля
    if UpdateList[iIndex].sName = EmptyStr then
      UpdateList[iIndex].sName := UpdateData.sName;
    if UpdateList[iIndex].sType = EmptyStr then
      UpdateList[iIndex].sType := UpdateData.sType;
    if UpdateList[iIndex].sDescr = EmptyStr then
      UpdateList[iIndex].sDescr := UpdateData.sDescr;
    if UpdateList[iIndex].sVersion = EmptyStr then
      UpdateList[iIndex].sVersion := UpdateData.sVersion;
    if UpdateList[iIndex].sProduct = EmptyStr then
    // if UpdateData.sProduct <> EmptyStr then
    // if (UpdateData.sProduct <> EmptyStr)
    // and not AnsiContainsText(UpdateData.sDescr, UpdateData.sProduct) then
      UpdateList[iIndex].sProduct := UpdateData.sProduct;
    if UpdateList[iIndex].sUserInst = EmptyStr then
      UpdateList[iIndex].sUserInst := UpdateData.sUserInst;
    if UpdateList[iIndex].dDateInst = 0 then
      UpdateList[iIndex].dDateInst := UpdateData.dDateInst;
  end;
end;

{ ** Main ********************************************************************** }

function GetUpdateData_Updates(Reg: TRegistry; const ProductName, KeyName: string): TUpdateDataEx;
begin
  InitUpdateData(Result);
  if Reg.OpenKeyReadOnly(KeyName) then
  begin
    try
      if Reg.ValueExists('PackageName')
      or Reg.ValueExists('Description') then
      begin
        Result.sId := ExtractKeyName(KeyName);
        Result.sName := Result.sId;
        Result.sProduct := ProductName;
        // Считываем основные данные
        if Reg.ValueExists('Type') then
          Result.sType := Reg.ReadString('Type');
        if Reg.ValueExists('ReleaseType') and (Result.sType = EmptyStr) then
          Result.sType := Reg.ReadString('ReleaseType');

        if Reg.ValueExists('PackageName') then
          Result.sDescr := Reg.ReadString('PackageName');
        if Reg.ValueExists('Description') and (Result.sDescr = EmptyStr) then
          Result.sDescr := Reg.ReadString('Description');

        if Reg.ValueExists('PackageVersion') then
          Result.sVersion := Reg.ReadString('PackageVersion');
        if Reg.ValueExists('DisplayVersion') and (Result.sVersion = EmptyStr) then
          Result.sVersion := Reg.ReadString('DisplayVersion');

        if Reg.ValueExists('InstalledBy') then
          Result.sUserInst := Reg.ReadString('InstalledBy');

        if Reg.ValueExists('InstallDate') then
          Result.dDateInst := RegReadDate(Reg, 'InstallDate');
        if Reg.ValueExists('InstalledDate') and (Result.dDateInst = 0) then
          Result.dDateInst := RegReadDate(Reg, 'InstalledDate');

        // Пробуем считать данные из SP0
        Reg.CloseKey;

        if Reg.OpenKeyReadOnly(ExtractKeyPath(KeyName) + '\SP0\' + ExtractKeyName(KeyName)) then
        begin
          if Reg.ValueExists('Type') then
            Result.sType := Reg.ReadString('Type');

          if Reg.ValueExists('Description') and (Result.sDescr = EmptyStr) then
            Result.sDescr := Reg.ReadString('Description');

          if Reg.ValueExists('InstalledBy') and (Result.sUserInst = EmptyStr) then
            Result.sUserInst := Reg.ReadString('InstalledBy');

          if Reg.ValueExists('InstallDate') and (Result.dDateInst = 0) then
            Result.dDateInst := RegReadDate(Reg, 'InstallDate');
          if Reg.ValueExists('InstalledDate') and (Result.dDateInst = 0) then
            Result.dDateInst := RegReadDate(Reg, 'InstalledDate');
        end;
        // Исправляем некорректные данные
        Result.sId := CheckUpdateName(Result.sId, Result.sDescr);
        Result.sName := CheckUpdateName(Result.sName, Result.sDescr);
        Result.sType := CheckUpdateType(Result.sType, Result.sDescr);
        Result.sProduct := CheckUpdateProduct(Result.sProduct, Result.sDescr);
      end;
    finally
      Reg.CloseKey;
    end;
  end;
end;

function GetUpdateData_Uninstall(Reg: TRegistry; const KeyName: string): TUpdateDataEx;
var
  rsLink: string;
  ruLink: TUpdateDataEx;
begin
  InitUpdateData(Result);
  if Reg.OpenKeyReadOnly(KeyName) then
  begin
    try
      if Reg.ValueExists('ParentKeyName') then
      begin
        Result.sId := ExtractKeyName(KeyName);
        Result.sName := Result.sId;
        // Считываем основные данные
        if Reg.ValueExists('ParentKeyName') then
          Result.sProduct := Reg.ReadString('ParentKeyName');

        if Reg.ValueExists('ReleaseType') then
          Result.sType := Reg.ReadString('ReleaseType');

        if Reg.ValueExists('DisplayName') then
          Result.sDescr := Reg.ReadString('DisplayName');

        if Reg.ValueExists('DisplayVersion') then
          Result.sVersion := Reg.ReadString('DisplayVersion');

        if Reg.ValueExists('InstallDate') then
          Result.dDateInst := RegReadDate(Reg, 'InstallDate');
          
        // Считываем связанные данные
        if Reg.ValueExists('RegistryLocation') then
        begin
          rsLink := AnsiReplaceText(AnsiReplaceText(
            Reg.ReadString('RegistryLocation'),
            'HKLM\', ''), 'HKEY_LOCAL_MACHINE\', '');
          // Закрываем текущий ключ
          Reg.CloseKey;
          // Считываем данные из альтернативного расположения
          ruLink := GetUpdateData_Updates(Reg, Result.sProduct, rsLink);
          // Переносим данные
          if ruLink.sName <> '' then
            Result.sName := ruLink.sName;
          if Result.sType = EmptyStr then
            Result.sType := ruLink.sType;
          if Result.sDescr = EmptyStr then
            Result.sDescr := ruLink.sDescr;
          if Result.sVersion = EmptyStr then
            Result.sVersion := ruLink.sVersion;
          if Result.sProduct = EmptyStr then
            Result.sProduct := ruLink.sProduct;
          if Result.sUserInst = EmptyStr then
            Result.sUserInst := ruLink.sUserInst;
          if Result.dDateInst = 0 then
            Result.dDateInst := ruLink.dDateInst;
        end;
        // Исправляем некорректные данные
        Result.sId := CheckUpdateName(Result.sId, Result.sDescr);
        Result.sName := CheckUpdateName(Result.sName, Result.sDescr);
        Result.sType := CheckUpdateType(Result.sType, Result.sDescr);
        Result.sProduct := CheckUpdateProduct(Result.sProduct, Result.sDescr);
      end;
    finally
      Reg.CloseKey;
    end;
  end;
end;

procedure ScanUpdates_Updates(Reg: TRegistry; const BaseKey: string; var UpdateList: TUpdateListEx);
var
  slProducts: TStringList;
  i: Integer;

  procedure ScanProductKey(const ProductId, ProductKey: string);
  var
    slKeys: TStringList;
    ruData: TUpdateDataEx;
    i: Integer;
  begin
    if Reg.OpenKeyReadOnly(BaseKey + '\' + ProductKey) then
    begin
      // Считывем название вложенных ключей
      slKeys := TStringList.Create;
      try
        try
          Reg.GetKeyNames(slKeys);
        finally
          Reg.CloseKey;
        end;
        for i := 0 to slKeys.Count - 1 do
        begin
          // Пытаемся считать данные об обновлении
          ruData := GetUpdateData_Updates(Reg, ProductId,
            BaseKey + '\' + ProductKey + '\' + slKeys[i]);
          // Если удалось определить ID обновления, обрабатываем его
          if (ruData.sId <> EmptyStr) and (ruData.sType <> EmptyStr) and (ruData.sDescr <> EmptyStr) then
          begin
            // Добавляем полученные значения в список
            ruData.sDescr := ruData.sDescr;
            PutUpdateData(ruData, UpdateList);
          end
          else begin
            // пытаемся проверить вложенные ключи
            ScanProductKey(ProductId, ProductKey + '\' + slKeys[i]);
          end;
        end;
      finally
        slKeys.Free;
      end;
    end;
  end;

begin
  if Reg.OpenKeyReadOnly(BaseKey) then
  begin
    // Считываем предаврительные названия продуктов
    slProducts := TStringList.Create;
    try
      try
        Reg.GetKeyNames(slProducts);
      finally
        Reg.CloseKey;
      end;
      // Сканируем вложенные ключи
      for i := 0 to slProducts.Count - 1 do
        ScanProductKey(slProducts[i], slProducts[i]);
    finally
      slProducts.Free;
    end;
  end;
end;

procedure ScanUpdates_Uninstall(Reg: TRegistry; const BaseKey: string; var UpdateList: TUpdateListEx);
var
  slKeys: TStringList;
  ruData: TUpdateDataEx;
  i: Integer;
begin
  if Reg.OpenKeyReadOnly(BaseKey) then
  begin
    // Считывем название вложенных ключей
    slKeys := TStringList.Create;
    try
      try
        Reg.GetKeyNames(slKeys);
      finally
        Reg.CloseKey;
      end;
      for i := 0 to slKeys.Count - 1 do
      begin
        // Пытаемся считать данные об обновлении
        ruData := GetUpdateData_Uninstall(Reg, BaseKey + '\' + slKeys[i]);
        // Если удалось определить ID обновления и ID продукта, обрабатываем его
        if (ruData.sId <> EmptyStr) and (ruData.sType <> EmptyStr) and (ruData.sDescr <> EmptyStr) then
        begin
          // Добавляем полученные значения в список
          PutUpdateData(ruData, UpdateList);
        end;
      end;
    finally
      slKeys.Free;
    end;
  end;
end;

procedure ScanUpdatesEx(var UpdateList: TUpdateListEx);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    ScanUpdates_Uninstall(Reg, sNameKey_Uninstall_x86, UpdateList);
    ScanUpdates_Uninstall(Reg, sNameKey_Uninstall_x64, UpdateList);
    ScanUpdates_Updates(Reg, sNameKey_Updates_x86, UpdateList);
    ScanUpdates_Updates(Reg, sNameKey_Updates_x64, UpdateList);
  finally
    Reg.Free;
  end;
end;

end.
