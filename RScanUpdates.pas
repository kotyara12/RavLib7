unit RScanUpdates;

interface

type
  TScanUpdatesKey = packed record
    RootKey: string[255];
    DescrValue: string[32];
    TypeValue: string[32];
    InstDateValue: string[32];
    InstDateFmt: string[10];
    InstDateUser: string[32];
  end;

  TScanUninstallKey = packed record
    RootKey: string[255];
    DescrValue: string[32];
    TypeValue: string[32];
    VersionValue: string[32];
    InstDateValue: string[32];
    InstDateFmt: string[10];
  end;

  TScanUpdData = packed record
    Masks: string[64];
    UpdatesParams: TScanUpdatesKey;
    UninstallParams: TScanUninstallKey;
  end;

  TUpdateData = packed record
    NameId: string[16];
    NameFull: string[64];
    Description: string[128];
    Version: string[24];
    UpdateType: string[32];
    ProductType: string[32];
    InstallDate: TDateTime;
    InstallUser: string[32];
  end;

  TUpdatesList = array of TUpdateData;

procedure ScanUpdates(const Params: TScanUpdData; var UpdatesList: TUpdatesList);

implementation

uses
  SysUtils, Classes, Registry, Windows,
  RxStrUtils, RVclUtils, RRegUtils, RDateUtils, RWinVer, RDialogs;

const
  NameIdDivs = [#32, '-', '.', ';'];

procedure InitUpdateData(var UpdateData: TUpdateData);
begin
  with UpdateData do
  begin
    NameId := EmptyStr;
    NameFull := EmptyStr;
    Description := EmptyStr;
    Version := EmptyStr;
    UpdateType := EmptyStr;
    ProductType := EmptyStr;
    InstallDate := 0;
    InstallUser := EmptyStr;
  end;
end;

function CheckMasks(const Str, Masks: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to WordCount(Masks, chDivChars) do
  begin
    Result := IsWild(Str, Trim(ExtractWord(i, Masks, chDivChars)), True);
    if Result then Break;
  end;
end;

procedure ScanUpdatesKey(const Params: TScanUpdatesKey; const Masks: string;
  var UpdatesList: TUpdatesList);
var
  Reg: TRegistry;
  RegRootKey: HKEY;
  RegRelativeKey: string;

  procedure ScanSubKeys(const RegKey: string);
  var
    SubKeys: TStringList;
    UpdBuf: TUpdateData;
    i, FoundIndex: Integer;
    ProductStr: string;
  begin
    SubKeys := TStringList.Create;
    try
      FoundIndex := intDisable;
      // Инициализируем буфер
      InitUpdateData(UpdBuf);
      // Открываем раздел реестра
      if Reg.OpenKeyReadOnly(RegKey) then
      begin
        try
          UpdBuf.NameFull := Trim(ExtractWord(WordCount(RegKey, [RegDivChar]), RegKey, [RegDivChar]));
          UpdBuf.NameId := Trim(ExtractWord(1, UpdBuf.NameFull, NameIdDivs));
          UpdBuf.UpdateType := Trim(Reg.ReadString(Params.TypeValue));
          if CheckMasks(UpdBuf.NameId, Masks) and (UpdBuf.UpdateType <> EmptyStr) then
          begin
            UpdBuf.Description := Reg.ReadString(Params.DescrValue);
            ProductStr := RegKey;
            Delete(ProductStr, 1, Length(RegRelativeKey) + 1);
            UpdBuf.ProductType := ExtractWord(1, ProductStr, [RegDivChar]);
            UpdBuf.InstallUser := Reg.ReadString(Params.InstDateUser);
            try
              UpdBuf.InstallDate := StrToDateFmt(Reg.ReadString(Params.InstDateValue), Params.InstDateFmt);
            except
              UpdBuf.InstallDate := 0;
            end;
            if Length(UpdatesList) > 0 then
              for i := Low(UpdatesList) to High(UpdatesList) do
                if SameText(UpdBuf.NameId, UpdatesList[i].NameId) then
                begin
                  FoundIndex := i;
                  Break;
                end;
            if FoundIndex = intDisable then
            begin
              SetLength(UpdatesList, Length(UpdatesList) + 1);
              FoundIndex := High(UpdatesList);
            end;
            if FoundIndex > intDisable then
            begin
              if UpdBuf.NameId <> EmptyStr then
                UpdatesList[FoundIndex].NameId := UpdBuf.NameId;
              if UpdBuf.NameFull <> EmptyStr then
                UpdatesList[FoundIndex].NameFull := UpdBuf.NameFull;
              if UpdBuf.Description <> EmptyStr then
                UpdatesList[FoundIndex].Description := UpdBuf.Description;
              if UpdBuf.UpdateType <> EmptyStr then
                UpdatesList[FoundIndex].UpdateType := UpdBuf.UpdateType;
              if UpdBuf.ProductType <> EmptyStr then
                UpdatesList[FoundIndex].ProductType := UpdBuf.ProductType;
              if UpdBuf.InstallDate > 0 then
                UpdatesList[FoundIndex].InstallDate := UpdBuf.InstallDate;
              if UpdBuf.InstallUser <> EmptyStr then
                UpdatesList[FoundIndex].InstallUser := UpdBuf.InstallUser;
            end;
          end;
          if FoundIndex = intDisable then Reg.GetKeyNames(SubKeys);
        finally
          Reg.CloseKey;
        end;
        for i := 0 to SubKeys.Count - 1 do
          ScanSubKeys(RegKey + RegDivChar + SubKeys[i]);
      end;
    finally
      SubKeys.Free;
    end;
  end;

begin
  if Params.RootKey <> EmptyStr then
  begin
    Reg := TRegistry.Create;
    try
      RegRootKey := Reg.RootKey;
      ExtractRootKey(Params.RootKey, RegRootKey, RegRelativeKey);
      Reg.RootKey := RegRootKey;
      ScanSubKeys(RegRelativeKey);
    finally
      Reg.Free;
    end;
  end;
end;

procedure ScanUninstallKey(const Params: TScanUninstallKey; const Masks: string;
  var UpdatesList: TUpdatesList);
var
  Reg: TRegistry;
  RegRootKey: HKEY;
  RegRelativeKey: string;
  UninstallList: TStringList;
  i: Integer;

  procedure ScanSubKeys(const RegKey: string);
  var
    UpdBuf: TUpdateData;
    i, FoundIndex: Integer;
  begin
    FoundIndex := intDisable;
    InitUpdateData(UpdBuf);
    if Reg.OpenKeyReadOnly(RegKey) then
    begin
      try
        UpdBuf.NameFull := Trim(ExtractWord(WordCount(RegKey, [RegDivChar]), RegKey, [RegDivChar]));
        UpdBuf.NameId := Trim(ExtractWord(1, UpdBuf.NameFull, NameIdDivs));
        UpdBuf.UpdateType := Trim(Reg.ReadString(Params.TypeValue));
        if CheckMasks(UpdBuf.NameId, Masks) and (UpdBuf.UpdateType <> EmptyStr) then
        begin
          UpdBuf.Description := Reg.ReadString(Params.DescrValue);
          UpdBuf.Version := Reg.ReadString(Params.VersionValue);
          try
            UpdBuf.InstallDate := StrToDateFmt(Reg.ReadString(Params.InstDateValue), Params.InstDateFmt);
          except
            UpdBuf.InstallDate := 0;
          end;
          if UpdBuf.Description <> EmptyStr then
          begin
            if Length(UpdatesList) > 0 then
              for i := Low(UpdatesList) to High(UpdatesList) do
                if SameText(UpdBuf.NameId, UpdatesList[i].NameId) then
                begin
                  FoundIndex := i;
                  Break;
                end;
            if FoundIndex = intDisable then
            begin
              SetLength(UpdatesList, Length(UpdatesList) + 1);
              FoundIndex := High(UpdatesList);
            end;
            if FoundIndex > intDisable then
            begin
              if (UpdBuf.NameId <> EmptyStr)
              and (UpdatesList[FoundIndex].NameId = EmptyStr) then
                UpdatesList[FoundIndex].NameId := UpdBuf.NameId;
              if (UpdBuf.NameFull <> EmptyStr)
              and (UpdatesList[FoundIndex].NameFull = EmptyStr) then
                UpdatesList[FoundIndex].NameFull := UpdBuf.NameFull;
              if (UpdBuf.Description <> EmptyStr)
              and (UpdatesList[FoundIndex].Description = EmptyStr) then
                UpdatesList[FoundIndex].Description := UpdBuf.Description;
              if UpdatesList[FoundIndex].ProductType = EmptyStr then
                UpdatesList[FoundIndex].ProductType :=
                  GetWindowsVersion(GetWindowsVersionData, 'Windows %4:s');
              if UpdBuf.Version <> EmptyStr then
                UpdatesList[FoundIndex].Version := UpdBuf.Version;
              if UpdBuf.UpdateType <> EmptyStr then
                UpdatesList[FoundIndex].UpdateType := UpdBuf.UpdateType;
              if UpdBuf.InstallDate > 0 then
                UpdatesList[FoundIndex].InstallDate := UpdBuf.InstallDate;
            end;
          end;
        end;
      finally
        Reg.CloseKey;
      end;
    end;
  end;

begin
  if Params.RootKey <> EmptyStr then
  begin
    Reg := TRegistry.Create;
    try
      RegRootKey := Reg.RootKey;
      ExtractRootKey(Params.RootKey, RegRootKey, RegRelativeKey);
      Reg.RootKey := RegRootKey;
      UninstallList := TStringList.Create;
      try
        if Reg.OpenKeyReadOnly(RegRelativeKey) then
        begin
          try
            Reg.GetKeyNames(UninstallList);
          finally
            Reg.CloseKey;
          end;
        end;
        for i := 0 to UninstallList.Count - 1 do
          ScanSubKeys(RegRelativeKey + RegDivChar + UninstallList[i]);
      finally
        UninstallList.Free;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure ScanUpdates(const Params: TScanUpdData; var UpdatesList: TUpdatesList);
begin
  if Params.Masks <> EmptyStr then
  begin
    ScanUpdatesKey(Params.UpdatesParams, Params.Masks, UpdatesList);
    ScanUninstallKey(Params.UninstallParams, Params.Masks, UpdatesList);
  end;
end;

end.
