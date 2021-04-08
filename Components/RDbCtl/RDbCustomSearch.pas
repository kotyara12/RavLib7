{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Database Additional Components         }
{                                                       }
{       Copyright (c) 2006-2012 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbCustomSearch;

interface

uses
  Classes, SysUtils, RDbCustom;

type
  TRDLOption = (olUseHistory, olStoreLastFind, olStoreHistory);

  TRDLOptions = set of TRDLOption;

  TRDbCustomFind = class(TRDbCustomDS)
  protected
    fLastFind: string;
    fDefField: string;
    fHistory: TStringList;
    fHistoryLimit: Integer;
    fOptions: TRDLOptions;
    fHiddenFields: Boolean;
    procedure SetOptions(const AValue: TRDLOptions);
    procedure InternalInit; override;
    procedure InternalDone; override;
    procedure InternalReset; override;
    function  IsStoreOptions: Boolean; override;
    function  GetIniSection: string; override;
  public
    procedure LoadData; override;
    procedure SaveData; override;
  published
    property DefaultField: string read fDefField write fDefField;
    property HistoryLimit: Integer read fHistoryLimit write fHistoryLimit default 32;
    property Options: TRDLOptions read fOptions write SetOptions;
    property UseHiddenFields: Boolean read fHiddenFields write fHiddenFields default False;
  end;

implementation

uses
  IniFiles;

const
  iniFind             = 'FIND_%s.%s';
  iniLastFind         = 'LastFind';
  iniCount            = 'HistoryCount';
  iniItem             = 'History_%d';

{ == TRDbCustomFind ============================================================ }

procedure TRDbCustomFind.InternalInit;
begin
  fLastFind := EmptyStr;
  fHistory := TStringList.Create;
  fHistory.Sorted := False;
  fHistory.Duplicates := dupIgnore;
  fHistoryLimit := 32;
  fOptions := [olUseHistory, olStoreLastFind];
end;

procedure TRDbCustomFind.InternalReset;
begin
end;

procedure TRDbCustomFind.InternalDone;
begin
  fHistory.Clear;
  fHistory.Free;
end;

function TRDbCustomFind.IsStoreOptions: Boolean;
begin
  Result := (olStoreLastFind in fOptions) or (olStoreHistory in fOptions);
end;

procedure TRDbCustomFind.SetOptions(const AValue: TRDLOptions);
begin
  if fOptions <> AValue then
  begin
    fOptions := AValue;
    if not ((csLoading in ComponentState) or (csDestroying in ComponentState))
    and (csDesigning in ComponentState) then
      CheckOptions;
  end;
end;

function TRDbCustomFind.GetIniSection: string;
begin
  CheckDbLink;
  Result := AnsiUpperCase(Format(iniFind, [OwnerName, DataSet.Name]) + GetIniSectionTag);
end;

procedure TRDbCustomFind.LoadData;
var
  Ini: TMemIniFile;
  strFN, strSN, strVAL: string;
  i, Max: Integer;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    strFN := GetIniFileName;
    strSN := GetIniSection;
    Ini := TMemIniFile.Create(strFN);
    try
      // Считываем опции
      if Ini.ReadBool(strSN, iniRestore, olStoreLastFind in fOptions)
      then fOptions := fOptions + [olStoreLastFind]
      else fOptions := fOptions - [olStoreLastFind];
      if Ini.ReadBool(strSN, iniRestore, olStoreHistory in fOptions)
      then fOptions := fOptions + [olStoreHistory]
      else fOptions := fOptions - [olStoreHistory];
      // Считываем последний поиск
      if olStoreLastFind in fOptions then
        fLastFind := Ini.ReadString(strSN, iniLastFind, EmptyStr);
      // Считываем историю
      if (olStoreHistory in fOptions) and (fHistoryLimit > 0) then
      begin
        fHistory.Clear;
        Max := Ini.ReadInteger(strSN, iniCount, 0);
        if fHistoryLimit < Max then Max := fHistoryLimit;
        for i := 0 to Max - 1 do
        begin
          strVAL := Ini.ReadString(strSN, Format(iniItem, [i + 1]), EmptyStr);
          if (strVAL <> EmptyStr) and (fHistory.IndexOf(strVAL) = -1) then
            fHistory.Add(strVAL);
        end;
      end;
    finally
      Ini.Free;
    end;
  end;
end;

procedure TRDbCustomFind.SaveData;
var
  Ini: TMemIniFile;
  strFN, strSN: string;
  i, Max: Integer;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    if (olStoreLastFind in fOptions) or (olStoreHistory in fOptions) then
    begin
      strFN := GetIniFileName;
      strSN := GetIniSection;
      Ini := TMemIniFile.Create(strFN);
      try
        // Удаляем секцию
        Ini.EraseSection(strSN);
        // Записываем последний поиск
        if olStoreLastFind in fOptions then
          Ini.WriteString(strSN, iniLastFind, fLastFind);
        // Записываем историю
        if (olStoreHistory in fOptions) and (fHistoryLimit > 0) then
        begin
          Max := fHistory.Count;
          if fHistoryLimit < Max then Max := fHistoryLimit;
          Ini.WriteInteger(strSN, iniCount, Max);
          for i := 0 to Max - 1 do
            Ini.WriteString(strSN, Format(iniItem, [i + 1]), fHistory.Strings[i]);
        end;
        Ini.UpdateFile;
      finally
        Ini.Free;
      end;
    end;
  end;
end;

end.
