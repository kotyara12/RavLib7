unit RIniFiles;

// (C) Copyright by RavSoft.
// ��������� ��� ������ � ������� ������������� � ���������� ������

interface

uses
  IniFiles, Controls, Classes;

const
  iniSettingsSection  = 'SETTINGS';

  ssIniCount          = 'Count';
  ssIniItem           = 'Item_%d';

// �������� ������������� ������
function  IniSectionExists(const AFileName, ASection: string): Boolean;
function  IniValueExists(const AFileName, ASection, AName: string): Boolean;

// �������� ������ INI �����
procedure DeleteSection(AFileName, ASection: string);
procedure DeleteSectionDef(ASection: string);

// ������ ��������� ��������
function  ReadIniString(AFileName, ASection, AName, ADefault: string): string;
function  ReadIniStringDef(ASection, AName, ADefault: string): string;
function  InitReadIniString(AFileName, ASection, AName, ADefault: string): string;
function  InitReadIniStringDef(ASection, AName, ADefault: string): string;
// ������ ��������� ��������
procedure SaveIniString(AFileName, ASection, AName, AValue: string);
procedure SaveIniStringDef(ASection, AName, AValue: string);

// ������ ����� ��������
function  ReadIniInteger(AFileName, ASection, AName: string; ADefault: Integer): Integer;
function  ReadIniIntegerDef(ASection, AName: string; ADefault: Integer): Integer;
function  InitReadIniInteger(AFileName, ASection, AName: string; ADefault: Integer): Integer;
function  InitReadIniIntegerDef(ASection, AName: string; ADefault: Integer): Integer;
// ������ ����� ��������
procedure SaveIniInteger(AFileName, ASection, AName: string; AValue: Integer);
procedure SaveIniIntegerDef(ASection, AName: string; AValue: Integer);

// ������ ����� � ��������� �������
function  ReadIniFloat(AFileName, ASection, AName: string; ADefault: Double): Double;
function  ReadIniFloatDef(ASection, AName: string; ADefault: Double): Double;
function  InitReadIniFloat(AFileName, ASection, AName: string; ADefault: Double): Double;
function  InitReadIniFloatDef(ASection, AName: string; ADefault: Double): Double;
// ������ ����� � ��������� �������
procedure SaveIniFloat(AFileName, ASection, AName: string; AValue: Double);
procedure SaveIniFloatDef(ASection, AName: string; AValue: Double);

// ������ ���������� ��������
function  ReadIniBoolean(AFileName, ASection, AName: string; ADefault: Boolean): Boolean;
function  ReadIniBooleanDef(ASection, AName: string; ADefault: Boolean): Boolean;
function  InitReadIniBoolean(AFileName, ASection, AName: string; ADefault: Boolean): Boolean;
function  InitReadIniBooleanDef(ASection, AName: string; ADefault: Boolean): Boolean;
// ������ ���������� ��������
procedure SaveIniBoolean(AFileName, ASection, AName: string; AValue: Boolean);
procedure SaveIniBooleanDef(ASection, AName: string; AValue: Boolean);

// ������ ����
function  ReadIniDate(AFileName, ASection, AName: string; ADefault: TDate): TDate;
function  ReadIniDateDef(ASection, AName: string; ADefault: TDate): TDate;
function  InitReadIniDate(AFileName, ASection, AName: string; ADefault: TDate): TDate;
function  InitReadIniDateDef(ASection, AName: string; ADefault: TDate): TDate;
// ������ ����
procedure SaveIniDate(AFileName, ASection, AName: string; AValue: TDate);
procedure SaveIniDateDef(ASection, AName: string; AValue: TDate);

// ������ �������
function  ReadIniTime(AFileName, ASection, AName: string; ADefault: TTime): TTime;
function  ReadIniTimeDef(ASection, AName: string; ADefault: TTime): TTime;
function  InitReadIniTime(AFileName, ASection, AName: string; ADefault: TTime): TTime;
function  InitReadIniTimeDef(ASection, AName: string; ADefault: TTime): TTime;
// ������ �������
procedure SaveIniTime(AFileName, ASection, AName: string; AValue: TTime);
procedure SaveIniTimeDef(ASection, AName: string; AValue: TTime);

// ������ ����-�������
function  ReadIniDateTime(AFileName, ASection, AName: string; ADefault: TDateTime): TDateTime;
function  ReadIniDateTimeDef(ASection, AName: string; ADefault: TDateTime): TDateTime;
function  InitReadIniDateTime(AFileName, ASection, AName: string; ADefault: TDateTime): TDateTime;
function  InitReadIniDateTimeDef(ASection, AName: string; ADefault: TDateTime): TDateTime;
// ������ ����-�������
procedure SaveIniDateTime(AFileName, ASection, AName: string; AValue: TDateTime);
procedure SaveIniDateTimeDef(ASection, AName: string; AValue: TDateTime);

// ������ ������ �����
procedure ReadIniTStrings(AFileName, ASection: string; TS: TStrings);
procedure ReadIniTStringsDef(ASection: string; TS: TStrings);
// ������ ������ �����
procedure SaveIniTStrings(AFileName, ASection: string; TS: TStrings);
procedure SaveIniTStringsDef(ASection: string; TS: TStrings);
// ������ ������
procedure ReadIniSection(AFileName, ASection: string; TS: TStrings);
procedure ReadIniSectionDef(ASection: string; TS: TStrings);
// ������ ������
procedure ReadIniSections(AFileName: string; TS: TStrings);
procedure ReadIniSectionsDef(TS: TStrings);

implementation

uses
  SysUtils, RSysUtils;

(*$HINTS OFF*)

// �������� ������������� ������ -----------------------------------------------
function IniSectionExists(const AFileName, ASection: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.SectionExists(ASection);
  finally
    FreeAndNil(Ini);
  end;
end;

function IniValueExists(const AFileName, ASection, AName: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ValueExists(ASection, AName);
  finally
    FreeAndNil(Ini);
  end;
end;

(*$HINTS ON*)

// �������� ������ INI ����� ---------------------------------------------------
procedure DeleteSection(AFileName, ASection: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if Ini.SectionExists(ASection) then Ini.EraseSection(ASection);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure DeleteSectionDef(ASection: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if Ini.SectionExists(ASection) then Ini.EraseSection(ASection);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ��������� �������� ---------------------------------------------------
function ReadIniString(AFileName, ASection, AName, ADefault: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadString(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniStringDef(ASection, AName, ADefault: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadString(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniString(AFileName, ASection, AName, ADefault: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteString(ASection, AName, ADefault);
    Result := Ini.ReadString(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniStringDef(ASection, AName, ADefault: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteString(ASection, AName, ADefault);
    Result := Ini.ReadString(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ��������� �������� ---------------------------------------------------
procedure SaveIniString(AFileName, ASection, AName, AValue: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteString(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniStringDef(ASection, AName, AValue: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteString(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ����� �������� -------------------------------------------------------
function ReadIniInteger(AFileName, ASection, AName: string; ADefault: Integer): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadInteger(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniIntegerDef(ASection, AName: string; ADefault: Integer): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadInteger(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniInteger(AFileName, ASection, AName: string; ADefault: Integer): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteInteger(ASection, AName, ADefault);
    Result := Ini.ReadInteger(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniIntegerDef(ASection, AName: string; ADefault: Integer): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteInteger(ASection, AName, ADefault);
    Result := Ini.ReadInteger(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ����� �������� -------------------------------------------------------
procedure SaveIniInteger(AFileName, ASection, AName: string; AValue: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteInteger(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniIntegerDef(ASection, AName: string; AValue: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteInteger(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ����� � ��������� ������� --------------------------------------------
function ReadIniFloat(AFileName, ASection, AName: string; ADefault: Double): Double;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadFloat(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniFloatDef(ASection, AName: string; ADefault: Double): Double;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadFloat(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniFloat(AFileName, ASection, AName: string; ADefault: Double): Double;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteFloat(ASection, AName, ADefault);
    Result := Ini.ReadFloat(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniFloatDef(ASection, AName: string; ADefault: Double): Double;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteFloat(ASection, AName, ADefault);
    Result := Ini.ReadFloat(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ����� � ��������� ������� --------------------------------------------
procedure SaveIniFloat(AFileName, ASection, AName: string; AValue: Double);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteFloat(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniFloatDef(ASection, AName: string; AValue: Double);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteFloat(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ���������� �������� --------------------------------------------------
function ReadIniBoolean(AFileName, ASection, AName: string; ADefault: Boolean): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadBool(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniBooleanDef(ASection, AName: string; ADefault: Boolean): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadBool(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniBoolean(AFileName, ASection, AName: string; ADefault: Boolean): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteBool(ASection, AName, ADefault);
    Result := Ini.ReadBool(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniBooleanDef(ASection, AName: string; ADefault: Boolean): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteBool(ASection, AName, ADefault);
    Result := Ini.ReadBool(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ���������� �������� --------------------------------------------------
procedure SaveIniBoolean(AFileName, ASection, AName: string; AValue: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteBool(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniBooleanDef(ASection, AName: string; AValue: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteBool(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ���� -----------------------------------------------------------------
function ReadIniDate(AFileName, ASection, AName: string; ADefault: TDate): TDate;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadDate(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniDateDef(ASection, AName: string; ADefault: TDate): TDate;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadDate(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniDate(AFileName, ASection, AName: string; ADefault: TDate): TDate;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteDate(ASection, AName, ADefault);
    Result := Ini.ReadDate(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniDateDef(ASection, AName: string; ADefault: TDate): TDate;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteDate(ASection, AName, ADefault);
    Result := Ini.ReadDate(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ���� -----------------------------------------------------------------
procedure SaveIniDate(AFileName, ASection, AName: string; AValue: TDate);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteDate(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniDateDef(ASection, AName: string; AValue: TDate);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteDate(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ������� --------------------------------------------------------------
function ReadIniTime(AFileName, ASection, AName: string; ADefault: TTime): TTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniTimeDef(ASection, AName: string; ADefault: TTime): TTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniTime(AFileName, ASection, AName: string; ADefault: TTime): TTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteTime(ASection, AName, ADefault);
    Result := Ini.ReadTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniTimeDef(ASection, AName: string; ADefault: TTime): TTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteTime(ASection, AName, ADefault);
    Result := Ini.ReadTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ������� --------------------------------------------------------------
procedure SaveIniTime(AFileName, ASection, AName: string; AValue: TTime);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteTime(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniTimeDef(ASection, AName: string; AValue: TTime);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteTime(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ����-������� ---------------------------------------------------------
function ReadIniDateTime(AFileName, ASection, AName: string; ADefault: TDateTime): TDateTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Result := Ini.ReadDateTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function ReadIniDateTimeDef(ASection, AName: string; ADefault: TDateTime): TDateTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Result := Ini.ReadDateTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniDateTime(AFileName, ASection, AName: string; ADefault: TDateTime): TDateTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteDateTime(ASection, AName, ADefault);
    Result := Ini.ReadDateTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

function InitReadIniDateTimeDef(ASection, AName: string; ADefault: TDateTime): TDateTime;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    if not Ini.ValueExists(ASection, AName) then Ini.WriteDateTime(ASection, AName, ADefault);
    Result := Ini.ReadDateTime(ASection, AName, ADefault);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ����-������� ---------------------------------------------------------
procedure SaveIniDateTime(AFileName, ASection, AName: string; AValue: TDateTime);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteDateTime(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure SaveIniDateTimeDef(ASection, AName: string; AValue: TDateTime);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.WriteDateTime(ASection, AName, AValue);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ������ ����� ---------------------------------------------------------
procedure ReadIniTStrings(AFileName, ASection: string; TS: TStrings);
var
  i: Integer;
  s: string;
begin
  TS.Clear;
  for i := 1 to ReadIniInteger(AFileName, ASection, ssIniCount, 0) do
  begin
    s := ReadIniString(AFileName, ASection, Format(ssIniItem, [i]), EmptyStr);
    if s <> EmptyStr then TS.Add(s);
  end;
end;

procedure ReadIniTStringsDef(ASection: string; TS: TStrings);
var
  i: Integer;
  s: string;
begin
  TS.Clear;
  for i := 1 to ReadIniIntegerDef(ASection, ssIniCount, 0) do
  begin
    s := ReadIniStringDef(ASection, Format(ssIniItem, [i]), EmptyStr);
    if s <> EmptyStr then TS.Add(s);
  end;
end;

// ������ ������ ����� ---------------------------------------------------------
procedure SaveIniTStrings(AFileName, ASection: string; TS: TStrings);
var
  i: Integer;
begin
  DeleteSection(AFileName, ASection);
  for i := 1 to TS.Count do
    SaveIniString(AFileName, ASection, Format(ssIniItem, [i]), TS.Strings[i - 1]);
  SaveIniInteger(AFileName, ASection, ssIniCount, TS.Count);
end;

procedure SaveIniTStringsDef(ASection: string; TS: TStrings);
var
  i: Integer;
begin
  DeleteSectionDef(ASection);
  for i := 1 to TS.Count do
    SaveIniStringDef(ASection, Format(ssIniItem, [i]), TS.Strings[i - 1]);
  SaveIniIntegerDef(ASection, ssIniCount, TS.Count);
end;

// ������ ������ ---------------------------------------------------------------
procedure ReadIniSection(AFileName, ASection: string; TS: TStrings);
var
  Ini: TIniFile;
begin
  TS.Clear;
  Ini := TIniFile.Create(AFileName);
  try
    Ini.ReadSection(ASection, TS);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure ReadIniSectionDef(ASection: string; TS: TStrings);
var
  Ini: TIniFile;
begin
  TS.Clear;
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.ReadSection(ASection, TS);
  finally
    FreeAndNil(Ini);
  end;
end;

// ������ ������ ---------------------------------------------------------------
procedure ReadIniSections(AFileName: string; TS: TStrings);
var
  Ini: TIniFile;
begin
  TS.Clear;
  Ini := TIniFile.Create(AFileName);
  try
    Ini.ReadSections(TS);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure ReadIniSectionsDef(TS: TStrings);
var
  Ini: TIniFile;
begin
  TS.Clear;
  Ini := TIniFile.Create(GetApplicationIniFile);
  try
    Ini.ReadSections(TS);
  finally
    FreeAndNil(Ini);
  end;
end;


end.
