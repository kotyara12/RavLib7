unit RIdData;

interface

uses
  Db;

type
  TIdData = packed record
    Id: Integer;
    Name: string;
    Notes: string;
  end;

function GreateIdData(const AId: Integer; const AName, ANotes: string): TIdData;
function CreateDbIdDataEx(DS: TDataSet; const IdfId, IdfName, IdfNotes: string): TIdData;
function CreateDbIdData(DS: TDataSet): TIdData;
function IsNotEmpty(const AIdData: TIdData): Boolean;
function GetIdTextEx(const AIdData: TIdData; const IdfId, IdfName, IdfNotes: string): string;
function GetIdText(const AIdData: TIdData): string;

implementation

uses
  SysUtils, RStrUtils, RConstLib, RDbConst;

function GreateIdData(const AId: Integer; const AName, ANotes: string): TIdData;
begin
  Result.Id := AId;
  Result.Name := AName;
  Result.Notes := ANotes;
end;

function CreateDbIdDataEx(DS: TDataSet; const IdfId, IdfName, IdfNotes: string): TIdData;
begin
  Result.Id := -1;
  Result.Name := EmptyStr;
  Result.Notes := EmptyStr;
  if Assigned(DS) then begin
    if Assigned(DS.FindField(IdfId)) then Result.Id := DS.FieldByName(IdfId).AsInteger;
    if Assigned(DS.FindField(IdfName)) then Result.Name := DS.FieldByName(IdfName).AsString;
    if Assigned(DS.FindField(IdfNotes)) then Result.Notes := DS.FieldByName(IdfNotes).AsString;
  end;
end;

function CreateDbIdData(DS: TDataSet): TIdData;
begin
  Result := CreateDbIdDataEx(DS, fnID, fnNAME, fnNOTES);
end;

function IsNotEmpty(const AIdData: TIdData): Boolean;
begin
  Result := (AIdData.Id > -1) or (AIdData.Name <> EmptyStr);
end;

function GetIdTextEx(const AIdData: TIdData; const IdfId, IdfName, IdfNotes: string): string;
begin
  Result := Format(SFieldText, [IdfId, IntToStr(AIdData.Id)]);
  Result := AddDelimStr(Result, Format(SFieldText, [IdfName, AIdData.Name]));
  if Trim(AIdData.Notes) <> EmptyStr
  then Result := AddDelimStr(Result, Format(SFieldText, [IdfNotes, AIdData.Notes]));
end;

function GetIdText(const AIdData: TIdData): string;
begin
  Result := GetIdTextEx(AIdData, fnID, fnNAME, fnNOTES);
end;

end.
