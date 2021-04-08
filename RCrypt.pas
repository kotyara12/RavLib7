unit RCrypt;

interface

function  Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function  Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function  EncryptHex(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function  DecryptHex(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function  StrToHexStr(const InString: string): string;
function  HexStrToStr(const InString: string): string;
procedure GenerateKey96(var StartKey, MultKey, AddKey: integer);

implementation

uses
  SysUtils;

const
  StartKey = 471;
  MultKey  = 62142;
  AddKey   = 11719;

function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(InString) do
  begin
    Result := Result + Char(Byte(InString[i]) xor (StartKey shr 8));
    StartKey := (Byte(Result[i]) + StartKey) * MultKey + AddKey;
  end;
end;

function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(InString) do
  begin
    Result := Result + Char(Byte(InString[i]) xor (StartKey shr 8));
    StartKey := (Byte(InString[i]) + StartKey) * MultKey + AddKey;
  end;
end;

function StrToHexStr(const InString: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(InString) do
    Result := Result + IntToHex(Ord(InString[i]), 2);
end;

function HexStrToStr(const InString: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(InString) do
    if Odd(i) then
      Result := Result + Chr(StrToIntDef('$' + InString[i] + InString[i + 1], 32));
end;

function EncryptHex(const InString: string; StartKey, MultKey, AddKey: Integer): string;
begin
  Result := StrToHexStr(Encrypt(InString, StartKey, MultKey, AddKey));
end;

function DecryptHex(const InString: string; StartKey, MultKey, AddKey: Integer): string;
begin
  Result := Decrypt(HexStrToStr(InString), StartKey, MultKey, AddKey);
end;

procedure GenerateKey96(var StartKey, MultKey, AddKey: integer);
var
  i: integer;
  a: array[1..12] of byte;

  procedure FillKey(var VarKey: integer; Index: integer);
  var
    j: integer;
  begin
    for j := 0 to SizeOf(VarKey) - 1 do
      VarKey := VarKey or a[Index + j] shl (8 * j);
  end;

begin
  System.Randomize();
  for i := Low(a) to High(a) do
    a[i] := System.Random(High(Byte));
  FillKey(StartKey, 1);
  FillKey(MultKey, 5);
  FillKey(AddKey, 9);
end;

end.
