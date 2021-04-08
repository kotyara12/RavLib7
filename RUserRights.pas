unit RUserRights;

interface

type
  PUserRights = ^TUserRights;
  TUserRights = record
    UserId: Integer;
    Registration: Boolean;
    PwdChange: Boolean;
    UserName: ShortString;
    FullName: ShortString;
  end;

procedure InitUserData(var AUser: TUserRights);
function  ChkRights(const AOprList: array of Integer; const ATagOper: Integer): Boolean;

implementation

procedure InitUserData(var AUser: TUserRights);
begin
  with AUser do
  begin
    UserId := 0;
    Registration := False;
    PwdChange := False;
    UserName := '';
    FullName := '';
  end;
end;

function ChkRights(const AOprList: array of Integer; const ATagOper: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(AOprList) to High(AOprList) do
  begin
    Result := AOprList[i] = ATagOper;
    if Result then Break;
  end;
end;

end.
