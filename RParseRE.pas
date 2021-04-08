unit RParseRE;

interface

function RegExpr_GetValue(const sText, sExpr: string; const bCaseIgnore: Boolean): string;

implementation

uses
  RegExpr;

function RegExpr_GetValue(const sText, sExpr: string; const bCaseIgnore: Boolean): string;
var
  fRegExp: TRegExpr;
begin
  fRegExp := TRegExpr.Create;
  try
    fRegExp.Expression := sExpr;
    fRegExp.ModifierI := bCaseIgnore;
    fRegExp.ModifierS := True;
    fRegExp.ModifierG := True;
    fRegExp.ModifierR := True;

    if fRegExp.Exec(sText) then
      Result := fRegExp.Substitute('$&');
  finally
    fRegExp.Free;
  end;
end;

end.
