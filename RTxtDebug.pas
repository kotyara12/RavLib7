unit RTxtDebug;

interface

procedure AddToDebugFile(const S: string);

implementation

uses
  SysUtils;

procedure AddToDebugFile(const S: string);
var
  F: TextFile;
begin
  AssignFile(F, ChangeFileExt(ParamStr(0), '.debug'));
  if FileExists(ChangeFileExt(ParamStr(0), '.debug'))
  then Append(F) else Rewrite(F);
  try
    WriteLn(F, S);
  finally
    CloseFile(F);
  end;
end;

end.
