unit RNamedPipes;

interface

uses
  Windows;

procedure RPipes_InitializeSecurity(var SA: SECURITY_ATTRIBUTES);
procedure RPipes_FinalizeSecurity(var SA: SECURITY_ATTRIBUTES);

implementation

uses
  SysUtils;

procedure RPipes_InitializeSecurity(var SA: SECURITY_ATTRIBUTES);
var
  SCD: PSECURITY_DESCRIPTOR;
begin
  SCD := AllocMem(SECURITY_DESCRIPTOR_MIN_LENGTH);
  try
    Win32Check(InitializeSecurityDescriptor(SCD, SECURITY_DESCRIPTOR_REVISION));
    Win32Check(SetSecurityDescriptorDacl(SCD, True, nil, False));
    with SA do
    begin
      nLength := SizeOf(SECURITY_ATTRIBUTES);
      lpSecurityDescriptor := SCD;
      bInheritHandle := True;
    end;
  except
    FreeMem(SCD);
    raise;
  end;
end;

procedure RPipes_FinalizeSecurity(var SA: SECURITY_ATTRIBUTES);
begin
  if Assigned(SA.lpSecurityDescriptor) then
  begin
    FreeMem(SA.lpSecurityDescriptor);
    SA.lpSecurityDescriptor := nil;
  end;
end;

end.
