unit RExHandlersMT;

interface

uses
  SysUtils, Classes, SyncObjs, RExHandlers;

procedure LockHandleExcept(E: Exception; Creator: TObject;
  const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
procedure LockHandleSqlExcept(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);

var
  LockExHandlers: TCriticalSection;

implementation

procedure LockHandleExcept(E: Exception; Creator: TObject;
  const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
begin
  LockExHandlers.Acquire;
  try
    HandleExcept(E, Creator, Description, ExceptId, CategoryId, ET);
  finally
    LockExHandlers.Release;
  end;
end;

procedure LockHandleSqlExcept(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
begin
  LockExHandlers.Acquire;
  try
    HandleSqlExcept(E, Creator, SqlCommand, Description, ExceptId, CategoryId, ET);
  finally
    LockExHandlers.Release;
  end;
end;

initialization
  LockExHandlers := TCriticalSection.Create;

finalization
  LockExHandlers.Free;

end.
