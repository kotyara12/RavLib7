unit BdeUtils;

interface

uses
  SysUtils, DbiProcs, DbiErrs, DBTables;
  
function CreateBdeAlias(const AliasName, DriverName, Parameters: string): Word;

implementation

resourcestring
  ERR_ALIASOPENBDE = 'Error initializing BDE. Cannot create Alias.';
  ERR_ALIASCLOSEBDE = 'Error closing the BDE.'#13#10'Please close all applications and restart Windows';
  ERR_ALIASDRIVERNOTFOUND = 'Specified driver does not exist.';
  ERR_ALIASINVALIDPARAM = 'Invalid Alias name.';
  ERR_ALIASALREADYEXISTS = 'The Alias (%s) already exists.'#13#10'Would you like to reconfigure it?';

const
  STDDBDRV = 'STANDARD';

function CreateBdeAlias(const AliasName, DriverName, Parameters: string): Word;
var
  dbEnv: DbiEnv;
begin
  with dbEnv do
  begin
    StrPCopy(szWorkDir, '');
    StrPCopy(szIniFile, '');
    bForceLocalInit := True;
    StrPCopy(szLang, '');
    StrPCopy(szClientName, 'dbClientName');
  end;
  Result := DbiInit(@dbEnv);
  if Result <> DbiERR_NONE then
    raise Exception.Create(ERR_ALIASOPENBDE);
  if UpperCase(DriverName) = STDDBDRV
  then Result := DbiAddAlias(nil, PChar(AliasName), nil, PChar(Parameters), True)
  else Result := DbiAddAlias(nil, PChar(AliasName), PChar(DriverName), PChar(Parameters), True);
  case Result of
    DbiERR_INVALIDPARAM:
      raise Exception.Create(ERR_ALIASINVALIDPARAM);
    DbiERR_NAMENOTUNIQUE:
    begin
      Check(DbiDeleteAlias(nil, PChar(AliasName)));
      if UpperCase(DriverName) = STDDBDRV
      then Result := DbiAddAlias(nil, PChar(AliasName), nil, PChar(Parameters), True)
      else Result := DbiAddAlias(nil, PChar(AliasName), PChar(DriverName), PChar(Parameters), True);
    end;
    DbiERR_UNKNOWNDRIVER:
      raise Exception.Create(ERR_ALIASDRIVERNOTFOUND);
  end;
  if DbiExit <> DbiERR_NONE then
    raise Exception.Create(ERR_ALIASCLOSEBDE);
end;

end.
