unit RDbiUtils;

interface

procedure RDbiInit;
procedure RDbiDone;
function  RDbiEnabled: Boolean;
procedure RDbiAliasCreate(const AliasName, Driver, Parameters: string);
procedure RDbiAliasCreateInit(const AliasName, Driver, Parameters: string);
procedure RDbiAliasDelete(const AliasName: string);
procedure RDbiAliasDeleteInit(const AliasName: string);
function  RDbiAliasIsExists(const AliasName: string): Boolean;

const
  DbiDrvDefault = 'PARADOX';

implementation

uses
  SysUtils, Windows, DbiProcs, DbiErrs;

type
  ERDbiError = class (Exception);

resourcestring
  S_DBIERR_CODE          = 'Borland Database Engine Error #%d!';
  S_DBIERR_MULTIPLEINIT  = 'BDE Error #%d: Illegal attempt to initialize BDE more than once!';
  S_DBIERR_NOTSUPPORTED  = 'BDE Error #%d: Property is not supported for this object!';
  S_DBIERR_SESSIONSLIMIT = 'BDE Error #%d: The maximum number of sessions are open!';
  S_DBIERR_INVALIDPARAM  = 'BDE Error #%d: Invalid alias name or parameter(s)!';
  S_DBIERR_NAMENOTUNIQUE = 'BDE Error #%d: Another alias with the same name already exists!';
  S_DBIERR_OBJNOTFOUND	 = 'BDE Error #%d: One (or more) of the optional parameters passed in through pszParams was not found as a valid type in the driver section of the configuration file!';
  S_DBIERR_UNKNOWNDRIVER = 'BDE Error #%d: No driver name found in configuration file matching pszDriverType!';

procedure RDbiCheck(const RetCode: Word);
begin
  if RetCode <> DBIERR_NONE	then
  begin
    case RetCode of
      DBIERR_MULTIPLEINIT:
        raise ERDbiError.CreateFmt(S_DBIERR_MULTIPLEINIT, [RetCode]);
      DBIERR_SESSIONSLIMIT:
        raise ERDbiError.CreateFmt(S_DBIERR_SESSIONSLIMIT, [RetCode]);
      DBIERR_INVALIDPARAM:
        raise ERDbiError.CreateFmt(S_DBIERR_INVALIDPARAM, [RetCode]);
      DBIERR_NAMENOTUNIQUE:
        raise ERDbiError.CreateFmt(S_DBIERR_NAMENOTUNIQUE, [RetCode]);
      DBIERR_OBJNOTFOUND:
        raise ERDbiError.CreateFmt(S_DBIERR_OBJNOTFOUND, [RetCode]);
      DBIERR_UNKNOWNDRIVER:
        raise ERDbiError.CreateFmt(S_DBIERR_UNKNOWNDRIVER, [RetCode]);
      DBIERR_NOTSUPPORTED:
        raise ERDbiError.CreateFmt(S_DBIERR_NOTSUPPORTED, [RetCode]);
      else
        raise ERDbiError.CreateFmt(S_DBIERR_CODE, [RetCode]);
    end;
  end;
end;

procedure RDbiInit;
begin
  RDbiCheck(DbiInit(nil));
end;

procedure RDbiDone;
begin
  if IsLibrary then DbiDllExit;
  RDbiCheck(DbiExit);
end;

function RDbiEnabled: Boolean;
begin
  Result := DbiInit(nil) = DBIERR_NONE;
  if Result then RDbiDone;
end;

procedure RDbiAliasCreate(const AliasName, Driver, Parameters: string);
begin
  RDbiCheck(DbiAddAlias(nil, PChar(AliasName), PChar(Driver), PChar(Parameters), Bool(-1)));
  RDbiCheck(DbiCfgSave(nil, nil, Bool(-1)));
end;

procedure RDbiAliasCreateInit(const AliasName, Driver, Parameters: string);
begin
  RDbiInit;
  try
    RDbiAliasCreate(AliasName, Driver, Parameters);
  finally
    RDbiDone;
  end;
end;

procedure RDbiAliasDelete(const AliasName: string);
begin
  RDbiCheck(DbiDeleteAlias(nil, PChar(AliasName)));
  RDbiCheck(DbiCfgSave(nil, nil, Bool(-1)));
end;

procedure RDbiAliasDeleteInit(const AliasName: string);
begin
  RDbiInit;
  try
    RDbiAliasDelete(AliasName);
  finally
    RDbiDone;
  end;
end;

function RDbiAliasIsExists(const AliasName: string): Boolean;
var
  DbiResult: Word;
begin
  Result := DbiInit(nil) = DBIERR_NONE;
  if Result then
  begin
    try
      DbiResult := DbiAddAlias(nil, PChar(AliasName), PChar(DbiDrvDefault), 'PATH:', Bool(-1));
      case DbiResult of
        DBIERR_NAMENOTUNIQUE:
          Result := True;
        DBIERR_NONE:
        begin
          DbiDeleteAlias(nil, PChar(AliasName));
          Result := False;
        end;
        else
          Result := False;
      end;
    finally
      RDbiDone;
    end;
  end;
end;

end.
