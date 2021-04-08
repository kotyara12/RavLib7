unit RExHandlers;

interface

uses
  Classes, SysUtils;

type
  RExceptRecord = packed record
    CategoryId: Integer;
    ExceptId: Integer;
    ExceptClass: string;
    ExceptMessage: string;
    CreatorClass: string;
    CreatorObject: string;
    SqlCommand: string;
    Description: string;
  end;

type
  TExceptChannelType = (etNone, ecMessage, ecHint, ecLog, ecDbLog, ecEventLog, ecVisible, ecHidden, ecOther);
  TExceptTypes       = set of TExceptChannelType;

const
  ecAll              = [ecMessage, ecHint, ecLog, ecDbLog, ecEventLog, ecVisible, ecHidden, ecOther];
  ecExcludeDbLog     = [ecMessage, ecHint, ecLog, ecEventLog, ecVisible, ecHidden, ecOther];
  esMsg              = [ecMessage, ecHint, ecVisible];
  esReg              = [ecLog, ecDbLog, ecEventLog, ecHidden];

type
  TExceptHandler = class;

  TCustomExceptChannel = class
  private
    FExceptHandler: TExceptHandler;
    FPriority: Byte;
    FEnabled: Boolean;
    FLocked: Boolean;
  protected
    function GetAutoDisabled: Boolean; virtual;
    function GetChannelType: TExceptChannelType; virtual;
    function FormatLine(const Str: string): string;
    function GetApplicationName: string;
    function GetErrorMessage(const ER: RExceptRecord): string;
    function GetErrorLineMessage(const ER: RExceptRecord): string;
    function GetExceptionMessage(const ER: RExceptRecord): string;
    function GetExceptionClass(const ER: RExceptRecord): string;
    function GetObjectInfo(const ER: RExceptRecord): string;
    function GetSqlCommand(const ER: RExceptRecord): string;
    procedure RegisterExceptRecord(const ER: RExceptRecord); virtual; abstract;
  public
    constructor Create(Handler: TExceptHandler; const APriority: Byte; const AEnabled: Boolean);
    procedure RegisterException(const ER: RExceptRecord; const ET: TExceptTypes);
    property AutoDisabled: Boolean read GetAutoDisabled;
    property ChannelType: TExceptChannelType read GetChannelType;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Priority: Byte read FPriority write FPriority;
  end;

  TExceptChannelClass = class of TCustomExceptChannel;

  TExceptHandler = class
  private
    FChannels: TList;
    FLockMessages: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ChannelCreate(ChannelClass: TExceptChannelClass;
      const Priority: Byte; const Enabled: Boolean): Integer;
    procedure AppExceptHandler(Sender: TObject; E: Exception);
    procedure RegisterException(E: Exception; Creator: TObject;
      const SqlCommand: string = ''; const Description: string = '';
      const ExceptId: Integer = 0; const CategoryId: Integer = 0;
      const ET: TExceptTypes = ecAll);
    property Channels: TList read FChannels;
    property LockMessages: Boolean read FLockMessages write FLockMessages;
  end;

var
  AppExceptionsHandler: TExceptHandler;

function CreateExceptRecord(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0): RExceptRecord;

procedure HandleExcept(E: Exception; Creator: TObject;
  const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
procedure HandleSqlExcept(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);

procedure HandlerBlockMessages;
procedure HandlerUnblockMessages;

implementation

uses
  Contnrs, Db, RDbUtils;

resourcestring
  SLineError            = '%s (%s)';
  SExceptInfo           = 'Сообщение: "%s"';
  SObjectInfo           = 'Класс, вызвавший ошибку: %s';
  SComponentInfo        = 'Объект, вызвавший ошибку: %s, класс: %s';
  SSqlInfo              = 'SQL запрос: [%s]';
  SClassInfo            = 'Класс exception: %s';
  SClassNotFound        = '<???>';

  SInternalChannelError = 'Сбой обработки исключительной ситуации в канале "%s"!';

{ == Global Handlers =========================================================== }
procedure HandleExcept(E: Exception; Creator: TObject;
  const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
begin
  if Assigned(AppExceptionsHandler) then
    AppExceptionsHandler.RegisterException(E, Creator,
      '', Description, ExceptId, CategoryId, ET);
end;

procedure HandleSqlExcept(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
begin
  if Assigned(AppExceptionsHandler) then
    AppExceptionsHandler.RegisterException(E, Creator,
      SqlCommand, Description, ExceptId, CategoryId, ET);
end;

procedure HandlerBlockMessages;
begin
  if Assigned(AppExceptionsHandler) then
    AppExceptionsHandler.LockMessages := True;
end;

procedure HandlerUnblockMessages;
begin
  if Assigned(AppExceptionsHandler) then
    AppExceptionsHandler.LockMessages := False;
end;

{ == ExceptRecord ============================================================== }

function CreateExceptRecord(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0): RExceptRecord;
begin
  Result.CategoryId := CategoryId;
  Result.ExceptId := ExceptId;
  Result.ExceptClass := E.ClassName;
  Result.ExceptMessage := E.Message;
  Result.CreatorClass := EmptyStr;
  Result.CreatorObject := EmptyStr;
  Result.Description := Description;
  if Assigned(Creator) then begin
    if Creator is TComponent
    then Result.CreatorObject := TComponent(Creator).Name
    else Result.CreatorObject := EmptyStr;
    Result.CreatorClass := Creator.ClassName;
    if (Creator is TDataSet) and (SqlCommand = EmptyStr)
    then IsSqlDataSet(TDataSet(Creator), Result.SqlCommand)
    else Result.SqlCommand := SqlCommand;
  end
  else Result.SqlCommand := SqlCommand;
  if Length(Result.SqlCommand) > 0 then
    while Result.SqlCommand[Length(Result.SqlCommand)] in [#10, #13] do
      Result.SqlCommand := Copy(Result.SqlCommand, 1, Length(Result.SqlCommand) - 1);
end;

{ == TCustomExceptChannel ====================================================== }

constructor TCustomExceptChannel.Create(Handler: TExceptHandler;
  const APriority: Byte; const AEnabled: Boolean);
begin
  inherited Create;
  FExceptHandler := Handler;
  FPriority := APriority;
  FEnabled := AEnabled;
  FLocked := False;
end;

function TCustomExceptChannel.GetAutoDisabled: Boolean;
begin
  Result := False;
end;

function TCustomExceptChannel.GetChannelType: TExceptChannelType;
begin
  Result := etNone;
end;

function TCustomExceptChannel.GetApplicationName: string;
begin
  Result := ParamStr(0);
end;

function TCustomExceptChannel.GetErrorMessage(const ER: RExceptRecord): string;
begin
  if ER.Description = EmptyStr
  then Result := ER.ExceptMessage
  else Result := ER.Description;
end;

function TCustomExceptChannel.GetExceptionMessage(const ER: RExceptRecord): string;
begin
  if ER.Description <> EmptyStr
  then Result := Format(SExceptInfo, [ER.ExceptMessage])
  else Result := EmptyStr;
end;

function TCustomExceptChannel.GetErrorLineMessage(const ER: RExceptRecord): string;
begin
  if ER.Description = EmptyStr
  then Result := ER.ExceptMessage
  else Result := Format(SLineError, [ER.Description, ER.ExceptMessage]);
end;

function TCustomExceptChannel.GetExceptionClass(const ER: RExceptRecord): string;
begin
  Result := Format(SClassInfo, [ER.ExceptClass]);
end;

function TCustomExceptChannel.GetObjectInfo(const ER: RExceptRecord): string;
begin
  if ER.CreatorObject <> EmptyStr then
  begin
    if ER.CreatorClass <> EmptyStr
    then Result := Format(SComponentInfo, [ER.CreatorObject, ER.CreatorClass])
    else Result := Format(SComponentInfo, [ER.CreatorObject, SClassNotFound])
  end
  else begin
    if ER.CreatorClass <> EmptyStr
    then Result := Format(SObjectInfo, [ER.CreatorClass])
    else Result := EmptyStr;
  end;
end;

function TCustomExceptChannel.GetSqlCommand(const ER: RExceptRecord): string;
begin
  if ER.SqlCommand <> EmptyStr
  then Result := Format(SSqlInfo, [ER.SqlCommand])
  else Result := EmptyStr;
end;

function TCustomExceptChannel.FormatLine(const Str: string): string;
begin
  Result := Trim(Str);
  if Result <> EmptyStr then Result := Result + #13#13;
end;

procedure TCustomExceptChannel.RegisterException(const ER: RExceptRecord; const ET: TExceptTypes);
begin
  if FEnabled and not FLocked and (ChannelType in ET) then
  begin
    FLocked := True;
    try
      try
        RegisterExceptRecord(ER);
      except
        on E: Exception do
        begin
          if AutoDisabled then FEnabled := False;
          if FExceptHandler <> nil then
            FExceptHandler.RegisterException(E, Self,
              Format(SInternalChannelError, [Self.ClassName]));
        end;
      end;
    finally
      FLocked := False;
    end;
  end;
end;

{ == TExceptHandler ============================================================ }

constructor TExceptHandler.Create;
begin
  inherited Create;
  FChannels := TObjectList.Create;
  FLockMessages := False;
end;

destructor TExceptHandler.Destroy;
begin
  FChannels.Clear;
  FChannels.Free;
  inherited Destroy;
end;

function TExceptHandler.ChannelCreate(ChannelClass: TExceptChannelClass;
  const Priority: Byte; const Enabled: Boolean): Integer;
var
  Index: Integer;
begin
  Index := 0;
  while (Index < FChannels.Count)
  and (TCustomExceptChannel(FChannels.Items[Index]).Priority <= Priority) do
    Inc(Index);
  if Index < FChannels.Count then
  begin
    Result := Index;
    FChannels.Insert(Index, ChannelClass.Create(Self, Priority, Enabled));
  end
  else
    Result := FChannels.Add(ChannelClass.Create(Self, Priority, Enabled));
end;

procedure TExceptHandler.RegisterException(E: Exception; Creator: TObject;
  const SqlCommand: string = ''; const Description: string = '';
  const ExceptId: Integer = 0; const CategoryId: Integer = 0;
  const ET: TExceptTypes = ecAll);
var
  i: Integer;
  InternalER: RExceptRecord;
  InternalET: TExceptTypes;
begin
  InternalET := ET;
  if FLockMessages then InternalET := InternalET - esMsg;
  InternalER := CreateExceptRecord(E, Creator, SqlCommand, Description, ExceptId, CategoryId);
  for i := 0 to FChannels.Count - 1 do
    TCustomExceptChannel(FChannels[i]).RegisterException(InternalER, InternalET);
end;

procedure TExceptHandler.AppExceptHandler(Sender: TObject; E: Exception);
begin
  RegisterException(E, Sender);
end;

initialization
  AppExceptionsHandler := nil;

finalization
  if AppExceptionsHandler <> nil then AppExceptionsHandler.Free;

end.

