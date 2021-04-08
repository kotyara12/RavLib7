unit RProxy;

interface

uses
  Classes, rHttpUtils;

type
  TProxyItem = record
    aData: TProxyData;
    iErrCnt: Integer;
  end;

  TProxyListCustom = class
  private
    fProxy: array of TProxyItem;
    fIndex: Integer;
    fAutoNext: Boolean;
    fMaxErr: Integer;
    fFileList: string;
    function  Find(const aProxy: TProxyData): Integer;
    procedure LoadList;
    procedure SaveList;
    procedure Add(const aProxy: TProxyData);
    procedure Del(const iIndex: Integer);
    procedure Fix(const iIdx: Integer; const bOk, bNextOk: Boolean);
  protected
    procedure AddProxy(const aServer: string; const aPort: Word; const aUsername: string; const aPassword: string);
    procedure FillList; virtual; abstract;
  public
    constructor Create(const fileList: string);
    destructor Destroy; override;

    procedure Append(const aProxy: TProxyData);
    function  Count: Integer;
    procedure Clear;
    procedure Update;
    procedure Reset;
    procedure Next;
    function  GetProxy: TProxyData;
    procedure FixTry(const bOk: Boolean); overload;
    procedure FixTry(const aProxy: TProxyData; const bOk: Boolean); overload;

    property MaxErrors: Integer read fMaxErr write fMaxErr;
    property AutoNext: Boolean read fAutoNext write fAutoNext;
  end;

  TProxyList = class (TProxyListCustom)
  protected
    procedure FillList; override;
  end;

  TProxyListParse = class (TProxyListCustom)
  private
    fProtocols: TProxyProtocols;
    fLevels: TProxyLevels;
    fCountry: string;
    fPort: Integer;
    fMinUpTime: Integer;
    fMaxRTime: Integer;
    fMaxCount: Integer;
  public
    constructor Create(const fileList: string;
      const aProtocols: TProxyProtocols;
      const aLevels: TProxyLevels; const aCountry: string;
      const aPort, aUpTime, aRTime, aCount: Integer);

    property Protocols: TProxyProtocols read fProtocols write fProtocols;
    property Levels: TProxyLevels read fLevels write fLevels;
    property Country: string read fCountry write fCountry;
    property Port: Integer read fPort write fPort;
    property MinUpTime: Integer read fMinUpTime write fMinUpTime;
    property MaxRTime: Integer read fMaxRTime write fMaxRTime;
    property MaxCount: Integer read fMaxCount write fMaxCount;
  end;

  TProxyDbNet = class (TProxyListParse)
  protected
    procedure FillList; override;
  end;

implementation

uses
  SysUtils, StrUtils, RxStrUtils, RDialogs;

const
  fnDefProxyFile               = 'proxy.lst';

resourcestring
  rsErrProxyListIsEmpty        = 'Список proxy серверов пуст!';

{ TProxyListCustom }

constructor TProxyListCustom.Create(const fileList: string);
begin
  fFileList := fileList;
  if fFileList = EmptyStr then
    fFileList := ExtractFilePath(ParamStr(0)) + fnDefProxyFile;

  fMaxErr := 5;
  fAutoNext := False;

  LoadList;
end;

destructor TProxyListCustom.Destroy;
begin
  SetLength(fProxy, 0);

  inherited Destroy;
end;

function TProxyListCustom.Count: Integer;
begin
  Result := Length(fProxy);
end;

function TProxyListCustom.Find(const aProxy: TProxyData): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Length(fProxy) > 0 then
  begin
    for i := Low(fProxy) to High(fProxy) do
    begin
      if SameText(fProxy[i].aData.sServer, aProxy.sServer)
      and (fProxy[i].aData.iPort = aProxy.iPort)
      and (fProxy[i].aData.sUsername = aProxy.sUsername)
      and (fProxy[i].aData.sPassword = aProxy.sPassword) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TProxyListCustom.Add(const aProxy: TProxyData);
begin
  SetLength(fProxy, Length(fProxy) + 1);
  with fProxy[High(fProxy)] do
  begin
    aData := aProxy;
    iErrCnt := 0;
  end;
end;

procedure TProxyListCustom.AddProxy(const aServer: string; const aPort: Word; const aUsername: string; const aPassword: string);
begin
  SetLength(fProxy, Length(fProxy) + 1);
  with fProxy[High(fProxy)] do
  begin
    aData.sServer := aServer;
    aData.iPort := aPort;
    aData.sUsername := aUsername;
    aData.sPassword := aPassword;
    iErrCnt := 0;
  end;
end;

procedure TProxyListCustom.Append(const aProxy: TProxyData);
begin
  if Find(aProxy) = -1 then
  begin
    Add(aProxy);
    SaveList;
  end;
end;

procedure TProxyListCustom.Del(const iIndex: Integer);
var
  iLast: Integer;
begin
  if Length(fProxy) > 0 then
  begin
    iLast := High(fProxy);
    if iIndex < iLast then
      Move(fProxy[iIndex + 1], fProxy[iIndex], SizeOf(TProxyItem) * (iLast - iIndex));
    SetLength(fProxy, Length(fProxy) - 1);
  end;

  if (Length(fProxy) = 0) or (fIndex > High(fProxy)) then
    Reset;
end;

procedure TProxyListCustom.Clear;
begin
  SetLength(fProxy, 0);
  fIndex := -1;
end;

procedure TProxyListCustom.Reset;
begin
  if Length(fProxy) > 0
  then fIndex := Low(fProxy)
  else fIndex := -1;
end;

procedure TProxyListCustom.Next;
begin
  if Length(fProxy) > 0 then
  begin
    Inc(fIndex);
    if fIndex > High(fProxy) then
      fIndex := Low(fProxy);
  end
  else fIndex := -1;
end;

procedure TProxyListCustom.LoadList;
var
  slProxy: TStringList;
  i, iCount: Integer;
begin
  SetLength(fProxy, 0);
  if (fFileList <> EmptyStr) and FileExists(fFileList) then
  begin
    slProxy := TStringList.Create;
    try
      slProxy.LoadFromFile(fFileList);
      iCount := slProxy.Count - 1;
      for i := 0 to iCount do
        Add(rProxy_StrToProxy(slProxy[i]));
    finally
      slProxy.Free;
    end;
  end;
  Reset;
end;

procedure TProxyListCustom.SaveList;
var
  slProxy: TStringList;
  i: Integer;
begin
  if fFileList <> EmptyStr then
  begin
    slProxy := TStringList.Create;
    try
      ForceDirectories(ExtractFilePath(fFileList));
      if Length(fProxy) > 0 then
      begin
        for i := Low(fProxy) to High(fProxy) do
          slProxy.Add(rProxy_ProxyToStr(fProxy[i].aData));
      end;
      slProxy.SaveToFile(fFileList);
    finally
      slProxy.Free;
    end;
  end;
end;

procedure TProxyListCustom.Update;
begin
  if Length(fProxy) = 0 then
  begin
    FillList;
    SaveList;
    Reset;
  end;

  if Length(fProxy) = 0 then
    raise Exception.Create(rsErrProxyListIsEmpty);
end;

function TProxyListCustom.GetProxy: TProxyData;
begin
  Update;

  if fIndex > -1 then
    Result := fProxy[fIndex].aData;
end;

procedure TProxyListCustom.Fix(const iIdx: Integer; const bOk, bNextOk: Boolean);
begin
  if (Length(fProxy) > 0) and (iIdx > -1) then
  begin
    if bOk then
    begin
      fProxy[iIdx].iErrCnt := 0;
      if bNextOk then Next;
    end
    else begin
      Inc(fProxy[iIdx].iErrCnt);
      if fProxy[iIdx].iErrCnt >= fMaxErr then
      begin
        Del(iIdx);
        SaveList;
      end
      else Next;
    end;
  end;
end;

procedure TProxyListCustom.FixTry(const bOk: Boolean);
begin
  Fix(fIndex, bOk, fAutoNext);
end;

procedure TProxyListCustom.FixTry(const aProxy: TProxyData; const bOk: Boolean);
begin
  Fix(Find(aProxy), bOk, False);
end;

{ TProxyList }

procedure TProxyList.FillList;
begin
  // nothing
end;

{ TProxyListParse }

constructor TProxyListParse.Create(const fileList: string;
  const aProtocols: TProxyProtocols; const aLevels: TProxyLevels;
  const aCountry: string; const aPort, aUpTime, aRTime, aCount: Integer);
begin
  inherited Create(fileList);

  fProtocols := aProtocols;
  fLevels := aLevels;
  fCountry := aCountry;
  fPort := aPort;
  fMinUpTime := aUpTime;
  fMaxRTime := aRTime;
  fMaxCount := aCount;
end;

{ TProxyDbNet }

procedure TProxyDbNet.FillList;
var
  slBuf: TStringList;
  i: Integer;
begin
  slBuf := TStringList.Create;
  try
    rProxy_ProxyDbNet(slBuf, fProtocols, fLevels, fCountry, fPort, fMinUpTime, fMaxRTime, fMaxCount);

    if slBuf.Count > 0 then
    begin
      for i := 0 to slBuf.Count - 1 do
        Add(rProxy_StrToProxy(slBuf[i]));
    end;
  finally
    slBuf.Free;
  end;
end;

end.
