unit RDbStatus;

interface

uses
  Classes, SysUtils, ComCtrls, Db, RDbFilter;

type

  ERavDbStatusError = class (Exception);

  TRDSOption        = (soRecordCount, soRecordNum, soShowCaptions);
  TRDSOptions       = set of TRDSOption;

{ == TRDbStatus ================================================================ }

  TRDbStatus = class (TComponent)
  private
    FDataLink: TDataLink;
    FStatusBar: TStatusBar;
    FOptions: TRDSOptions;
    FRecordsPanel: Integer;
    FStatePanel: Integer;
    function  GetDataSet: TDataSet;
    function  GetDataSource: TDataSource;
    function  GetDatasetState: TDataSetState;
    procedure SetDataSource(Value: TDataSource);
    procedure SetStatusBar(Value: TStatusBar);
    procedure SetOptions(Value: TRDSOptions);
    procedure SetRecordsPanel(Value: Integer);
    procedure SetStatePanel(Value: Integer);
  protected
    procedure Loaded; override;
    function GetRecordsText(const ShowCaptions: Boolean): string;
    function GetStateText(const ShowCaptions: Boolean): string;
    function GetInternalCompleteText: string; virtual;
    procedure SetInternalPanelsText; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateStatus;
    function GetStatus: string;
    property DatasetState: TDataSetState read GetDatasetState;
    property DataSet: TDataSet read GetDataSet;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Panel_RecNum: Integer read FRecordsPanel write SetRecordsPanel default 0;
    property Panel_DbState: Integer read FStatePanel write SetStatePanel default 1;
    property StatusBar: TStatusBar read FStatusBar write SetStatusBar;
    property Options: TRDSOptions read FOptions write SetOptions;
  end;

{ == TRDbFilterStatus ========================================================== }

  TRDbFilterStatus = class (TRDbStatus)
  private
    FFilter: TRDbFilter;
    FFilterPanel: Integer;
    procedure SetFilter(Value: TRDbFilter);
    procedure SetFilterPanel(Value: Integer);
  protected
    function GetFilterText: string;
    function GetInternalCompleteText: string; override;
    procedure SetInternalPanelsText; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property RDbFilter: TRDbFilter read FFilter write SetFilter;
    property Panel_DbFilter: Integer read FFilterPanel write SetFilterPanel default 2;
  end;

implementation

uses
  RMsgRu, RDbState;

const
  ComplDivider           = '; ';

{ == TStatusDataLink =========================================================== }

type
  TStatusDataLink = class(TDataLink)
  private
    FStatusControl: TRDbStatus;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure LayoutChanged; override;
  public
    constructor Create(AStatusControl: TRDbStatus);
    destructor Destroy; override;
  end;

constructor TStatusDataLink.Create(AStatusControl: TRDbStatus);
begin
  inherited Create;
  FStatusControl := AStatusControl;
end;

destructor TStatusDataLink.Destroy;
begin
  FStatusControl := nil;
  inherited Destroy;
end;

procedure TStatusDataLink.ActiveChanged;
begin
  DataSetChanged;
end;

procedure TStatusDataLink.DataSetScrolled(Distance: Integer);
begin
  if (FStatusControl <> nil) and (soRecordNum in FStatusControl.Options) then
    FStatusControl.UpdateStatus;
end;

procedure TStatusDataLink.EditingChanged;
begin
  if FStatusControl <> nil then
    FStatusControl.UpdateStatus;
end;

procedure TStatusDataLink.DataSetChanged;
begin
  if FStatusControl <> nil then
    FStatusControl.UpdateStatus;
end;

procedure TStatusDataLink.LayoutChanged;
begin
  DataSetChanged; { ??? }
end;

{ == TRDbStatus ================================================================ }

constructor TRDbStatus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TStatusDataLink.Create(Self);
  FStatusBar := nil;
  FRecordsPanel := 0;
  FStatePanel := 1;
  FOptions := [soRecordCount, soShowCaptions];
end;

destructor TRDbStatus.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FStatusBar := nil;
  inherited Destroy;
end;

procedure TRDbStatus.Loaded;
begin
  inherited Loaded;
  UpdateStatus;
end;

procedure TRDbStatus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
  if (Operation = opRemove) and (StatusBar <> nil) and
    (AComponent = StatusBar) then StatusBar := nil;
end;

function TRDbStatus.GetDataSource: TDataSource;
begin
  if FDataLink <> nil
  then Result := FDataLink.DataSource
  else Result := nil;
end;

function TRDbStatus.GetDataSet: TDataSet;
begin
  if FDataLink <> nil
  then Result := FDataLink.DataSet
  else Result := nil;
end;

procedure TRDbStatus.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if not (csLoading in ComponentState) then UpdateStatus;
end;

function TRDbStatus.GetDatasetState: TDataSetState;
begin
  if DataSource <> nil
  then Result := DataSource.State
  else Result := dsInactive;
end;

procedure TRDbStatus.SetStatusBar(Value: TStatusBar);
begin
  if FStatusBar <> Value then
  begin
    FStatusBar := Value;
    if Value <> nil then Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then UpdateStatus;
  end;
end;

procedure TRDbStatus.SetOptions(Value: TRDSOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if not (csLoading in ComponentState) then UpdateStatus;
  end;
end;

procedure TRDbStatus.SetRecordsPanel(Value: Integer);
begin
  if FRecordsPanel <> Value then
  begin
    FRecordsPanel := Value;
    if not (csLoading in ComponentState) then UpdateStatus;
  end;
end;

procedure TRDbStatus.SetStatePanel(Value: Integer);
begin
  if FStatePanel <> Value then
  begin
    FStatePanel := Value;
    if not (csLoading in ComponentState) then UpdateStatus;
  end;
end;

function TRDbStatus.GetRecordsText(const ShowCaptions: Boolean): string;
var
  RecordNum, RecordCount, RecordNumCount: string;
begin
  if (FDataLink <> nil) and FDataLink.Active and (DataSet <> nil) then
  begin
    if ShowCaptions then begin
      RecordNum := SCaptionRecordNum;
      RecordCount := SCaptionRecordCount;
      RecordNumCount := SCaptionRecordNumCount;
    end
    else begin
      RecordNum := SRecordNum;
      RecordCount := SRecordCount;
      RecordNumCount := SRecordNumCount;
    end;
    if (soRecordCount in FOptions) then begin
      if (soRecordNum in FOptions) and DataSet.IsSequenced then
        Result := Format(RecordNumCount, [DataSet.RecNo, DataSet.RecordCount])
      else
        Result := Format(RecordCount, [DataSet.RecordCount]);
    end
    else begin
      if (soRecordNum in FOptions) and DataSet.IsSequenced then
        Result := Format(RecordNum, [DataSet.RecNo])
      else
        Result := SRecordInactive;
    end;
  end
  else Result := SRecordInactive;
end;

function TRDbStatus.GetStateText(const ShowCaptions: Boolean): string;
begin
  if ShowCaptions
  then Result := Format(SCaptionState, [GetDataSetStateText(DataSetState)])
  else Result := GetDataSetStateText(DataSetState);
end;

function TRDbStatus.GetInternalCompleteText: string;
begin
  if FRecordsPanel > -1 then
  begin
    if FStatePanel > -1 then
    begin
      if FRecordsPanel <= FStatePanel then
        Result := GetRecordsText(True) + ComplDivider + GetStateText(True)
      else
        Result := GetStateText(True) + ComplDivider + GetRecordsText(True);
    end
    else Result := GetRecordsText(True);
  end
  else begin
    if FStatePanel > -1 then Result := GetStateText(False) else Result := EmptyStr;
  end;
end;

procedure TRDbStatus.SetInternalPanelsText;
begin
  if Assigned(FStatusBar) and (FRecordsPanel > -1)
  and (FRecordsPanel < FStatusBar.Panels.Count) then
    FStatusBar.Panels[FRecordsPanel].Text := GetRecordsText(soShowCaptions in FOptions);
  if Assigned(FStatusBar) and (FStatePanel > -1)
  and (FStatePanel < FStatusBar.Panels.Count) then
    FStatusBar.Panels[FStatePanel].Text := GetStateText(False);
end;

procedure TRDbStatus.UpdateStatus;
begin
  if Assigned(FStatusBar) then
  begin
    if FStatusBar.SimplePanel
    then StatusBar.SimpleText := GetInternalCompleteText
    else SetInternalPanelsText;
  end;
end;

function TRDbStatus.GetStatus: string;
begin
  Result := GetDataSetStateText(DataSetState);
end;

{ == TRDbFilterStatus ========================================================== }

constructor TRDbFilterStatus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := nil;
  FFilterPanel := 2;
end;

destructor TRDbFilterStatus.Destroy;
begin
  FFilter := nil;
  inherited Destroy;
end;

procedure TRDbFilterStatus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (RDbFilter <> nil) and
    (AComponent = RDbFilter) then RDbFilter := nil;
end;

procedure TRDbFilterStatus.SetFilter(Value: TRDbFilter);
begin
  if FFilter <> Value then
  begin
    FFilter := Value;
    if Value <> nil then Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then UpdateStatus;
  end;
end;

procedure TRDbFilterStatus.SetFilterPanel(Value: Integer);
begin
  if FFilterPanel <> Value then
  begin
    FFilterPanel := Value;
    if (FStatusBar <> nil) and FStatusBar.SimplePanel
    and (FFilterPanel > -1) then begin
      if FRecordsPanel < FStatePanel
      then FFilterPanel := FStatePanel + 1
      else FFilterPanel := FRecordsPanel + 1;
    end;
    if not (csLoading in ComponentState) then UpdateStatus;
  end;
end;

function TRDbFilterStatus.GetFilterText: string;
begin
  if Assigned(FFilter) and FFilter.Active
  then Result := FFilter.GetTextString
  else Result := EmptyStr;
end;

function TRDbFilterStatus.GetInternalCompleteText: string;
var
  FilterText: string;
begin
  Result := inherited GetInternalCompleteText;
  if FFilterPanel > -1 then begin
    FilterText := GetFilterText;
    if (FilterText <> EmptyStr) then begin
      if Result <> EmptyStr then Result := Result + ComplDivider;
      Result := Result + FilterText;
    end;
  end;
end;

procedure TRDbFilterStatus.SetInternalPanelsText;
begin
  inherited SetInternalPanelsText;
  if Assigned(FStatusBar) and (FFilterPanel > -1)
  and (FFilterPanel < FStatusBar.Panels.Count) then
    FStatusBar.Panels[FFilterPanel].Text := GetFilterText;
end;

end.
