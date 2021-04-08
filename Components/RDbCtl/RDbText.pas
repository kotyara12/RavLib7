unit RDbText;

interface

uses
  Classes, Controls, StdCtrls, Messages, Db, DbCtrls;

type

{ == TRDbText ================================================================== }

  TRDbText = class (TCustomStaticText)
  private
    FDataLink: TFieldDataLink;
    FNullText: string;
    procedure DataChange(Sender: TObject);
    function  GetDataField: string;
    function  GetDataSource: TDataSource;
    function  GetField: TField;
    function  GetFieldText: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetNullText(const Value: string);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property FieldText: string read GetFieldText;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color nodefault;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property NullText: string read FNullText write SetNullText;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property Height;
    property Width;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Themes;

{ == TRDbText ================================================================== }

constructor TRDbText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := True;
  ShowAccelChar := False;
  if ThemeServices.ThemesEnabled then
    Transparent := False;
  FNullText := '';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TRDbText.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TRDbText.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TRDbText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TRDbText.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TRDbText.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
    inherited SetAutoSize(Value);
end;

function TRDbText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRDbText.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TRDbText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRDbText.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TRDbText.SetNullText(const Value: string);
begin
  if FNullText <> Value then begin
    FNullText := Value;
    DataChange(Self);
  end;
end;

function TRDbText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TRDbText.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field.IsNull
    then Result := FNullText
    else begin
      if FDataLink.Field.DataType in [ftMemo, ftFmtMemo]
      then Result := FDataLink.Field.AsString
      else Result := FDataLink.Field.DisplayText;
    end;
  end
  else begin
    if csDesigning in ComponentState
    then Result := Name
    else Result := '';
  end;
end;

procedure TRDbText.DataChange(Sender: TObject);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil)
  and not DataSource.DataSet.ControlsDisabled then
    Caption := GetFieldText;
end;

procedure TRDbText.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TRDbText.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TRDbText.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.
