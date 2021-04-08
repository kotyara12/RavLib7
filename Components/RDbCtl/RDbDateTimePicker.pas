unit RDbDateTimePicker;

interface

uses
  SysUtils, Classes, Controls, ComCtrls, Windows, Messages, Db, DbCtrls;

type
  TRDbDateTimePicker = class(TDateTimePicker)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function  GetDataField: string;
    function  GetDataSource: TDataSource;
    function  GetReadOnly: Boolean;
    function  GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

(* procedure Register;
begin
  RegisterComponents('Rav Soft', [TRDbDateTimePicker]);
end; *)

{ TRDbDateTimePicker }

constructor TRDbDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TRDbDateTimePicker.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TRDbDateTimePicker.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TRDbDateTimePicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TRDbDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TRDbDateTimePicker.KeyPress(var Key: Char);
begin
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        Key := #0;
      end;
  end;
end;

procedure TRDbDateTimePicker.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TRDbDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRDbDateTimePicker.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TRDbDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRDbDateTimePicker.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TRDbDateTimePicker.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRDbDateTimePicker.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TRDbDateTimePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TRDbDateTimePicker.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TRDbDateTimePicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TRDbDateTimePicker.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    Checked := not FDataLink.Field.IsNull;
    if Checked then
      DateTime := FDataLink.Field.AsDateTime
    else
      DateTime := Now;
  end
  else begin
    Checked := False;
    DateTime := Now;
  end;
end;

procedure TRDbDateTimePicker.UpdateData(Sender: TObject);
begin
  if Checked then
    FDataLink.Field.AsDateTime := DateTime
  else
    FDataLink.Field.Clear;
end;

end.
