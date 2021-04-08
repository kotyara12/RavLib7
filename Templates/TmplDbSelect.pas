unit TmplDbSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, DB, ADODB, StdCtrls, Buttons, ExtCtrls, DBCtrls;

type
  TDbSelectTemplate = class(TDialogTemplate)
    GroupsDataSource: TDataSource;
    ItemsDataSource: TDataSource;
    GroupsDBLookupComboBoxLabel: TLabel;
    RefButton: TSpeedButton;
    ItemsDBLookupComboBoxLabel: TLabel;
    InfoGroupBox: TGroupBox;
    ITEMS_DATA: TADOQuery;
    GROUPS_DATA: TADOQuery;
    GroupsBox: TDBLookupComboBox;
    ItemsBox: TDBLookupComboBox;
    procedure FormShow(Sender: TObject);
    procedure GroupsScroll(DataSet: TDataSet);
    procedure ItemsScroll(DataSet: TDataSet);
  private
    FParam0: Integer;
    FParam1: Integer;
    FGroupKey: Integer;
    FItemKey: Integer;
    procedure SetGroupKey(const Value: Integer);
    procedure SetItemKey(const Value: Integer);
    procedure SetParam0(const Value: Integer);
    procedure SetParam1(const Value: Integer);
  protected
    procedure StartForm; override;
    procedure DoneForm; override;
    procedure LoadData; dynamic;
    procedure CloseData; dynamic;
  public
    property QryParam0: Integer read FParam0 write SetParam0;
    property QryParam1: Integer read FParam1 write SetParam1;
    property GroupKeyValue: Integer read FGroupKey write SetGroupKey;
    property ItemKeyValue: Integer read FItemKey write SetItemKey;
  end;

  TDbSelectClass = class of TDbSelectTemplate;

implementation

{$R *.dfm}

uses
  RVclUtils, RDbConst, RDialogs, RMsgRu, BaseDbUnit;

procedure TDbSelectTemplate.LoadData;
var
  StartGroup, StartItem: Integer;
begin
  StartGroup := FGroupKey; StartItem := FItemKey;
  if BaseData.OpenDataSetWait(GROUPS_DATA)
  and BaseData.OpenDataSetWait(ITEMS_DATA) then begin
    SetGroupKey(StartGroup);
    SetItemKey(StartItem);
  end;
end;

procedure TDbSelectTemplate.CloseData;
begin
  if ITEMS_DATA.Active then ITEMS_DATA.Close;
  if GROUPS_DATA.Active then GROUPS_DATA.Close;
end;

procedure TDbSelectTemplate.FormShow(Sender: TObject);
begin
  try
    inherited;
  finally
    OkBtn.Enabled := (GroupKeyValue > intDisable) and (ItemKeyValue > intDisable);
  end;
end;

procedure TDbSelectTemplate.StartForm;
begin
  try
    inherited;
  finally
    LoadData;
  end;
end;

procedure TDbSelectTemplate.DoneForm;
begin
  try
    CloseData;
  finally
    inherited;
  end;
end;

procedure TDbSelectTemplate.SetGroupKey(const Value: Integer);
begin
  if GROUPS_DATA.Active then begin
    if GROUPS_DATA.IsEmpty
    then GroupsBox.KeyValue := intDisable
    else begin
      if (Value > intDisable)
      and not GROUPS_DATA.Locate(GroupsBox.KeyField, Value, [])
      then begin
        if Value > 0
        then ErrorBox(Format(SErrDSIdNotFound, [Value, GROUPS_DATA.Name]));
      end;
      if GROUPS_DATA.FieldByName(GroupsBox.KeyField).Value <> GroupsBox.KeyValue
      then GroupsBox.KeyValue := GROUPS_DATA.FieldByName(GroupsBox.KeyField).Value;
    end;
  end
  else FGroupKey := Value;
end;

procedure TDbSelectTemplate.SetItemKey(const Value: Integer);
begin
  if ITEMS_DATA.Active then begin
    if ITEMS_DATA.IsEmpty
    then ItemsBox.KeyValue := intDisable
    else begin
      if (Value > intDisable)
      and not ITEMS_DATA.Locate(ItemsBox.KeyField, Value, [])
      then begin
        if Value > 0
        then ErrorBox(Format(SErrDSIdNotFound, [Value, ITEMS_DATA.Name]));
      end;
      if ITEMS_DATA.FieldByName(ItemsBox.KeyField).Value <> ItemsBox.KeyValue
      then ItemsBox.KeyValue := ITEMS_DATA.FieldByName(ItemsBox.KeyField).Value;
    end;
  end
  else FItemKey := Value;
end;

procedure TDbSelectTemplate.SetParam0(const Value: Integer);
var
  DSP: TParameter;
begin
  if Value <> FParam0 then begin
    FParam0 := Value;
    DSP := GROUPS_DATA.Parameters.FindParam(pnPARAM_0);
    if Assigned(DSP) then begin
      DSP.Value := FParam0;
      if GROUPS_DATA.Active then GROUPS_DATA.Requery([]);
    end;
    DSP := ITEMS_DATA.Parameters.FindParam(pnPARAM_0);
    if Assigned(DSP) then begin
      DSP.Value := FParam0;
      if ITEMS_DATA.Active then ITEMS_DATA.Requery([]);
    end;
    SetGroupKey(FGroupKey);
    SetItemKey(FItemKey);
  end;
end;

procedure TDbSelectTemplate.SetParam1(const Value: Integer);
var
  DSP: TParameter;
begin
  if Value <> FParam1 then begin
    FParam1 := Value;
    DSP := GROUPS_DATA.Parameters.FindParam(pnPARAM_1);
    if Assigned(DSP) then begin
      DSP.Value := FParam1;
      if GROUPS_DATA.Active then GROUPS_DATA.Requery([]);
    end;
    DSP := ITEMS_DATA.Parameters.FindParam(pnPARAM_1);
    if Assigned(DSP) then begin
      DSP.Value := FParam1;
      if ITEMS_DATA.Active then ITEMS_DATA.Requery([]);
    end;
    SetGroupKey(FGroupKey);
    SetItemKey(FItemKey);
  end;
end;

procedure TDbSelectTemplate.GroupsScroll(DataSet: TDataSet);
begin
  FGroupKey := GROUPS_DATA.FieldByName(GroupsBox.KeyField).AsInteger;
  SetItemKey(FItemKey);
end;

procedure TDbSelectTemplate.ItemsScroll(DataSet: TDataSet);
begin
  FItemKey := ITEMS_DATA.FieldByName(ItemsBox.KeyField).AsInteger;
end;

end.
