unit RDUItemCreate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, RDbUpdater;

type
  TRDUItemsCreate = class(TForm)
    TypeComboBox: TComboBox;
    TypeComboBoxLabel: TLabel;
    NameEdit: TEdit;
    NameEditLabel: TLabel;
    FieldComboBox: TComboBox;
    CaptionComboBox: TComboBox;
    FieldComboBoxLabel: TLabel;
    CaptionComboBoxLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FieldComboBoxChange(Sender: TObject);
    procedure TypeComboBoxChange(Sender: TObject);
  private
    function GenerateUniqueName(const ClassName, FieldName: string): string;
  public
    Updater: TRDbUpdater;
  end;

function CreateNewUpdaterItem(Updater: TRDbUpdater): TRDUItem;

implementation

{$R *.DFM}

function CreateNewUpdaterItem(Updater: TRDbUpdater): TRDUItem;
var
  RDUItemsCreate: TRDUItemsCreate;
  i: TRDUItemType;
  j: Integer;
begin
  Result := nil;
  RDUItemsCreate := TRDUItemsCreate.Create(Application);
  try
    RDUItemsCreate.Updater := Updater;
    RDUItemsCreate.TypeComboBox.Items.Clear;
    for i := Low(TRDUItemType) to High(TRDUItemType) do
      RDUItemsCreate.TypeComboBox.Items.Add(SRDUItemNames[i]);
    if RDUItemsCreate.TypeComboBox.Items.Count > 0 then
      RDUItemsCreate.TypeComboBox.ItemIndex := 0;
    RDUItemsCreate.TypeComboBoxChange(nil);
    RDUItemsCreate.FieldComboBox.Items.Clear;
    if (Updater <> nil) and (Updater.Editor <> nil) and (Updater.Editor.DataSet <> nil) then
    begin
      for j := 0 to Updater.Editor.DataSet.FieldCount - 1 do
        if Updater.Editor.DataSet.Fields[j].FieldKind = fkData then
          RDUItemsCreate.FieldComboBox.Items.Add(Updater.Editor.DataSet.Fields[j].FieldName);
      if RDUItemsCreate.FieldComboBox.Items.Count > 0 then
        RDUItemsCreate.FieldComboBox.Text := RDUItemsCreate.FieldComboBox.Items.Strings[0];
      RDUItemsCreate.FieldComboBoxChange(nil);
    end;
    if RDUItemsCreate.ShowModal = mrOk then
    begin
      Result := TRDUItemClasses[TRDUItemType(RDUItemsCreate.TypeComboBox.ItemIndex)].Create(Updater.Owner);
      try
        Result.Name := RDUItemsCreate.NameEdit.Text;
        Result.FieldName := RDUItemsCreate.FieldComboBox.Text;
        Result.FieldCaption := RDUItemsCreate.CaptionComboBox.Text;
        Result.Active := True;
        Result.Visible := True;
        // Updater.AddItem(Result);
      except
        FreeAndNil(Result);
      end;
    end;
  finally
    FreeAndNil(RDUItemsCreate);
  end;
end;

procedure TRDUItemsCreate.FormShow(Sender: TObject);
begin
  OkButton.Enabled := (TypeComboBox.ItemIndex > -1) and (NameEdit.Text <> EmptyStr);
end;

procedure TRDUItemsCreate.FieldComboBoxChange(Sender: TObject);
begin
  CaptionComboBox.Items.Clear;
  if (Updater <> nil) and (Updater.Editor.DataSet <> nil)
  and (Updater.Editor.DataSet.FindField(FieldComboBox.Text) <> nil) then
  begin
    CaptionComboBox.Items.Add(Updater.Editor.DataSet.FieldByName(FieldComboBox.Text).DisplayLabel);
    CaptionComboBox.Text := Updater.Editor.DataSet.FieldByName(FieldComboBox.Text).DisplayLabel;
    case Updater.Editor.DataSet.FieldByName(FieldComboBox.Text).DataType of
      ftBoolean:
        TypeComboBox.ItemIndex := Integer(uiBoolean);
      ftInteger, ftSmallint, ftLargeInt, ftWord, ftAutoInc:
        TypeComboBox.ItemIndex := Integer(uiInteger);
      ftFloat, ftCurrency, ftBCD:
        TypeComboBox.ItemIndex := Integer(uiFloat);
      ftDate, ftDateTime, ftTime:
        TypeComboBox.ItemIndex := Integer(uiDateTime);
      ftString, ftWideString, ftBlob, ftMemo, ftFmtMemo:
        TypeComboBox.ItemIndex := Integer(uiString);
    end;
    TypeComboBoxChange(Sender);
  end;
  FormShow(Sender);
end;

procedure TRDUItemsCreate.TypeComboBoxChange(Sender: TObject);
begin
  NameEdit.Text := GenerateUniqueName(TRDUItemClasses[TRDUItemType(TypeComboBox.ItemIndex)].ClassName,
    FieldComboBox.Text);
  FormShow(Sender);
end;

function TRDUItemsCreate.GenerateUniqueName(const ClassName, FieldName: string): string;
var
  n: Integer;

  function IsUniqueName(const AName: string): Boolean;
  var
    i: Integer;
  begin
    Result := Updater.Owner = nil;
    if Result then Exit;
    with Updater.Owner do
      for i := 0 to ComponentCount - 1 do
        if CompareText(AName, TRDUItem(Components[i]).Name) = 0 then Exit;
    Result := True;
  end;

begin
  if Trim(FieldName) <> EmptyStr
  then Result := Updater.Name + '_' + Trim(FieldName)
  else Result := ClassName;
  if IsUniqueName(Result) then Exit;
  n := 1;
  while not IsUniqueName(Format('%s%d', [Result, n])) do
    Inc(n);
  Result := Format('%s%d', [Result, n]);
end;

end.

