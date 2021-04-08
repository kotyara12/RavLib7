unit RDFItemCreate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, RDbFilter;

type
  TRDFItemsCreate = class(TForm)
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
    ActiveCheckBox: TCheckBox;
    InvertedCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FieldComboBoxChange(Sender: TObject);
    procedure TypeComboBoxChange(Sender: TObject);
  private
    function GenerateUniqueName(const ClassName, FieldName: string): string;
  public
    Filter: TRDbFilter;
  end;

function CreateNewFilterItem(Filter: TRDbFilter): TRDFItem;

implementation

{$R *.DFM}

function CreateNewFilterItem(Filter: TRDbFilter): TRDFItem;
var
  RDFItemsCreate: TRDFItemsCreate;
  i: TRDFItemType;
  j: Integer;
begin
  Result := nil;
  RDFItemsCreate := TRDFItemsCreate.Create(Application);
  try
    RDFItemsCreate.Filter := Filter;
    RDFItemsCreate.TypeComboBox.Items.Clear;
    for i := Low(TRDFItemType) to High(TRDFItemType) do
      RDFItemsCreate.TypeComboBox.Items.Add(SRDFItemNames[i]);
    if RDFItemsCreate.TypeComboBox.Items.Count > 0 then RDFItemsCreate.TypeComboBox.ItemIndex := 0;
    RDFItemsCreate.TypeComboBoxChange(nil);
    RDFItemsCreate.FieldComboBox.Items.Clear;
    if (Filter <> nil) and (Filter.DataSet <> nil) then
    begin
      for j := 0 to Filter.DataSet.FieldCount - 1 do
        if Filter.DataSet.Fields[j].FieldKind = fkData then
          RDFItemsCreate.FieldComboBox.Items.Add(Filter.DataSet.Fields[j].FieldName);
      if RDFItemsCreate.FieldComboBox.Items.Count > 0 then
        RDFItemsCreate.FieldComboBox.Text := RDFItemsCreate.FieldComboBox.Items.Strings[0];
      RDFItemsCreate.FieldComboBoxChange(nil);
    end;
    if RDFItemsCreate.ShowModal = mrOk then
    begin
      Result := TRDFItemClasses[TRDFItemType(RDFItemsCreate.TypeComboBox.ItemIndex)].Create(Filter.Owner);
      try
        Result.Name := RDFItemsCreate.NameEdit.Text;
        Result.FieldName := RDFItemsCreate.FieldComboBox.Text;
        Result.FieldCaption := RDFItemsCreate.CaptionComboBox.Text;
        Result.Active := RDFItemsCreate.ActiveCheckBox.Checked;
        Result.Invert := RDFItemsCreate.InvertedCheckBox.Checked;
        // Filter.AddItem(Result);
      except
        FreeAndNil(Result);
      end;
    end;
  finally
    FreeAndNil(RDFItemsCreate);
  end;
end;

procedure TRDFItemsCreate.FormShow(Sender: TObject);
begin
  ActiveCheckBox.Enabled := FieldComboBox.Text <> EmptyStr;
  if not ActiveCheckBox.Enabled then ActiveCheckBox.Checked := False;
  OkButton.Enabled := (TypeComboBox.ItemIndex > -1) and (NameEdit.Text <> EmptyStr);
end;

procedure TRDFItemsCreate.FieldComboBoxChange(Sender: TObject);
begin
  CaptionComboBox.Items.Clear;
  if (Filter <> nil) and (Filter.DataSet <> nil)
  and (Filter.DataSet.FindField(FieldComboBox.Text) <> nil) then
  begin
    CaptionComboBox.Items.Add(Filter.DataSet.FieldByName(FieldComboBox.Text).DisplayLabel);
    CaptionComboBox.Text := Filter.DataSet.FieldByName(FieldComboBox.Text).DisplayLabel;
    case Filter.DataSet.FieldByName(FieldComboBox.Text).DataType of
      ftBoolean:
        TypeComboBox.ItemIndex := Integer(fiBoolean);
      ftInteger, ftSmallint, ftLargeInt, ftWord, ftAutoInc:
        TypeComboBox.ItemIndex := Integer(fiInteger);
      ftFloat, ftCurrency, ftBCD:
        TypeComboBox.ItemIndex := Integer(fiFloat);
      ftDate, ftDateTime, ftTime:
        TypeComboBox.ItemIndex := Integer(fiDate);
      ftString, ftWideString:
        TypeComboBox.ItemIndex := Integer(fiString);
      ftBlob, ftMemo, ftFmtMemo:
        TypeComboBox.ItemIndex := Integer(fiText);
    end;
    TypeComboBoxChange(Sender);
  end;
  FormShow(Sender);
end;

procedure TRDFItemsCreate.TypeComboBoxChange(Sender: TObject);
begin
  NameEdit.Text := GenerateUniqueName(TRDFItemClasses[TRDFItemType(TypeComboBox.ItemIndex)].ClassName,
    FieldComboBox.Text);
  FormShow(Sender);
end;

function TRDFItemsCreate.GenerateUniqueName(const ClassName, FieldName: string): string;
var
  n: Integer;

  function IsUniqueName(const AName: string): Boolean;
  var
    i: Integer;
  begin
    Result := Filter.Owner = nil;
    if Result then Exit;
    with Filter.Owner do
      for i := 0 to ComponentCount - 1 do
        if CompareText(AName, TRDFItem(Components[i]).Name) = 0 then Exit;
    Result := True;
  end;

begin
  if Trim(FieldName) <> EmptyStr
  then Result := Filter.Name + '_' + Trim(FieldName)
  else Result := ClassName;
  if IsUniqueName(Result) then Exit;
  n := 1;
  while not IsUniqueName(Format('%s%d', [Result, n])) do
    Inc(n);
  Result := Format('%s%d', [Result, n]);
end;

end.

