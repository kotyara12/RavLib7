unit RDbGroupPrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Db, RDbEditor,
  ComCtrls, RavListView, ImgList, Menus, ActnList;

type
  TFormGroupPrm = class(TDialogTemplate)
    lblGroups: TLabel;
    ImageList: TImageList;
    lvGroups: TRSortListView;
    btnGroupAdd: TBitBtn;
    btnGroupDelete: TBitBtn;
    btnGroupUp: TBitBtn;
    btnGroupDown: TBitBtn;
    lblFunctions: TLabel;
    lvFunctions: TRSortListView;
    btnFunctionAdd: TBitBtn;
    btnFunctionDelete: TBitBtn;
    btnFunctionUp: TBitBtn;
    btnFunctionDown: TBitBtn;
    ActionList: TActionList;
    pmGroups: TPopupMenu;
    pmFunctions: TPopupMenu;
    GroupAdd: TAction;
    GroupDelete: TAction;
    GroupUp: TAction;
    GroupDown: TAction;
    FunctionAdd: TAction;
    FunctionDelete: TAction;
    FunctionUp: TAction;
    FunctionDown: TAction;
    itemGroupAdd: TMenuItem;
    itemGroupDelete: TMenuItem;
    N1: TMenuItem;
    itemGroupUp: TMenuItem;
    itemGroupDown: TMenuItem;
    itemFunctionAdd: TMenuItem;
    itemFunctionDelete: TMenuItem;
    N2: TMenuItem;
    itemFunctionUp: TMenuItem;
    itemFunctionDown: TMenuItem;
    procedure GroupAddUpdate(Sender: TObject);
    procedure GroupAddExecute(Sender: TObject);
    procedure GroupDeleteUpdate(Sender: TObject);
    procedure GroupDeleteExecute(Sender: TObject);
    procedure GroupUpUpdate(Sender: TObject);
    procedure GroupUpExecute(Sender: TObject);
    procedure GroupDownUpdate(Sender: TObject);
    procedure GroupDownExecute(Sender: TObject);
    procedure FunctionAddUpdate(Sender: TObject);
    procedure FunctionAddExecute(Sender: TObject);
    procedure FunctionDeleteUpdate(Sender: TObject);
    procedure FunctionDeleteExecute(Sender: TObject);
    procedure FunctionUpUpdate(Sender: TObject);
    procedure FunctionUpExecute(Sender: TObject);
    procedure FunctionDownUpdate(Sender: TObject);
    procedure FunctionDownExecute(Sender: TObject);
    procedure CheckButtons(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormActivate(Sender: TObject);
  private
    fEditor: TRDbCustomEditor;
    function  GetImageOnFieldType(Field: TField): Integer;
    procedure GroupLoadFields(cbList: TComboBox);
    procedure GroupAddField(Field: TField);
    procedure FunctionLoadFields(cbList, cbFunc: TComboBox);
    procedure FunctionAddField(Field: TField; const FncType: Integer);
  public
    procedure LoadIni;
    procedure StoreIni;
  end;

procedure ShowGroupingData(DS: TRDbCustomEditor);

implementation

uses
  IniFiles, RxStrUtils,
  RVclUtils, RSysUtils, RMsgRu, RListView, RDialogs, RProgress,
  RDbGroupGrp, RDbGroupFnc, RDbGroupRes;

{$R *.dfm}

const
  imString   = 0;
  imNumber   = 1;
  imDateTime = 2;
  imLogical  = 3;
  imBinary   = 4;
  imOther    = 5;

  siField    = 0;
  siFunction = 1;

  sFncNames  : array [fsSum..fsCount] of string = ('SUM', 'MIN', 'MAX', 'AVG', 'COUNT');
  sFncDescr  : array [fsSum..fsCount] of string = ('Сумма значений', 'Минимальное значение', 'Максимальное значение', 'Среднее значение', 'Количество значений');

procedure ShowGroupingData(DS: TRDbCustomEditor);
begin
  with TFormGroupPrm.Create(Application) do
  begin
    try
      fEditor := DS;
      LoadIni;
      if ShowModal = mrOk then
      begin
        StoreIni;
        ShowDbGrouping(fEditor, lvGroups, lvFunctions);
      end;
    finally
      Free;
    end;
  end;
end;

{ TFormGroupPrm }

procedure TFormGroupPrm.FormActivate(Sender: TObject);
begin
  inherited;

  CheckButtons(nil, nil, ctState);
end;

procedure TFormGroupPrm.CheckButtons(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  OkBtn.Enabled := (lvGroups.Items.Count > 0) and (lvFunctions.Items.Count > 0);
end;

function TFormGroupPrm.GetImageOnFieldType(Field: TField): Integer;
begin
  case Field.DataType of
    ftString, ftFixedChar, ftWideString, ftGuid:
      Result := imString;

    ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftAutoInc, ftLargeint:
      Result := imNumber;

    ftDate, ftTime, ftDateTime:
      Result := imDateTime;

    ftBoolean:
      Result := imLogical;

    ftBytes, ftVarBytes, ftBlob, ftGraphic, ftMemo, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftOraBlob, ftOraClob:
      Result := imBinary;

    else
      Result := imOther;
  end;
end;

{ INI- файл }

const
  secGroup      = 'GROUPING_%s.%s';
  iniGroup      = 'GroupField_%d';
  iniFunction   = 'FunctionField_%d';

procedure TFormGroupPrm.LoadIni;
var
  Ini: TMemIniFile;
  sSection, sValue: string;
  fField: TField;
  i, j, iFncType: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    lvGroups.Items.BeginUpdate;
    lvFunctions.Items.BeginUpdate;
    try
      lvGroups.Items.Clear;
      lvFunctions.Items.Clear;

      Ini := TMemIniFile.Create(GetApplicationIniFile);
      try
        sSection := AnsiUpperCase(Format(secGroup, [fEditor.Owner.ClassName, fEditor.GetObjectName(etView)]));

        i := 1;
        while Ini.ValueExists(sSection, Format(iniGroup, [i])) do
        begin
          sValue := Ini.ReadString(sSection, Format(iniGroup, [i]), EmptyStr);
          if sValue <> '' then
          begin
            fField := fEditor.DataSet.FindField(sValue);
            if Assigned(fField) then
              GroupAddField(fField);
          end;
          Inc(i);
        end;

        i := 1;
        while Ini.ValueExists(sSection, Format(iniFunction, [i])) do
        begin
          sValue := Ini.ReadString(sSection, Format(iniFunction, [i]), EmptyStr);
          if (sValue <> '') and (WordCount(sValue, [';']) = 2) then
          begin
            fField := fEditor.DataSet.FindField(ExtractWord(1, sValue, [';']));

            iFncType := fsSum;
            sValue := Trim(ExtractWord(2, sValue, [';']));
            for j := fsSum to fsCount do
              if SameText(sFncNames[j], sValue) then
              begin
                iFncType := j;
                Break;
              end;

            if Assigned(fField) then
              FunctionAddField(fField, iFncType);
          end;
          Inc(i);
        end;
      finally
        Ini.Free;
      end;

      lvGroups.ClearSelection;
      lvFunctions.ClearSelection;
    finally
      lvGroups.Items.EndUpdate;
      lvFunctions.Items.EndUpdate;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormGroupPrm.StoreIni;
var
  Ini: TMemIniFile;
  sSection: string;
  i, iCount: Integer;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    Ini := TMemIniFile.Create(GetApplicationIniFile);
    try
      sSection := AnsiUpperCase(Format(secGroup, [fEditor.Owner.ClassName, fEditor.GetObjectName(etView)]));
      Ini.EraseSection(sSection);

      iCount := lvGroups.Items.Count - 1;
      for i := 0 to iCount do
        Ini.WriteString(sSection, Format(iniGroup, [i + 1]), lvGroups.Items[i].SubItems[siField]);

      iCount := lvFunctions.Items.Count - 1;
      for i := 0 to iCount do
        Ini.WriteString(sSection, Format(iniFunction, [i + 1]),
          lvFunctions.Items[i].SubItems[siField] + ';' + lvFunctions.Items[i].SubItems[siFunction]);

      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ Группировка }

procedure TFormGroupPrm.GroupAddUpdate(Sender: TObject);
begin
  GroupAdd.Enabled := IsNotWait and Assigned(fEditor);
end;

procedure TFormGroupPrm.GroupLoadFields(cbList: TComboBox);
var
  i, iCount: Integer;
  fField: TField;

  function CheckExists(Field: TField): Boolean;
  var
    i, iCount: Integer;
  begin
    Result := False;
    iCount := lvGroups.Items.Count - 1;
    for i := 0 to iCount do
      if lvGroups.Items[i].Data = Field then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  cbList.Items.BeginUpdate;
  try
    cbList.Items.Clear;
    iCount := fEditor.DataSet.Fields.Count - 1;
    for i := 0 to iCount do
    begin
      fField := fEditor.DataSet.Fields[i];
      if (GetImageOnFieldType(fField) < imBinary) and not CheckExists(fField) then
        cbList.AddItem(Format('%s {%s}', [fField.DisplayName, fField.FieldName]), fField);
    end;
  finally
    cbList.Items.EndUpdate;
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormGroupPrm.GroupAddField(Field: TField);
begin
  lvGroups.Selected := lvGroups.Items.Add;
  with lvGroups.Selected do
  begin
    Data := Field;
    ImageIndex := GetImageOnFieldType(Field);
    Caption := Field.DisplayName;
    Subitems.Add(Field.FieldName);
  end;
end;

procedure TFormGroupPrm.GroupAddExecute(Sender: TObject);
begin
  with TFormGroupGrp.Create(Self) do
  begin
    try
      GroupLoadFields(cbFields);
      if ShowModal = mrOk then
        GroupAddField(TField(cbFields.Items.Objects[cbFields.ItemIndex]));
    finally
      Free;
    end;
  end;
end;

procedure TFormGroupPrm.GroupDeleteUpdate(Sender: TObject);
begin
  GroupDelete.Enabled := IsNotWait and Assigned(fEditor) and Assigned(lvGroups.Selected);
end;

procedure TFormGroupPrm.GroupDeleteExecute(Sender: TObject);
begin
  if DeleteQueryName(lvGroups.Selected.Caption) then
    lvGroups.DeleteSelected;
end;

procedure TFormGroupPrm.GroupUpUpdate(Sender: TObject);
begin
  GroupUp.Enabled := IsNotWait and Assigned(fEditor) and Assigned(lvGroups.Selected)
    and (lvGroups.Selected.Index > 0);
end;

procedure TFormGroupPrm.GroupUpExecute(Sender: TObject);
begin
  MoveSelectedItemUp(lvGroups);
end;

procedure TFormGroupPrm.GroupDownUpdate(Sender: TObject);
begin
  GroupDown.Enabled := IsNotWait and Assigned(fEditor) and Assigned(lvGroups.Selected)
    and (lvGroups.Selected.Index < (lvGroups.Items.Count - 1));
end;

procedure TFormGroupPrm.GroupDownExecute(Sender: TObject);
begin
  MoveSelectedItemDown(lvGroups);
end;

{ Функции }

procedure TFormGroupPrm.FunctionAddUpdate(Sender: TObject);
begin
  FunctionAdd.Enabled := IsNotWait and Assigned(fEditor);
end;

procedure TFormGroupPrm.FunctionLoadFields(cbList, cbFunc: TComboBox);
var
  i, iCount: Integer;
  fField: TField;
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  cbList.Items.BeginUpdate;
  cbFunc.Items.BeginUpdate;
  try
    cbList.Items.Clear;
    iCount := fEditor.DataSet.Fields.Count - 1;
    for i := 0 to iCount do
    begin
      fField := fEditor.DataSet.Fields[i];
      if (GetImageOnFieldType(fField) = imNumber) then
        cbList.AddItem(Format('%s {%s}', [fField.DisplayName, fField.FieldName]), fField);
    end;

    cbFunc.Items.Clear;
    for i := fsSum to fsCount do
      cbFunc.Items.Add(sFncDescr[i]);
    cbFunc.ItemIndex := 0;
  finally
    cbList.Items.EndUpdate;
    cbFunc.Items.EndUpdate;
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormGroupPrm.FunctionAddField(Field: TField; const FncType: Integer);
begin
  lvFunctions.Selected := lvFunctions.Items.Add;
  with lvFunctions.Selected do
  begin
    Data := Field;
    ImageIndex := GetImageOnFieldType(Field);
    StateIndex := FncType;
    Caption := Field.DisplayName;
    Subitems.Add(Field.FieldName);
    Subitems.Add(sFncNames[FncType]);
  end;
end;

procedure TFormGroupPrm.FunctionAddExecute(Sender: TObject);
begin
  with TFormGroupFnc.Create(Self) do
  begin
    try
      FunctionLoadFields(cbFields, cbFunc);
      if ShowModal = mrOk then
        FunctionAddField(TField(cbFields.Items.Objects[cbFields.ItemIndex]), cbFunc.ItemIndex);
    finally
      Free;
    end;
  end;
end;

procedure TFormGroupPrm.FunctionDeleteUpdate(Sender: TObject);
begin
  FunctionDelete.Enabled := IsNotWait and Assigned(fEditor) and Assigned(lvFunctions.Selected);
end;

procedure TFormGroupPrm.FunctionDeleteExecute(Sender: TObject);
begin
  if DeleteQueryName(lvFunctions.Selected.Caption) then
    lvFunctions.DeleteSelected;
end;

procedure TFormGroupPrm.FunctionUpUpdate(Sender: TObject);
begin
  FunctionUp.Enabled := IsNotWait and Assigned(fEditor) and Assigned(lvFunctions.Selected)
    and (lvFunctions.Selected.Index > 0);
end;

procedure TFormGroupPrm.FunctionUpExecute(Sender: TObject);
begin
  MoveSelectedItemUp(lvFunctions);
end;

procedure TFormGroupPrm.FunctionDownUpdate(Sender: TObject);
begin
  FunctionDown.Enabled := IsNotWait and Assigned(fEditor) and Assigned(lvFunctions.Selected)
    and (lvFunctions.Selected.Index < (lvFunctions.Items.Count - 1));
end;

procedure TFormGroupPrm.FunctionDownExecute(Sender: TObject);
begin
  MoveSelectedItemDown(lvFunctions);
end;

end.
