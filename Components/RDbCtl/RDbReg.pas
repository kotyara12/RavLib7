unit RDbReg;

interface

procedure Register;

implementation

uses
  Windows, TypInfo, Classes, Forms, SysUtils, Db, DesignIntf, DesignEditors,
  RDbColorGrid, RDbCustom, RDbFind, RDbSearch, RDbOrder, RDbFilter, RDbUpdater,
  RDbGridTuner, RDbStatus, RDbPanel, RDbText, RDbEditor, RDbTree,
  RDFItemsEditor, RDUItemsEditor, RDbFldListEditor;

{ == TRDbFilterEditor ========================================================== }

type
  TRDbFilterEditor = class(TComponentEditor)
  private
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TRDbFilterEditor.Edit;
begin
  EditRDbFilterItems(Designer, TRDbFilter(Component));
  Designer.Modified;
end;

procedure TRDbFilterEditor.ExecuteVerb(Index: Integer);
var
  CurrActive: Boolean;
  CurrOptions: TRDFOptions;
begin
  if Component is TRDbFilter then
  begin
    case Index of
      0: Edit;
      1: begin
           CurrActive := TRDbFilter(Component).Active;
           CurrOptions := TRDbFilter(Component).Options;
           try
             TRDbFilter(Component).Active := False;
             TRDbFilter(Component).Options := TRDbFilter(Component).Options + [foDialogOnActivate];
             TRDbFilter(Component).Active := True;
             Application.MessageBox(PChar(TRDbFilter(Component).GetWhereString), 'Where string', MB_OK + MB_ICONINFORMATION);
           finally
             TRDbFilter(Component).Active := CurrActive;
             TRDbFilter(Component).Options := CurrOptions;
           end;
         end;
      2: begin
           CurrActive := TRDbFilter(Component).Active;
           CurrOptions := TRDbFilter(Component).Options;
           try
             TRDbFilter(Component).Active := False;
             TRDbFilter(Component).Options := TRDbFilter(Component).Options + [foDialogOnActivate];
             TRDbFilter(Component).Active := True;
             Application.MessageBox(PChar(TRDbFilter(Component).GetFilterString), 'Filter string', MB_OK + MB_ICONINFORMATION);
           finally
             TRDbFilter(Component).Active := CurrActive;
             TRDbFilter(Component).Options := CurrOptions;
           end;
         end;
    end;
  end;
end;

function TRDbFilterEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit items';
    1: Result := 'Test where string';
    2: Result := 'Test filter string';
  end;
end;

function TRDbFilterEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ == TRDFItemsProperty ========================================================= }

type
  TRDFItemsProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function TRDFItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TRDFItemsProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0)
  then Result := '(None)'
  else Result := '(TRDFItems)';
end;

procedure TRDFItemsProperty.Edit;
begin
  EditRDbFilterItems(Designer, TRDbFilter(GetComponent(0)));
  Designer.Modified;
end;

{ == TRDFIFieldNameProperty ==================================================== }

type
  TRDFIFieldNameProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDFIFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDFIFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Item: TRDFItem;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDFItem) then
  begin
    Item := TRDFItem(GetComponent(0));
    if (Item.Filter <> nil) and (Item.Filter.DataSet <> nil) then begin
      for i := 0 to Item.Filter.DataSet.FieldCount - 1 do
        Proc(Item.Filter.DataSet.Fields.Fields[i].FullName);
    end;
  end;
end;

{ == TRDFILinkFieldNameProperty ================================================ }

type
  TRDFILinkFieldNameProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDFILinkFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDFILinkFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Item: TRDFLinkItem;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDFLinkItem) then
  begin
    Item := TRDFLinkItem(GetComponent(0));
    if Item.LookupDataSet <> nil then
    begin
      for i := 0 to Item.LookupDataSet.FieldCount - 1 do
        Proc(Item.LookupDataSet.Fields.Fields[i].FullName);
    end;
  end;
end;

{ == TRDFIFieldCaptionProperty ================================================= }

type
  TRDFIFieldCaptionProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDFIFieldCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TRDFIFieldCaptionProperty.GetValues(Proc: TGetStrProc);
var
  Item: TRDFItem;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDFItem) then
  begin
    Item := TRDFItem(GetComponent(0));
    if (Item.FieldName <> EmptyStr) and (Item.Filter <> nil)
    and (Item.Filter.DataSet <> nil) and (Item.Filter.DataSet.FindField(Item.FieldName) <> nil)
    then Proc(Item.Filter.DataSet.FieldByName(Item.FieldName).DisplayLabel);
  end;
end;

{ == TRDFILinkFieldCaptionProperty ============================================= }

type
  TRDFILinkFieldCaptionProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDFILinkFieldCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDFILinkFieldCaptionProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  AllItems: string;
  Item: TRDFLinkItem;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDFLinkItem) then
  begin
    Item := TRDFLinkItem(GetComponent(0));
    if Item.LookupDataSet <> nil then
    begin
      AllItems := EmptyStr;
      for i := 0 to Item.GetLookupFieldsCount - 1 do
        AllItems := AllItems + Item.GetLookupField(i).DisplayLabel + ';';
      if AllItems <> EmptyStr then Proc(Copy(AllItems, 1, Length(AllItems) - 1));
      for i := 0 to Item.GetLookupFieldsCount - 1 do
        if Item.GetLookupField(i) <> nil
        then Proc(Item.GetLookupField(i).DisplayLabel);
    end;
  end;
end;

{ == TRDbUpdaterEditor ========================================================== }

type
  TRDbUpdaterEditor = class(TComponentEditor)
  private
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TRDbUpdaterEditor.Edit;
begin
  EditRDbUpdaterItems(Designer, TRDbUpdater(Component));
  Designer.Modified;
end;

procedure TRDbUpdaterEditor.ExecuteVerb(Index: Integer);
begin
  if Component is TRDbUpdater then Edit;
end;

function TRDbUpdaterEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit items';
end;

function TRDbUpdaterEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ == TRDUItemsProperty ========================================================= }

type
  TRDUItemsProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function TRDUItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TRDUItemsProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0)
  then Result := '(None)'
  else Result := '(TRDUItems)';
end;

procedure TRDUItemsProperty.Edit;
begin
  EditRDbUpdaterItems(Designer, TRDbUpdater(GetComponent(0)));
  Designer.Modified;
end;

{ == TRDUIFieldNameProperty ==================================================== }

type
  TRDUIFieldNameProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDUIFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDUIFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Item: TRDUItem;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDUItem) then
  begin
    Item := TRDUItem(GetComponent(0));
    if (Item.Updater <> nil) and (Item.Updater.Editor <> nil)
      and (Item.Updater.Editor.DataSet <> nil) then
    begin
      for i := 0 to Item.Updater.Editor.DataSet.FieldCount - 1 do
        Proc(Item.Updater.Editor.DataSet.Fields[i].FullName);
    end;
  end;
end;

{ == TRDUILinkFieldNameProperty ================================================ }

type
  TRDUILinkFieldNameProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDUILinkFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDUILinkFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Item: TRDULinkComboItem;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDULinkComboItem) then
  begin
    Item := TRDULinkComboItem(GetComponent(0));
    if Item.LookupDataSet <> nil then
    begin
      for i := 0 to Item.LookupDataSet.FieldCount - 1 do
        Proc(Item.LookupDataSet.Fields.Fields[i].FullName);
    end;
  end;
end;

{ == TRDbOrderEditor =========================================================== }

type
  TRDbOrderEditor = class(TComponentEditor)
  private
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TRDbOrderEditor.ExecuteVerb(Index: Integer);
var
  CurrActive: Boolean;
begin
  if Component is TRDbOrder then
  begin
    CurrActive := TRDbOrder(Component).Active;
    try
      TRDbOrder(Component).Active := True;
      TRDbOrder(Component).ShowDialog;
      Application.MessageBox(PChar(TRDbOrder(Component).GetOrderString), 'Order string', MB_OK + MB_ICONINFORMATION);
    finally
      TRDbOrder(Component).Active := CurrActive;
    end;
  end;
end;

function TRDbOrderEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Test order string';
end;

function TRDbOrderEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ == TRDbFieldNameProperty ==================================================== }

type
  TRDbFieldNameProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDbFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDbFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Item: TRDbCustomDS;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDbCustomDS) then
  begin
    Item := TRDbCustomDS(GetComponent(0));
    if (Item <> nil) and (Item.DataSet <> nil) then
    begin
      for i := 0 to Item.DataSet.FieldCount - 1 do
        Proc(Item.DataSet.Fields.Fields[i].FullName);
    end;
  end;
end;

{ == TRDOrderProperty ========================================================== }

type
  TRDOrderProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDOrderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDOrderProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Ord: TRDbOrder;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDbOrder) then
  begin
    Ord := TRDbOrder(GetComponent(0));
    if Ord.DataSet <> nil then
    begin
      for i := 0 to Ord.DataSet.FieldCount - 1 do
        Proc(Ord.DataSet.Fields.Fields[i].FullName);
    end;
  end;
end;

{ == TRDbEditorFieldProperty ==================================================== }

type
  TRDbEditorFieldProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TRDbEditorFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRDbEditorFieldProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Item: TRDbCustomEditor;
begin
  if (GetComponent(0) <> nil) and (GetComponent(0) is TRDbCustomEditor) then
  begin
    Item := TRDbCustomEditor(GetComponent(0));
    if (Item <> nil) and (Item.DataSet <> nil) then
    begin
      for i := 0 to Item.DataSet.FieldCount - 1 do
        Proc(Item.DataSet.Fields.Fields[i].FieldName);
    end;
  end;
end;

{ == TRDbEditorCopyFieldsProperty ============================================== }

type
  TRDbEditorCopyFieldsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TRDbEditorCopyFieldsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TRDbEditorCopyFieldsProperty.Edit;
var
  Fields: string;
begin
  Fields := TRDbCustomEditor(GetComponent(0)).CopiedFields;
  if EditFieldsList(TRDbCustomEditor(GetComponent(0)), Fields, True) then
  begin
    TRDbCustomEditor(GetComponent(0)).CopiedFields := Fields;
    Designer.Modified;
  end;
end;

{ == TRDbEditorStatFieldsProperty ============================================== }

type
  TRDbEditorStatFieldsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TRDbEditorStatFieldsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TRDbEditorStatFieldsProperty.Edit;
var
  Fields: string;
begin
  Fields := TRDbCustomEditor(GetComponent(0)).StatisticFields;
  if EditFieldsList(TRDbCustomEditor(GetComponent(0)), Fields, False) then
  begin
    TRDbCustomEditor(GetComponent(0)).StatisticFields := Fields;
    Designer.Modified;
  end;
end;

procedure Register;
var
  i: TRDFItemType;
  j: TRDUItemType;
begin
  // TRDbFind
  RegisterComponents('Rav Soft', [TRDbFind, TRDbSearch]);
  RegisterPropertyEditor(TypeInfo(string), TRDbFind, 'DefaultField', TRDbFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbSearch, 'DefaultField', TRDbFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbSearch, 'KeyField', TRDbFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbSearch, 'ListFields', TRDbFieldNameProperty);
  // TRDbOrder
  RegisterComponents('Rav Soft', [TRDbOrder]);
  RegisterComponentEditor(TRDbOrder, TRDbOrderEditor);
  RegisterPropertyEditor(TypeInfo(string), TRDbOrder, 'OrderString', TRDOrderProperty);
  // TRDbFilter
  for i := Low(TRDFItemType) to High(TRDFItemType) do
    RegisterNoIcon([TRDFItemClasses[i]]);
  RegisterComponents('Rav Soft', [TRDbFilter]);
  RegisterComponentEditor(TRDbFilter, TRDbFilterEditor);
  RegisterPropertyEditor(TypeInfo(TList), TRDbFilter, 'Items', TRDFItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDFItem, 'FieldName', TRDFIFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDFItem, 'FieldCaption', TRDFIFieldCaptionProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDFLinkItem, 'KeyField', TRDFILinkFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDFLinkItem, 'LookupFields', TRDFILinkFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDFLinkItem, 'LookupCaptions', TRDFILinkFieldCaptionProperty);
  // TRDbUpdater
  for j := Low(TRDUItemType) to High(TRDUItemType) do
    RegisterNoIcon([TRDUItemClasses[j]]);
  RegisterComponents('Rav Soft', [TRDbUpdater]);
  RegisterComponentEditor(TRDbUpdater, TRDbUpdaterEditor);
  RegisterPropertyEditor(TypeInfo(TList), TRDbUpdater, 'Items', TRDUItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDUItem, 'FieldName', TRDUIFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDULinkComboItem, 'KeyField', TRDUILinkFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDULinkComboItem, 'LookupFields', TRDUILinkFieldNameProperty);
  // TRDbGridTuner
  RegisterComponents('Rav Soft', [TRDbGridTuner]);
  // TRDbStatus
  RegisterComponents('Rav Soft', [TRDbStatus, TRDbFilterStatus]);
  // TRDbText
  RegisterComponents('Rav Soft', [TRDbText]);
  // TRDbInfoPanel
  RegisterComponents('Rav Soft', [TRDbInfoPanel]);
  // TRDbStyledGrid, TRDbBalanceGrid
  RegisterComponents('Rav Soft', [TRDbStyledGrid, TRDbBalanceGrid]);
  RegisterNonActiveX([TRDbStyledGrid, TRDbBalanceGrid], axrComponentOnly);
  // TRDbEditor
  RegisterComponents('Rav Soft', [TRDbEditor, TRDbExportEditor]);
  RegisterPropertyEditor(TypeInfo(string), TRDbEditor, 'KeyFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbEditor, 'OwnerFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbEditor, 'BlockFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbEditor, 'CopiedFields', TRDbEditorCopyFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbEditor, 'StatisticFields', TRDbEditorStatFieldsProperty);
  // TRDbTreeEditor
  RegisterComponents('Rav Soft', [TRDbTreeLoader, TRDbTreeEditor]);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'KeyFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'OwnerFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'NameFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'NotesFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'SortFieldName', TRDbEditorFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'CopiedFields', TRDbEditorCopyFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRDbTreeEditor, 'StatisticFields', TRDbEditorStatFieldsProperty);
end;

end.
