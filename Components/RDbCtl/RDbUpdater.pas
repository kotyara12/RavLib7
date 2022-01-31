{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Database Additional Components         }
{                                                       }
{       Copyright (c) 2006-2012 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbUpdater;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ImgList, ComCtrls, ToolWin, Db, StdCtrls, RavClrCombo,
  RDbCustom, RDbEditor, TmplEditors, RDbUpdaterItem;

type
{ == Base definitions ========================================================== }

  ERDbUpdaterError = class(Exception);

  TRDbUpdater = class;

  TOnGetTextNotifyEvent = procedure(Sender: TObject; var Value: string) of object;
  TOnValueNotifyEvent   = procedure(Sender: TObject; Field: TField; const NullValue: Boolean; var State: Boolean) of object;

{ == TRDUItem ================================================================== }

  TRDUItemType      = (uiNull, uiBoolean, uiInteger, uiInt64, uiColor, uiFloat, uiDateTime, uiString, uiStringList, uiLinkCombo);

  TRDUItem = class(TComponent)
  private
    fUpdater: TRDbUpdater;
    fActive: Boolean;
    fActive_Buf: Boolean;
    fNullAuto: Boolean;
    fNullEnable: Boolean;
    fNullValue: Boolean;
    fNullValue_Buf: Boolean;
    fEnabled: Boolean;
    fVisible: Boolean;
    fFieldName: string;
    fFieldCaption: string;
    fOnAfterEdit: TNotifyEvent;
    fOnBeforeEdit: TNotifyEvent;
    fOnGetText: TOnGetTextNotifyEvent;
    fOnCheckValue: TOnValueNotifyEvent;
    fOnModifyValue: TOnValueNotifyEvent;
    function  GetActive: Boolean;
    procedure SetActive(const aValue: Boolean);
    procedure SetFieldName(const aValue: string);
  protected
    procedure SetParentComponent(AParent: TComponent); override;
    procedure ReadState(Reader: TReader); override;
    function  GetEditorClass: TFormDbUpdaterItemClass; virtual;
    procedure BeforeShowEditor; virtual;
    procedure AfterShowEditor; virtual;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); virtual; abstract;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); virtual; abstract;
    procedure CheckDbField;
    procedure CopyToEditBuffer; virtual;
    procedure RestoreFromEditBuffer; virtual;
    function  GetItemText: string; virtual; abstract;
    function  CheckFieldClear(aField: TField): Boolean; virtual;
    function  CheckFieldValue(aField: TField): Boolean; virtual; abstract;
    procedure SetFieldClear(aField: TField; var Complete: Boolean); virtual;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); virtual; abstract;
    procedure DoGetItemText(var Text: string);
    procedure DoModifyRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  GetParentComponent: TComponent; override;
    function  HasParent: Boolean; override;
    function  GetItemValue: string; virtual;
    function  GetItemField: TField; virtual;
    function  IsNeedChange: Boolean; virtual;
    function  ShowDialog(AOwner: TComponent; const ALeft, ATop: Integer): Boolean; virtual;
    property  Active: Boolean read GetActive write SetActive default False;
    function  CheckVisible: Boolean;
    property  Updater: TRDbUpdater read fUpdater;
  published
    property NullAutoCheck: Boolean read fNullAuto write fNullAuto default True;
    property NullEnable: Boolean read fNullEnable write fNullEnable default True;
    property Enabled: Boolean read fEnabled write fEnabled default True;
    property Visible: Boolean read fVisible write fVisible default True;
    property FieldName: string read fFieldName write SetFieldName;
    property FieldCaption: string read fFieldCaption write fFieldCaption;
    property OnAfterEdit: TNotifyEvent read fOnAfterEdit write fOnAfterEdit;
    property OnBeforeEdit: TNotifyEvent read fOnBeforeEdit write fOnBeforeEdit;
    property OnGetText: TOnGetTextNotifyEvent read fOnGetText write fOnGetText;
    property OnCheckValue: TOnValueNotifyEvent read fOnCheckValue write fOnCheckValue;
    property OnModifyValue: TOnValueNotifyEvent read fOnModifyValue write fOnModifyValue;
  end;

  TRDUItemClass = class of TRDUItem;

  TRDUNullItem = class(TRDUItem)
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
    property  NullEnable;
  end;

  TRDUBooleanItem = class(TRDUItem)
  private
    fValue: Boolean;
    fValue_Buf: Boolean;
    fTextTrue: string;
    fTextFalse: string;
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property NullEnable default False;
    property ValueTextTrue: string read fTextTrue write fTextTrue;
    property ValueTextFalse: string read fTextFalse write fTextFalse;
    property Value: Boolean read fValue write fValue;
  end;

  TRDUIntegerItem = class(TRDUItem)
  private
    fValue: Integer;
    fValue_Buf: Integer;
    fMinValue: Integer;
    fMaxValue: Integer;
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: Integer read fValue write fValue;
    property MinValue: Integer read fMinValue write fMinValue default 0;
    property MaxValue: Integer read fMaxValue write fMaxValue default 0;
  end;

  TRDUInt64Item = class(TRDUItem)
  private
    fValue: Int64;
    fValue_Buf: Int64;
    fMinValue: Int64;
    fMaxValue: Int64;
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: Int64 read fValue write fValue;
    property MinValue: Int64 read fMinValue write fMinValue default 0;
    property MaxValue: Int64 read fMaxValue write fMaxValue default 0;
  end;

  TRDUColorItem = class(TRDUItem)
  private
    fValue: TColor;
    fValue_Buf: TColor;
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldClear(aField: TField): Boolean; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldClear(aField: TField; var Complete: Boolean); override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: TColor read fValue write fValue;
  end;

  TRDUFloatItem = class(TRDUItem)
  private
    fValue: Double;
    fValue_Buf: Double;
    fMinValue: Double;
    fMaxValue: Double;
    fDisplayFormat: string;
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayFormat: string read fDisplayFormat write fDisplayFormat;
    property Value: Double read fValue write fValue;
    property MinValue: Double read fMinValue write fMinValue;
    property MaxValue: Double read fMaxValue write fMaxValue;
  end;

  TRDUDateTimeKind = (dtDate, dtTime, dtDateTime);

  TRDUDateTimeItem = class(TRDUItem)
  private
    fValue: TDateTime;
    fValue_Buf: TDateTime;
    fKind: TRDUDateTimeKind;
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetFormatText: string;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Kind: TRDUDateTimeKind read fKind write fKind default dtDate;
    property Value: TDateTime read fValue write fValue;
  end;

  TRDUStringItem = class(TRDUItem)
  private
    fValue: string;
    fValue_Buf: string;
    fFindAndReplace: Boolean;
    fFindAndReplace_Buf: Boolean;
    fFindValue: string;
    fFindValue_Buf: string;
    fReplaceValue: string;
    fReplaceValue_Buf: string;
    fUseHistory: Boolean;
    fHistory: TStringList;
    fStrings: TStringList;
    procedure SetStrings(const Value: TStringList);
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: string read fValue write fValue;
    property ValueFind: string read fFindValue write fFindValue;
    property ValueReplace: string read fReplaceValue write fReplaceValue;
    property FindAndReplace: Boolean read fFindAndReplace write fFindAndReplace default False;
    property UseHistory: Boolean read fUseHistory write fUseHistory default True;
    property Strings: TStringList read fStrings write SetStrings;
  end;

  TRDUStringListItem = class(TRDUItem)
  private
    fValue: Integer;
    fValue_Buf: Integer;
    fValues: TStringList;
    fStrings: TStringList;
    procedure SetStrings(const Value: TStringList);
    procedure SetValues(const Value: TStringList);
  protected
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddItem(const Value, Description: string);
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Values: TStringList read fValues write SetValues;
    property Strings: TStringList read fStrings write SetStrings;
  end;

  TOnSelectKeyNotifyEvent = procedure(Sender: TObject; var Key: Integer; out Complete: Boolean) of object;

  TRDULinkComboItem = class(TRDUItem)
  private
    fValue: Integer;
    fValue_Buf: Integer;
    fLinkDataSet: TDataSet;
    fKeyField: string;
    fLookupFields: string;
    fLookupFieldIndex: Integer;
    fLookupDisabled: Boolean;
    fLookupFilter: string;
    fComboBoxRO: Boolean;
    fOnSelectKey: TOnSelectKeyNotifyEvent;
    procedure SetDataSet(aValue: TDataSet);
    procedure SetKeyField(const aValue: string);
    procedure SetLookupFields(const aValue: string);
    function  GetLookupFieldName(const Index: Integer): string;
    function  GetLookupField(const Index: Integer): TField;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetEditorClass: TFormDbUpdaterItemClass; override;
    procedure BeforeShowEditor; override;
    procedure AfterShowEditor; override;
    procedure LoadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure ReadEditorControls(Editor: TFormDbUpdaterItem); override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    function  GetItemText: string; override;
    function  CheckFieldValue(aField: TField): Boolean; override;
    procedure SetFieldValue(aField: TField; var Complete: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: Integer read fValue write fValue default -1;
    property LookupDataSet: TDataSet read fLinkDataSet write SetDataSet;
    property KeyField: string read fKeyField write SetKeyField;
    property LookupFields: string read fLookupFields write SetLookupFields;
    property LookupFieldIndex: Integer read fLookupFieldIndex write fLookupFieldIndex default 0;
    property LookupFilter: string read fLookupFilter write fLookupFilter;
    property LookupDisabled: Boolean read fLookupDisabled write fLookupDisabled default True;
    property ComboBoxReadOnly: Boolean read fComboBoxRO write fComboBoxRO default False;
    property OnSelectKey: TOnSelectKeyNotifyEvent read fOnSelectKey write fOnSelectKey;
  end;

{ == TRDbUpdater =============================================================== }

  TRDbUpdater = class(TComponent)
  private
    fItems: TList;
    fEditor: TRDbEditor;
    fDisableCtrls: Boolean;
    fForwardScan: Boolean;
    fShowResults: Boolean;
    fOnBeforeDialog: TNotifyEvent;
    fOnAfterDialog: TNotifyEvent;
    procedure SetEditor(const aValue: TRDbEditor);
  protected
    function  GetItem(Index: Integer): TRDUItem;
    procedure SetItem(Index: Integer; Value: TRDUItem);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean); virtual;
    procedure UpdateRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean); virtual;
    procedure ModifyRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteItems;
    procedure AddItem(Item: TRDUItem);
    procedure RemoveItem(Item: TRDUItem);
    procedure DeleteItem(Item: TRDUItem);
    procedure Delete(Index: Integer);
    function  IndexOf(Item: TRDUItem): Integer;
    function  GetCount: Integer;
    procedure CheckEditorLink;
    procedure CopyToEditBuffer; virtual;
    procedure RestoreFromEditBuffer; virtual;
    procedure CheckItems_NullAutoEnabled;
    function  ShowDialogDefault: Boolean; virtual;
    function  ShowDialog(const Data: Pointer): Boolean; virtual;
    function  UpdateDataSetDefault(const Data: Pointer): Boolean; virtual;
    function  UpdateDataSet(const Data: Pointer): Boolean; virtual;
    property  Item[Index: Integer]: TRDUItem read GetItem write SetItem; default;
  published
    property Items: TList read fItems;
    property Editor: TRDbEditor read fEditor write SetEditor;
    property DisableCtrls: Boolean read fDisableCtrls write fDisableCtrls default True;
    property ForwardScan: Boolean read fForwardScan write fForwardScan default True;
    property ShowResults: Boolean read fShowResults write fShowResults default True;
    property OnBeforeDialog: TNotifyEvent read fOnBeforeDialog write fOnBeforeDialog;
    property OnAfterDialog: TNotifyEvent read fOnAfterDialog write fOnAfterDialog;
  end;

{ == TFormDbUpdater ============================================================ }

  TFormDbUpdater = class(TEditorsTemplate)
    CloseOkToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
    ItemProperties: TAction;
    ItemPropertiesToolButton: TToolButton;
    Separator2: TToolButton;
    itemItemPropertiesP: TMenuItem;
    divItemProperties: TMenuItem;
    itemItemProperties: TMenuItem;
    ClearAll: TAction;
    ClearAllToolButton: TToolButton;
    Separator1: TToolButton;
    itemClearAllP: TMenuItem;
    divClearAllP: TMenuItem;
    itemClearAll: TMenuItem;
    divClearAll: TMenuItem;
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListViewChanging(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
    procedure ItemPropertiesUpdate(Sender: TObject);
    procedure ItemPropertiesExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ClearAllUpdate(Sender: TObject);
    procedure ClearAllExecute(Sender: TObject);
  private
    fUpdater: TRDbUpdater;
    procedure SetUpdater(const Value: TRDbUpdater);
    procedure EditItem(LI: TListItem);
  protected
    procedure InitForm; override;
    procedure DoneForm; override;
  public
    property  Updater: TRDbUpdater read fUpdater write SetUpdater;
  end;

const
  SRDUItemNames: array [TRDUItemType] of string[10] =
    ('Null', 'Boolean', 'Integer', 'Int64', 'Color', 'Float', 'DateTime', 'String', 'StringList', 'LinkCombo');
  TRDUItemClasses: array [TRDUItemType] of TRDUItemClass =
    (TRDUNullItem, TRDUBooleanItem, TRDUIntegerItem, TRDUInt64Item, TRDUColorItem, TRDUFloatItem, TRDUDateTimeItem, TRDUStringItem, TRDUStringListItem, TRDULinkComboItem);

{ == Utilites ================================================================== }
procedure RegisterUpdItemsClasses;

implementation

{$R *.dfm}

uses
  DateUtils, RxStrUtils, StrUtils, RVclUtils, RMsgRu, RDialogs,
  RDbUpdaterItem_Null, RDbUpdaterItem_Boolean,
  RDbUpdaterItem_Integer, RDbUpdaterItem_Int64,
  RDbUpdaterItem_Color, RDbUpdaterItem_Float, RDbUpdaterItem_Date,
  RDbUpdaterItem_String, RDbUpdaterItem_Combo;

resourcestring
  SSetNullBalue        = 'Очистить поле (null)';
  STitleDV             = 'Значения "по умолчанию"...';
  STitleME             = 'Мультиредактор';

  EListIndexError      = 'Неверный индекс элемента: %d.'#13'Компонент: %s.';
  EFieldNameNotDefine  = 'Для компонента "%s" не определено имя поля набора данных FieldName!';
  EEditorNotDefine     = 'Для компонента "%s.%s" не определено свойство "Editor"!';
  EFieldNotFound       = 'В наборе данных "%s" поле с именем "%s" не найдено!';
  EFieldNotDefine      = 'Элемент "%s.%s" не смог получить доступ к полю "%s"!';

var
  ClassesRegistered: Boolean = False;

{ == Utilites ================================================================== }
procedure RegisterUpdItemsClasses;
var
  i: TRDUItemType;
begin
  if not ClassesRegistered then
  begin
    for i := Low(TRDUItemType) to High(TRDUItemType) do
      RegisterClasses([TRDUItemClasses[i]]);
    ClassesRegistered := True;
  end;
end;

{ TRDUItem }

constructor TRDUItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fUpdater := nil;
  fActive := False;
  fActive_Buf := False;
  fEnabled := True;
  fVisible := True;
  fNullAuto := True;
  fNullEnable := True;
  fNullValue := True;
  fNullValue_Buf := True;
  fFieldName := '';
  fFieldCaption := '';
end;

destructor TRDUItem.Destroy;
begin
  if Assigned(fUpdater) then fUpdater.RemoveItem(Self);
  inherited Destroy;
end;

procedure TRDUItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUItem) then
  begin
    fActive := TRDUItem(Source).fActive;
    fActive_Buf := TRDUItem(Source).fActive_Buf;
    fEnabled := TRDUItem(Source).fEnabled;
    fVisible := TRDUItem(Source).fVisible;
    fNullEnable := TRDUItem(Source).fNullEnable;
    fNullValue := TRDUItem(Source).fNullValue;
    fNullValue_Buf := TRDUItem(Source).fNullValue_Buf;
    fFieldName := TRDUItem(Source).fFieldName;
    fFieldCaption := TRDUItem(Source).fFieldCaption;
    fOnAfterEdit := TRDUItem(Source).fOnAfterEdit;
    fOnBeforeEdit := TRDUItem(Source).fOnBeforeEdit;
    fOnGetText := TRDUItem(Source).fOnGetText;
    fOnCheckValue := TRDUItem(Source).fOnCheckValue;
    fOnModifyValue := TRDUItem(Source).fOnModifyValue;
  end;
end;

function TRDUItem.CheckVisible: Boolean;
var
  fField: TField;
begin
  Result := fVisible;
  if Result then
  begin
    fField := GetItemField;
    Result := Assigned(fField) and not fField.ReadOnly;
  end;
end;

function TRDUItem.GetActive: Boolean;
begin
  Result := fActive and fEnabled and fVisible and CheckVisible;
end;

procedure TRDUItem.SetActive(const aValue: Boolean);
begin
  if aValue <> fActive then
  begin
    if aValue and (fFieldName = EmptyStr) then
      raise ERDbUpdaterError.CreateFmt(EFieldNameNotDefine, [Name]);
    fActive := aValue;
  end;
end;

procedure TRDUItem.SetFieldName(const aValue: string);
begin
  if Trim(aValue) <> fFieldName then
  begin
    fFieldName := Trim(aValue);
    if fFieldName = EmptyStr then
      Active := False
    else begin
      if (fUpdater <> nil) and (fUpdater.fEditor <> nil)
      and (fUpdater.fEditor.DataSet <> nil)
      and (fUpdater.fEditor.DataSet.FindField(fFieldName) <> nil)
      and (fFieldCaption = EmptyStr)
      then fFieldCaption := fUpdater.fEditor.DataSet.FieldByName(fFieldName).DisplayLabel;
    end;
  end;
end;

procedure TRDUItem.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TRDbUpdater then fUpdater := TRDbUpdater(Reader.Parent);
end;

function TRDUItem.HasParent: Boolean;
begin
  Result := True;
end;

function TRDUItem.GetParentComponent: TComponent;
begin
  Result := fUpdater;
end;

procedure TRDUItem.SetParentComponent(AParent: TComponent);
begin
  if fUpdater <> nil then fUpdater.RemoveItem(Self);
  if (AParent <> nil) and (AParent is TRDbUpdater) then
    TRDbUpdater(AParent).AddItem(Self);
end;

function TRDUItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem;
end;

procedure TRDUItem.BeforeShowEditor;
begin
  if Assigned(fOnBeforeEdit) then
    fOnBeforeEdit(Self);
end;

procedure TRDUItem.AfterShowEditor;
begin
  if Assigned(fOnAfterEdit) then
    fOnAfterEdit(Self);
end;

function TRDUItem.ShowDialog(AOwner: TComponent; const ALeft, ATop: Integer): Boolean;
var
  Editor: TFormDbUpdaterItem;
begin
  Editor := GetEditorClass.Create(Application);
  try
    StartWait;
    try
      Editor.Top := ATop;
      Editor.Left := ALeft;
      if Editor.Top + Editor.Height + PanelHeight > Screen.DesktopHeight then
        Editor.Top := Screen.DesktopHeight - Editor.Height - PanelHeight;
      if Editor.Left + Editor.Width > Screen.DesktopWidth then
        Editor.Left := Screen.DesktopWidth - Editor.Width;
      Editor.FieldText.Caption := FieldCaption;
      if fNullEnable then
      begin
        Editor.ClearRadioButton.Enabled := True;
        Editor.SetRadioButton.Enabled := True;
        Editor.ClearRadioButton.Checked := fNullValue;
      end
      else begin
        Editor.ClearRadioButton.Enabled := False;
        Editor.SetRadioButton.Enabled := False;
        Editor.SetRadioButton.Checked := True;
      end;
      LoadEditorControls(Editor);
      BeforeShowEditor;
    finally
      StopWait;
    end;
    Result := Editor.ShowModal = mrOk;
    if Result then
    begin
      StartWait;
      try
        fNullValue := Editor.ClearRadioButton.Checked;
        ReadEditorControls(Editor);
        AfterShowEditor;
      finally
        StopWait;
      end;
    end
    else begin
      StartWait;
      try
        AfterShowEditor;
      finally
        StopWait;
      end;
    end;
  finally
    Editor.Free;
  end;
end;

function TRDUItem.GetItemValue: string;
begin
  if fActive then
  begin
    if fNullEnable and fNullValue
    then Result := SSetNullBalue
    else Result := GetItemText;
  end
  else Result := '';
end;

function TRDUItem.GetItemField: TField;
begin
  Result := nil;
  if Assigned(fUpdater) and Assigned(fUpdater.fEditor)
    and Assigned(fUpdater.fEditor.DataSet) then
      Result := fUpdater.fEditor.DataSet.FindField(fFieldName);
end;

procedure TRDUItem.CheckDbField;
begin
  if GetItemField = nil then
    raise ERDbUpdaterError.CreateFmt(EFieldNotDefine, [Owner.Name, Name, fFieldName]);
end;

procedure TRDUItem.DoGetItemText(var Text: string);
begin
  if Assigned(fOnGetText) then fOnGetText(Self, Text);
end;

procedure TRDUItem.SetFieldClear(aField: TField; var Complete: Boolean);
begin
  aField.Clear;
end;

procedure TRDUItem.DoModifyRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean);
var
  flField: TField;
begin
  flField := GetItemField;
  if Assigned(flField) then
  begin
    if fNullEnable and fNullValue
    then SetFieldClear(flField, Complete)
    else SetFieldValue(flField, Complete);
    if Assigned(fOnModifyValue) then
      fOnModifyValue(Self, flField, fNullEnable and fNullValue, Complete)
  end
  else Complete := False;
end;

function TRDUItem.CheckFieldClear(aField: TField): Boolean;
begin
  Result := aField.IsNull;
end;

function TRDUItem.IsNeedChange: Boolean;
var
  flField: TField;
begin
  Result := False;
  flField := GetItemField;
  if Assigned(flField) and not flField.ReadOnly then
  begin
    if fNullEnable and fNullValue
    then Result := not CheckFieldClear(flField)
    else Result := not CheckFieldValue(flField);
    if Assigned(fOnCheckValue) then
      fOnCheckValue(Self, flField, fNullEnable and fNullValue, Result)
  end;
end;

procedure TRDUItem.CopyToEditBuffer;
begin
  inherited;
  fActive_Buf := fActive;
  fNullValue_Buf := fNullValue;
end;

procedure TRDUItem.RestoreFromEditBuffer;
begin
  inherited;
  fActive := fActive_Buf;
  fNullValue := fNullValue_Buf;
end;

{ TRDUNullItem }

function TRDUNullItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Null;
end;

procedure TRDUNullItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  inherited;
end;

procedure TRDUNullItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  inherited;
end;

function TRDUNullItem.GetItemText: string;
begin
  Result := SSetNullBalue;
end;

procedure TRDUNullItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  aField.Clear;
end;

function TRDUNullItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := aField.IsNull;
end;

{ TRDUBooleanItem }

resourcestring
  STextTrue    = 'Истина';
  STextFalse   = 'Ложь';

constructor TRDUBooleanItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := False;
  fValue_Buf := False;
  fTextTrue := STextTrue;
  fTextFalse := STextFalse;
  fNullEnable := False;
end;

procedure TRDUBooleanItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUBooleanItem) then
  begin
    fValue := TRDUBooleanItem(Source).fValue;
    fValue_Buf := TRDUBooleanItem(Source).fValue_Buf;
    fTextTrue := TRDUBooleanItem(Source).fTextTrue;
    fTextFalse := TRDUBooleanItem(Source).fTextFalse;
  end;
end;

function TRDUBooleanItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Boolean;
end;

procedure TRDUBooleanItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Boolean(Editor) do
  begin
    ValueComboBox.Items.BeginUpdate;
    try
      ValueComboBox.Items.Clear;
      ValueComboBox.Items.Add(fTextFalse);
      ValueComboBox.Items.Add(fTextTrue);
      if fValue
      then ValueComboBox.ItemIndex := 1
      else ValueComboBox.ItemIndex := 0;
    finally
      ValueComboBox.Items.EndUpdate;
    end;
  end;
end;

procedure TRDUBooleanItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Boolean(Editor) do
    fValue := ValueComboBox.ItemIndex = 1;
end;

function TRDUBooleanItem.GetItemText: string;
begin
  if fValue
  then Result := fTextTrue
  else Result := fTextFalse;
  DoGetItemText(Result);
end;

procedure TRDUBooleanItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  aField.AsBoolean := fValue;
end;

function TRDUBooleanItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := aField.AsBoolean = fValue;
end;

procedure TRDUBooleanItem.CopyToEditBuffer;
begin
  inherited;
  fActive_Buf := fActive;
end;

procedure TRDUBooleanItem.RestoreFromEditBuffer;
begin
  inherited;
  fActive := fActive_Buf;
end;

{ TRDUIntegerItem }

constructor TRDUIntegerItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := 0;
  fValue_Buf := 0;
  fMinValue := 0;
  fMaxValue := 0;
end;

procedure TRDUIntegerItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUIntegerItem) then
  begin
    fValue := TRDUIntegerItem(Source).fValue;
    fValue_Buf := TRDUIntegerItem(Source).fValue_Buf;
    fMinValue := TRDUIntegerItem(Source).fMinValue;
    fMaxValue := TRDUIntegerItem(Source).fMaxValue;
  end;
end;

function TRDUIntegerItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Integer;
end;

function TRDUIntegerItem.GetItemText: string;
begin
  Result := IntToStr(fValue);
end;

procedure TRDUIntegerItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Integer(Editor) do
  begin
    SpinEdit.Value := fValue;
    SpinEdit.MinValue := fMinValue;
    SpinEdit.MaxValue := fMaxValue;
  end;
end;

procedure TRDUIntegerItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Integer(Editor) do
    fValue := SpinEdit.Value;
end;

procedure TRDUIntegerItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  aField.AsInteger := fValue;
end;

function TRDUIntegerItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := aField.AsInteger = fValue;
end;

procedure TRDUIntegerItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDUIntegerItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDUInt64Item }

constructor TRDUInt64Item.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := 0;
  fValue_Buf := 0;
  fMinValue := 0;
  fMaxValue := 0;
end;

procedure TRDUInt64Item.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUInt64Item) then
  begin
    fValue := TRDUInt64Item(Source).fValue;
    fValue_Buf := TRDUInt64Item(Source).fValue_Buf;
    fMinValue := TRDUInt64Item(Source).fMinValue;
    fMaxValue := TRDUInt64Item(Source).fMaxValue;
  end;
end;

function TRDUInt64Item.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Int64;
end;

function TRDUInt64Item.GetItemText: string;
begin
  Result := IntToStr(fValue);
end;

procedure TRDUInt64Item.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Int64(Editor) do
  begin
    SpinEdit.MinValue := fMinValue;
    SpinEdit.MaxValue := fMaxValue;
    SpinEdit.Value := fValue;
  end;
end;

procedure TRDUInt64Item.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Int64(Editor) do
    fValue := SpinEdit.Value;
end;

procedure TRDUInt64Item.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  if aField is TLargeIntField then
    TLargeIntField(aField).AsLargeInt := fValue;
end;

function TRDUInt64Item.CheckFieldValue(aField: TField): Boolean;
begin
  Result := (aField is TLargeIntField) and (TLargeIntField(aField).AsLargeInt = fValue);
end;

procedure TRDUInt64Item.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDUInt64Item.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDUColorItem }

constructor TRDUColorItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := -1;
  fValue_Buf := -1;
end;

procedure TRDUColorItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUColorItem) then
  begin
    fValue := TRDUColorItem(Source).fValue;
    fValue_Buf := TRDUColorItem(Source).fValue_Buf;
  end;
end;

function TRDUColorItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Color;
end;

function TRDUColorItem.GetItemText: string;
begin
  if fValue = -1
  then Result := ''
  else Result := GetColorName(fValue);
end;

procedure TRDUColorItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Color(Editor) do
    RColorCombo.ColorValue := fValue;
end;

procedure TRDUColorItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Color(Editor) do
    fValue := RColorCombo.ColorValue;
end;

procedure TRDUColorItem.SetFieldClear(aField: TField; var Complete: Boolean);
begin
  aField.AsInteger := -1;
end;

procedure TRDUColorItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  aField.AsInteger := fValue;
end;

function TRDUColorItem.CheckFieldClear(aField: TField): Boolean;
begin
  Result := aField.AsInteger = -1;
end;

function TRDUColorItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := aField.AsInteger = fValue;
end;

procedure TRDUColorItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDUColorItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDUFloatItem }

constructor TRDUFloatItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := 0;
  fValue_Buf := 0;
  fMinValue := 0;
  fMaxValue := 0;
  fDisplayFormat := '%.2n';
end;

procedure TRDUFloatItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUFloatItem) then
  begin
    fValue := TRDUFloatItem(Source).fValue;
    fValue_Buf := TRDUFloatItem(Source).fValue_Buf;
    fMinValue := TRDUFloatItem(Source).fMinValue;
    fMaxValue := TRDUFloatItem(Source).fMaxValue;
    fDisplayFormat := TRDUFloatItem(Source).fDisplayFormat;
  end;
end;

function TRDUFloatItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Float;
end;

function TRDUFloatItem.GetItemText: string;
begin
  Result := Format(fDisplayFormat, [fValue]);
end;

procedure TRDUFloatItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Float(Editor) do
  begin
    FloatEdit.Value := fValue;
    FloatEdit.MinValue := fMinValue;
    FloatEdit.MaxValue := fMaxValue;
  end;
end;

procedure TRDUFloatItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Float(Editor) do
    fValue := FloatEdit.Value;
end;

procedure TRDUFloatItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  aField.AsFloat := fValue;
end;

function TRDUFloatItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := aField.AsFloat = fValue;
end;

procedure TRDUFloatItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDUFloatItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDUDateTimeItem }

constructor TRDUDateTimeItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := 0;
  fValue_Buf := 0;
  fKind := dtDate;
end;

procedure TRDUDateTimeItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUDateTimeItem) then
  begin
    fValue := TRDUDateTimeItem(Source).fValue;
    fValue_Buf := TRDUDateTimeItem(Source).fValue_Buf;
    fKind := TRDUDateTimeItem(Source).fKind;
  end;
end;

function TRDUDateTimeItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Date;
end;

function TRDUDateTimeItem.GetFormatText: string;
begin
  case fKind of
    dtDate: Result := 'dd.mm.yyyy';
    dtTime: Result := 'hh.nn.ss';
    dtDateTime: Result := 'dd.mm.yyyy hh.nn.ss';
  end;
end;

function TRDUDateTimeItem.GetItemText: string;
begin
  Result := FormatDateTime(GetFormatText, fValue);
end;

procedure TRDUDateTimeItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Date(Editor) do
  begin
    case fKind of
      dtDate:
      begin
        TimePicker.Visible := False;
        DatePicker.Visible := True;
        DatePicker.Left := 32;
        if fValue = 0
        then DatePicker.Date := Date
        else DatePicker.Date := DateOf(fValue);
      end;
      dtTime:
      begin
        DatePicker.Visible := False;
        TimePicker.Visible := True;
        TimePicker.Left := 32;
        if fValue = 0
        then TimePicker.Time := Time
        else TimePicker.Time := TimeOf(fValue);
      end;
      dtDateTime:
      begin
        DatePicker.Visible := True;
        DatePicker.Left := 32;
        if fValue = 0
        then DatePicker.Date := Date
        else DatePicker.Date := DateOf(fValue);
        TimePicker.Visible := True;
        TimePicker.Left := 184;
        if fValue = 0
        then TimePicker.Time := Time
        else TimePicker.Time := TimeOf(fValue);
      end;
    end;
  end;
end;

procedure TRDUDateTimeItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Date(Editor) do
  begin
    case fKind of
      dtDate: fValue := DateOf(DatePicker.Date);
      dtTime: fValue := TimeOf(TimePicker.Time);
      dtDateTime: fValue := DateOf(DatePicker.Date) + TimeOf(TimePicker.Time);
    end;
  end;
end;

procedure TRDUDateTimeItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  case fKind of
    dtDate: aField.AsDateTime := DateOf(fValue);
    dtTime: aField.AsDateTime := TimeOf(fValue);
    else aField.AsDateTime := fValue;
  end;
end;

function TRDUDateTimeItem.CheckFieldValue(aField: TField): Boolean;
begin
  case fKind of
    dtDate: Result := aField.AsDateTime = DateOf(fValue);
    dtTime: Result := aField.AsDateTime = TimeOf(fValue);
    else Result := aField.AsDateTime = fValue;
  end;
end;

procedure TRDUDateTimeItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDUDateTimeItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDUStringItem }

constructor TRDUStringItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHistory := TStringList.Create;
  fHistory.Duplicates := dupIgnore;
  fHistory.Sorted := True;
  fUseHistory := True;
  fFindAndReplace := False;
  fFindAndReplace_Buf := False;
  fStrings := TStringList.Create;
  fValue := '';
  fFindValue := '';
  fReplaceValue := '';
  fValue_Buf := '';
  fFindValue_Buf := '';
  fReplaceValue_Buf := '';
end;

destructor TRDUStringItem.Destroy;
begin
  fStrings.Free;
  fHistory.Free;
  inherited Destroy;
end;

procedure TRDUStringItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUStringItem) then
  begin
    fFindAndReplace := TRDUStringItem(Source).fFindAndReplace;
    fFindAndReplace_Buf := TRDUStringItem(Source).fFindAndReplace_Buf;
    fValue := TRDUStringItem(Source).fValue;
    fFindValue := TRDUStringItem(Source).fFindValue;
    fReplaceValue := TRDUStringItem(Source).fReplaceValue;
    fValue_Buf := TRDUStringItem(Source).fValue_Buf;
    fFindValue_Buf := TRDUStringItem(Source).fFindValue_Buf;
    fReplaceValue_Buf := TRDUStringItem(Source).fReplaceValue_Buf;
    fUseHistory := TRDUStringItem(Source).fUseHistory;
    fHistory.Assign(TRDUStringItem(Source).fHistory);
    fStrings.Assign(TRDUStringItem(Source).fStrings);
  end;
end;

procedure TRDUStringItem.SetStrings(const Value: TStringList);
begin
  fStrings.Assign(Value);
end;

function TRDUStringItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_String;
end;

function TRDUStringItem.GetItemText: string;
begin
  if fFindAndReplace
  then Result := Format('"%s" >> "%s"', [fFindValue, fReplaceValue])
  else Result := fValue;
end;

procedure TRDUStringItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
var
  i: Integer;
begin
  with TFormDbUpdaterItem_String(Editor) do
  begin
    ComboBox.Items.Assign(fStrings);
    if fUseHistory then
    begin
      for i := 0 to fHistory.Count - 1 do
        if ComboBox.Items.IndexOf(fHistory[i]) = -1 then
          ComboBox.Items.Add(fHistory[i]);
    end;
    ComboBox.Style := csDropDown;
    ComboBox.Text := fValue;
    ReplaceRadioButton.Visible := True;
    ReplaceRadioButton.Checked := fFindAndReplace;
    FindEdit.Visible := True;
    FindEdit.Text := fFindValue;
    ReplaceEdit.Visible := True;
    ReplaceEdit.Text := fReplaceValue;
    if not fNullEnable then
    begin
      SetRadioButton.Enabled := True;
      ReplaceRadioButton.Enabled := True;
    end;
    Height := 240;
  end;
end;

procedure TRDUStringItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_String(Editor) do
  begin
    fFindAndReplace := ReplaceRadioButton.Checked;
    fValue := ComboBox.Text;
    fFindValue := FindEdit.Text;
    fReplaceValue := ReplaceEdit.Text;
    if fUseHistory and (fValue <> '') then
      fHistory.Add(fValue);
  end;
end;

procedure TRDUStringItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  if fFindAndReplace
  then aField.AsString := AnsiReplaceText(aField.AsString, fFindValue, fReplaceValue)
  else aField.AsString := fValue;
end;

function TRDUStringItem.CheckFieldValue(aField: TField): Boolean;
begin
  if fFindAndReplace
  then Result := not AnsiContainsText(aField.AsString, fFindValue)
  else Result := AnsiSameText(aField.AsString, fValue);
end;

procedure TRDUStringItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
  fFindValue_Buf := fFindValue;
  fReplaceValue_Buf := fReplaceValue;
  fFindAndReplace_Buf := fFindAndReplace;
end;

procedure TRDUStringItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
  fFindValue := fFindValue_Buf;
  fReplaceValue := fReplaceValue_Buf;
  fFindAndReplace := fFindAndReplace_Buf;
end;

{ TRDUStringListItem }

constructor TRDUStringListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fValue := intDisable;
  fValue_Buf := intDisable;
  fValues := TStringList.Create;
  fStrings := TStringList.Create;
end;

destructor TRDUStringListItem.Destroy;
begin
  fValues.Free;
  fStrings.Free;
  inherited Destroy;
end;

procedure TRDUStringListItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDUStringListItem) then
  begin
    fValue := TRDUStringListItem(Source).fValue;
    fValue_Buf := TRDUStringListItem(Source).fValue_Buf;
    fValues.Assign(TRDUStringListItem(Source).fValues);
    fStrings.Assign(TRDUStringListItem(Source).fStrings);
  end;
end;

procedure TRDUStringListItem.SetStrings(const Value: TStringList);
begin
  fStrings.Assign(Value);
end;

procedure TRDUStringListItem.SetValues(const Value: TStringList);
begin
  fValues.Assign(Value);
end;

function TRDUStringListItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_String;
end;

function TRDUStringListItem.GetItemText: string;
begin
  Result := EmptyStr;
  if (fValue > intDisable) and (fValue < fStrings.Count) then
    Result := fStrings[fValue];
end;

procedure TRDUStringListItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_String(Editor) do
  begin
    ComboBox.Items.Assign(fStrings);
    ComboBox.Style := csDropDownList;
    if (fValue = intDisable) and (ComboBox.Items.Count > 0)
    then ComboBox.ItemIndex := 0
    else ComboBox.ItemIndex := fValue;
    ReplaceRadioButton.Visible := False;
    ReplaceRadioButton.Checked := False;
    FindEdit.Visible := False;
    ReplaceEdit.Visible := False;
    // if not fNullEnable then
    //  SetRadioButton.Enabled := True;
    Height := 190;
  end;
end;

procedure TRDUStringListItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_String(Editor) do
    fValue := ComboBox.ItemIndex;
end;

procedure TRDUStringListItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  if (fValue > intDisable) and (fValue < fValues.Count)
  then aField.AsString := fValues[fValue]
  else aField.Clear;
end;

function TRDUStringListItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := (fValue > intDisable) and (fValue < fValues.Count)
    and SameText(aField.AsString, fValues[fValue]);
end;

procedure TRDUStringListItem.AddItem(const Value, Description: string);
begin
  Values.Add(Value);
  Strings.Add(Description);
end;

procedure TRDUStringListItem.Clear;
begin
  Values.Clear;
  Strings.Clear;
end;

procedure TRDUStringListItem.BeginUpdate;
begin
  Values.BeginUpdate;
  Strings.BeginUpdate;
end;

procedure TRDUStringListItem.EndUpdate;
begin
  Values.EndUpdate;
  Strings.EndUpdate;
end;

procedure TRDUStringListItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDUStringListItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDULinkComboItem }

resourcestring
  ELookupDataSetNotDefine = 'ОШИБКА! Не определено свойство LookupDataSet!';
  ELookupDataSetNotActive = 'ОШИБКА! Нет доступа к LookupDataSet!';
  EKeyFieldNotDefine      = 'ОШИБКА! Не определено свойство KeyField!';
  ELookupFieldNotDefine   = 'ОШИБКА! Не определено свойство LookupFieldName[%d]!';
  ELookupFieldNotFound    = 'ОШИБКА! Поле "%s" в наборе данных не найдено!';
  EKeyValueNotFound       = '< Выберите запись из списка >'; // 'ОШИБКА! Запись с %s=%d не найдена!';
  EKeyValueNotFoundStr    = '< Выберите запись из списка >'; // 'ОШИБКА! Запись с %s=%s не найдена!';

const
  FieldDelims = [';'];

constructor TRDULinkComboItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLinkDataSet := nil;
  fKeyField := EmptyStr;
  fLookupFields := EmptyStr;
  fLookupFieldIndex := 0;
  fLookupFilter := EmptyStr;
  fLookupDisabled := True;
  fComboBoxRO := False;
  fValue := 0;
  fValue_Buf := 0;
end;

destructor TRDULinkComboItem.Destroy;
begin
  fLinkDataSet := nil;
  inherited Destroy;
end;

procedure TRDULinkComboItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(Source) and (Source is TRDULinkComboItem) then
  begin
    fValue := TRDULinkComboItem(Source).fValue;
    fValue_Buf := TRDULinkComboItem(Source).fValue_Buf;
    LookupDataSet := TRDULinkComboItem(Source).fLinkDataSet;
    fKeyField := TRDULinkComboItem(Source).fKeyField;
    fLookupFields := TRDULinkComboItem(Source).fLookupFields;
    fLookupFieldIndex := TRDULinkComboItem(Source).fLookupFieldIndex;
    fLookupFilter := TRDULinkComboItem(Source).fLookupFilter;
    fLookupDisabled := TRDULinkComboItem(Source).fLookupDisabled;
    fComboBoxRO := TRDULinkComboItem(Source).fComboBoxRO;
  end;
end;

procedure TRDULinkComboItem.SetDataSet(aValue: TDataSet);
begin
  if fLinkDataSet <> aValue then
  begin
    fLinkDataSet := aValue;
    if fLinkDataSet <> nil then aValue.FreeNotification(Self)
    else begin
      if not (csLoading in ComponentState) then
      begin
        fKeyField := EmptyStr;
        fLookupFields := EmptyStr;
        fLookupFieldIndex := 0;
      end;
    end;
  end;
end;

procedure TRDULinkComboItem.SetKeyField(const aValue: string);
begin
  if fKeyField <> aValue then
  begin
    if (aValue <> EmptyStr) and (fLinkDataSet <> nil) and (fLinkDataSet.FindField(aValue) = nil) then
      raise ERDbUpdaterError.CreateFmt(EFieldNotFound, [fLinkDataSet.Name, aValue]);
    fKeyField := aValue;
  end;
end;

procedure TRDULinkComboItem.SetLookupFields(const aValue: string);
var
  i: Integer;
begin
  if fLookupFields <> aValue then
  begin
    if (aValue <> EmptyStr) and (fLinkDataSet <> nil) then
    begin
      for i := 1 to WordCount(aValue, FieldDelims) do
        if fLinkDataSet.FindField(Trim(ExtractWord(i, aValue, FieldDelims))) = nil then
          raise ERDbUpdaterError.CreateFmt(EFieldNotFound, [fLinkDataSet.Name,
            Trim(ExtractWord(i, aValue, FieldDelims))]);
    end;
    fLookupFields := aValue;
    if fLookupFieldIndex < WordCount(fLookupFields, FieldDelims) then
      fLookupFieldIndex := 0;
  end;
end;

procedure TRDULinkComboItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (LookupDataSet <> nil) and (AComponent = LookupDataSet) then
    LookupDataSet := nil;
end;

function TRDULinkComboItem.GetEditorClass: TFormDbUpdaterItemClass;
begin
  Result := TFormDbUpdaterItem_Combo;
end;

function TRDULinkComboItem.GetLookupFieldName(const Index: Integer): string;
begin
  if (Index >= 0) and (Index < WordCount(fLookupFields, FieldDelims))
  then Result := Trim(ExtractWord(Index + 1, fLookupFields, FieldDelims))
  else Result := EmptyStr;
end;

function TRDULinkComboItem.GetLookupField(const Index: Integer): TField;
begin
  if (fLinkDataSet <> nil) and (GetLookupFieldName(Index) <> EmptyStr)
  then Result := fLinkDataSet.FindField(GetLookupFieldName(Index))
  else Result := nil;
end;

function TRDULinkComboItem.GetItemText: string;
begin
  if fLinkDataSet <> nil then
  begin
    if fLinkDataSet.Active then
    begin
      if fKeyField <> EmptyStr then
      begin
        if fLinkDataSet.FindField(fKeyField) <> nil then
        begin
          if GetLookupFieldName(fLookupFieldIndex) <> EmptyStr then
          begin
            if GetLookupField(fLookupFieldIndex) <> nil then
            begin
              if fLookupDisabled then fLinkDataSet.DisableControls;
              try
                if fLinkDataSet.Locate(fKeyField, Value, [])
                then Result := GetLookupField(fLookupFieldIndex).AsString
                else Result := Format(EKeyValueNotFound, [fKeyField, Value]);
              finally
                if fLookupDisabled then fLinkDataSet.EnableControls;
              end;
            end
            else Result := Format(ELookupFieldNotFound, [GetLookupFieldName(fLookupFieldIndex)]);
          end
          else Result := Format(ELookupFieldNotDefine, [fLookupFieldIndex]);
        end
        else Result := Format(ELookupFieldNotFound, [GetLookupFieldName(fLookupFieldIndex)]);
      end
      else Result := EKeyFieldNotDefine;
    end
    else Result := ELookupDataSetNotActive;
  end
  else Result := ELookupDataSetNotDefine;
end;

procedure TRDULinkComboItem.BeforeShowEditor;
begin
  inherited BeforeShowEditor;
  if fLookupFilter <> EmptyStr then
  begin
    fLinkDataSet.Filter := fLookupFilter;
    fLinkDataSet.Filtered := True;
  end;
end;

procedure TRDULinkComboItem.AfterShowEditor;
begin
  if fLookupFilter <> EmptyStr then
  begin
    fLinkDataSet.Filter := EmptyStr;
    fLinkDataSet.Filtered := False;
  end;
  inherited AfterShowEditor;
end;

procedure TRDULinkComboItem.LoadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Combo(Editor) do
  begin
    DataSource.DataSet := fLinkDataSet;
    DBLookupComboBox.KeyField := fKeyField;
    DBLookupComboBox.ListFieldIndex := fLookupFieldIndex;
    DBLookupComboBox.ListField := fLookupFields;
    DBLookupComboBox.KeyValue := fValue;
    DBLookupComboBox.ReadOnly := fComboBoxRO;
    DBLookupComboBox.ParentColor := fComboBoxRO;
    if Assigned(fOnSelectKey) then
    begin
      DBLookupComboBox.Width := 270;
      BtnSelectKey.Visible := True;
      SelectKeyProc := fOnSelectKey;
    end
    else begin
      DBLookupComboBox.Width := 293;
      BtnSelectKey.Visible := False;
      SelectKeyProc := nil;
    end;
  end;
end;

procedure TRDULinkComboItem.ReadEditorControls(Editor: TFormDbUpdaterItem);
begin
  with TFormDbUpdaterItem_Combo(Editor) do
    fValue := DBLookupComboBox.KeyValue;
end;

procedure TRDULinkComboItem.SetFieldValue(aField: TField; var Complete: Boolean);
begin
  aField.AsInteger := fValue;
end;

function TRDULinkComboItem.CheckFieldValue(aField: TField): Boolean;
begin
  Result := aField.AsInteger = fValue;
end;

procedure TRDULinkComboItem.CopyToEditBuffer;
begin
  inherited;
  fValue_Buf := fValue;
end;

procedure TRDULinkComboItem.RestoreFromEditBuffer;
begin
  inherited;
  fValue := fValue_Buf;
end;

{ TRDbUpdater }

constructor TRDbUpdater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEditor := nil;
  fItems := TList.Create;
  fForwardScan := True;
  fDisableCtrls := True;
  fShowResults := True;
  RegisterUpdItemsClasses;
end;

destructor TRDbUpdater.Destroy;
begin
  if Assigned(fItems) then
  begin
    DeleteItems;
    FreeAndNil(fItems);
  end;
  fEditor := nil;
  inherited Destroy;
end;

procedure TRDbUpdater.SetEditor(const aValue: TRDbEditor);
begin
  if fEditor <> AValue then
  begin
    fEditor := AValue;
    if Assigned(fEditor) then
      AValue.FreeNotification(Self);
    CheckItems_NullAutoEnabled;
  end;
end;

procedure TRDbUpdater.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fEditor) and (AComponent = fEditor) then
    Editor := nil;
end;

{ Items Management }

function TRDbUpdater.GetItem(Index: Integer): TRDUItem;
begin
  Result := fItems[Index];
end;

procedure TRDbUpdater.SetItem(Index: Integer; Value: TRDUItem);
begin
  Item[Index].Assign(Value);
  Item[Index].fUpdater := Self;
end;

procedure TRDbUpdater.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
  Item: TRDUItem;
begin
  for i := 0 to fItems.Count - 1 do
  begin
    Item := fItems[i];
    if Item.Owner = Root then Proc(Item);
  end;
end;

function TRDbUpdater.IndexOf(Item: TRDUItem): Integer;
begin
  Result := fItems.IndexOf(Item);
end;

procedure TRDbUpdater.AddItem(Item: TRDUItem);
begin
  fItems.Add(Item);
  Item.fUpdater := Self;
end;

procedure TRDbUpdater.RemoveItem(Item: TRDUItem);
begin
  Item.fUpdater := nil;
  fItems.Remove(Item);
end;

procedure TRDbUpdater.DeleteItem(Item: TRDUItem);
begin
  RemoveItem(Item);
  if not (csDestroying in Item.ComponentState) then Item.Free;
end;

procedure TRDbUpdater.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= fItems.Count) then
    raise ERDbUpdaterError.CreateFmt(EListIndexError, [Index, Self.Name]);
  DeleteItem(fItems.Items[Index]);
end;

procedure TRDbUpdater.DeleteItems;
begin
  while fItems.Count > 0 do
    DeleteItem(Item[fItems.Count - 1]);
end;

function TRDbUpdater.GetCount: Integer;
begin
  Result := fItems.Count;
end;

procedure TRDbUpdater.CheckEditorLink;
begin
  if not (csLoading in ComponentState) and (fEditor = nil) then
    raise ERDbUpdaterError.CreateFmt(EEditorNotDefine, [Owner.Name, Name]);
end;

function TRDbUpdater.ShowDialogDefault: Boolean;
begin
  CheckEditorLink;
  CheckItems_NullAutoEnabled;
  with TFormDbUpdater.Create(Application) do
  begin
    try
      Caption := STitleDV;
      Updater := Self;
      CopyToEditBuffer;
      if Assigned(fOnBeforeDialog) then
        fOnBeforeDialog(Self);
      Result := ShowModal = mrOk;
      if Assigned(fOnAfterDialog) then
        fOnAfterDialog(Self);
      if not Result then
        RestoreFromEditBuffer;
    finally
      Free;
    end;
  end;
end;

function TRDbUpdater.ShowDialog(const Data: Pointer): Boolean;
begin
  CheckEditorLink;
  CheckItems_NullAutoEnabled;
  with TFormDbUpdater.Create(Application) do
  begin
    try
      Caption := STitleME;
      Updater := Self;
      CopyToEditBuffer;
      if Assigned(fOnBeforeDialog) then
        fOnBeforeDialog(Self);
      Result := ShowModal = mrOk;
      if Assigned(fOnAfterDialog) then
        fOnAfterDialog(Self);
      if Result then
        Result := UpdateDataSet(Data)
      else
        RestoreFromEditBuffer;
    finally
      Free;
    end;
  end;
end;

procedure TRDbUpdater.CheckRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean);
var
  i: Integer;
begin
  Complete := False;
  if fEditor.RecordCanEdited(True) then
  begin
    for i := 0 to fItems.Count - 1 do
      if TRDUItem(fItems[i]).Active and TRDUItem(fItems[i]).IsNeedChange then
      begin
        Complete := True;
        Break;
      end;
  end;
end;

procedure TRDbUpdater.ModifyRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean);
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    if TRDUItem(fItems[i]).Active then
    begin
      TRDUItem(fItems[i]).DoModifyRecord(Self, Data, Complete);
      if not Complete then Break;
    end;
end;

procedure TRDbUpdater.UpdateRecord(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := fEditor.ModifyRecord(ModifyRecord, Data, SMsgSaveDataWait);
end;

function TRDbUpdater.UpdateDataSetDefault(const Data: Pointer): Boolean;
var
  i: Integer;
begin
  CheckEditorLink;
  Result := True;
  for i := 0 to fItems.Count - 1 do
    if TRDUItem(fItems[i]).Active and TRDUItem(fItems[i]).IsNeedChange then
    begin
      TRDUItem(fItems[i]).DoModifyRecord(Self, Data, Result);
      if not Result then Break;
    end;
end;

function TRDbUpdater.UpdateDataSet(const Data: Pointer): Boolean;
begin
  Result := fEditor.ProcessSelectedRecords(CheckRecord, UpdateRecord,
    nil, SMsgSaveDataWait, fShowResults, fForwardScan, fDisableCtrls);
end;

procedure TRDbUpdater.CopyToEditBuffer;
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    TRDUItem(fItems[i]).CopyToEditBuffer;
end;

procedure TRDbUpdater.RestoreFromEditBuffer;
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    TRDUItem(fItems[i]).RestoreFromEditBuffer;
end;

procedure TRDbUpdater.CheckItems_NullAutoEnabled;
var
  i, j: Integer;
begin
  if Assigned(fEditor) and Assigned(fEditor.DataSet) then
  begin
    for i := 0 to fEditor.DataSet.FieldCount - 1 do
    begin
      for j := 0 to Items.Count - 1 do
      begin
        if SameText(Item[j].FieldName, fEditor.DataSet.Fields[i].FieldName) then
        begin
          if Item[j].NullAutoCheck then
            Item[j].NullEnable := not fEditor.DataSet.Fields[i].Required;
        end;
      end;
    end;
  end;
end;

{ TFormDbUpdater }

const
  siText = 0;

procedure TFormDbUpdater.InitForm;
begin
  inherited InitForm;
  ListView.OnChange := nil;
  ListView.OnChanging := nil;
end;

procedure TFormDbUpdater.DoneForm;
begin
  ListView.OnChange := nil;
  ListView.OnChanging := nil;
  inherited DoneForm;
end;

procedure TFormDbUpdater.SetUpdater(const Value: TRDbUpdater);
var
  i: Integer;
begin
  if fUpdater <> Value then
  begin
    StartWait;
    ListView.Items.BeginUpdate;
    ListView.OnChange := nil;
    ListView.OnChanging := nil;
    fUpdater := Value;
    try
      for i := 0 to fUpdater.Items.Count - 1 do
        if TRDUItem(fUpdater.Items[i]).CheckVisible then
          with ListView.Items.Add do
          begin
            Data := fUpdater.Items[i];
            Checked := TRDUItem(fUpdater.Items[i]).Active;
            Caption := TRDUItem(fUpdater.Items[i]).FieldCaption;
            SubItems.Add(TRDUItem(fUpdater.Items[i]).GetItemValue);
          end;
    finally
      ListView.OnChange := ListViewChange;
      ListView.OnChanging := ListViewChanging;
      ListView.Items.EndUpdate;
      StopWait;
    end;
  end;
end;

procedure TFormDbUpdater.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if TRDUItem(Item.Data).Active <> Item.Checked then
  begin
    TRDUItem(Item.Data).Active := Item.Checked;
    Item.SubItems[siText] := TRDUItem(Item.Data).GetItemValue;
    if Item.Checked and (Item.SubItems[siText] = EmptyStr) then
      EditItem(Item);
  end;
end;

procedure TFormDbUpdater.ListViewChanging(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
begin
  AllowChange := TRDUItem(Item.Data).Enabled;
end;

procedure TFormDbUpdater.ListViewDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
    Item.Data := nil;
end;

procedure TFormDbUpdater.EditItem(LI: TListItem);
var
  FI: TRDUItem;
  R: TRect;
  P: TPoint;
begin
  if Assigned(LI) then
  begin
    FI := TRDUItem(LI.Data);
    R := LI.DisplayRect(drBounds);
    P.x := R.Left + ListView.Columns[0].Width + 1;
    P.y := R.Bottom - 1;
    P := ListView.ClientToScreen(P);
    if FI.ShowDialog(Self, P.x, P.y) then
    begin
      ListView.Items.BeginUpdate;
      try
        FI.Active := True;
        LI.Checked := FI.Active;
        LI.SubItems[siText] := FI.GetItemValue;
      finally
        ListView.Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TFormDbUpdater.ItemPropertiesUpdate(Sender: TObject);
begin
  ItemProperties.Enabled := Assigned(fUpdater) and Assigned(ListView.Selected)
    and TRDUItem(ListView.Selected.Data).Enabled;
end;

procedure TFormDbUpdater.ItemPropertiesExecute(Sender: TObject);
begin
  EditItem(ListView.Selected);
end;

procedure TFormDbUpdater.ListViewDblClick(Sender: TObject);
begin
  if ItemProperties.Enabled then ItemProperties.Execute;
end;

procedure TFormDbUpdater.ClearAllUpdate(Sender: TObject);
begin
  ClearAll.Enabled := Assigned(fUpdater) and (ListView.Items.Count > 0);
end;

procedure TFormDbUpdater.ClearAllExecute(Sender: TObject);
var
  i: Integer;
begin
  StartWait;
  try
    for i := 0 to ListView.Items.Count - 1 do
      ListView.Items[i].Checked := False;
  finally
    StopWait;
  end;
end;

end.
