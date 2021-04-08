{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Database Additional Components         }
{                                                       }
{       Copyright (c) 2006-2012 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbFilter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ImgList, ComCtrls, StdCtrls, ToolWin, Db, IniFiles,
  RDbCustom, TmplEditors, RDbFilterItem;

{ == Base definitions ========================================================== }

type
  ERDbFilterError = class(Exception);

  TRDbFilter = class;

  TOnGetTextNotifyEvent = procedure(Sender: TObject; var Value: string) of object;
  TOnCreateItems = procedure(Sender: TObject) of object;

  TLogicalOperation = (loAnd, loOr);

{ == TRDFItem ================================================================== }

  TRDFItemType      = (fiNull, fiBoolean, fiInteger, fiInt64, fiFloat, fiDate, fiString, fiStringList,
                       fiLinkCombo, fiLinkComboDate, fiLinkList, fiLinkProc, fiText);

  TRDFItem = class(TComponent)
  private
    fFilter: TRDbFilter;
    fActive: Boolean;
    fInvert: Boolean;
    fEnabled: Boolean;
    fBlocked: Boolean;
    fCaseEnabled: Boolean;
    fVisible: Boolean;
    fActive_Def: Boolean;
    fInvert_Def: Boolean;
    fActive_Buf: Boolean;
    fInvert_Buf: Boolean;
    fOperation: TLogicalOperation;
    fOperation_Def: TLogicalOperation;
    fOperation_Buf: TLogicalOperation;
    fFieldName: string;
    fFieldCaption: string;
    fData: Pointer;
    fOnCreate: TNotifyEvent;
    fOnDestroy: TNotifyEvent;
    fOnGetText: TOnGetTextNotifyEvent;
    fOnGetWhere: TOnGetTextNotifyEvent;
    fOnGetFilter: TOnGetTextNotifyEvent;
    function  GetActive: Boolean;
    procedure SetActive(const aValue: Boolean);
    procedure SetActive_Def(const aValue: Boolean);
    procedure SetFieldName(const aValue: string);
    procedure DoGetItemText(var Text: string);
    procedure DoGetWhereString(var Text: string);
    procedure DoGetFilterString(var Text: string);
  protected
    procedure SetParentComponent(AParent: TComponent); override;
    procedure ReadState(Reader: TReader); override;
    function  GetEditorClass: TFormDbFilterItemClass; virtual;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); virtual; abstract;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    function  GetParentComponent: TComponent; override;
    function  HasParent: Boolean; override;
    procedure ResetItem; virtual;
    procedure CopyToEditBuffer; virtual;
    procedure RestoreFromEditBuffer; virtual;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); virtual;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); virtual;
    function  AutoActivateEnabled: Boolean; virtual;
    function  IsFirstActiveItem: Boolean;
    function  ShowDialog(AOwner: TComponent; const ALeft, ATop: Integer): Boolean; virtual;
    function  GetOperationText(const Forced: Boolean = False): string;
    function  GetItemText: string; virtual; abstract;
    function  GetWhereString: string; virtual; abstract;
    function  GetFilterString: string; virtual; abstract;
    function  GetDbFilterOptions: TFilterOptions; virtual;
    property  Item_Active: Boolean read GetActive write SetActive default False;
    property  Item_Invert: Boolean read fInvert write fInvert default False;
    property  Item_Operation: TLogicalOperation read fOperation write fOperation;
    property  Filter: TRDbFilter read fFilter;
    property  Item_Data: Pointer read fData write fData;
  published
    property Active: Boolean read fActive_Def write SetActive_Def default False;
    property Invert: Boolean read fInvert_Def write fInvert_Def default False;
    property Enabled: Boolean read fEnabled write fEnabled default True;
    property Blocked: Boolean read fBlocked write fBlocked default False;
    property CaseStringsEnabled: Boolean read fCaseEnabled write fCaseEnabled default True;
    property Visible: Boolean read fVisible write fVisible default True;
    property Operation: TLogicalOperation read fOperation_Def write fOperation_Def;
    property FieldName: string read fFieldName write SetFieldName;
    property FieldCaption: string read fFieldCaption write fFieldCaption;
    property OnGetText: TOnGetTextNotifyEvent read fOnGetText write fOnGetText;
    property OnGetWhere: TOnGetTextNotifyEvent read fOnGetWhere write fOnGetWhere;
    property OnGetFilter: TOnGetTextNotifyEvent read fOnGetFilter write fOnGetFilter;
    property OnCreate: TNotifyEvent read fOnCreate write fOnCreate;
    property OnDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;
  end;

  TRDFItemClass = class of TRDFItem;

{ == TRDFNullItem =========================================================== }

  TRDFNullItem = class(TRDFItem)
  private
    fMode: Boolean;
    fMode_Def: Boolean;
    fMode_Buf: Boolean;
  protected
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_CompareMode: Boolean read fMode write fMode;
  published
    property CompareMode: Boolean read fMode_Def write fMode_Def default True;
  end;

{ == TRDFBooleanItem =========================================================== }

  TBooleanEqMode = (embNull, embTrue, embFalse);

  TRDFBooleanItem = class(TRDFItem)
  private
    fWhereTrue: string;
    fWhereFalse: string;
    fFilterTrue: string;
    fFilterFalse: string;
    fTextTrue: string;
    fTextFalse: string;
    fMode: TBooleanEqMode;
    fMode_Def: TBooleanEqMode;
    fMode_Buf: TBooleanEqMode;
  protected
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_CompareMode: TBooleanEqMode read fMode write fMode;
  published
    property CompareMode: TBooleanEqMode read fMode_Def write fMode_Def default embTrue;
    property ValueWhereTrue: string read fWhereTrue write fWhereTrue;
    property ValueWhereFalse: string read fWhereFalse write fWhereFalse;
    property ValueFilterTrue: string read fFilterTrue write fFilterTrue;
    property ValueFilterFalse: string read fFilterFalse write fFilterFalse;
    property ValueTextTrue: string read fTextTrue write fTextTrue;
    property ValueTextFalse: string read fTextFalse write fTextFalse;
  end;

{ == TRDFIntegerItem =========================================================== }

  TNumbersEqMode = (emnNull, emnEqual, emnMore, emnMoreEq, emnLess, emnLessEq, emnBetween);

  TRDFIntegerItem = class(TRDFItem)
  private
    fValue_1: Integer;
    fValue_2: Integer;
    fValue_1_Def: Integer;
    fValue_2_Def: Integer;
    fValue_1_Buf: Integer;
    fValue_2_Buf: Integer;
    fMode: TNumbersEqMode;
    fMode_Def: TNumbersEqMode;
    fMode_Buf: TNumbersEqMode;
  protected
    procedure SetValue_1(const aValue: Integer);
    procedure SetValue_2(const aValue: Integer);
    procedure SetValue_1_Def(const aValue: Integer);
    procedure SetValue_2_Def(const aValue: Integer);
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_CompareMode: TNumbersEqMode read fMode write fMode;
    property Item_Value_1: Integer read fValue_1 write SetValue_1 default 0;
    property Item_Value_2: Integer read fValue_2 write SetValue_2 default 0;
  published
    property CompareMode: TNumbersEqMode read fMode_Def write fMode_Def default emnEqual;
    property Value_1: Integer read fValue_1_Def write SetValue_1_Def default 0;
    property Value_2: Integer read fValue_2_Def write SetValue_2_Def default 0;
  end;

{ == TRDFInt64Item =========================================================== }

  TRDFInt64Item = class(TRDFItem)
  private
    fValue_1: Int64;
    fValue_2: Int64;
    fValue_1_Def: Int64;
    fValue_2_Def: Int64;
    fValue_1_Buf: Int64;
    fValue_2_Buf: Int64;
    fMode: TNumbersEqMode;
    fMode_Def: TNumbersEqMode;
    fMode_Buf: TNumbersEqMode;
  protected
    procedure SetValue_1(const aValue: Int64);
    procedure SetValue_2(const aValue: Int64);
    procedure SetValue_1_Def(const aValue: Int64);
    procedure SetValue_2_Def(const aValue: Int64);
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_CompareMode: TNumbersEqMode read fMode write fMode;
    property Item_Value_1: Int64 read fValue_1 write SetValue_1 default 0;
    property Item_Value_2: Int64 read fValue_2 write SetValue_2 default 0;
  published
    property CompareMode: TNumbersEqMode read fMode_Def write fMode_Def default emnEqual;
    property Value_1: Int64 read fValue_1_Def write SetValue_1_Def default 0;
    property Value_2: Int64 read fValue_2_Def write SetValue_2_Def default 0;
  end;

{ == TRDFFloatItem ============================================================= }

  TRDFFloatItem = class(TRDFItem)
  private
    fValue_1: Double;
    fValue_2: Double;
    fValue_1_Def: Double;
    fValue_2_Def: Double;
    fValue_1_Buf: Double;
    fValue_2_Buf: Double;
    fMode: TNumbersEqMode;
    fMode_Def: TNumbersEqMode;
    fMode_Buf: TNumbersEqMode;
  protected
    procedure SetValue_1(const aValue: Double);
    procedure SetValue_2(const aValue: Double);
    procedure SetValue_1_Def(const aValue: Double);
    procedure SetValue_2_Def(const aValue: Double);
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_CompareMode: TNumbersEqMode read fMode write fMode;
    property Item_Value_1: Double read fValue_1 write SetValue_1;
    property Item_Value_2: Double read fValue_2 write SetValue_2;
  published
    property CompareMode: TNumbersEqMode read fMode_Def write fMode_Def default emnEqual;
    property Value_1: Double read fValue_1_Def write SetValue_1_Def;
    property Value_2: Double read fValue_2_Def write SetValue_2_Def;
  end;

{ == TRDFDateItem ============================================================== }

  TDateEqMode = (emdNull, emdEqual, emdAfter, emdBefore, emdPeriod,
                 emdCurrentDay, emdAfterCurrentDay, emdBeforeCurrentDay, emdPrevDay, emdNextDay,
                 emdCurrentWeek, emdPrevWeek, emdNextWeek,
                 emdCurrentMonth, emdPrevMonth, emdNextMonth,
                 emdCurrentYear, emdPrevYear, emdNextYear,
                 emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days,
                 emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days);

  TRDFDateItem = class(TRDFItem)
  private
    fValue_1: TDate;
    fValue_2: TDate;
    fValue_1_Def: TDate;
    fValue_2_Def: TDate;
    fValue_1_Buf: TDate;
    fValue_2_Buf: TDate;
    fMode: TDateEqMode;
    fMode_Def: TDateEqMode;
    fMode_Buf: TDateEqMode;
    function  GetInitValue_1(const AMode: TDateEqMode; ADefValue: TDate): TDate;
    function  GetInitValue_2(const AMode: TDateEqMode; ADefValue: TDate): TDate;
  protected
    procedure SetValue_1(const aValue: TDate);
    procedure SetValue_2(const aValue: TDate);
    procedure SetValue_1_Def(const aValue: TDate);
    procedure SetValue_2_Def(const aValue: TDate);
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_CompareMode: TDateEqMode read fMode write fMode;
    property Item_Value_1: TDate read fValue_1 write SetValue_1;
    property Item_Value_2: TDate read fValue_2 write SetValue_2;
  published
    property CompareMode: TDateEqMode read fMode_Def write fMode_Def default emdLast30Days;
    property Value_1: TDate read fValue_1_Def write SetValue_1_Def;
    property Value_2: TDate read fValue_2_Def write SetValue_2_Def;
  end;

{ == TRDFStringItem ============================================================ }

  TStringEqMode = (emsNull, emsEqual, emsLike, emsBegin);

  TRDFStringItem = class(TRDFItem)
  private
    fValue: string;
    fValue_Def: string;
    fValue_Buf: string;
    fCase: Boolean;
    fCase_Def: Boolean;
    fCase_Buf: Boolean;
    fMode: TStringEqMode;
    fMode_Def: TStringEqMode;
    fMode_Buf: TStringEqMode;
    fStrings: TStrings;
    fHistory: Boolean;
    procedure SetStrings(aValue: TStrings);
  protected
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function  AutoActivateEnabled: Boolean; override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    function GetDbFilterOptions: TFilterOptions; override;
    property Item_CompareMode: TStringEqMode read fMode write fMode;
    property Item_Value: string read fValue write fValue;
    property Item_CaseSensitive: Boolean read FCase write FCase;
  published
    property CompareMode: TStringEqMode read fMode_Def write fMode_Def default emsLike;
    property Value: string read fValue_Def write fValue_Def;
    property CaseSensitive: Boolean read FCase_Def write FCase_Def default False;
    property ValuesList: TStrings read fStrings write SetStrings;
    property ValuesHistory: Boolean read fHistory write fHistory default True;
  end;

{ == TRDFTextItem ============================================================== }

  TTextEqMode = (emtNull, emtLike, emtBegin);

  TRDFTextItem = class(TRDFItem)
  private
    fValue: string;
    fValue_Def: string;
    fValue_Buf: string;
    fMode: TTextEqMode;
    fMode_Def: TTextEqMode;
    fMode_Buf: TTextEqMode;
    fStrings: TStrings;
    fHistory: Boolean;
    procedure SetStrings(aValue: TStrings);
  protected
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function  AutoActivateEnabled: Boolean; override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    function GetDbFilterOptions: TFilterOptions; override;
    property Item_CompareMode: TTextEqMode read fMode write fMode;
    property Item_Value: string read fValue write fValue;
  published
    property CompareMode: TTextEqMode read fMode_Def write fMode_Def default emtLike;
    property Value: string read fValue_Def write fValue_Def;
    property ValuesList: TStrings read fStrings write SetStrings;
    property ValuesHistory: Boolean read fHistory write fHistory default True;
  end;

{ == TRDFStringListItem ======================================================== }

  TOnGetValueNotifyEvent = procedure(Sender: TObject;
    const Index: Integer; const StringValue: string;
    var Value: Integer) of object;

  TRDFStringListItem = class(TRDFItem)
  private
    fKeyValues: string;
    fKeyValues_Buf: string;
    fKeyValues_Def: string;
    fStartIndex: Integer;
    fStrings: TStrings;
    fValues: array of Integer;
    fDelimiter: string[2];
    fTextDelimiter: string;
    fMaxLookupItems: Integer;
    fOnGetStrings: TNotifyEvent;
    fOnGetValue: TOnGetValueNotifyEvent;
    function  GetDelimiter: string;
    procedure SetDelimiter(const aValue: string);
    procedure SetStrings(aValue: TStrings);
    function  DelimiterChar: Char;
  protected
    procedure Loaded; override;
    function CheckKeysList(const KeysList: string): string;
    function GenerateKeysList: string;
    function GenerateLookupList(const KeysList: string): string;
    function GetEditorClass: TFormDbFilterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ListView_CreateColumns(LV: TListView);
    procedure ListView_LoadTable(LV: TListView);
    procedure ListView_ReadTable(LV: TListView);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateValues;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function AutoActivateEnabled: Boolean; override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_KeyValues: string read fKeyValues write fKeyValues;
  published
    property StartKeyIndex: Integer read fStartIndex write fStartIndex default 0;
    property KeyValues: string read fKeyValues_Def write fKeyValues_Def;
    property KeysDelimiter: string read GetDelimiter write SetDelimiter;
    property LookupDelimiter: string read fTextDelimiter write fTextDelimiter;
    property MaxTextValues: Integer read fMaxLookupItems write fMaxLookupItems default 10;
    property Strings: TStrings read fStrings write SetStrings;
    property OnGetStrings: TNotifyEvent read fOnGetStrings write fOnGetStrings;
    property OnGetValue: TOnGetValueNotifyEvent read fOnGetValue write fOnGetValue;
  end;

{ == TRDFLinkItem ============================================================== }

  TRDFLinkItem = class (TRDFItem)
  private
    fLinkDataSet: TDataSet;
    fKeyField: string;
    fLookupFields: string;
    fLookupFieldIndex: Integer;
    fLookupCaptions: string;
    fLookupDisabled: Boolean;
    fKeyNull: Integer;
    FUseNull: Boolean;
    // --- Только для SQL !!! -------
    fSubQueryLink: Boolean;
    fSubQueryTableName: string;
    fSubQueryKey: string;
    fSubQueryListKey: string;
    // ------------------------------
    procedure SetDataSet(aValue: TDataSet);
    procedure SetKeyField(const aValue: string);
    procedure SetLookupFields(const aValue: string);
    function  GetLookupFieldsCount_Internal: Integer;
    function  GetLookupCaptionsCount_Internal: Integer;
  protected
    function  DataSetActive: Boolean;
    function  KeyFieldExist: Boolean;
    function  LookupFieldExist(const Index: Integer): Boolean;
    function  KeyFieldActive: Boolean;
    function  LookupFieldActive(const Index: Integer): Boolean;
    function  KeyFieldLocate(const aValue: Integer): Boolean;
    function  LookupFieldLocate(const aValue: string; Index: Integer): Boolean;
    procedure PrepareDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetLookupFieldsCount(const CheckCaptions: Boolean = False): Integer;
    function  GetLookupFieldName(const Index: Integer): string;
    function  GetLookupFieldCaption(const Index: Integer): string;
    function  GetKeyField: TField;
    function  GetLookupField(const Index: Integer): TField;
    function  GetLookupText(const Index, Value: Integer; out Text: string): Boolean;
    function  ShowDialog(AOwner: TComponent; const ALeft, ATop: Integer): Boolean; override;
  published
    property LookupDataSet: TDataSet read fLinkDataSet write SetDataSet;
    property KeyField: string read fKeyField write SetKeyField;
    property LookupFields: string read fLookupFields write SetLookupFields;
    property LookupFieldIndex: Integer read fLookupFieldIndex write fLookupFieldIndex default 0;
    property LookupCaptions: string read fLookupCaptions write fLookupCaptions;
    property LookupDisabled: Boolean read fLookupDisabled write fLookupDisabled default True;
    property KeyNullValue: Integer read fKeyNull write fKeyNull default -1;
    property UseNullValue: Boolean read FUseNull write FUseNull default True;
    // --- Только для SQL !!! -------
    property SubQueryLink: Boolean read fSubQueryLink write fSubQueryLink default False;
    property SubQueryTableName: string read fSubQueryTableName write fSubQueryTableName;
    property SubQueryKeyField: string read fSubQueryKey write fSubQueryKey;
    property SubQueryParentField: string read fSubQueryListKey write fSubQueryListKey;
    // ------------------------------
  end;

{ == TRDFComboLinkItem ========================================================= }

  TRDFComboLinkItem = class(TRDFLinkItem)
  private
    fKeyValue: Integer;
    fKeyValue_Buf: Integer;
    fKeyValue_Def: Integer;
    fLookupValue_Def: string;
  protected
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function AutoActivateEnabled: Boolean; override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_KeyValue: Integer read fKeyValue write fKeyValue;
  published
    property KeyValue: Integer read fKeyValue_Def write fKeyValue_Def default -1;
    property LookupValue: string read fLookupValue_Def write fLookupValue_Def;
  end;

{ == TRDFComboDateItem ========================================================= }

  TRDFComboDateItem = class(TRDFLinkItem)
  private
    // KeyValue
    fKeyFieldName: string;
    fKeyValue: Integer;
    fKeyValue_Buf: Integer;
    fKeyValue_Def: Integer;
    fLookupValue_Def: string;
    // DateValues
    fDateFieldName: string;
    fDateValue_1: TDate;
    fDateValue_2: TDate;
    fDateValue_1_Def: TDate;
    fDateValue_2_Def: TDate;
    fDateValue_1_Buf: TDate;
    fDateValue_2_Buf: TDate;
    fDateMode: TDateEqMode;
    fDateMode_Def: TDateEqMode;
    fDateMode_Buf: TDateEqMode;
    procedure SetKeyFieldName(const AValue: string);
    procedure SetDateFieldName(const AValue: string);
    function  GetDateInitValue_1(const AMode: TDateEqMode; ADefValue: TDate): TDate;
    function  GetDateInitValue_2(const AMode: TDateEqMode; ADefValue: TDate): TDate;
  protected
    // DateValues
    procedure SetDateValue_1(const aValue: TDate);
    procedure SetDateValue_2(const aValue: TDate);
    procedure SetDateValue_1_Def(const aValue: TDate);
    procedure SetDateValue_2_Def(const aValue: TDate);
    // Other
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure UpdateEdits(Edit1, Edit2: TControl; const ADateMode: Integer);
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function  AutoActivateEnabled: Boolean; override;
    function  GetItemText: string; override;
    function  GetWhereString: string; override;
    function  GetFilterString: string; override;
    // KeyValue
    property  Item_KeyValue: Integer read fKeyValue write fKeyValue;
    // DateValues
    property  Item_DateCompareMode: TDateEqMode read fDateMode write fDateMode;
    property  Item_DateValue_1: TDate read fDateValue_1 write SetDateValue_1;
    property  Item_DateValue_2: TDate read fDateValue_2 write SetDateValue_2;
  published
    // KeyValue
    property KeyFieldName: string read fKeyFieldName write SetKeyFieldName;
    property KeyValue: Integer read fKeyValue_Def write fKeyValue_Def default -1;
    property LookupValue: string read fLookupValue_Def write fLookupValue_Def;
    // DateValues
    property  DateFieldName: string read fDateFieldName write SetDateFieldName;
    property  CompareMode: TDateEqMode read fDateMode_Def write fDateMode_Def default emdLast30Days;
    property  DateValue_1: TDate read fDateValue_1_Def write SetDateValue_1_Def;
    property  DateValue_2: TDate read fDateValue_2_Def write SetDateValue_2_Def;
  end;

{ == TRDFListLinkItem ========================================================== }

  TRDFListLinkItem = class(TRDFLinkItem)
  private
    fKeyValues: string;
    fKeyValues_Buf: string;
    fKeyValues_Def: string;
    fLookupFilter: string;
    FMaxLookupItems: Integer;
    fDelimiter: string[2];
    fTextDelimiter: string;
    function  GetDelimiter: string;
    procedure SetDelimiter(const aValue: string);
    function  DelimiterChar: Char;
  protected
    function CheckKeysList(const KeysList: string): string;
    function GenerateKeysList(const Filter: string): string;
    function GenerateLookupList(const KeysList: string): string;
    function GetEditorClass: TFormDbFilterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ListView_CreateColumns(LV: TListView);
    procedure ListView_LoadTable(LV: TListView);
    procedure ListView_ReadTable(LV: TListView);
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function AutoActivateEnabled: Boolean; override;
    function GetItemText: string; override;
    function GetWhereString: string; override;
    function GetFilterString: string; override;
    property Item_KeyValues: string read fKeyValues write fKeyValues;
  published
    property KeyValues: string read fKeyValues_Def write fKeyValues_Def;
    property DefaultSelection: string read fLookupFilter write fLookupFilter;
    property MaxTextValues: Integer read FMaxLookupItems write FMaxLookupItems default 5;
    property KeysDelimiter: string read GetDelimiter write SetDelimiter;
    property LookupDelimiter: string read fTextDelimiter write fTextDelimiter;
  end;

{ == TRDFProcLinkItem ========================================================== }

  TOnGetLookupNotifyEvent = procedure(Sender: TObject; const Key: Integer; out Text: string) of object;
  TOnSelectKeyNotifyEvent = procedure(Sender: TObject; var Key: Integer; out Complete: Boolean) of object;

  TRDFProcLinkItem = class(TRDFLinkItem)
  private
    fKeyValue: Integer;
    fKeyValue_Sel: Integer;
    fKeyValue_Buf: Integer;
    fKeyValue_Def: Integer;
    fForceSelect: Boolean;
    fIntLookup: Boolean;
    fOnGetLookup: TOnGetLookupNotifyEvent;
    fOnSelectKey: TOnSelectKeyNotifyEvent;
  protected
    procedure BtnSelectKey(Sender: TObject);
    function  BtnGetLookupText(Sender: TObject): string;
    function  GetEditorClass: TFormDbFilterItemClass; override;
    procedure LoadEditorControls(Editor: TFormDbFilterItem); override;
    procedure ReadEditorControls(Editor: TFormDbFilterItem); override;
    procedure PrepareDialog; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure ResetItem; override;
    procedure CopyToEditBuffer; override;
    procedure RestoreFromEditBuffer; override;
    procedure ReadItemData(Ini: TMemIniFile; const Section: string); override;
    procedure SaveItemData(Ini: TMemIniFile; const Section: string); override;
    function  AutoActivateEnabled: Boolean; override;
    function  GetItemText: string; override;
    function  GetWhereString: string; override;
    function  GetFilterString: string; override;
    property Item_KeyValue: Integer read fKeyValue write fKeyValue;
  published
    property KeyValue: Integer read fKeyValue_Def write fKeyValue_Def default -1;
    property ForceSelect: Boolean read fForceSelect write fForceSelect default True;
    property IntLookupText: Boolean read fIntLookup write fIntLookup default True;
    property OnGetLookupText: TOnGetLookupNotifyEvent read fOnGetLookup write fOnGetLookup;
    property OnSelectKeyValue: TOnSelectKeyNotifyEvent read fOnSelectKey write fOnSelectKey;
  end;

{ == TRDbFilter ================================================================ }

  TRDFOption        = (foDialogOnActivate,
                       foStoreItemsState, foChangeOptions,
                       foItemActivateOnChange,
                       foItemsDisabledActive, foItemsHiddenActive);

  TRDFOptions       = set of TRDFOption;


  TRDbFilter = class(TRDbCustomDS)
  private
    fItems: TList;
    fDateFmtWhere: string;
    fDateFmtFilter: string;
    fKeysWhere: string;
    fKeysFilter: string;
    fKeysText: string;
    fCaseEnabled: Boolean;
    fOptions: TRDfOptions;
    fAddBrackets: Boolean;
    fOnCreateItems: TOnCreateItems;
    procedure LoadItems(Ini: TMemIniFile);
    procedure SaveItems(Ini: TMemIniFile);
    procedure SetOptions(const aValue: TRDfOptions);
    function  InternalShowDialog(const OkOnly: Boolean): Boolean;
  protected
    function  GetItem(Index: Integer): TRDFItem;
    procedure SetItem(Index: Integer; Value: TRDFItem);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InternalInit; override;
    procedure InternalDone; override;
    procedure InternalReset; override;
    function  IsStoreOptions: Boolean; override;
    function  GetIniSection: string; override;
    procedure DoActivate; override;
  public
    procedure DeleteItems;
    procedure AddItem(Item: TRDFItem);
    procedure RemoveItem(Item: TRDFItem);
    procedure DeleteItem(Item: TRDFItem);
    procedure Delete(Index: Integer);
    function  IndexOf(Item: TRDFItem): Integer;
    function  GetCount: Integer;
    function  GetActiveCount: Integer;
    function  GetFirstActiveItem: TRDFItem;
    procedure ClearFilter;
    procedure ResetFilter;
    procedure CopyToEditBuffer;
    procedure RestoreFromEditBuffer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadData; override;
    procedure SaveData; override;
    function  ShowDialog: Boolean; override;
    function  GetItemOnField(const FieldName: string): TRDFItem;
    function  GetTextString: string;
    function  GetWhereString: string;
    function  GetFilterString: string;
    function  GetDbFilterOptions(ADefault: TFilterOptions): TFilterOptions;
    property  Item[Index: Integer]: TRDFItem read GetItem write SetItem; default;
  published
    property AddBrackets: Boolean read fAddBrackets write fAddBrackets default False;
    property Items: TList read fItems;
    property CaseStringsEnabled: Boolean read fCaseEnabled write fCaseEnabled default True;
    property DateFormatWhere: string read fDateFmtWhere write fDateFmtWhere;
    property DateFormatFilter: string read fDateFmtFilter write fDateFmtFilter;
    property KeysWhere: string read fKeysWhere write fKeysWhere;
    property KeysFilter: string read fKeysFilter write fKeysFilter;
    property KeysText: string read fKeysText write fKeysText;
    property Options: TRDfOptions read fOptions write SetOptions;
    property OnCreateItems: TOnCreateItems read fOnCreateItems write fOnCreateItems;
  end;

{ == Editor TRDbFilter ========================================================= }
  TFormDbFilter = class(TEditorsTemplate)
    CloseOkToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
    ClearAll: TAction;
    ClearAllToolButton: TToolButton;
    SeparatorReset: TToolButton;
    itemClearAll: TMenuItem;
    itemClearAllP: TMenuItem;
    divPopup2: TMenuItem;
    divPopup3: TMenuItem;
    divPopup4: TMenuItem;
    ResetAll: TAction;
    ResetAllToolButton: TToolButton;
    itemResetAll: TMenuItem;
    itemResetAllP: TMenuItem;
    ActiveAll: TAction;
    itemActiveAll: TMenuItem;
    itemActiveAllP: TMenuItem;
    ActiveAllToolButton: TToolButton;
    ItemProperties: TAction;
    SeparatorEdit: TToolButton;
    ItemPropertiesToolButton: TToolButton;
    itemItemPropertiesP: TMenuItem;
    divEdit1: TMenuItem;
    itemItemProperties: TMenuItem;
    LoadFromFile: TAction;
    SaveToFile: TAction;
    itemLoadFromFileP: TMenuItem;
    itemSaveToFileP: TMenuItem;
    itemLoadFromFile: TMenuItem;
    itemSaveToFile: TMenuItem;
    divFile1: TMenuItem;
    RestoreFilter: TAction;
    RequeryFilter: TAction;
    itemRestoreFilter: TMenuItem;
    itemRequeryFilter: TMenuItem;
    divService1: TMenuItem;
    itemRestoreFilterP: TMenuItem;
    itemRequeryFilterP: TMenuItem;
    LoadFromFileToolButton: TToolButton;
    SeparatorFile: TToolButton;
    SaveToFileToolButton: TToolButton;
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListViewChanging(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
    procedure ClearAllUpdate(Sender: TObject);
    procedure ClearAllExecute(Sender: TObject);
    procedure ResetAllUpdate(Sender: TObject);
    procedure ResetAllExecute(Sender: TObject);
    procedure ActiveAllUpdate(Sender: TObject);
    procedure ActiveAllExecute(Sender: TObject);
    procedure ItemPropertiesUpdate(Sender: TObject);
    procedure ItemPropertiesExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure LoadFromFileUpdate(Sender: TObject);
    procedure LoadFromFileExecute(Sender: TObject);
    procedure SaveToFileUpdate(Sender: TObject);
    procedure SaveToFileExecute(Sender: TObject);
    procedure RestoreFilterUpdate(Sender: TObject);
    procedure RestoreFilterExecute(Sender: TObject);
    procedure RequeryFilterUpdate(Sender: TObject);
    procedure RequeryFilterExecute(Sender: TObject);
    procedure CloseCancelUpdate(Sender: TObject);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
  private
    fFilter: TRDbFilter;
    procedure SetFilter(Value: TRDbFilter);
    procedure ResetItemsState;
  protected
    OnlyOk: Boolean;
    procedure InitForm; override;
    procedure DoneForm; override;
  public
    property Filter: TRDbFilter read fFilter write SetFilter;
  end;

const
  SRDFItemNames: array [TRDFItemType] of string[32] =
    ('Null', 'Boolean', 'Integer', 'Int64', 'Float', 'Date', 'String', 'StringList',
     'DbLink ComboBox', 'DbLink ComboBox + Date', 'DbLink ListBox', 'DbLink ExtProcs', 'Text');
  TRDFItemClasses: array [TRDFItemType] of TRDFItemClass =
    (TRDFNullItem, TRDFBooleanItem, TRDFIntegerItem, TRDFInt64Item, TRDFFloatItem, TRDFDateItem,
     TRDFStringItem, TRDFStringListItem,
     TRDFComboLinkItem, TRDFComboDateItem, TRDFListLinkItem, TRDFProcLinkItem, TRDFTextItem);

{ == Utilites ================================================================== }
procedure RegisterItemsClasses;
procedure LoadLogicalOperations(StrList: TStrings);

implementation

{$R *.dfm}

uses
  DateUtils, RVclUtils, RxStrUtils, RProgress, RDialogs, RDbUtils,
  RDbFilterItem_Null, RDbFilterItem_Boolean, RDbFilterItem_Integer, RDbFilterItem_Int64,
  RDbFilterItem_Float, RDbFilterItem_Date, RDbFilterItem_String,
  RDbFilterItem_LinkCombo, RDbFilterItem_LinkComboDate,
  RDbFilterItem_LinkList, RDbFilterItem_LinkProc;

resourcestring
  EListIndexError      = 'Неверный индекс элемента: %d.'#13'Компонент: %s.';
  EFieldNameNotDefine  = 'Для компонента фильтра "%s" не определено имя поля набора данных FieldName!';
  EFieldNotFound       = 'В наборе данных "%s" поле с именем "%s" не найдено!';
  EErrorRangeValues    = 'Значение нижней границы диапазона "%s" не может быть больше значения верхней границы диапазона "%s"!';
  EErrorCompareMode    = 'Некорректное значение CompareMode!';
  EErrorLookupFilter   = 'Некорректное значение фильтра значений "по-умолчанию"!'#13 +
                         '%s.%s.DefaultSelection = ''%s''.'#13'%s';
  EReadIniErrorField   = 'При чтении данных элемента фильтра "%s" обнаружено несоответствие имени поля набора данных!'#13 +
                         'Сохраненные данные для указанного элемента потеряны!';
  EFilterFileNotFound  = 'Файл фильтра "%s" не найден!';
  EFilterNotFound      = 'В файле "%s" фильтр "%s" не найден!';
  SReplaceFilter       = 'В файле "%s" фильтр "%s" уже сохранен!'#13'Заменить сохраненный фильтр новым?';

  SShortAnd            = 'И';
  SShortOr             = 'ИЛИ';
  SLongAnd             = 'И (сложение)';
  SLongOr              = 'ИЛИ (умножение)';
  SInvertText          = 'кроме (%s)';
  SOpBlockText         = 'БЛК %s';
  SOPBlockTextEmpty    = 'БЛК';
  SFilterOn            = '%s';
  SFilterOff           = 'Нет фильтра';
  STextItem            = '%s: "%s"';
  SNullText            = 'значение не задано (null)';
  SDataText            = 'любое действительное значение (кроме null)';
  SEqualText           = 'равно указанному (=)';
  SMoreText            = 'больше укзанного (>)';
  SMoreEqText          = 'больше или равно указанного (>=)';
  SLessText            = 'меньше указанного (<)';
  SLessEqText          = 'меньше или равно указанного (<=)';
  SBetweenText         = 'диапазон значений включительно (>.<)';

const
  iniFilter            = 'FILTER_%s.%s';
  iniFieldName         = '%s_FieldName';
  iniActive            = '%s_Active';
  iniInvert            = '%s_Inverted';
  iniOperation         = '%s_LogicalOperator';
  iniMode              = '%s_CompareMode';
  iniDateMode          = '%s_DateCompareMode';
  iniCase              = '%s_CaseSensitive';
  iniValue_1           = '%s_Value_1';
  iniValue_2           = '%s_Value_2';
  iniKeyValue_1        = '%s_KeyValue_1';
  iniKeyValue_2        = '%s_KeyValue_2';
  iniDateValue_1       = '%s_DateValue_1';
  iniDateValue_2       = '%s_DateValue_2';

  Brackets             = '(%s)';
  InvertItem           = 'NOT (%s)';
  UpperCaseItem        = 'UPPER(%s)';

  MinProgressCount     = 32;

  LogicalOperations    : array [TLogicalOperation] of string = (SShortAnd, SShortOr);
  LogicalOperationsExt : array [TLogicalOperation] of string = (SLongAnd, SLongOr);
  LogicalOperationsSql : array [TLogicalOperation] of string = (' AND ', ' OR ');

var
  ClassesRegistered: Boolean = False;

{ == Utilites ================================================================== }
procedure RegisterItemsClasses;
var
  i: TRDFItemType;
begin
  if not ClassesRegistered then
  begin
    for i := Low(TRDFItemType) to High(TRDFItemType) do
      RegisterClasses([TRDFItemClasses[i]]);
    ClassesRegistered := True;
  end;
end;

procedure LoadLogicalOperations(StrList: TStrings);
var
  i: TLogicalOperation;
begin
  StrList.BeginUpdate;
  try
    StrList.Clear;
    for i := Low(TLogicalOperation) to High(TLogicalOperation) do
      StrList.Add(LogicalOperationsExt[i]);
  finally
    StrList.EndUpdate;
  end;
end;

function ConcatFilters(const sFilter1, sFilter2, sOperator: string;
  const bPartBrackets, bFineBrackets: Boolean): string;
begin
  Result := EmptyStr;

  if (sFilter1 <> EmptyStr) and (sFilter2 <> EmptyStr) then
  begin
    if bPartBrackets
    then Result := Format(Brackets, [sFilter1]) + sOperator + Format(Brackets, [sFilter2])
    else Result := sFilter1 + sOperator + sFilter2;
  end
  else begin
    if sFilter1 <> EmptyStr
    then Result := sFilter1
    else Result := sFilter2;
  end;

  if (Result <> EmptyStr) and bFineBrackets then
    Result := Format(Result, [sFilter1]);
end;

{ == TRDFItem ================================================================== }

constructor TRDFItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseEnabled := True;
  fFilter := nil;
  if Assigned(fOnCreate) then
    fOnCreate(Self);
  Clear;
end;

destructor TRDFItem.Destroy;
begin
  if Assigned(fFilter) and fFilter.Active then
    fFilter.Close;
  SetActive(False);
  if Assigned(fFilter) then
    fFilter.RemoveItem(Self);
  if Assigned(fOnDestroy) then
    fOnDestroy(Self);
  fOnGetText := nil;
  fOnGetWhere := nil;
  fOnGetFilter := nil;
  inherited Destroy;
end;

procedure TRDFItem.Clear;
begin
  fActive := False;
  fInvert := False;
  fEnabled := True;
  fBlocked := False;
  fVisible := True;
  fActive_Def := False;
  fInvert_Def := False;
  fActive_Buf := False;
  fInvert_Buf := False;
  fOperation := loAnd;
  fOperation_Def := loAnd;
  fOperation_Buf := loAnd;
  fFieldName := EmptyStr;
  fFieldCaption := EmptyStr;
  fOnGetText := nil;
  fOnGetWhere := nil;
  fOnGetFilter := nil;
end;

procedure TRDFItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else
    if Source is TRDFItem then
    begin
      fActive := TRDFItem(Source).fActive;
      fInvert := TRDFItem(Source).fInvert;
      fEnabled := TRDFItem(Source).fEnabled;
      fVisible := TRDFItem(Source).fVisible;
      fActive_Def := TRDFItem(Source).fActive_Def;
      fInvert_Def := TRDFItem(Source).fInvert_Def;
      fActive_Buf := TRDFItem(Source).fActive_Buf;
      fInvert_Buf := TRDFItem(Source).fInvert_Buf;
      fOperation := TRDFItem(Source).fOperation;
      fOperation_Def := TRDFItem(Source).fOperation_Def;
      fOperation_Buf := TRDFItem(Source).fOperation_Buf;
      fFieldName := TRDFItem(Source).fFieldName;
      fFieldCaption := TRDFItem(Source).fFieldCaption;
      fOnGetText := TRDFItem(Source).fOnGetText;
      fOnGetWhere := TRDFItem(Source).fOnGetWhere;
      fOnGetFilter := TRDFItem(Source).fOnGetFilter;
    end;
end;

procedure TRDFItem.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Assigned(Reader) and Assigned(Reader.Parent) and (Reader.Parent is TRDbFilter) then
    fFilter := TRDbFilter(Reader.Parent);
end;

function TRDFItem.HasParent: Boolean;
begin
  Result := True;
end;

function TRDFItem.GetParentComponent: TComponent;
begin
  Result := fFilter;
end;

procedure TRDFItem.SetParentComponent(AParent: TComponent);
begin
  if Assigned(fFilter) then fFilter.RemoveItem(Self);
  if (AParent <> nil) and (AParent is TRDbFilter) then
    TRDbFilter(AParent).AddItem(Self);
end;

function TRDFItem.GetActive: Boolean;
begin
  Result := fActive and
    (fEnabled or (foItemsDisabledActive in Filter.Options)) and
    (fVisible or (foItemsHiddenActive in Filter.Options));
end;

procedure TRDFItem.SetActive(const aValue: Boolean);
begin
  if aValue <> fActive then
  begin
    if aValue and (fFieldName = EmptyStr) then
      raise ERDbFilterError.CreateFmt(EFieldNameNotDefine, [Name]);
    fActive := aValue;
  end;
end;

procedure TRDFItem.SetActive_Def(const aValue: Boolean);
begin
  if aValue <> fActive_Def then
  begin
    if not (csLoading in ComponentState) and aValue
    and (fFieldName = EmptyStr) then
      raise ERDbFilterError.CreateFmt(EFieldNameNotDefine, [Name]);
    fActive_Def := aValue;
  end;
end;

procedure TRDFItem.SetFieldName(const aValue: string);
begin
  if Trim(aValue) <> fFieldName then
  begin
    fFieldName := Trim(aValue);
    if fFieldName = EmptyStr then
    begin
      Active := False;
      Item_Active := False;
    end
    else begin
      if (fFilter <> nil) and (fFilter.DataSet <> nil)
      and (fFilter.DataSet.FindField(fFieldName) <> nil)
      and (fFieldCaption = EmptyStr)
      then fFieldCaption := fFilter.DataSet.FieldByName(fFieldName).DisplayLabel;
    end;
  end;
end;

procedure TRDFItem.ResetItem;
begin
  SetActive(fActive_Def);
  fInvert := fInvert_Def;
  fOperation := fOperation_Def;
end;

procedure TRDFItem.CopyToEditBuffer;
begin
  fActive_Buf := fActive;
  fInvert_Buf := fInvert;
  fOperation_Buf := fOperation;
end;

procedure TRDFItem.RestoreFromEditBuffer;
begin
  fActive := fActive_Buf;
  fInvert := fInvert_Buf;
  fOperation := fOperation_Buf;
end;

procedure TRDFItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  SetActive(Ini.ReadBool(Section, Format(iniActive, [Name]), fActive_Def));
  fInvert := Ini.ReadBool(Section, Format(iniInvert, [Name]), fInvert_Def);
  fOperation := TLogicalOperation(Ini.ReadInteger(Section, Format(iniOperation, [Name]), Integer(fOperation_Def)));
end;

procedure TRDFItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  Ini.WriteString(Section, Format(iniFieldName, [Name]), fFieldName);
  Ini.WriteBool(Section, Format(iniActive, [Name]), fActive);
  Ini.WriteBool(Section, Format(iniInvert, [Name]), fInvert);
  Ini.WriteInteger(Section, Format(iniOperation, [Name]), Integer(fOperation));
end;

function TRDFItem.AutoActivateEnabled: Boolean;
begin
  Result := fFieldName <> EmptyStr;
end;

function TRDFItem.IsFirstActiveItem: Boolean;
var
  FirstActiveItem: TRDFItem;
begin
  Result := True;
  if Assigned(fFilter) then
  begin
    FirstActiveItem := fFilter.GetFirstActiveItem;
    Result := (FirstActiveItem = nil) or
      (fFilter.fItems.IndexOf(Self) <= fFilter.fItems.IndexOf(FirstActiveItem));
  end;
end;

function TRDFItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem;
end;

function TRDFItem.ShowDialog(AOwner: TComponent; const ALeft, ATop: Integer): Boolean;
var
  Editor: TFormDbFilterItem;
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
      LoadLogicalOperations(Editor.OperationComboBox.Items);
      Editor.FieldText.Caption := FieldCaption;
      Editor.ActiveCheckBox.Checked := Item_Active;
      Editor.InvertCheckBox.Checked := Item_Invert;
      Editor.OperationComboBox.ItemIndex := Integer(Item_Operation);
      Editor.OperationComboBox.Enabled := not IsFirstActiveItem;
      LoadEditorControls(Editor);
    finally
      StopWait;
    end;
    Result := Editor.ShowModal = mrOk;
    if Result then
    begin
      StartWait;
      try
        Item_Active := Editor.ActiveCheckBox.Checked;
        Item_Invert := Editor.InvertCheckBox.Checked;
        Item_Operation := TLogicalOperation(Editor.OperationComboBox.ItemIndex);
        ReadEditorControls(Editor);
      finally
        StopWait;
      end;
    end;
  finally
    Editor.Free;
  end;
end;

function TRDFItem.GetOperationText(const Forced: Boolean = False): string;
begin
  if not Item_Active or (IsFirstActiveItem and not Forced)
  then Result := EmptyStr
  else Result := LogicalOperations[fOperation];
  if not fEnabled or fBlocked then
  begin
    if Result = EmptyStr
    then Result := SOpBlockTextEmpty
    else Result := Format(SOpBlockText, [Result]);
  end;
end;

function TRDFItem.GetDbFilterOptions: TFilterOptions;
begin
  Result := [];
end;

procedure TRDFItem.DoGetItemText(var Text: string);
begin
  if Assigned(fOnGetText) then
    fOnGetText(Self, Text);
end;

procedure TRDFItem.DoGetWhereString(var Text: string);
begin
  if Assigned(fOnGetWhere) then
    fOnGetWhere(Self, Text);
end;

procedure TRDFItem.DoGetFilterString(var Text: string);
begin
  if Assigned(fOnGetFilter) then
    fOnGetFilter(Self, Text);
end;

{ == TRDFNullItem ============================================================== }

const
  NullEqModesText: array [Boolean] of string =
    (SNullText, SDataText);
  NullEqModesWhere: array [Boolean] of string =
    ('%0:s IS NULL', '%0:s IS NOT NULL');
  NullEqModesFilter: array [Boolean] of string =
    ('[%0:s]=NULL', '[%0:s]<>NULL');

procedure TRDFNullItem.Clear;
begin
  inherited Clear;
  fMode := True;
  fMode_Def := True;
  fMode_Buf := True;
end;

procedure TRDFNullItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFNullItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFNullItem then
    begin
      SrcItem := Source as TRDFNullItem;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
    end;
  end;
end;

procedure TRDFNullItem.ResetItem;
begin
  inherited ResetItem;
  fMode := fMode_Def;
end;

procedure TRDFNullItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fMode_Buf := fMode;
end;

procedure TRDFNullItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fMode := fMode_Buf;
end;

procedure TRDFNullItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fMode := Ini.ReadBool(Section, Format(iniMode, [Name]), fMode_Def);
end;

procedure TRDFNullItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteBool(Section, Format(iniMode, [Name]), fMode);
end;

function TRDFNullItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_Null;
end;

procedure TRDFNullItem.LoadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Null(Editor) do
  begin
    if fMode
    then ModeRadioGroup.ItemIndex := 1
    else ModeRadioGroup.ItemIndex := 0;
  end;
end;

procedure TRDFNullItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Null(Editor) do
  begin
    Item_CompareMode := ModeRadioGroup.ItemIndex = 1;
  end;
end;

function TRDFNullItem.GetItemText: string;
begin
  if Item_Active then
  begin
    Result := NullEqModesText[fMode];
    if fInvert and (Result <> EmptyStr) then
      Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFNullItem.GetWhereString: string;
begin
  if Item_Active
  then Result := Format(NullEqModesWhere[fMode], [fFieldName])
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFNullItem.GetFilterString: string;
begin
  if Item_Active
  then Result := Format(NullEqModesFilter[fMode], [fFieldName])
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFBooleanItem =========================================================== }

resourcestring
  STextTrue    = 'Истина';
  STextFalse   = 'Ложь';

const
  SWhereTrue   = '1';
  SWhereFalse  = '0';
  SFilterTrue  = 'TRUE';
  SFilterFalse = 'FALSE';

  BooleanEqModesText: array [TBooleanEqMode] of string =
    (SNullText, '%s', '%s');
  BooleanEqModesWhere: array [TBooleanEqMode] of string =
    ('%0:s IS NULL', '%0:s=%1:s', '%0:s=%1:s');
  BooleanEqModesFilter: array [TBooleanEqMode] of string =
    ('[%0:s]=NULL', '[%0:s]=%1:s', '[%0:s]=%1:s');

procedure TRDFBooleanItem.Clear;
begin
  inherited Clear;
  fWhereTrue := SWhereTrue;
  fWhereFalse := SWhereFalse;
  fFilterTrue := SFilterTrue;
  fFilterFalse := SFilterFalse;
  fTextTrue := STextTrue;
  fTextFalse := STextFalse;
  fMode := embTrue;
  fMode_Def := embTrue;
  fMode_Buf := embTrue;
end;

procedure TRDFBooleanItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFBooleanItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFBooleanItem then
    begin
      SrcItem := Source as TRDFBooleanItem;
      fWhereTrue := SrcItem.fWhereTrue;
      fWhereFalse := SrcItem.fWhereFalse;
      fFilterTrue := SrcItem.fFilterTrue;
      fFilterFalse := SrcItem.fFilterFalse;
      fTextTrue := SrcItem.fTextTrue;
      fTextFalse := SrcItem.fTextFalse;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
    end;
  end;
end;

procedure TRDFBooleanItem.ResetItem;
begin
  inherited ResetItem;
  fMode := fMode_Def;
end;

procedure TRDFBooleanItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fMode_Buf := fMode;
end;

procedure TRDFBooleanItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fMode := fMode_Buf;
end;

procedure TRDFBooleanItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fMode := TBooleanEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
end;

procedure TRDFBooleanItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
end;

function TRDFBooleanItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_Boolean;
end;

procedure TRDFBooleanItem.LoadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Boolean(Editor) do
  begin
    ModeComboBox.Items.Add(BooleanEqModesText[embNull]);
    ModeComboBox.Items.Add(Format(BooleanEqModesText[embTrue], [fTextTrue]));
    ModeComboBox.Items.Add(Format(BooleanEqModesText[embFalse], [fTextFalse]));
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
  end;
end;

procedure TRDFBooleanItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Boolean(Editor) do
  begin
    Item_CompareMode := TBooleanEqMode(ModeComboBox.ItemIndex);
  end;
end;

function TRDFBooleanItem.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      embNull: Result := BooleanEqModesText[fMode];
      embTrue: Result := Format(BooleanEqModesText[fMode], [fTextTrue]);
      embFalse: Result := Format(BooleanEqModesText[fMode], [fTextFalse]);
      else Result := EErrorCompareMode;
    end;
    if fInvert and (Result <> EmptyStr) then
      Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFBooleanItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      embNull: Result := Format(BooleanEqModesWhere[fMode], [fFieldName]);
      embTrue: Result := Format(BooleanEqModesWhere[fMode], [fFieldName, fWhereTrue]);
      embFalse: Result := Format(BooleanEqModesWhere[fMode], [fFieldName, fWhereFalse]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFBooleanItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      embNull: Result := Format(BooleanEqModesFilter[fMode], [fFieldName]);
      embTrue: Result := Format(BooleanEqModesFilter[fMode], [fFieldName, fFilterTrue]);
      embFalse: Result := Format(BooleanEqModesFilter[fMode], [fFieldName, fFilterFalse]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFIntegerItem =========================================================== }

resourcestring
  SEqualInteger   = 'равно %d';
  SMoreInteger    = 'больше %d';
  SMoreEqInteger  = 'больше или равно %d';
  SLessInteger    = 'меньше %d';
  SLessEqInteger  = 'меньше или равно %d';
  SBetweenInteger = 'от %d до %d включительно';

const
  NumbersEqModes: array [TNumbersEqMode] of string =
    (SNullText, SEqualText, SMoreText, SMoreEqText, SLessText, SLessEqText, SBetweenText);

  IntegerEqModesText: array [TNumbersEqMode] of string =
    (SNullText, SEqualInteger, SMoreInteger, SMoreEqInteger,
     SLessInteger, SLessEqInteger, SBetweenInteger);
  IntegerEqModesWhere: array [TNumbersEqMode] of string =
    ('%0:s IS NULL', '%0:s=%1:d', '%0:s>%1:d', '%0:s>=%1:d',
     '%0:s<%1:d', '%0:s<=%1:d', '(%0:s>=%1:d) and (%0:s<=%2:d)');
  IntegerEqModesFilter: array [TNumbersEqMode] of string =
    ('[%0:s]=NULL', '[%0:s]=%1:d', '[%0:s]>%1:d', '[%0:s]>=%1:d',
     '[%0:s]<%1:d', '[%0:s]<=%1:d', '([%0:s]>=%1:d) and ([%0:s]<=%2:d)');

procedure TRDFIntegerItem.Clear;
begin
  inherited Clear;
  fValue_1 := 0;
  fValue_2 := 0;
  fValue_1_Def := 0;
  fValue_2_Def := 0;
  fValue_1_Buf := 0;
  fValue_2_Buf := 0;
  fMode := emnEqual;
  fMode_Def := emnEqual;
  fMode_Buf := emnEqual;
end;

procedure TRDFIntegerItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFIntegerItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFIntegerItem then
    begin
      SrcItem := Source as TRDFIntegerItem;
      fValue_1 := SrcItem.fValue_1;
      fValue_2 := SrcItem.fValue_2;
      fValue_1_Def := SrcItem.fValue_1_Def;
      fValue_2_Def := SrcItem.fValue_2_Def;
      fValue_1_Buf := SrcItem.fValue_1_Buf;
      fValue_2_Buf := SrcItem.fValue_2_Buf;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
    end;
  end;
end;

procedure TRDFIntegerItem.SetValue_1(const aValue: Integer);
begin
  if aValue <> fValue_1 then
  begin
    fValue_1 := aValue;
  end;
end;

procedure TRDFIntegerItem.SetValue_2(const aValue: Integer);
begin
  if aValue <> fValue_2 then
  begin
    fValue_2 := aValue;
  end;
end;

procedure TRDFIntegerItem.SetValue_1_Def(const aValue: Integer);
begin
  if aValue <> fValue_1_Def then
  begin
    fValue_1_Def := aValue;
  end;
end;

procedure TRDFIntegerItem.SetValue_2_Def(const aValue: Integer);
begin
  if aValue <> fValue_2_Def then
  begin
    fValue_2_Def := aValue;
  end;
end;

procedure TRDFIntegerItem.ResetItem;
begin
  inherited ResetItem;
  SetValue_1(fValue_1_Def);
  SetValue_2(fValue_2_Def);
  fMode := fMode_Def;
end;

procedure TRDFIntegerItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fValue_1_Buf := fValue_1;
  fValue_2_Buf := fValue_2;
  fMode_Buf := fMode;
end;

procedure TRDFIntegerItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fValue_1 := fValue_1_Buf;
  fValue_2 := fValue_2_Buf;
  fMode := fMode_Buf;
end;

procedure TRDFIntegerItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  SetValue_1(Ini.ReadInteger(Section, Format(iniValue_1, [Name]), fValue_1_Def));
  SetValue_2(Ini.ReadInteger(Section, Format(iniValue_2, [Name]), fValue_2_Def));
  fMode := TNumbersEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
end;

procedure TRDFIntegerItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteInteger(Section, Format(iniValue_1, [Name]), fValue_1);
  Ini.WriteInteger(Section, Format(iniValue_2, [Name]), fValue_2);
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
end;

function TRDFIntegerItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_Integer;
end;

procedure TRDFIntegerItem.UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
begin
  Edit1.Enabled := TNumbersEqMode(AMode) in [emnEqual, emnMore, emnMoreEq, emnBetween];
  Edit2.Enabled := TNumbersEqMode(AMode) in [emnLess, emnLessEq, emnBetween];
end;

procedure TRDFIntegerItem.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TNumbersEqMode;
begin
  with TFormDbFilterItem_Integer(Editor) do
  begin
    for i := Low(TNumbersEqMode) to High(TNumbersEqMode) do
      ModeComboBox.Items.Add(NumbersEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
    SpinEdit1.Value := Item_Value_1;
    SpinEdit2.Value := Item_Value_2;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFIntegerItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Integer(Editor) do
  begin
    Item_CompareMode := TNumbersEqMode(ModeComboBox.ItemIndex);
    Item_Value_1 := SpinEdit1.Value;
    Item_Value_2 := SpinEdit2.Value;
  end;
end;

function TRDFIntegerItem.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := IntegerEqModesText[fMode];
      emnEqual, emnMore, emnMoreEq: Result := Format(IntegerEqModesText[fMode], [fValue_1]);
      emnLess, emnLessEq: Result := Format(IntegerEqModesText[fMode], [fValue_2]);
      emnBetween: Result := Format(IntegerEqModesText[fMode], [fValue_1, fValue_2]);
      else Result := EErrorCompareMode;
    end;
    if fInvert and (Result <> EmptyStr) then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFIntegerItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := Format(IntegerEqModesWhere[fMode], [fFieldName]);
      emnEqual, emnMore, emnMoreEq: Result := Format(IntegerEqModesWhere[fMode], [fFieldName, fValue_1]);
      emnLess, emnLessEq: Result := Format(IntegerEqModesWhere[fMode], [fFieldName, fValue_2]);
      emnBetween: Result := Format(IntegerEqModesWhere[fMode], [fFieldName, fValue_1, fValue_2]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFIntegerItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := Format(IntegerEqModesFilter[fMode], [fFieldName]);
      emnEqual, emnMore, emnMoreEq: Result := Format(IntegerEqModesFilter[fMode], [fFieldName, fValue_1]);
      emnLess, emnLessEq: Result := Format(IntegerEqModesFilter[fMode], [fFieldName, fValue_2]);
      emnBetween: Result := Format(IntegerEqModesFilter[fMode], [fFieldName, fValue_1, fValue_2]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFInt64Item =========================================================== }

procedure TRDFInt64Item.Clear;
begin
  inherited Clear;
  fValue_1 := 0;
  fValue_2 := 0;
  fValue_1_Def := 0;
  fValue_2_Def := 0;
  fValue_1_Buf := 0;
  fValue_2_Buf := 0;
  fMode := emnEqual;
  fMode_Def := emnEqual;
  fMode_Buf := emnEqual;
end;

procedure TRDFInt64Item.Assign(Source: TPersistent);
var
  SrcItem: TRDFInt64Item;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFInt64Item then
    begin
      SrcItem := Source as TRDFInt64Item;
      fValue_1 := SrcItem.fValue_1;
      fValue_2 := SrcItem.fValue_2;
      fValue_1_Def := SrcItem.fValue_1_Def;
      fValue_2_Def := SrcItem.fValue_2_Def;
      fValue_1_Buf := SrcItem.fValue_1_Buf;
      fValue_2_Buf := SrcItem.fValue_2_Buf;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
    end;
  end;
end;

procedure TRDFInt64Item.SetValue_1(const aValue: Int64);
begin
  if aValue <> fValue_1 then
  begin
    fValue_1 := aValue;
  end;
end;

procedure TRDFInt64Item.SetValue_2(const aValue: Int64);
begin
  if aValue <> fValue_2 then
  begin
    fValue_2 := aValue;
  end;
end;

procedure TRDFInt64Item.SetValue_1_Def(const aValue: Int64);
begin
  if aValue <> fValue_1_Def then
  begin
    fValue_1_Def := aValue;
  end;
end;

procedure TRDFInt64Item.SetValue_2_Def(const aValue: Int64);
begin
  if aValue <> fValue_2_Def then
  begin
    fValue_2_Def := aValue;
  end;
end;

procedure TRDFInt64Item.ResetItem;
begin
  inherited ResetItem;
  SetValue_1(fValue_1_Def);
  SetValue_2(fValue_2_Def);
  fMode := fMode_Def;
end;

procedure TRDFInt64Item.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fValue_1_Buf := fValue_1;
  fValue_2_Buf := fValue_2;
  fMode_Buf := fMode;
end;

procedure TRDFInt64Item.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fValue_1 := fValue_1_Buf;
  fValue_2 := fValue_2_Buf;
  fMode := fMode_Buf;
end;

procedure TRDFInt64Item.ReadItemData(Ini: TMemIniFile; const Section: string);

  function ReadInt64(const Section, Ident: string; Default: Longint): Longint;
  var
    IntStr: string;
  begin
    IntStr := Ini.ReadString(Section, Ident, '');
    if (Length(IntStr) > 2) and (IntStr[1] = '0') and
       ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
      IntStr := '$' + Copy(IntStr, 3, Maxint);
    Result := StrToInt64Def(IntStr, Default);
  end;

begin
  inherited ReadItemData(Ini, Section);
  SetValue_1(ReadInt64(Section, Format(iniValue_1, [Name]), fValue_1_Def));
  SetValue_2(ReadInt64(Section, Format(iniValue_2, [Name]), fValue_2_Def));
  fMode := TNumbersEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
end;

procedure TRDFInt64Item.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteString(Section, Format(iniValue_1, [Name]), IntToStr(fValue_1));
  Ini.WriteString(Section, Format(iniValue_2, [Name]), IntToStr(fValue_2));
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
end;

function TRDFInt64Item.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_Int64;
end;

procedure TRDFInt64Item.UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
begin
  Edit1.Enabled := TNumbersEqMode(AMode) in [emnEqual, emnMore, emnMoreEq, emnBetween];
  Edit2.Enabled := TNumbersEqMode(AMode) in [emnLess, emnLessEq, emnBetween];
end;

procedure TRDFInt64Item.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TNumbersEqMode;
begin
  with TFormDbFilterItem_Int64(Editor) do
  begin
    for i := Low(TNumbersEqMode) to High(TNumbersEqMode) do
      ModeComboBox.Items.Add(NumbersEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
    SpinEdit1.Value := Item_Value_1;
    SpinEdit2.Value := Item_Value_2;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFInt64Item.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Int64(Editor) do
  begin
    Item_CompareMode := TNumbersEqMode(ModeComboBox.ItemIndex);
    Item_Value_1 := SpinEdit1.Value;
    Item_Value_2 := SpinEdit2.Value;
  end;
end;

function TRDFInt64Item.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := IntegerEqModesText[fMode];
      emnEqual, emnMore, emnMoreEq: Result := Format(IntegerEqModesText[fMode], [fValue_1]);
      emnLess, emnLessEq: Result := Format(IntegerEqModesText[fMode], [fValue_2]);
      emnBetween: Result := Format(IntegerEqModesText[fMode], [fValue_1, fValue_2]);
      else Result := EErrorCompareMode;
    end;
    if fInvert and (Result <> EmptyStr) then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFInt64Item.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := Format(IntegerEqModesWhere[fMode], [fFieldName]);
      emnEqual, emnMore, emnMoreEq: Result := Format(IntegerEqModesWhere[fMode], [fFieldName, fValue_1]);
      emnLess, emnLessEq: Result := Format(IntegerEqModesWhere[fMode], [fFieldName, fValue_2]);
      emnBetween: Result := Format(IntegerEqModesWhere[fMode], [fFieldName, fValue_1, fValue_2]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFInt64Item.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := Format(IntegerEqModesFilter[fMode], [fFieldName]);
      emnEqual, emnMore, emnMoreEq: Result := Format(IntegerEqModesFilter[fMode], [fFieldName, fValue_1]);
      emnLess, emnLessEq: Result := Format(IntegerEqModesFilter[fMode], [fFieldName, fValue_2]);
      emnBetween: Result := Format(IntegerEqModesFilter[fMode], [fFieldName, fValue_1, fValue_2]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFFloatItem ============================================================= }

resourcestring
  SEqualFloat   = 'равно %f';
  SMoreFloat    = 'больше %f';
  SMoreEqFloat  = 'больше или равно %f';
  SLessFloat    = 'меньше %f';
  SLessEqFloat  = 'меньше или равно %f';
  SBetweenFloat = 'от %f до %f включительно';

const
  FloatEqModesText: array [TNumbersEqMode] of string =
    (SNullText, SEqualFloat, SMoreFloat, SMoreEqFloat,
     SLessFloat, SLessEqFloat, SBetweenFloat);
  FloatEqModesWhere: array [TNumbersEqMode] of string =
    ('%0:s IS NULL', '%0:s=%1:s', '%0:s>%1:s', '%0:s>=%1:s',
     '%0:s<%1:s', '%0:s<=%1:s', '(%0:s>=%1:s) and (%0:s<=%2:s)');
  FloatEqModesFilter: array [TNumbersEqMode] of string =
    ('[%0:s]=NULL', '[%0:s]=%1:f', '[%0:s]>%1:f', '[%0:s]>=%1:f',
     '[%0:s]<%1:f', '[%0:s]<=%1:f', '([%0:s]>=%1:f) and ([%0:s]<=%2:f)');

procedure TRDFFloatItem.Clear;
begin
  inherited Clear;
  fValue_1 := 0;
  fValue_2 := 0;
  fValue_1_Def := 0;
  fValue_2_Def := 0;
  fValue_1_Buf := 0;
  fValue_2_Buf := 0;
  fMode := emnEqual;
  fMode_Def := emnEqual;
  fMode_Buf := emnEqual;
end;

procedure TRDFFloatItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFFloatItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFFloatItem then
    begin
      SrcItem := Source as TRDFFloatItem;
      fValue_1 := SrcItem.fValue_1;
      fValue_2 := SrcItem.fValue_2;
      fValue_1_Def := SrcItem.fValue_1_Def;
      fValue_2_Def := SrcItem.fValue_2_Def;
      fValue_1_Buf := SrcItem.fValue_1_Buf;
      fValue_2_Buf := SrcItem.fValue_2_Buf;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
    end;
  end;
end;

procedure TRDFFloatItem.SetValue_1(const aValue: Double);
begin
  if aValue <> fValue_1 then
  begin
    fValue_1 := aValue;
  end;
end;

procedure TRDFFloatItem.SetValue_2(const aValue: Double);
begin
  if aValue <> fValue_2 then
  begin
    fValue_2 := aValue;
  end;
end;

procedure TRDFFloatItem.SetValue_1_Def(const aValue: Double);
begin
  if aValue <> fValue_1_Def then
  begin
    fValue_1_Def := aValue;
  end;
end;

procedure TRDFFloatItem.SetValue_2_Def(const aValue: Double);
begin
  if aValue <> fValue_2_Def then
  begin
    fValue_2_Def := aValue;
  end;
end;

procedure TRDFFloatItem.ResetItem;
begin
  inherited ResetItem;
  SetValue_1(fValue_1_Def);
  SetValue_2(fValue_2_Def);
  fMode := fMode_Def;
end;

procedure TRDFFloatItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fValue_1_Buf := fValue_1;
  fValue_2_Buf := fValue_2;
  fMode_Buf := fMode;
end;

procedure TRDFFloatItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fValue_1 := fValue_1_Buf;
  fValue_2 := fValue_2_Buf;
  fMode := fMode_Buf;
end;

procedure TRDFFloatItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  SetValue_1(Ini.ReadFloat(Section, Format(iniValue_1, [Name]), fValue_1_Def));
  SetValue_2(Ini.ReadFloat(Section, Format(iniValue_2, [Name]), fValue_2_Def));
  fMode := TNumbersEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
end;

procedure TRDFFloatItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteFloat(Section, Format(iniValue_1, [Name]), fValue_1);
  Ini.WriteFloat(Section, Format(iniValue_2, [Name]), fValue_2);
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
end;

function TRDFFloatItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_Float;
end;

procedure TRDFFloatItem.UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
begin
  Edit1.Enabled := TNumbersEqMode(AMode) in [emnEqual, emnMore, emnMoreEq, emnBetween];
  Edit2.Enabled := TNumbersEqMode(AMode) in [emnLess, emnLessEq, emnBetween];
end;

procedure TRDFFloatItem.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TNumbersEqMode;
begin
  with TFormDbFilterItem_Float(Editor) do
  begin
    for i := Low(TNumbersEqMode) to High(TNumbersEqMode) do
      ModeComboBox.Items.Add(NumbersEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
    RFloatEdit1.Value := Item_Value_1;
    RFloatEdit2.Value := Item_Value_2;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFFloatItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Float(Editor) do
  begin
    Item_CompareMode := TNumbersEqMode(ModeComboBox.ItemIndex);
    Item_Value_1 := RFloatEdit1.Value;
    Item_Value_2 := RFloatEdit2.Value;
  end;
end;

function TRDFFloatItem.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := FloatEqModesText[fMode];
      emnEqual, emnMore, emnMoreEq: Result := Format(FloatEqModesText[fMode], [fValue_1]);
      emnLess, emnLessEq: Result := Format(FloatEqModesText[fMode], [fValue_2]);
      emnBetween: Result := Format(FloatEqModesText[fMode], [fValue_1, fValue_2]);
      else Result := EErrorCompareMode;
    end;
    if fInvert and (Result <> EmptyStr) then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFFloatItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := Format(FloatEqModesWhere[fMode], [fFieldName]);
      emnEqual, emnMore, emnMoreEq:
        Result := Format(FloatEqModesWhere[fMode], [fFieldName, FloatToSqlStr(fValue_1)]);
      emnLess, emnLessEq:
        Result := Format(FloatEqModesWhere[fMode], [fFieldName, FloatToSqlStr(fValue_2)]);
      emnBetween: Result :=
        Format(FloatEqModesWhere[fMode], [fFieldName, FloatToSqlStr(fValue_1), FloatToSqlStr(fValue_2)]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFFloatItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emnNull: Result := Format(FloatEqModesFilter[fMode], [fFieldName]);
      emnEqual, emnMore, emnMoreEq: Result := Format(FloatEqModesFilter[fMode], [fFieldName, fValue_1]);
      emnLess, emnLessEq: Result := Format(FloatEqModesFilter[fMode], [fFieldName, fValue_2]);
      emnBetween: Result := Format(FloatEqModesFilter[fMode], [fFieldName, fValue_1, fValue_2]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFDateItem ============================================================== }

resourcestring
  SEqualDateText        = 'равно %s г.';
  SAfterDateText        = 'с %s г. включительно';
  SBeforeDateText       = 'до %s г. включительно';
  SPeriodDateText       = 'с %s г. по %s г. включительно';
  SCurrentDayDateText   = 'текущий день: %s г.';
  SCurrentWeekDateText  = 'текущая неделя: с %s г. по %s г.';
  SCurrentMonthDateText = 'текущий месяц: с %s г. по %s г.';
  SCurrentYearDateText  = 'текущий год: с %s г. по %s г.';
  SPrevDayDateText      = 'предыдущий день: %s г.';
  SPrevWeekDateText     = 'предыдущая неделя: с %s г. по %s г.';
  SPrevMonthDateText    = 'предыдущий месяц: с %s г. по %s г.';
  SPrevYearDateText     = 'предыдущий год: с %s г. по %s г.';
  SNextDayDateText      = 'следующий день: %s г.';
  SNextWeekDateText     = 'следующая неделя: с %s г. по %s г.';
  SNextMonthDateText    = 'следующий месяц: с %s г. по %s г.';
  SNextYearDateText     = 'следующий год: с %s г. по %s г.';
  SLast3DaysDateText    = 'последние 3 дня: с %s г. по %s г.';
  SLast10DaysDateText   = 'последние 10 дней: с %s г. по %s г.';
  SLast20DaysDateText   = 'последние 20 дней: с %s г. по %s г.';
  SLast30DaysDateText   = 'последние 30 дней: с %s г. по %s г.';
  SLast60DaysDateText   = 'последние 60 дней: с %s г. по %s г.';
  SLast90DaysDateText   = 'последние 90 дней: с %s г. по %s г.';
  SLast180DaysDateText  = 'последние 180 дней: с %s г. по %s г.';
  SNext3DaysDateText    = 'следующие 3 дня: с %s г. по %s г.';
  SNext10DaysDateText   = 'следующие 10 дней: с %s г. по %s г.';
  SNext20DaysDateText   = 'следующие 20 дней: с %s г. по %s г.';
  SNext30DaysDateText   = 'следующие 30 дней: с %s г. по %s г.';
  SNext60DaysDateText   = 'следующие 60 дней: с %s г. по %s г.';
  SNext90DaysDateText   = 'следующие 90 дней: с %s г. по %s г.';
  SNext180DaysDateText  = 'следующие 180 дней: с %s г. по %s г.';
  SAfterCurrentDayText  = 'от текущего дня и позже (с %s г. включительно)';
  SBeforeCurrentDayText = 'до текущего дня и ранее (до %s г. включительно)';
  SEqualDate            = 'за указанную дату';
  SAfterDate            = 'с указанной даты';
  SBeforeDate           = 'до указанной даты';
  SPeriodDate           = 'произвольный период';
  SCurrentDayDate       = 'текущий день';
  SCurrentWeekDate      = 'текущая неделя';
  SCurrentMonthDate     = 'текущий месяц';
  SCurrentYearDate      = 'текущий год';
  SPrevDayDate          = 'предыдущий день';
  SPrevWeekDate         = 'предыдущая неделя';
  SPrevMonthDate        = 'предыдущий месяц';
  SPrevYearDate         = 'предыдущий год';
  SNextDayDate          = 'следующий день';
  SNextWeekDate         = 'следующая неделя';
  SNextMonthDate        = 'следующий месяц';
  SNextYearDate         = 'следующий год';
  SLast3DaysDate        = 'последние 3 дня';
  SLast10DaysDate       = 'последние 10 дней';
  SLast20DaysDate       = 'последние 20 дней';
  SLast30DaysDate       = 'последние 30 дней';
  SLast60DaysDate       = 'последние 60 дней';
  SLast90DaysDate       = 'последние 90 дней';
  SLast180DaysDate      = 'последние 180 дней';
  SNext3DaysDate        = 'следующие 3 дня';
  SNext10DaysDate       = 'следующие 10 дней';
  SNext20DaysDate       = 'следующие 20 дней';
  SNext30DaysDate       = 'следующие 30 дней';
  SNext60DaysDate       = 'следующие 60 дней';
  SNext90DaysDate       = 'следующие 90 дней';
  SNext180DaysDate      = 'следующие 180 дней';
  SAfterCurrentDay      = 'от текущего дня и позже';
  SBeforeCurrentDay     = 'до текущего дня и ранее';

const
  DateEqModes : array [TDateEqMode] of string =
    (SNullText, SEqualDate, SAfterDate, SBeforeDate, SPeriodDate,
     SCurrentDayDate, SAfterCurrentDay, SBeforeCurrentDay, SPrevDayDate, SNextDayDate,
     SCurrentWeekDate, SPrevWeekDate, SNextWeekDate,
     SCurrentMonthDate, SPrevMonthDate, SNextMonthDate,
     SCurrentYearDate, SPrevYearDate, SNextYearDate,
     SLast3DaysDate, SLast10DaysDate, SLast20DaysDate,
     SLast30DaysDate, SLast60DaysDate, SLast90DaysDate, SLast180DaysDate,
     SNext3DaysDate, SNext10DaysDate, SNext20DaysDate,
     SNext30DaysDate, SNext60DaysDate, SNext90DaysDate, SNext180DaysDate);
  DateEqModesText: array [TDateEqMode] of string =
    (SNullText, SEqualDateText, SAfterDateText, SBeforeDateText, SPeriodDateText,
     SCurrentDayDateText, SAfterCurrentDayText, SBeforeCurrentDayText, SPrevDayDateText, SNextDayDateText,
     SCurrentWeekDateText, SPrevWeekDateText, SNextWeekDateText,
     SCurrentMonthDateText, SPrevMonthDateText, SNextMonthDateText,
     SCurrentYearDateText, SPrevYearDateText, SNextYearDateText,
     SLast3DaysDateText, SLast10DaysDateText, SLast20DaysDateText,
     SLast30DaysDateText, SLast60DaysDateText, SLast90DaysDateText, SLast180DaysDateText,
     SNext3DaysDateText, SNext10DaysDateText, SNext20DaysDateText,
     SNext30DaysDateText, SNext60DaysDateText, SNext90DaysDateText, SNext180DaysDateText);

  DateEqModesWhere: array [TDateEqMode] of string =
    ('%0:s IS NULL',
     '(%0:s>=%1:s) and (%0:s<%2:s)',
     '%0:s>=%1:s', '%0:s<%1:s',
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Period
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // CurrentDayDate
     '%0:s>=%1:s',                        // AfterCurrentDay
     '%0:s<%1:s',                         // BeforeCurrentDay
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // PrevDayDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // NextDayDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // CurrentWeekDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // PrevWeekDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // NextWeekDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // CurrentMonthDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // PrevMonthDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // NextMonthDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // CurrentYearDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // PrevYearDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // NextYearDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last3DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last10DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last20DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last30DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last60DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last90DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Last180DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Next3DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Next10DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Next20DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Next30DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Next60DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)',      // Next90DaysDate
     '(%0:s>=%1:s) and (%0:s<%2:s)');     // Next180DaysDate
  DateEqModesFilter: array [TDateEqMode] of string =
    ('[%0:s]=NULL',
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',
     '[%0:s]>=''%1:s''', '[%0:s]<''%1:s''',
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Period
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // CurrentDayDate
     '[%0:s]>=''%1:s''',                          // AfterCurrentDay
     '[%0:s]<''%1:s''',                           // BeforeCurrentDay
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // PrevDayDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // NextDayDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // CurrentWeekDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // PrevWeekDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // NextWeekDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // CurrentMonthDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // PrevMonthDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // NextMonthDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // CurrentYearDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // PrevYearDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // NextYearDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last3DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last10DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last20DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last30DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last60DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last90DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Last180DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Next3DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Next10DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Next20DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Next30DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Next60DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')',  // Next90DaysDate
     '([%0:s]>=''%1:s'') and ([%0:s]<''%2:s'')'); // Next180DaysDate

procedure TRDFDateItem.Clear;
begin
  inherited Clear;
  fValue_1 := 0;
  fValue_2 := 0;
  fValue_1_Def := 0;
  fValue_2_Def := 0;
  fValue_1_Buf := 0;
  fValue_2_Buf := 0;
  fMode := emdLast30Days;
  fMode_Def := emdLast30Days;
  fMode_Buf := emdLast30Days;
end;

procedure TRDFDateItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFDateItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFDateItem then
    begin
      SrcItem := Source as TRDFDateItem;
      fValue_1 := SrcItem.fValue_1;
      fValue_2 := SrcItem.fValue_2;
      fValue_1_Def := SrcItem.fValue_1_Def;
      fValue_2_Def := SrcItem.fValue_2_Def;
      fValue_1_Buf := SrcItem.fValue_1_Buf;
      fValue_2_Buf := SrcItem.fValue_2_Buf;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
    end
  end;
end;

procedure TRDFDateItem.SetValue_1(const aValue: TDate);
begin
  if aValue <> fValue_1 then
  begin
    fValue_1 := aValue;
  end;
end;

procedure TRDFDateItem.SetValue_2(const aValue: TDate);
begin
  if aValue <> fValue_2 then
  begin
    fValue_2 := aValue;
  end;
end;

procedure TRDFDateItem.SetValue_1_Def(const aValue: TDate);
begin
  if aValue <> fValue_1_Def then
  begin
    fValue_1_Def := aValue;
  end;
end;

procedure TRDFDateItem.SetValue_2_Def(const aValue: TDate);
begin
  if aValue <> fValue_2_Def then
  begin
    fValue_2_Def := aValue;
  end;
end;

function TRDFDateItem.GetInitValue_1(const AMode: TDateEqMode; ADefValue: TDate): TDate;
begin
  case AMode of
    emdNull, emdEqual, emdAfter, emdBefore, emdPeriod: Result := ADefValue;
    emdCurrentDay:   Result := Date;
    emdAfterCurrentDay: Result := Date;
    emdBeforeCurrentDay: Result := Date;
    emdPrevDay:      Result := IncDay(Date, -1);
    emdNextDay:      Result := IncDay(Date, 1);
    emdCurrentWeek:  Result := DateOf(StartOfTheWeek(Date));
    emdPrevWeek:     Result := DateOf(StartOfTheWeek(IncWeek(Date, -1)));
    emdNextWeek:     Result := DateOf(StartOfTheWeek(IncWeek(Date, 1)));
    emdCurrentMonth: Result := DateOf(StartOfTheMonth(Date));
    emdPrevMonth:    Result := DateOf(StartOfTheMonth(IncMonth(Date, -1)));
    emdNextMonth:    Result := DateOf(StartOfTheMonth(IncMonth(Date, 1)));
    emdCurrentYear:  Result := DateOf(StartOfTheYear(Date));
    emdPrevYear:     Result := DateOf(StartOfTheYear(IncYear(Date, -1)));
    emdNextYear:     Result := DateOf(StartOfTheYear(IncYear(Date, 1)));
    emdLast3Days:    Result := IncDay(Date, -3);
    emdLast10Days:   Result := IncDay(Date, -10);
    emdLast20Days:   Result := IncDay(Date, -20);
    emdLast30Days:   Result := IncDay(Date, -30);
    emdLast60Days:   Result := IncDay(Date, -60);
    emdLast90Days:   Result := IncDay(Date, -90);
    emdLast180Days:  Result := IncDay(Date, -180);
    emdNext3Days:    Result := Date;
    emdNext10Days:   Result := Date;
    emdNext20Days:   Result := Date;
    emdNext30Days:   Result := Date;
    emdNext60Days:   Result := Date;
    emdNext90Days:   Result := Date;
    emdNext180Days:  Result := Date;
    else Result := ADefValue;
  end;
end;

function TRDFDateItem.GetInitValue_2(const AMode: TDateEqMode; ADefValue: TDate): TDate;
begin
  case AMode of
    emdNull, emdEqual, emdAfter, emdBefore, emdPeriod: Result := ADefValue;
    emdCurrentDay:   Result := Date;
    emdAfterCurrentDay: Result := Date;
    emdBeforeCurrentDay: Result := Date;
    emdPrevDay:      Result := IncDay(Date, -1);
    emdNextDay:      Result := IncDay(Date, 1);
    emdCurrentWeek:  Result := DateOf(EndOfTheWeek(Date));
    emdPrevWeek:     Result := DateOf(EndOfTheWeek(IncWeek(Date, -1)));
    emdNextWeek:     Result := DateOf(EndOfTheWeek(IncWeek(Date, 1)));
    emdCurrentMonth: Result := DateOf(EndOfTheMonth(Date));
    emdPrevMonth:    Result := DateOf(EndOfTheMonth(IncMonth(Date, -1)));
    emdNextMonth:    Result := DateOf(EndOfTheMonth(IncMonth(Date, 1)));
    emdCurrentYear:  Result := DateOf(EndOfTheYear(Date));
    emdPrevYear:     Result := DateOf(EndOfTheYear(IncYear(Date, -1)));
    emdNextYear:     Result := DateOf(EndOfTheYear(IncYear(Date, 1)));
    emdLast3Days:    Result := Date;
    emdLast10Days:   Result := Date;
    emdLast20Days:   Result := Date;
    emdLast30Days:   Result := Date;
    emdLast60Days:   Result := Date;
    emdLast90Days:   Result := Date;
    emdLast180Days:  Result := Date;
    emdNext3Days:    Result := IncDay(Date, 3);
    emdNext10Days:   Result := IncDay(Date, 10);
    emdNext20Days:   Result := IncDay(Date, 20);
    emdNext30Days:   Result := IncDay(Date, 30);
    emdNext60Days:   Result := IncDay(Date, 60);
    emdNext90Days:   Result := IncDay(Date, 90);
    emdNext180Days:  Result := IncDay(Date, 180);
    else Result := ADefValue;
  end;
end;

procedure TRDFDateItem.ResetItem;
begin
  inherited ResetItem;
  fMode := fMode_Def;
  SetValue_1(GetInitValue_1(fMode, fValue_1_Def));
  SetValue_2(GetInitValue_2(fMode, fValue_2_Def));
end;

procedure TRDFDateItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fMode_Buf := fMode;
  fValue_1_Buf := fValue_1;
  fValue_2_Buf := fValue_2;
end;

procedure TRDFDateItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fValue_1 := fValue_1_Buf;
  fValue_2 := fValue_2_Buf;
  fMode := fMode_Buf;
end;

procedure TRDFDateItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fMode := TDateEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
  SetValue_1(GetInitValue_1(fMode, Ini.ReadDate(Section, Format(iniValue_1, [Name]), fValue_1_Def)));
  SetValue_2(GetInitValue_2(fMode, Ini.ReadDate(Section, Format(iniValue_2, [Name]), fValue_2_Def)));
end;

procedure TRDFDateItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteDate(Section, Format(iniValue_1, [Name]), fValue_1);
  Ini.WriteDate(Section, Format(iniValue_2, [Name]), fValue_2);
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
end;

function TRDFDateItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_Date;
end;

procedure TRDFDateItem.UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
begin
  TDateTimePicker(Edit1).Date := GetInitValue_1(TDateEqMode(AMode), Item_Value_1);
  Edit1.Enabled := TDateEqMode(AMode) in [emdEqual, emdAfter, emdPeriod];
  TDateTimePicker(Edit2).Date := GetInitValue_2(TDateEqMode(AMode), Item_Value_2);
  Edit2.Enabled := TDateEqMode(AMode) in [emdBefore, emdPeriod];
end;

procedure TRDFDateItem.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TDateEqMode;
begin
  with TFormDbFilterItem_Date(Editor) do
  begin
    for i := Low(TDateEqMode) to High(TDateEqMode) do
      ModeComboBox.Items.Add(DateEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
    DateTimePicker1.Date := Item_Value_1;
    DateTimePicker2.Date := Item_Value_2;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFDateItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_Date(Editor) do
  begin
    Item_CompareMode := TDateEqMode(ModeComboBox.ItemIndex);
    Item_Value_1 := DateTimePicker1.Date;
    Item_Value_2 := DateTimePicker2.Date;
  end;
end;

function TRDFDateItem.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      emdNull:
        Result := DateEqModesText[fMode];

      emdEqual, emdAfter, emdAfterCurrentDay,
      emdCurrentDay, emdPrevDay, emdNextDay:
        Result := Format(DateEqModesText[fMode], [DateToStr(GetInitValue_1(fMode, fValue_1))]);

      emdBefore, emdBeforeCurrentDay:
        Result := Format(DateEqModesText[fMode], [DateToStr(GetInitValue_2(fMode, fValue_2))]);

      emdPeriod,
      emdCurrentWeek, emdPrevWeek, emdNextWeek,
      emdCurrentMonth, emdPrevMonth, emdNextMonth,
      emdCurrentYear, emdPrevYear, emdNextYear,
      emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days,
      emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days:
        Result := Format(DateEqModesText[fMode], [DateToStr(GetInitValue_1(fMode, fValue_1)),
                                                  DateToStr(GetInitValue_2(fMode, fValue_2))]);
      else Result := EmptyStr;
    end;
    if fInvert and (Result <> EmptyStr) then
      Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFDateItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emdNull:
        Result := Format(DateEqModesWhere[fMode], [fFieldName]);

      emdEqual:
        Result := Format(DateEqModesWhere[fMode], [fFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(GetInitValue_1(fMode, fValue_1))),
          DateToSqlStr(Filter.DateFormatWhere, DateOf(IncDay(GetInitValue_1(fMode, fValue_1), 1)))]);

      emdAfter, emdAfterCurrentDay:
        Result := Format(DateEqModesWhere[fMode], [fFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(GetInitValue_1(fMode, fValue_1)))]);

      emdBefore, emdBeforeCurrentDay:
        Result := Format(DateEqModesWhere[fMode], [fFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(IncDay(GetInitValue_2(fMode, fValue_2), 1)))]);

      emdPeriod,
      emdCurrentDay, emdCurrentWeek, emdCurrentMonth, emdCurrentYear,
      emdPrevDay, emdPrevWeek, emdPrevMonth, emdPrevYear,
      emdNextDay, emdNextWeek, emdNextMonth, emdNextYear,
      emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days,
      emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days:
        Result := Format(DateEqModesWhere[fMode], [fFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(GetInitValue_1(fMode, fValue_1))),
          DateToSqlStr(Filter.DateFormatWhere, DateOf(IncDay(GetInitValue_2(fMode, fValue_2), 1)))]);

      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFDateItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emdNull:
        Result := Format(DateEqModesFilter[fMode], [fFieldName]);

      emdEqual:
        Result := Format(DateEqModesFilter[fMode], [fFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(GetInitValue_1(fMode, fValue_1))),
          FormatDateTime(Filter.DateFormatFilter, DateOf(IncDay(GetInitValue_1(fMode, fValue_1), 1)))]);

      emdAfter, emdAfterCurrentDay:
        Result := Format(DateEqModesFilter[fMode], [fFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(GetInitValue_1(fMode, fValue_1)))]);

      emdBefore, emdBeforeCurrentDay:
        Result := Format(DateEqModesFilter[fMode], [fFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(IncDay(GetInitValue_2(fMode, fValue_2), 1)))]);

      emdPeriod,
      emdCurrentDay, emdCurrentWeek, emdCurrentMonth, emdCurrentYear,
      emdPrevDay, emdPrevWeek, emdPrevMonth, emdPrevYear,
      emdNextDay, emdNextWeek, emdNextMonth, emdNextYear,
      emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days,
      emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days:
        Result := Format(DateEqModesFilter[fMode], [fFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(GetInitValue_1(fMode, fValue_1))),
          FormatDateTime(Filter.DateFormatFilter, DateOf(IncDay(GetInitValue_2(fMode, fValue_2), 1)))]);

      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFStringItem ============================================================ }

resourcestring
  SEqualStr      = 'точное соответствие';
  SLikeStr       = 'содержит введенные символы';
  SBeginStr      = 'начинается с строки символов';
  SEqualStrText  = 'равно "%s"';
  SLikeStrText   = 'содержит "%s"';
  SBeginStrText  = 'начинается с "%s"';

const
  StringEqModes : array [TStringEqMode] of string =
    (SNullText, SEqualStr, SLikeStr, SBeginStr);
  StringEqModesText: array [TStringEqMode] of string =
    (SNullText, SEqualStrText, SLikeStrText, SBeginStrText);
  StringEqModesWhere: array [TStringEqMode] of string =
    ('%s IS NULL', '%s=''%s''',
     '%s LIKE ''%%%s%%''', '%s LIKE ''%s%%''');
  StringEqModesFilter: array [TStringEqMode] of string =
    ('[%s]=NULL', '[%s]=''%s''',
     '[%s]=''%s*''', '[%s]=''%s*''');

constructor TRDFStringItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStrings := TStringList.Create;
end;

destructor TRDFStringItem.Destroy;
begin
  fStrings.Clear;
  fStrings.Free;
  inherited Destroy;
end;

procedure TRDFStringItem.Clear;
begin
  inherited Clear;
  fValue := EmptyStr;
  fValue_Def := EmptyStr;
  fValue_Buf := EmptyStr;
  fMode := emsLike;
  fMode_Def := emsLike;
  fMode_Buf := emsLike;
  fHistory := True;
  FCase := False;
  FCase_Def := False;
  FCase_Buf := False;
end;

procedure TRDFStringItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFStringItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFStringItem then
    begin
      SrcItem := Source as TRDFStringItem;
      fValue := SrcItem.fValue;
      fValue_Def := SrcItem.fValue_Def;
      fValue_Buf := SrcItem.fValue_Buf;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
      FCase := SrcItem.FCase;
      FCase_Def := SrcItem.FCase_Def;
      FCase_Buf := SrcItem.FCase_Buf;
      fStrings.Assign(SrcItem.fStrings);
      fHistory := SrcItem.fHistory;
    end
  end;
end;

procedure TRDFStringItem.SetStrings(aValue: TStrings);
begin
  if Assigned(aValue) then fStrings.Assign(aValue);
end;

procedure TRDFStringItem.ResetItem;
begin
  inherited ResetItem;
  fMode := fMode_Def;
  fValue := fValue_Def;
  FCase := FCase_Def;
end;

procedure TRDFStringItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fMode_Buf := fMode;
  fValue_Buf := fValue;
  FCase_Buf := FCase;
end;

procedure TRDFStringItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fMode := fMode_Buf;
  fValue := fValue_Buf;
  FCase := FCase_Buf;
end;

procedure TRDFStringItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fMode := TStringEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
  fValue := Ini.ReadString(Section, Format(iniValue_1, [Name]), fValue_Def);
  FCase := Ini.ReadBool(Section, Format(iniCase, [Name]), FCase_Def);
end;

procedure TRDFStringItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
  Ini.WriteString(Section, Format(iniValue_1, [Name]), fValue);
  Ini.WriteBool(Section, Format(iniCase, [Name]), FCase);
end;

function TRDFStringItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled and (fValue <> EmptyStr);
end;

function TRDFStringItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_String;
end;

procedure TRDFStringItem.UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
begin
  TComboBox(Edit1).Text := Item_Value;
  Edit1.Enabled := TStringEqMode(AMode) in [emsEqual, emsLike, emsBegin];
  Edit2.Enabled := (TStringEqMode(AMode) in [emsEqual, emsLike, emsBegin])
               and CaseStringsEnabled and Filter.CaseStringsEnabled;
end;

procedure TRDFStringItem.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TStringEqMode;
begin
  with TFormDbFilterItem_String(Editor) do
  begin
    for i := Low(TStringEqMode) to High(TStringEqMode) do
      ModeComboBox.Items.Add(StringEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
    Edit.Items.Assign(fStrings);
    Edit.Text := Item_Value;
    CaseCheckBox.Enabled := False;
    CaseCheckBox.Checked := Item_CaseSensitive or not CaseStringsEnabled or not Filter.CaseStringsEnabled;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFStringItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_String(Editor) do
  begin
    Item_CompareMode := TStringEqMode(ModeComboBox.ItemIndex);
    Item_Value := Edit.Text;
    Item_CaseSensitive := CaseCheckBox.Checked;
    if fHistory and (Trim(Item_Value) <> EmptyStr)
    and (fStrings.IndexOf(Item_Value) < 0) then
    fStrings.Add(Item_Value);
  end;
end;

function TRDFStringItem.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      emsNull:
        Result := StringEqModesText[fMode];
      emsEqual, emsLike, emsBegin:
        Result := Format(StringEqModesText[fMode], [fValue]);
      else Result := EmptyStr;
    end;
    if fInvert and (Result <> EmptyStr) then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFStringItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emsNull:
        Result := Format(StringEqModesWhere[fMode], [fFieldName]);
      emsEqual, emsLike, emsBegin:
        if FCase or not fCaseEnabled or not Filter.fCaseEnabled
        then Result := Format(StringEqModesWhere[fMode], [fFieldName, fValue])
        else Result := Format(StringEqModesWhere[fMode], [Format(UpperCaseItem, [fFieldName]), AnsiUpperCase(fValue)]);
      {
      emsLike, emsBegin:
        if FCase or not fCaseEnabled or not Filter.fCaseEnabled
        then Result := Format(StringEqModesWhere[fMode], [fFieldName, StrToLikeStr(fValue)])
        else Result := Format(StringEqModesWhere[fMode], [Format(UpperCaseItem, [fFieldName]), AnsiUpperCase(StrToLikeStr(fValue))]);
      }
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFStringItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emsNull:
        Result := Format(StringEqModesFilter[fMode], [fFieldName]);
      emsEqual, emsLike, emsBegin:
        Result := Format(StringEqModesFilter[fMode], [fFieldName, fValue]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

function TRDFStringItem.GetDbFilterOptions: TFilterOptions;
begin
  Result := [];
  if not FCase then Result := Result + [foCaseInsensitive];
  if fMode = emsEqual then Result := Result + [foNoPartialCompare];
end;

{ == TRDFTextItem ============================================================ }

const
  TextEqModes : array [TTextEqMode] of string =
    (SNullText, SLikeStr, SBeginStr);
  TextEqModesText: array [TTextEqMode] of string =
    (SNullText, SLikeStrText, SBeginStrText);
  TextEqModesWhere: array [TTextEqMode] of string =
    ('%s IS NULL', '%s like ''%%%s%%''', '%s like ''%s%%''');
  TextEqModesFilter: array [TTextEqMode] of string =
    ('[%s]=NULL', '[%s]=''%s*''', '[%s]=''%s*''');

constructor TRDFTextItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStrings := TStringList.Create;
end;

destructor TRDFTextItem.Destroy;
begin
  fStrings.Clear;
  fStrings.Free;
  inherited Destroy;
end;

procedure TRDFTextItem.Clear;
begin
  inherited Clear;
  fValue := EmptyStr;
  fValue_Def := EmptyStr;
  fValue_Buf := EmptyStr;
  fMode := emtLike;
  fMode_Def := emtLike;
  fMode_Buf := emtLike;
  fHistory := True;
end;

procedure TRDFTextItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFTextItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFTextItem then
    begin
      SrcItem := Source as TRDFTextItem;
      fValue := SrcItem.fValue;
      fValue_Def := SrcItem.fValue_Def;
      fValue_Buf := SrcItem.fValue_Buf;
      fMode := SrcItem.fMode;
      fMode_Def := SrcItem.fMode_Def;
      fMode_Buf := SrcItem.fMode_Buf;
      fStrings.Assign(SrcItem.fStrings);
      fHistory := SrcItem.fHistory;
    end
  end;
end;

procedure TRDFTextItem.SetStrings(aValue: TStrings);
begin
  if Assigned(aValue) then fStrings.Assign(aValue);
end;

procedure TRDFTextItem.ResetItem;
begin
  inherited ResetItem;
  fMode := fMode_Def;
  fValue := fValue_Def;
end;

procedure TRDFTextItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fMode_Buf := fMode;
  fValue_Buf := fValue;
end;

procedure TRDFTextItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fMode := fMode_Buf;
  fValue := fValue_Buf;
end;

procedure TRDFTextItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fMode := TTextEqMode(Ini.ReadInteger(Section, Format(iniMode, [Name]), Integer(fMode_Def)));
  fValue := Ini.ReadString(Section, Format(iniValue_1, [Name]), fValue_Def);
end;

procedure TRDFTextItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteInteger(Section, Format(iniMode, [Name]), Integer(fMode));
  Ini.WriteString(Section, Format(iniValue_1, [Name]), fValue);
end;

function TRDFTextItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled and (fValue <> EmptyStr);
end;

function TRDFTextItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_String;
end;

procedure TRDFTextItem.UpdateEdits(Edit1, Edit2: TControl; const AMode: Integer);
begin
  TComboBox(Edit1).Text := Item_Value;
  Edit1.Enabled := TStringEqMode(AMode) in [emsEqual, emsLike, emsBegin];
  Edit2.Enabled := (TStringEqMode(AMode) in [emsEqual, emsLike, emsBegin])
               and CaseStringsEnabled and Filter.CaseStringsEnabled;
end;

procedure TRDFTextItem.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TTextEqMode;
begin
  with TFormDbFilterItem_String(Editor) do
  begin
    for i := Low(TTextEqMode) to High(TTextEqMode) do
      ModeComboBox.Items.Add(TextEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_CompareMode);
    Edit.Items.Assign(fStrings);
    Edit.Text := Item_Value;
    CaseCheckBox.Enabled := False;
    CaseCheckBox.Checked := True;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFTextItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_String(Editor) do
  begin
    Item_CompareMode := TTextEqMode(ModeComboBox.ItemIndex);
    Item_Value := Edit.Text;
    if fHistory and (Trim(Item_Value) <> EmptyStr)
    and (fStrings.IndexOf(Item_Value) < 0) then
    fStrings.Add(Item_Value);
  end;
end;

function TRDFTextItem.GetItemText: string;
begin
  if Item_Active then
  begin
    case fMode of
      emtNull:
        Result := TextEqModesText[fMode];
      emtLike, emtBegin:
        Result := Format(TextEqModesText[fMode], [fValue]);
      else Result := EmptyStr;
    end;
    if fInvert and (Result <> EmptyStr) then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFTextItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emtNull:
        Result := Format(TextEqModesWhere[fMode], [fFieldName]);
      emtLike, emtBegin:
        Result := Format(TextEqModesWhere[fMode], [fFieldName, fValue]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFTextItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    case fMode of
      emtNull:
        Result := Format(TextEqModesFilter[fMode], [fFieldName]);
      emtLike, emtBegin:
        Result := Format(TextEqModesFilter[fMode], [fFieldName, fValue]);
      else Result := EmptyStr;
    end;
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

function TRDFTextItem.GetDbFilterOptions: TFilterOptions;
begin
  Result := [foCaseInsensitive];
end;

{ == TRDFStringListItem ======================================================== }

resourcestring
  fKeyName           = 'Код';
  fTextName          = 'Значение';
  SKeyStrText        = 'список ID: %s';
  SLoadList          = 'Загрузка записей...';
  SReadList          = 'Генерация списка значений...';
  EBadDelimiter      = 'Не определено или некорректное значение свойства KeysDelimiter!';

const
  LookupEqModesFilter: array [0..2] of string =
    ('[%s]=NULL', '[%s]=%d', '[%s]=%s');
  LookupEqModesWhere: array [0..2] of string =
    ('%s IS NULL', '%s=%d', '%s IN (%s)');

constructor TRDFStringListItem.Create(AOwner: TComponent);
begin
  inherited;
  fStartIndex := 0;
  fStrings := TStringList.Create;
  SetLength(fValues, 0);
  fOnGetValue := nil;
end;

destructor TRDFStringListItem.Destroy;
begin
  SetLength(fValues, 0);
  fStrings.Clear;
  fStrings.Free;
  fOnGetValue := nil;
  inherited;
end;

procedure TRDFStringListItem.Clear;
begin
  inherited Clear;
  fKeyValues := EmptyStr;
  fKeyValues_Buf := EmptyStr;
  fKeyValues_Def := EmptyStr;
  fDelimiter := ',';
  fTextDelimiter := '; ';
  fMaxLookupItems := 10;
end;

procedure TRDFStringListItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFStringListItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFStringListItem then
    begin
      SrcItem := Source as TRDFStringListItem;
      fKeyValues := SrcItem.fKeyValues;
      fKeyValues_Buf := SrcItem.fKeyValues_Buf;
      fKeyValues_Def := SrcItem.fKeyValues_Def;
      fDelimiter := SrcItem.fDelimiter;
      fTextDelimiter := SrcItem.fTextDelimiter;
      FMaxLookupItems := SrcItem.FMaxLookupItems;
      fStrings.Assign(SrcItem.fStrings);
      UpdateValues;
    end
  end;
end;

procedure TRDFStringListItem.UpdateValues;
var
  i, Value: Integer;
begin
  SetLength(fValues, fStrings.Count);
  for i := 0 to fStrings.Count - 1 do
  begin
    Value := fStartIndex+i;
    if Assigned(fOnGetValue) then
      fOnGetValue(Self, i, fStrings[i], Value);
    fValues[i] := Value;
  end;
end;

procedure TRDFStringListItem.SetStrings(aValue: TStrings);
begin
  if Assigned(aValue) then fStrings.Assign(aValue);
  UpdateValues;
end;

procedure TRDFStringListItem.Loaded;
begin
  inherited Loaded;
  if Assigned(fOnGetStrings) then fOnGetStrings(Self);
end;

function TRDFStringListItem.GetDelimiter: string;
begin
  Result := fDelimiter;
end;

procedure TRDFStringListItem.SetDelimiter(const aValue: string);
begin
  fDelimiter := Copy(aValue, 1, 2);
end;

function TRDFStringListItem.DelimiterChar: Char;
begin
  if fDelimiter = '' then raise ERDbFilterError.Create(EBadDelimiter);
  Result := Trim(fDelimiter)[1];
end;

function TRDFStringListItem.CheckKeysList(const KeysList: string): string;
var
  i: Integer;
  Dlm: Char;
  Key: string;

  function CheckKey(const Key: string): Boolean;
  var
    i, KeyInt: Integer;
  begin
    Result := False;
    KeyInt := StrToIntDef(Key, -1);
    if KeyInt > -1 then
      for i := Low(fValues) to High(fValues) do
      begin
        if fValues[i] = KeyInt then
        begin
          Result := True;
          Break;
        end;
      end;
  end;

begin
  // InfoBox(fStrings.CommaText);
  Result := EmptyStr;
  Dlm := DelimiterChar;
  if not (csDesigning in ComponentState) then
  begin
    StartWait;
    try
      UpdateValues;
      for i := 1 to WordCount(KeysList, [Dlm]) do
      begin
        Key := Trim(ExtractWord(i, KeysList, [Dlm]));
        if CheckKey(Key) then
        begin
          if Result = EmptyStr then Result := Key
          else Result := Result + fDelimiter + Key;
        end;
        Application.ProcessMessages;
      end;
    finally
      StopWait;
    end;
  end
  else Result := KeysList;
end;

function TRDFStringListItem.GenerateKeysList: string;
var
  i: Integer;
begin
  // InfoBox(fStrings.CommaText);
  Result := EmptyStr;
  StartWait;
  try
    UpdateValues;
    for i := Low(fValues) to High(fValues) do
      if Result = EmptyStr then Result := IntToStr(fValues[i])
      else Result := Result + fDelimiter + IntToStr(fValues[i]);
  finally
    StopWait;
  end;
end;

function TRDFStringListItem.GenerateLookupList(const KeysList: string): string;
var
  i, N, Key: Integer;
  Dlm: Char;
begin
  Result := EmptyStr;
  if fDelimiter = '' then
  begin
    Result := EBadDelimiter;
    raise ERDbFilterError.Create(EBadDelimiter);
  end;
  Dlm := Trim(fDelimiter)[1];
  N := WordCount(KeysList, [Dlm]);
  if N > FMaxLookupItems then
    Result := Format(SKeyStrText, [KeysList])
  else begin
    StartWait;
    try
      UpdateValues;
      if KeysList <> EmptyStr then
      begin
        for i := 1 to WordCount(KeysList, [Dlm]) do
        begin
          Key := StrToIntDef(Trim(ExtractWord(i, KeysList, [Dlm])), -1);
          for N := Low(fValues) to High(fValues) do
            if Key = fValues[N] then
            begin
              if Result = EmptyStr then Result := fStrings[N]
              else Result := Result + fTextDelimiter + fStrings[N];
            end;
        end;
      end
      else Result := SNullText;
    finally
      StopWait;
    end;
  end;
end;

procedure TRDFStringListItem.ResetItem;
begin
  inherited ResetItem;
  if fKeyValues_Def <> EmptyStr
  then fKeyValues := CheckKeysList(fKeyValues_Def)
  else fKeyValues := GenerateKeysList;
end;

procedure TRDFStringListItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fKeyValues_Buf := fKeyValues;
end;

procedure TRDFStringListItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fKeyValues := fKeyValues_Buf;
end;

procedure TRDFStringListItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  if fKeyValues_Def <> EmptyStr
  then fKeyValues := CheckKeysList(fKeyValues_Def)
  else fKeyValues := GenerateKeysList;
  if Ini.ValueExists(Section, Format(iniValue_1, [Name])) then
    fKeyValues := CheckKeysList(Ini.ReadString(Section, Format(iniValue_1, [Name]), fKeyValues));
end;

procedure TRDFStringListItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteString(Section, Format(iniValue_1, [Name]), fKeyValues);
end;

function TRDFStringListItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled
    and (fKeyValues <> EmptyStr);
end;

function TRDFStringListItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_LinkList;
end;

procedure TRDFStringListItem.ListView_CreateColumns(LV: TListView);
begin
  LV.Columns.BeginUpdate;
  try
    LV.Columns.Clear;
    with LV.Columns.Add do
      Caption := fKeyName;
    with LV.Columns.Add do
    begin
      Caption := fTextName;
      Autosize := True;
    end;
  finally
    LV.Columns.EndUpdate;
  end;
end;

procedure TRDFStringListItem.ListView_LoadTable(LV: TListView);
var
  Dlm: Char;
  i: Integer;
  Id: ^Integer;
  SP: Boolean;

  function KeyInList(const Key: Integer; List: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to WordCount(List, [Dlm]) do
    begin
      Result := IntToStr(Key) = Trim(ExtractWord(i, List, [Dlm]));
      if Result then Break;
    end;
  end;

begin
  Dlm := DelimiterChar;
  LV.Items.BeginUpdate;
  SP := fStrings.Count > MinProgressCount;
  if SP then ShowProgress(SLoadList, fStrings.Count);
  try
    UpdateValues;
    LV.Items.Clear;
    for i := 0 to fStrings.Count - 1 do
    begin
      with LV.Items.Add do
      begin
        New(Id);
        Id^ := fValues[i];
        Data := Id;
        Caption := IntToStr(fValues[i]);
        Subitems.Add(fStrings[i]);
        Checked := KeyInList(fValues[i], fKeyValues);
      end;
      if SP then UpdateProgressStep(1);
    end;
  finally
    if SP then CloseProgress;
    LV.Items.EndUpdate;
  end;
end;

procedure TRDFStringListItem.ListView_ReadTable(LV: TListView);
var
  i: Integer;
  Id: ^Integer;
  SP: Boolean;
begin
  fKeyValues := EmptyStr;
  SP := LV.Items.Count > MinProgressCount;
  if SP then ShowProgress(SReadList, LV.Items.Count);
  try
    for i := 0 to LV.Items.Count - 1 do
    begin
      if LV.Items[i].Checked then
      begin
        Id := LV.Items[i].Data;
        if fKeyValues = EmptyStr then fKeyValues := IntToStr(Id^)
        else fKeyValues := fKeyValues + fDelimiter + IntToStr(Id^);
      end;
      if SP then UpdateProgressStep(1);
    end;
  finally
    if SP then CloseProgress;
  end;
end;

procedure TRDFStringListItem.LoadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkList(Editor) do
  begin
    ListView_CreateColumns(ListView);
    ListView_LoadTable(ListView);
  end;
end;

procedure TRDFStringListItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkList(Editor) do
  begin
    ListView_ReadTable(ListView);
  end;
end;

function TRDFStringListItem.GetItemText: string;
begin
  if Item_Active then
  begin
    Result := GenerateLookupList(fKeyValues);
    if fInvert then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFStringListItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    if fKeyValues <> EmptyStr
    then Result := Format(LookupEqModesWhere[2], [FieldName, fKeyValues])
    else Result := Format(LookupEqModesWhere[0], [FieldName]);
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFStringListItem.GetFilterString: string;
var
  i, KeyCnt: Integer;
  Dlm: Char;
  Key, KeyItem: string;
begin
  Result := EmptyStr;
  if Item_Active then
  begin
    if fKeyValues <> EmptyStr then
    begin
      Dlm := DelimiterChar;
      KeyCnt := WordCount(fKeyValues, [Dlm]);
      for i := 1 to KeyCnt do
      begin
        Key := Trim(ExtractWord(i, fKeyValues, [Dlm]));
        KeyItem := Format(LookupEqModesFilter[2], [FieldName, Key]);
        if KeyCnt > 1 then KeyItem := Format(Brackets, [KeyItem]);
        if Result = EmptyStr then Result := KeyItem
        else Result := Result + LogicalOperationsSql[loOr] + KeyItem;
      end;
    end
    else Result := Format(LookupEqModesFilter[0], [FieldName]);
  end;
  DoGetFilterString(Result);
end;

{ == TRDFLinkItem ============================================================== }

resourcestring
  ELookupDataSetNotDefine = 'ОШИБКА! Не определено свойство LookupDataSet!';
  ELookupDataSetNotActive = 'ОШИБКА! Нет доступа к LookupDataSet!';
  EKeyFieldNotDefine      = 'ОШИБКА! Не определено свойство KeyField!';
  ELookupFieldNotDefine   = 'ОШИБКА! Не определено свойство LookupFieldName[%d]!';
  ELookupFieldNotFound    = 'ОШИБКА! Поле "%s" в наборе данных не найдено!';
  EKeyValueNotFound       = 'ОШИБКА! Запись с %s=%d не найдена!';
  EKeyValueNotFoundStr    = 'ОШИБКА! Запись с %s=%s не найдена!';

const
  FieldDelims = [';'];

  LookupEqModesSubWhere: array [0..3] of string =
    ('%s IN (SELECT %s FROM %s WHERE %s IS NULL)',
     '%s IN (SELECT %s FROM %s WHERE %s=%d)',
     '%s IN (SELECT %s FROM %s WHERE %s IN (%s))',
     '%s IN (SELECT %s FROM %s WHERE %s)');

constructor TRDFLinkItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLinkDataSet := nil;
  fKeyField := EmptyStr;
  fLookupFields := EmptyStr;
  fLookupFieldIndex := 0;
  fLookupCaptions := EmptyStr;
  fLookupDisabled := True;
  fKeyNull := -1;
  fUseNull := True;
  // --- Только для SQL !!! -------
  fSubQueryLink := False;
  fSubQueryTableName := EmptyStr;
  fSubQueryKey := EmptyStr;
  fSubQueryListKey := EmptyStr;
  // ------------------------------
end;

destructor TRDFLinkItem.Destroy;
begin
  fLinkDataSet := nil;
  inherited Destroy;
end;

procedure TRDFLinkItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFLinkItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFLinkItem then
    begin
      SrcItem := Source as TRDFLinkItem;
      LookupDataSet := SrcItem.fLinkDataSet;
      fKeyField := SrcItem.fKeyField;
      fLookupFields := SrcItem.fLookupFields;
      fLookupFieldIndex := SrcItem.fLookupFieldIndex;
      fLookupCaptions := SrcItem.fLookupCaptions;
      fLookupDisabled := SrcItem.fLookupDisabled;
      fKeyNull := SrcItem.fKeyNull;
      fUseNull := SrcItem.fUseNull;
    end;
  end;
end;

procedure TRDFLinkItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (LookupDataSet <> nil) and (AComponent = LookupDataSet) then
    LookupDataSet := nil;
end;

procedure TRDFLinkItem.SetDataSet(aValue: TDataSet);
begin
  if fLinkDataSet <> aValue then
  begin
    fLinkDataSet := aValue;
    if fLinkDataSet <> nil then
      aValue.FreeNotification(Self)
    else begin
      if not (csLoading in ComponentState) then
      begin
        fKeyField := EmptyStr;
        fLookupFields := EmptyStr;
        fLookupFieldIndex := 0;
        fLookupCaptions := EmptyStr;
      end;
    end;
  end;
end;

procedure TRDFLinkItem.SetKeyField(const aValue: string);
begin
  if fKeyField <> aValue then
  begin
    if (aValue <> EmptyStr) and (fLinkDataSet <> nil) and (fLinkDataSet.FindField(aValue) = nil) then
      raise ERDbFilterError.CreateFmt(EFieldNotFound, [fLinkDataSet.Name, aValue]);
    fKeyField := aValue;
  end;
end;

procedure TRDFLinkItem.SetLookupFields(const aValue: string);
var
  i: Integer;
begin
  if fLookupFields <> aValue then
  begin
    if (aValue <> EmptyStr) and (fLinkDataSet <> nil) then
    begin
      for i := 1 to WordCount(aValue, FieldDelims) do
        if fLinkDataSet.FindField(Trim(ExtractWord(i, aValue, FieldDelims))) = nil then
          raise ERDbFilterError.CreateFmt(EFieldNotFound, [fLinkDataSet.Name,
            Trim(ExtractWord(i, aValue, FieldDelims))]);
    end;
    fLookupFields := aValue;
    if GetLookupFieldsCount >= fLookupFieldIndex then
      fLookupFieldIndex := 0;
  end;
end;

function TRDFLinkItem.DataSetActive: Boolean;
begin
  Result := (fLinkDataSet <> nil) and fLinkDataSet.Active;
end;

function TRDFLinkItem.GetLookupFieldsCount_Internal: Integer;
begin
  if Trim(fLookupFields) <> EmptyStr
  then Result := WordCount(fLookupFields, FieldDelims)
  else Result := 0;
end;

function TRDFLinkItem.GetLookupCaptionsCount_Internal: Integer;
begin
  if Trim(fLookupCaptions) <> EmptyStr
  then Result := WordCount(fLookupCaptions, FieldDelims)
  else Result := 0;
end;

function TRDFLinkItem.GetLookupFieldsCount(const CheckCaptions: Boolean = False): Integer;
var
  N1, N2: Integer;
begin
  if CheckCaptions then
  begin
    N1 := GetLookupFieldsCount_Internal;
    N2 := GetLookupCaptionsCount_Internal;
    if N1 <= N2 then Result := N1 else Result := N2;
  end
  else Result := GetLookupFieldsCount_Internal;
end;

function TRDFLinkItem.GetLookupFieldName(const Index: Integer): string;
begin
  if (Index >= 0) and (Index < GetLookupFieldsCount_Internal)
  then Result := Trim(ExtractWord(Index + 1, fLookupFields, FieldDelims))
  else Result := EmptyStr;
end;

function TRDFLinkItem.GetLookupFieldCaption(const Index: Integer): string;
begin
  if (Index >= 0) and (Index < GetLookupCaptionsCount_Internal)
  then Result := Trim(ExtractWord(Index + 1, fLookupCaptions, FieldDelims))
  else Result := EmptyStr;
end;

function TRDFLinkItem.GetKeyField: TField;
begin
  if (fLinkDataSet <> nil) and (fKeyField <> EmptyStr)
  then Result := fLinkDataSet.FindField(fKeyField)
  else Result := nil;
end;

function TRDFLinkItem.GetLookupField(const Index: Integer): TField;
begin
  if (fLinkDataSet <> nil) and (GetLookupFieldName(Index) <> EmptyStr)
  then Result := fLinkDataSet.FindField(GetLookupFieldName(Index))
  else Result := nil;
end;

function TRDFLinkItem.KeyFieldExist: Boolean;
begin
  Result := GetKeyField <> nil;
end;

function TRDFLinkItem.LookupFieldExist(const Index: Integer): Boolean;
begin
  Result := GetLookupField(Index) <> nil;
end;

function TRDFLinkItem.KeyFieldActive: Boolean;
begin
  Result := DataSetActive and KeyFieldExist;
end;

function TRDFLinkItem.LookupFieldActive(const Index: Integer): Boolean;
begin
  Result := DataSetActive and LookupFieldExist(Index);
end;

function TRDFLinkItem.KeyFieldLocate(const aValue: Integer): Boolean;
begin
  if fLookupDisabled then fLinkDataSet.DisableControls;
  try
    Result := KeyFieldActive and fLinkDataSet.Locate(fKeyField, aValue, []);
  finally
    if fLookupDisabled then fLinkDataSet.EnableControls;
  end;
end;

function TRDFLinkItem.LookupFieldLocate(const aValue: string; Index: Integer): Boolean;
begin
  if fLookupDisabled then fLinkDataSet.DisableControls;
  try
    Result := LookupFieldActive(Index) and fLinkDataSet.Locate(GetLookupFieldName(Index), aValue, []);
  finally
    if fLookupDisabled then fLinkDataSet.EnableControls;
  end;
end;

function TRDFLinkItem.GetLookupText(const Index, Value: Integer; out Text: string): Boolean;
begin
  Result := False;
  if not fUseNull or (Value <> fKeyNull) then
  begin
    if fLinkDataSet <> nil then
    begin
      if fLinkDataSet.Active then
      begin
        if fKeyField <> EmptyStr then
        begin
          if fLinkDataSet.FindField(fKeyField) <> nil then
          begin
            if GetLookupFieldName(Index) <> EmptyStr then
            begin
              if GetLookupField(Index) <> nil then
              begin
                if fLookupDisabled then fLinkDataSet.DisableControls;
                try
                  if fLinkDataSet.Locate(fKeyField, Value, []) then
                  begin
                    Text := GetLookupField(Index).AsString;
                    Result := True;
                  end
                  else Text := Format(EKeyValueNotFound, [fKeyField, Value]);
                finally
                  if fLookupDisabled then fLinkDataSet.EnableControls;
                end;
              end
              else Text := Format(ELookupFieldNotFound, [GetLookupFieldName(Index)]);
            end
            else Text := Format(ELookupFieldNotDefine, [Index]);
          end
          else Text := Format(ELookupFieldNotFound, [GetLookupFieldName(Index)]);
        end
        else Text := EKeyFieldNotDefine;
      end
      else Text := ELookupDataSetNotActive;
    end
    else Text := ELookupDataSetNotDefine;
  end
  else begin
    Text := SNullText;
    Result := True;
  end;
end;

procedure TRDFLinkItem.PrepareDialog;
begin
  if fLinkDataSet = nil then
    raise ERDbFilterError.Create(ELookupDataSetNotDefine);
  if not fLinkDataSet.Active then
    raise ERDbFilterError.Create(ELookupDataSetNotActive);
end;

function TRDFLinkItem.ShowDialog(AOwner: TComponent; const ALeft, ATop: Integer): Boolean;
begin
  PrepareDialog;
  Result := inherited ShowDialog(AOwner, ALeft, ATop);
end;

{ == TRDFComboLinkItem ========================================================= }

procedure TRDFComboLinkItem.Clear;
begin
  inherited Clear;
  fKeyValue := fKeyNull;
  fKeyValue_Buf := fKeyNull;
  fKeyValue_Def := fKeyNull;
  fLookupValue_Def := EmptyStr;
end;

procedure TRDFComboLinkItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFComboLinkItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFComboLinkItem then
    begin
      SrcItem := Source as TRDFComboLinkItem;
      fKeyValue := SrcItem.fKeyValue;
      fKeyValue_Buf := SrcItem.fKeyValue_Buf;
      fKeyValue_Def := SrcItem.fKeyValue_Def;
      fLookupValue_Def := SrcItem.fLookupValue_Def;
    end;
  end;
end;

procedure TRDFComboLinkItem.ResetItem;
begin
  inherited ResetItem;
  if KeyFieldLocate(fKeyValue_Def) then
    fKeyValue := fKeyValue_Def
  else
    if LookupFieldLocate(fLookupValue_Def, fLookupFieldIndex)
    then fKeyValue := GetKeyField.AsInteger
    else fKeyValue := fKeyNull;
end;

procedure TRDFComboLinkItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fKeyValue_Buf := fKeyValue;
end;

procedure TRDFComboLinkItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fKeyValue := fKeyValue_Buf;
end;

procedure TRDFComboLinkItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fKeyValue := Ini.ReadInteger(Section, Format(iniValue_1, [Name]), fKeyValue_Def);
end;

procedure TRDFComboLinkItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteInteger(Section, Format(iniValue_1, [Name]), fKeyValue);
end;

function TRDFComboLinkItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled
    and (fLinkDataSet <> nil) and (GetKeyField <> nil)
    and (GetLookupField(fLookupFieldIndex) <> nil)
    and fLinkDataSet.Active;
end;

function TRDFComboLinkItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_LinkCombo;
end;

procedure TRDFComboLinkItem.LoadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkCombo(Editor) do
  begin
    DataSource.DataSet := fLinkDataSet;
    ClearButton.Visible := fUseNull;
    if fUseNull
    then DBLookupComboBox.Width := 289
    else DBLookupComboBox.Width := 312;
    DBLookupComboBox.KeyField := fKeyField;
    DBLookupComboBox.ListFieldIndex := fLookupFieldIndex;
    DBLookupComboBox.ListField := fLookupFields;
    DBLookupComboBox.Hint := fLookupCaptions;
    if fUseNull and (Item_KeyValue = fKeyNull)
    then DBLookupComboBox.KeyValue := Null
    else begin
      try
        DBLookupComboBox.KeyValue := Item_KeyValue;
      except
        DBLookupComboBox.KeyValue := Null;
      end;
    end;
  end;
end;

procedure TRDFComboLinkItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkCombo(Editor) do
  begin
    if fUseNull and (DBLookupComboBox.KeyValue = Null)
    then Item_KeyValue := fKeyNull
    else Item_KeyValue := DBLookupComboBox.KeyValue;
  end;
end;

function TRDFComboLinkItem.GetItemText: string;
begin
  if Item_Active then
  begin
    if GetLookupText(fLookupFieldIndex, fKeyValue, Result) and fInvert
    then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFComboLinkItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    if fSubQueryLink then begin
      if fUseNull and (fKeyValue = fKeyNull)
      then Result := Format(LookupEqModesSubWhere[0],
        [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey])
      else Result := Format(LookupEqModesSubWhere[1],
        [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey, fKeyValue])
    end
    else begin
      if fUseNull and (fKeyValue = fKeyNull)
      then Result := Format(LookupEqModesWhere[0], [FieldName])
      else Result := Format(LookupEqModesWhere[1], [FieldName, fKeyValue])
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFComboLinkItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    if fUseNull and (fKeyValue = fKeyNull)
    then Result := Format(LookupEqModesFilter[0], [FieldName])
    else Result := Format(LookupEqModesFilter[1], [FieldName, fKeyValue])
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFComboDateItem ========================================================= }

procedure TRDFComboDateItem.Clear;
begin
  inherited Clear;
  // KeyValue
  fKeyValue := fKeyNull;
  fKeyValue_Buf := fKeyNull;
  fKeyValue_Def := fKeyNull;
  fLookupValue_Def := EmptyStr;
  // DateValues
  fDateValue_1 := 0;
  fDateValue_2 := 0;
  fDateValue_1_Def := 0;
  fDateValue_2_Def := 0;
  fDateValue_1_Buf := 0;
  fDateValue_2_Buf := 0;
  fDateMode := emdLast30Days;
  fDateMode_Def := emdLast30Days;
  fDateMode_Buf := emdLast30Days;
end;

procedure TRDFComboDateItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFComboDateItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFComboDateItem then
    begin
      SrcItem := Source as TRDFComboDateItem;
      // KeyValue
      fKeyValue := SrcItem.fKeyValue;
      fKeyValue_Buf := SrcItem.fKeyValue_Buf;
      fKeyValue_Def := SrcItem.fKeyValue_Def;
      fLookupValue_Def := SrcItem.fLookupValue_Def;
      // DateValues
      fDateValue_1 := SrcItem.fDateValue_1;
      fDateValue_2 := SrcItem.fDateValue_2;
      fDateValue_1_Def := SrcItem.fDateValue_1_Def;
      fDateValue_2_Def := SrcItem.fDateValue_2_Def;
      fDateValue_1_Buf := SrcItem.fDateValue_1_Buf;
      fDateValue_2_Buf := SrcItem.fDateValue_2_Buf;
      fDateMode := SrcItem.fDateMode;
      fDateMode_Def := SrcItem.fDateMode_Def;
      fDateMode_Buf := SrcItem.fDateMode_Buf;
    end
  end;
end;

procedure TRDFComboDateItem.SetKeyFieldName(const AValue: string);
begin
  if Trim(aValue) <> fKeyFieldName then
  begin
    fKeyFieldName := Trim(aValue);
    if fKeyFieldName = EmptyStr then
    begin
      Active := False;
      Item_Active := False;
    end;
  end;
end;

procedure TRDFComboDateItem.SetDateFieldName(const AValue: string);
begin
  if Trim(aValue) <> fDateFieldName then
  begin
    fDateFieldName := Trim(aValue);
    if fDateFieldName = EmptyStr then
    begin
      Active := False;
      Item_Active := False;
    end;
  end;
end;

procedure TRDFComboDateItem.SetDateValue_1(const aValue: TDate);
begin
  if aValue <> fDateValue_1 then
  begin
    fDateValue_1 := aValue;
  end;
end;

procedure TRDFComboDateItem.SetDateValue_2(const aValue: TDate);
begin
  if aValue <> fDateValue_2 then
  begin
    fDateValue_2 := aValue;
  end;
end;

procedure TRDFComboDateItem.SetDateValue_1_Def(const aValue: TDate);
begin
  if aValue <> fDateValue_1_Def then
  begin
    fDateValue_1_Def := aValue;
  end;
end;

procedure TRDFComboDateItem.SetDateValue_2_Def(const aValue: TDate);
begin
  if aValue <> fDateValue_2_Def then
  begin
    fDateValue_2_Def := aValue;
  end;
end;

function TRDFComboDateItem.GetDateInitValue_1(const AMode: TDateEqMode; ADefValue: TDate): TDate;
begin
  case AMode of
    emdNull, emdEqual, emdAfter, emdBefore, emdPeriod: Result := ADefValue;
    emdCurrentDay:   Result := Date;
    emdAfterCurrentDay: Result := Date;
    emdBeforeCurrentDay: Result := Date;
    emdPrevDay:      Result := IncDay(Date, -1);
    emdNextDay:      Result := IncDay(Date, 1);
    emdCurrentWeek:  Result := DateOf(StartOfTheWeek(Date));
    emdPrevWeek:     Result := DateOf(StartOfTheWeek(IncWeek(Date, -1)));
    emdNextWeek:     Result := DateOf(StartOfTheWeek(IncWeek(Date, 1)));
    emdCurrentMonth: Result := DateOf(StartOfTheMonth(Date));
    emdPrevMonth:    Result := DateOf(StartOfTheMonth(IncMonth(Date, -1)));
    emdNextMonth:    Result := DateOf(StartOfTheMonth(IncMonth(Date, 1)));
    emdCurrentYear:  Result := DateOf(StartOfTheYear(Date));
    emdPrevYear:     Result := DateOf(StartOfTheYear(IncYear(Date, -1)));
    emdNextYear:     Result := DateOf(StartOfTheYear(IncYear(Date, 1)));
    emdLast3Days:    Result := IncDay(Date, -3);
    emdLast10Days:   Result := IncDay(Date, -10);
    emdLast20Days:   Result := IncDay(Date, -20);
    emdLast30Days:   Result := IncDay(Date, -30);
    emdLast60Days:   Result := IncDay(Date, -60);
    emdLast90Days:   Result := IncDay(Date, -90);
    emdLast180Days:  Result := IncDay(Date, -180);
    emdNext3Days:    Result := Date;
    emdNext10Days:   Result := Date;
    emdNext20Days:   Result := Date;
    emdNext30Days:   Result := Date;
    emdNext60Days:   Result := Date;
    emdNext90Days:   Result := Date;
    emdNext180Days:  Result := Date;
    else Result := ADefValue;
  end;
end;

function TRDFComboDateItem.GetDateInitValue_2(const AMode: TDateEqMode; ADefValue: TDate): TDate;
begin
  case AMode of
    emdNull, emdEqual, emdAfter, emdBefore, emdPeriod: Result := ADefValue;
    emdCurrentDay:   Result := Date;
    emdAfterCurrentDay: Result := Date;
    emdBeforeCurrentDay: Result := Date;
    emdPrevDay:      Result := IncDay(Date, -1);
    emdNextDay:      Result := IncDay(Date, 1);
    emdCurrentWeek:  Result := DateOf(EndOfTheWeek(Date));
    emdPrevWeek:     Result := DateOf(EndOfTheWeek(IncWeek(Date, -1)));
    emdNextWeek:     Result := DateOf(EndOfTheWeek(IncWeek(Date, 1)));
    emdCurrentMonth: Result := DateOf(EndOfTheMonth(Date));
    emdPrevMonth:    Result := DateOf(EndOfTheMonth(IncMonth(Date, -1)));
    emdNextMonth:    Result := DateOf(EndOfTheMonth(IncMonth(Date, 1)));
    emdCurrentYear:  Result := DateOf(EndOfTheYear(Date));
    emdPrevYear:     Result := DateOf(EndOfTheYear(IncYear(Date, -1)));
    emdNextYear:     Result := DateOf(EndOfTheYear(IncYear(Date, 1)));
    emdLast3Days:    Result := Date;
    emdLast10Days:   Result := Date;
    emdLast20Days:   Result := Date;
    emdLast30Days:   Result := Date;
    emdLast60Days:   Result := Date;
    emdLast90Days:   Result := Date;
    emdLast180Days:  Result := Date;
    emdNext3Days:    Result := IncDay(Date, 3);
    emdNext10Days:   Result := IncDay(Date, 10);
    emdNext20Days:   Result := IncDay(Date, 20);
    emdNext30Days:   Result := IncDay(Date, 30);
    emdNext60Days:   Result := IncDay(Date, 60);
    emdNext90Days:   Result := IncDay(Date, 90);
    emdNext180Days:  Result := IncDay(Date, 180);
    else Result := ADefValue;
  end;
end;

procedure TRDFComboDateItem.ResetItem;
begin
  inherited ResetItem;
  // KeyValue
  if KeyFieldLocate(fKeyValue_Def) then
    fKeyValue := fKeyValue_Def
  else
    if LookupFieldLocate(fLookupValue_Def, fLookupFieldIndex)
    then fKeyValue := GetKeyField.AsInteger
    else fKeyValue := fKeyNull;
  // DateValues
  fDateMode := fDateMode_Def;
  SetDateValue_1(GetDateInitValue_1(fDateMode, fDateValue_1_Def));
  SetDateValue_2(GetDateInitValue_2(fDateMode, fDateValue_2_Def));
end;

procedure TRDFComboDateItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  // KeyValue
  fKeyValue_Buf := fKeyValue;
  // DateValues
  fDateMode_Buf := fDateMode;
  fDateValue_1_Buf := fDateValue_1;
  fDateValue_2_Buf := fDateValue_2;
end;

procedure TRDFComboDateItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  // KeyValue
  fKeyValue := fKeyValue_Buf;
  // DateValues
  fDateValue_1 := fDateValue_1_Buf;
  fDateValue_2 := fDateValue_2_Buf;
  fDateMode := fDateMode_Buf;
end;

procedure TRDFComboDateItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  // KeyValue
  fKeyValue := Ini.ReadInteger(Section, Format(iniKeyValue_1, [Name]), fKeyValue_Def);
  // DateValues
  fDateMode := TDateEqMode(Ini.ReadInteger(Section, Format(iniDateMode, [Name]), Integer(fDateMode_Def)));
  SetDateValue_1(GetDateInitValue_1(fDateMode, Ini.ReadDate(Section, Format(iniDateValue_1, [Name]), fDateValue_1_Def)));
  SetDateValue_2(GetDateInitValue_2(fDateMode, Ini.ReadDate(Section, Format(iniDateValue_2, [Name]), fDateValue_2_Def)));
end;

procedure TRDFComboDateItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  // KeyValue
  Ini.WriteInteger(Section, Format(iniKeyValue_1, [Name]), fKeyValue);
  // DateValues
  Ini.WriteDate(Section, Format(iniDateValue_1, [Name]), fDateValue_1);
  Ini.WriteDate(Section, Format(iniDateValue_2, [Name]), fDateValue_2);
  Ini.WriteInteger(Section, Format(iniDateMode, [Name]), Integer(fDateMode));
end;

function TRDFComboDateItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled
    and (fLinkDataSet <> nil) and (GetKeyField <> nil)
    and (GetLookupField(fLookupFieldIndex) <> nil)
    and fLinkDataSet.Active;
end;

function TRDFComboDateItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_LinkComboDate;
end;

procedure TRDFComboDateItem.UpdateEdits(Edit1, Edit2: TControl; const ADateMode: Integer);
begin
  // DateValues
  TDateTimePicker(Edit1).Date := GetDateInitValue_1(TDateEqMode(ADateMode), Item_DateValue_1);
  Edit1.Enabled := TDateEqMode(ADateMode) in [emdEqual, emdAfter, emdPeriod];
  TDateTimePicker(Edit2).Date := GetDateInitValue_2(TDateEqMode(ADateMode), Item_DateValue_2);
  Edit2.Enabled := TDateEqMode(ADateMode) in [emdBefore, emdPeriod];
end;

procedure TRDFComboDateItem.LoadEditorControls(Editor: TFormDbFilterItem);
var
  i: TDateEqMode;
begin
  with TFormDbFilterItem_LinkComboDate(Editor) do
  begin
    // KeyValue
    DataSource.DataSet := fLinkDataSet;
    ClearButton.Visible := fUseNull;
    if fUseNull
    then DBLookupComboBox.Width := 289
    else DBLookupComboBox.Width := 312;
    DBLookupComboBox.KeyField := fKeyField;
    DBLookupComboBox.ListFieldIndex := fLookupFieldIndex;
    DBLookupComboBox.ListField := fLookupFields;
    DBLookupComboBox.Hint := fLookupCaptions;
    if fUseNull and (Item_KeyValue = fKeyNull)
    then DBLookupComboBox.KeyValue := Null
    else begin
      try
        DBLookupComboBox.KeyValue := Item_KeyValue;
      except
        DBLookupComboBox.KeyValue := Null;
      end;
    end;
    // DateValues
    for i := Low(TDateEqMode) to High(TDateEqMode) do
      ModeComboBox.Items.Add(DateEqModes[i]);
    ModeComboBox.ItemIndex := Integer(Item_DateCompareMode);
    DateTimePicker1.Date := Item_DateValue_1;
    DateTimePicker2.Date := Item_DateValue_2;
    CheckControls := UpdateEdits;
  end;
end;

procedure TRDFComboDateItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkComboDate(Editor) do
  begin
    // KeyValue
    if fUseNull and (DBLookupComboBox.KeyValue = Null)
    then Item_KeyValue := fKeyNull
    else Item_KeyValue := DBLookupComboBox.KeyValue;
    // DateValues
    Item_DateCompareMode := TDateEqMode(ModeComboBox.ItemIndex);
    Item_DateValue_1 := DateTimePicker1.Date;
    Item_DateValue_2 := DateTimePicker2.Date;
  end;
end;

function TRDFComboDateItem.GetItemText: string;
var
  sKeyPart, sDatePart: string;
begin
  if Item_Active then
  begin
    // KeyValue
    GetLookupText(fLookupFieldIndex, fKeyValue, sKeyPart);
    // DateValues
    case fDateMode of
      emdNull:
        sDatePart := DateEqModesText[fDateMode];

      emdEqual, emdAfter, emdAfterCurrentDay,
      emdCurrentDay, emdPrevDay, emdNextDay:
        sDatePart := Format(DateEqModesText[fDateMode], [DateToStr(GetDateInitValue_1(fDateMode, fDateValue_1))]);

      emdBefore, emdBeforeCurrentDay:
        sDatePart := Format(DateEqModesText[fDateMode], [DateToStr(GetDateInitValue_2(fDateMode, fDateValue_2))]);

      emdPeriod,
      emdCurrentWeek, emdPrevWeek, emdNextWeek,
      emdCurrentMonth, emdPrevMonth, emdNextMonth,
      emdCurrentYear, emdPrevYear, emdNextYear,
      emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days,
      emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days:
        sDatePart := Format(DateEqModesText[fDateMode], [DateToStr(GetDateInitValue_1(fDateMode, fDateValue_1)),
                                                  DateToStr(GetDateInitValue_2(fDateMode, fDateValue_2))]);
      else sDatePart := EmptyStr;
    end;
    // Concat
    Result := ConcatFilters(sKeyPart, sDatePart, ' + ', True, False);
    if fInvert and (Result <> EmptyStr) then
      Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFComboDateItem.GetWhereString: string;
var
  sKeyPart, sDatePart: string;
begin
  if Item_Active then
  begin
    // KeyValue
    if fUseNull and (fKeyValue = fKeyNull)
    then sKeyPart := Format(LookupEqModesWhere[0], [fKeyFieldName])
    else sKeyPart := Format(LookupEqModesWhere[1], [fKeyFieldName, fKeyValue]);
    // DateValues
    case fDateMode of
      emdNull:
        sDatePart := Format(DateEqModesWhere[fDateMode], [fDateFieldName]);

      emdEqual:
        sDatePart := Format(DateEqModesWhere[fDateMode], [fDateFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(GetDateInitValue_1(fDateMode, fDateValue_1))),
          DateToSqlStr(Filter.DateFormatWhere, DateOf(IncDay(GetDateInitValue_1(fDateMode, fDateValue_1), 1)))]);

      emdAfter, emdAfterCurrentDay:
        sDatePart := Format(DateEqModesWhere[fDateMode], [fDateFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(GetDateInitValue_1(fDateMode, fDateValue_1)))]);

      emdBefore, emdBeforeCurrentDay:
        sDatePart := Format(DateEqModesWhere[fDateMode], [fDateFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(IncDay(GetDateInitValue_2(fDateMode, fDateValue_2), 1)))]);

      emdPeriod,
      emdCurrentDay, emdCurrentWeek, emdCurrentMonth, emdCurrentYear,
      emdPrevDay, emdPrevWeek, emdPrevMonth, emdPrevYear,
      emdNextDay, emdNextWeek, emdNextMonth, emdNextYear,
      emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days,
      emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days:
        sDatePart := Format(DateEqModesWhere[fDateMode], [fDateFieldName,
          DateToSqlStr(Filter.DateFormatWhere, DateOf(GetDateInitValue_1(fDateMode, fDateValue_1))),
          DateToSqlStr(Filter.DateFormatWhere, DateOf(IncDay(GetDateInitValue_2(fDateMode, fDateValue_2), 1)))]);

      else sDatePart := EmptyStr;
    end;
    // Concat
    Result := ConcatFilters(sKeyPart, sDatePart, LogicalOperationsSql[loAnd], True, False);
    // Subquery
    if fSubQueryLink then
      Result := Format(LookupEqModesSubWhere[3], [FieldName, fSubQueryListKey, fSubQueryTableName, Result]);
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFComboDateItem.GetFilterString: string;
var
  sKeyPart, sDatePart: string;
begin
  if Item_Active then
  begin
    // KeyValue
    if fUseNull and (fKeyValue = fKeyNull)
    then sKeyPart := Format(LookupEqModesFilter[0], [fKeyFieldName])
    else sKeyPart := Format(LookupEqModesFilter[1], [fKeyFieldName, fKeyValue]);
    // DateValues
    case fDateMode of
      emdNull:
        sDatePart := Format(DateEqModesFilter[fDateMode], [fDateFieldName]);

      emdEqual:
        sDatePart := Format(DateEqModesFilter[fDateMode], [fDateFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(GetDateInitValue_1(fDateMode, fDateValue_1))),
          FormatDateTime(Filter.DateFormatFilter, DateOf(IncDay(GetDateInitValue_1(fDateMode, fDateValue_1), 1)))]);

      emdAfter, emdAfterCurrentDay:
        sDatePart := Format(DateEqModesFilter[fDateMode], [fDateFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(GetDateInitValue_1(fDateMode, fDateValue_1)))]);

      emdBefore, emdBeforeCurrentDay:
        sDatePart := Format(DateEqModesFilter[fDateMode], [fDateFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(IncDay(GetDateInitValue_2(fDateMode, fDateValue_2), 1)))]);

      emdPeriod,
      emdCurrentDay, emdCurrentWeek, emdCurrentMonth, emdCurrentYear,
      emdPrevDay, emdPrevWeek, emdPrevMonth, emdPrevYear,
      emdNextDay, emdNextWeek, emdNextMonth, emdNextYear,
      emdLast3Days, emdLast10Days, emdLast20Days, emdLast30Days, emdLast60Days, emdLast90Days, emdLast180Days,
      emdNext3Days, emdNext10Days, emdNext20Days, emdNext30Days, emdNext60Days, emdNext90Days, emdNext180Days:
        sDatePart := Format(DateEqModesFilter[fDateMode], [fDateFieldName,
          FormatDateTime(Filter.DateFormatFilter, DateOf(GetDateInitValue_1(fDateMode, fDateValue_1))),
          FormatDateTime(Filter.DateFormatFilter, DateOf(IncDay(GetDateInitValue_2(fDateMode, fDateValue_2), 1)))]);

      else sDatePart := EmptyStr;
    end;
    // Concat
    Result := ConcatFilters(sKeyPart, sDatePart, LogicalOperationsSql[loAnd], True, False);
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDFListLinkItem ========================================================== }

resourcestring
  SKeyListText       = 'список %s: %s';
  SLoadTable         = 'Загрузка записей...';
  SReadTable         = 'Генерация списка значений...';
  // EBadDelimiter      = 'Не определено или некорректное значение свойства KeysDelimiter!';

const
  AllRecordsDefault  = '*';

procedure TRDFListLinkItem.Clear;
begin
  inherited Clear;
  fKeyValues := EmptyStr;
  fKeyValues_Buf := EmptyStr;
  fKeyValues_Def := EmptyStr;
  fDelimiter := ',';
  fTextDelimiter := '; ';
  fLookupFilter := AllRecordsDefault;
  FMaxLookupItems := 5;
end;

procedure TRDFListLinkItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFListLinkItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFListLinkItem then
    begin
      SrcItem := Source as TRDFListLinkItem;
      fKeyValues := SrcItem.fKeyValues;
      fKeyValues_Buf := SrcItem.fKeyValues_Buf;
      fKeyValues_Def := SrcItem.fKeyValues_Def;
      fDelimiter := SrcItem.fDelimiter;
      fTextDelimiter := SrcItem.fTextDelimiter;
      fLookupFilter := SrcItem.fLookupFilter;
      FMaxLookupItems := SrcItem.FMaxLookupItems;
    end;
  end;
end;

function TRDFListLinkItem.GetDelimiter: string;
begin
  Result := fDelimiter;
end;

procedure TRDFListLinkItem.SetDelimiter(const aValue: string);
begin
  fDelimiter := Copy(aValue, 1, 2);
end;

function TRDFListLinkItem.DelimiterChar: Char;
begin
  if fDelimiter = '' then raise ERDbFilterError.Create(EBadDelimiter);
  Result := Trim(fDelimiter)[1];
end;

function TRDFListLinkItem.CheckKeysList(const KeysList: string): string;
var
  CurrFilter: string;
  CurrFState: Boolean;
  Bmk: TBookmark;
  i: Integer;
  Dlm: Char;
  Key: string;
begin
  Result := EmptyStr;
  Dlm := DelimiterChar;
  if not (csDesigning in ComponentState)
  and KeyFieldExist and DataSetActive then begin
    StartWait;
    if fLookupDisabled then fLinkDataSet.DisableControls;
    try
      CurrFilter := fLinkDataSet.Filter;
      CurrFState := fLinkDataSet.Filtered;
      Bmk := fLinkDataSet.GetBookmark;
      try
        fLinkDataSet.Filtered := False;
        for i := 1 to WordCount(KeysList, [Dlm]) do
        begin
          Key := Trim(ExtractWord(i, KeysList, [Dlm]));
          if fLinkDataSet.Locate(fKeyField, Key, []) then
          begin
            if Result = EmptyStr then Result := Key
            else Result := Result + fDelimiter + Key;
          end;
          Application.ProcessMessages;
        end;
      finally
        fLinkDataSet.Filter := CurrFilter;
        fLinkDataSet.Filtered := CurrFState;
        try
          fLinkDataSet.GotoBookmark(Bmk);
        except
        end;
        fLinkDataSet.FreeBookmark(Bmk);
      end;
    finally
      if fLookupDisabled then fLinkDataSet.EnableControls;
      StopWait;
    end;
  end
  else Result := KeysList;
end;

function TRDFListLinkItem.GenerateKeysList(const Filter: string): string;
var
  CurrFilter: string;
  CurrFState: Boolean;
  Bmk: TBookmark;
  KeyFld: TField;
begin
  Result := EmptyStr;
  KeyFld := GetKeyField;
  if (Filter <> EmptyStr) and (KeyFld <> nil) and DataSetActive then
  begin
    StartWait;
    if fLookupDisabled then fLinkDataSet.DisableControls;
    try
      CurrFilter := fLinkDataSet.Filter;
      CurrFState := fLinkDataSet.Filtered;
      Bmk := fLinkDataSet.GetBookmark;
      try
        if Filter = AllRecordsDefault then
          fLinkDataSet.Filtered := False
        else begin
          try
            fLinkDataSet.Filter := Filter;
            fLinkDataSet.Filtered := True;
          except
            on E: Exception do
              raise ERDbFilterError.CreateFmt(EErrorLookupFilter,
                [Self.Filter.Name, Name, Filter, E.Message]);
          end;
        end;
        fLinkDataSet.First;
        while not fLinkDataSet.EOF do
        begin
          if Result = EmptyStr then Result := KeyFld.AsString
          else Result := Result + fDelimiter + KeyFld.AsString;
          fLinkDataSet.Next;
          Application.ProcessMessages;
        end;
      finally
        fLinkDataSet.Filter := CurrFilter;
        fLinkDataSet.Filtered := CurrFState;
        try
          fLinkDataSet.GotoBookmark(Bmk);
        except
        end;
        fLinkDataSet.FreeBookmark(Bmk);
      end;
    finally
      if fLookupDisabled then fLinkDataSet.EnableControls;
      StopWait;
    end;
  end;
end;

function TRDFListLinkItem.GenerateLookupList(const KeysList: string): string;
var
  CurrFilter: string;
  CurrFState: Boolean;
  Bmk: TBookmark;
  i, N: Integer;
  Dlm: Char;
  Key: string;
  TxtFld: TField;
begin
  Result := EmptyStr;
  if fDelimiter = '' then
  begin
    Result := EBadDelimiter;
    raise ERDbFilterError.Create(EBadDelimiter);
  end;
  Dlm := fDelimiter[1];
  N := WordCount(KeysList, [Dlm]);
  if N > FMaxLookupItems then
    Result := Format(SKeyListText, [fKeyField, KeysList])
  else begin
    TxtFld := GetLookupField(fLookupFieldIndex);
    if (KeysList <> EmptyStr) and KeyFieldExist and (TxtFld <> nil) and DataSetActive then
    begin
      StartWait;
      if fLookupDisabled then fLinkDataSet.DisableControls;
      try
        CurrFilter := fLinkDataSet.Filter;
        CurrFState := fLinkDataSet.Filtered;
        Bmk := fLinkDataSet.GetBookmark;
        try
          fLinkDataSet.Filtered := False;
          for i := 1 to N do
          begin
            Key := Trim(ExtractWord(i, KeysList, [Dlm]));
            if fLinkDataSet.Locate(fKeyField, Key, []) then
            begin
              if Result = EmptyStr then Result := TxtFld.AsString
              else Result := Result + fTextDelimiter + TxtFld.AsString;
            end
            else begin
              if Result = EmptyStr then Result := Format(EKeyValueNotFoundStr, [fKeyField, Key])
              else Result := Result + fTextDelimiter + Format(EKeyValueNotFoundStr, [fKeyField, Key]);
            end;
            Application.ProcessMessages;
          end;
        finally
          fLinkDataSet.Filter := CurrFilter;
          fLinkDataSet.Filtered := CurrFState;
          try
            fLinkDataSet.GotoBookmark(Bmk);
          except
          end;
          fLinkDataSet.FreeBookmark(Bmk);
        end;
      finally
        if fLookupDisabled then fLinkDataSet.EnableControls;
        StopWait;
      end;
    end
    else GetLookupText(fLookupFieldIndex, fKeyNull, Result);
  end;
end;

procedure TRDFListLinkItem.ResetItem;
begin
  inherited ResetItem;
  if fKeyValues_Def <> EmptyStr
  then fKeyValues := CheckKeysList(fKeyValues_Def)
  else fKeyValues := GenerateKeysList(fLookupFilter);
end;

procedure TRDFListLinkItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fKeyValues_Buf := fKeyValues;
end;

procedure TRDFListLinkItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fKeyValues := fKeyValues_Buf;
end;

procedure TRDFListLinkItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  if fKeyValues_Def <> EmptyStr
  then fKeyValues := CheckKeysList(fKeyValues_Def)
  else fKeyValues := GenerateKeysList(fLookupFilter);
  if Ini.ValueExists(Section, Format(iniValue_1, [Name])) then
    fKeyValues := CheckKeysList(Ini.ReadString(Section, Format(iniValue_1, [Name]), fKeyValues));
end;

procedure TRDFListLinkItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteString(Section, Format(iniValue_1, [Name]), fKeyValues);
end;

function TRDFListLinkItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled
    and (fLinkDataSet <> nil) and (GetKeyField <> nil)
    and (GetLookupField(fLookupFieldIndex) <> nil)
    and fLinkDataSet.Active;
end;

procedure TRDFListLinkItem.ListView_CreateColumns(LV: TListView);
const
  SizeCorr1 = 2;
  SizeCorr2 = 4;
var
  i: Integer;
  F: TField;
begin
  LV.Columns.BeginUpdate;
  try
    LV.Columns.Clear;
    F := GetKeyField;
    if F <> nil then
    begin
      with LV.Columns.Add do
      begin
        Width := F.DisplayWidth * (LV.Font.Size - SizeCorr1);
        Caption := fKeyField;
      end;
    end;
    for i := 0 to GetLookupFieldsCount(True) do
    begin
      F := GetLookupField(i);
      if F <> nil then
      begin
        with LV.Columns.Add do
        begin
          Width := F.DisplayWidth * (LV.Font.Size - SizeCorr2);
          Caption := GetLookupFieldCaption(i);
        end;
      end;
    end;
  finally
    LV.Columns.EndUpdate;
  end;
end;

procedure TRDFListLinkItem.ListView_LoadTable(LV: TListView);
var
  SP, CurrFState: Boolean;
  Bmk: TBookmark;
  Dlm: Char;
  KeyFld, LkpFld: TField;
  i: Integer;
  Id: ^Integer;

  function KeyInList(const Key: Integer; List: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to WordCount(List, [Dlm]) do
    begin
      Result := IntToStr(Key) = Trim(ExtractWord(i, List, [Dlm]));
      if Result then Break;
    end;
  end;

begin
  Dlm := DelimiterChar;
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    KeyFld := GetKeyField;
    if (KeyFld <> nil) and DataSetActive then
    begin
      if fLookupDisabled then fLinkDataSet.DisableControls;
      try
        CurrFState := fLinkDataSet.Filtered;
        Bmk := fLinkDataSet.GetBookmark;
        try
          SP := fLinkDataSet.RecordCount > MinProgressCount;
          if SP then ShowProgress(SLoadTable, fLinkDataSet.RecordCount);
          fLinkDataSet.Filtered := False;
          try
            fLinkDataSet.First;
            while not fLinkDataSet.EOF do
            begin
              with LV.Items.Add do
              begin
                New(Id);
                Id^ := KeyFld.AsInteger;
                Caption := KeyFld.AsString;
                Checked := KeyInList(KeyFld.AsInteger, fKeyValues);
                Data := Id;
                for i := 0 to GetLookupFieldsCount(True) do
                begin
                  LkpFld := GetLookupField(i);
                  if Assigned(LkpFld) then Subitems.Add(LkpFld.AsString);
                end;
              end;
              fLinkDataSet.Next;
              if SP then UpdateProgressStep(1);
            end;
          finally
            if SP then CloseProgress;
            fLinkDataSet.Filtered := CurrFState;
          end;
        finally
          try
            fLinkDataSet.GotoBookmark(Bmk);
          except
          end;
          fLinkDataSet.FreeBookmark(Bmk);
        end;
      finally
        if fLookupDisabled then fLinkDataSet.EnableControls;
      end;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TRDFListLinkItem.ListView_ReadTable(LV: TListView);
var
  i: Integer;
  Id: ^Integer;
  SP: Boolean;
begin
  fKeyValues := EmptyStr;
  SP := LV.Items.Count > MinProgressCount;
  if SP then ShowProgress(SReadTable, LV.Items.Count);
  try
    for i := 0 to LV.Items.Count - 1 do
    begin
      if LV.Items[i].Checked then
      begin
        Id := LV.Items[i].Data;
        if fKeyValues = EmptyStr then fKeyValues := IntToStr(Id^)
        else fKeyValues := fKeyValues + fDelimiter + IntToStr(Id^);
      end;
      if SP then UpdateProgressStep(1);
    end;
  finally
    if SP then CloseProgress;
  end;
end;

function TRDFListLinkItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_LinkList;
end;

procedure TRDFListLinkItem.LoadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkList(Editor) do
  begin
    ListView_CreateColumns(ListView);
    ListView_LoadTable(ListView);
  end;
end;

procedure TRDFListLinkItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkList(Editor) do
  begin
    ListView_ReadTable(ListView);
  end;
end;

function TRDFListLinkItem.GetItemText: string;
begin
  if Item_Active then
  begin
    Result := GenerateLookupList(fKeyValues);
    if fInvert then Result := Format(SInvertText, [Result]);
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFListLinkItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    if fSubQueryLink then begin
      if fKeyValues <> EmptyStr
      then Result := Format(LookupEqModesSubWhere[2],
        [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey, fKeyValues])
      else begin
        if fUseNull
        then Result := Format(LookupEqModesSubWhere[0],
          [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey])
        else Result := Format(LookupEqModesSubWhere[1],
          [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey, fKeyNull]);
      end;
    end
    else begin
      if fKeyValues <> EmptyStr
      then Result := Format(LookupEqModesWhere[2], [FieldName, fKeyValues])
      else begin
        if fUseNull
        then Result := Format(LookupEqModesWhere[0], [FieldName])
        else Result := Format(LookupEqModesWhere[1], [FieldName, fKeyNull]);
      end;
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFListLinkItem.GetFilterString: string;
var
  i, KeyCnt: Integer;
  Dlm: Char;
  Key, KeyItem: string;
begin
  Result := EmptyStr;
  if Item_Active then
  begin
    if fKeyValues <> EmptyStr then
    begin
      Dlm := DelimiterChar;
      KeyCnt := WordCount(fKeyValues, [Dlm]);
      for i := 1 to KeyCnt do
      begin
        Key := Trim(ExtractWord(i, fKeyValues, [Dlm]));
        KeyItem := Format(LookupEqModesFilter[2], [FieldName, Key]);
        if KeyCnt > 1 then KeyItem := Format(Brackets, [KeyItem]);
        if Result = EmptyStr then Result := KeyItem
        else Result := Result + LogicalOperationsSql[loOr] + KeyItem;
      end;
    end
    else
      if fUseNull
      then Result := Format(LookupEqModesFilter[0], [FieldName])
      else Result := Format(LookupEqModesFilter[1], [FieldName, fKeyNull]);
  end;
  DoGetFilterString(Result);
end;

{ == TRDFProcLinkItem ========================================================== }

resourcestring
  EGetLookupNotDefined = 'ОШИБКА! Не определен метод OnGetLookupText!';
  ESelectKeyNotDefined = 'Не определен метод OnSelectKeyValue!';

destructor TRDFProcLinkItem.Destroy;
begin
  fOnGetLookup := nil;
  fOnSelectKey := nil;
  inherited Destroy;
end;

procedure TRDFProcLinkItem.Clear;
begin
  inherited Clear;
  fKeyValue := fKeyNull;
  fKeyValue_Sel := fKeyNull;
  fKeyValue_Buf := fKeyNull;
  fKeyValue_Def := fKeyNull;
  fForceSelect := True;
  fIntLookup := True;
  fOnGetLookup := nil;
  fOnSelectKey := nil;
end;

procedure TRDFProcLinkItem.Assign(Source: TPersistent);
var
  SrcItem: TRDFProcLinkItem;
begin
  inherited Assign(Source);
  if Source = nil then Clear
  else begin
    if Source is TRDFProcLinkItem then
    begin
      SrcItem := Source as TRDFProcLinkItem;
      fKeyValue := SrcItem.fKeyValue;
      fKeyValue_Sel := SrcItem.fKeyValue_Sel;
      fKeyValue_Buf := SrcItem.fKeyValue_Buf;
      fKeyValue_Def := SrcItem.fKeyValue_Def;
      fForceSelect := SrcItem.fForceSelect;
      fIntLookup := SrcItem.fIntLookup;
      fOnGetLookup := SrcItem.fOnGetLookup;
      fOnSelectKey := SrcItem.fOnSelectKey;
    end;
  end;
end;

procedure TRDFProcLinkItem.ResetItem;
begin
  inherited ResetItem;
  if KeyFieldLocate(fKeyValue_Def)
  then fKeyValue := fKeyValue_Def
  else fKeyValue := fKeyNull;
end;

procedure TRDFProcLinkItem.CopyToEditBuffer;
begin
  inherited CopyToEditBuffer;
  fKeyValue_Buf := fKeyValue;
end;

procedure TRDFProcLinkItem.RestoreFromEditBuffer;
begin
  inherited RestoreFromEditBuffer;
  fKeyValue := fKeyValue_Buf;
end;

procedure TRDFProcLinkItem.ReadItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited ReadItemData(Ini, Section);
  fKeyValue := Ini.ReadInteger(Section, Format(iniValue_1, [Name]), fKeyValue_Def);
end;

procedure TRDFProcLinkItem.SaveItemData(Ini: TMemIniFile; const Section: string);
begin
  inherited SaveItemData(Ini, Section);
  Ini.WriteInteger(Section, Format(iniValue_1, [Name]), fKeyValue);
end;

function TRDFProcLinkItem.AutoActivateEnabled: Boolean;
begin
  Result := inherited AutoActivateEnabled and Assigned(fOnSelectKey)
    and (fLinkDataSet <> nil) and (GetKeyField <> nil)
    and (GetLookupField(fLookupFieldIndex) <> nil)
    and fLinkDataSet.Active;
end;

procedure TRDFProcLinkItem.BtnSelectKey(Sender: TObject);
var
  RunKey: Integer;
  RunRes: Boolean;
begin
  if not Assigned(fOnSelectKey) then
    raise ERDbFilterError.Create(ESelectKeyNotDefined);
  RunKey := fKeyValue_Sel;
  fOnSelectKey(Sender, RunKey, RunRes);
  if RunRes then
  begin
    fKeyValue_Sel := RunKey;
    if Sender is TButton
    then TButton(Sender).Caption := BtnGetLookupText(Sender);
  end;
end;

function TRDFProcLinkItem.BtnGetLookupText(Sender: TObject): string;
begin
  if fIntLookup then
    GetLookupText(fLookupFieldIndex, fKeyValue_Sel, Result)
  else begin
    if Assigned(fOnGetLookup)
    then fOnGetLookup(Sender, fKeyValue_Sel, Result)
    else Result := EGetLookupNotDefined;
  end;
end;

function TRDFProcLinkItem.GetEditorClass: TFormDbFilterItemClass;
begin
  Result := TFormDbFilterItem_LinkProc;
end;

procedure TRDFProcLinkItem.LoadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkProc(Editor) do
  begin
    SelectBtn.OnClick := BtnSelectKey;
    SelectBtn.Caption := BtnGetLookupText(Self);
  end;
end;

procedure TRDFProcLinkItem.ReadEditorControls(Editor: TFormDbFilterItem);
begin
  with TFormDbFilterItem_LinkProc(Editor) do
  begin
    Item_KeyValue := fKeyValue_Sel;
  end;
end;

procedure TRDFProcLinkItem.PrepareDialog;
begin
  inherited;
  fKeyValue_Sel := Item_KeyValue;
  if fForceSelect then BtnSelectKey(Self);
end;

function TRDFProcLinkItem.GetItemText: string;
begin
  if Item_Active then
  begin
    if fIntLookup then
    begin
      if GetLookupText(fLookupFieldIndex, fKeyValue, Result) and fInvert
      then Result := Format(SInvertText, [Result]);
    end
    else begin
      if Assigned(fOnGetLookup) then
      begin
        fOnGetLookup(Self, fKeyValue, Result);
        if fInvert then Result := Format(SInvertText, [Result]);
      end
      else Result := EGetLookupNotDefined;
    end;
  end
  else Result := EmptyStr;
  DoGetItemText(Result);
end;

function TRDFProcLinkItem.GetWhereString: string;
begin
  if Item_Active then
  begin
    if fSubQueryLink then begin
      if fUseNull and (fKeyValue = fKeyNull)
      then Result := Format(LookupEqModesSubWhere[0],
        [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey])
      else Result := Format(LookupEqModesSubWhere[1],
        [FieldName, fSubQueryListKey, fSubQueryTableName, fSubQueryKey, fKeyValue])
    end
    else begin
      if fUseNull and (fKeyValue = fKeyNull)
      then Result := Format(LookupEqModesWhere[0], [FieldName])
      else Result := Format(LookupEqModesWhere[1], [FieldName, fKeyValue])
    end;
  end
  else Result := EmptyStr;
  DoGetWhereString(Result);
end;

function TRDFProcLinkItem.GetFilterString: string;
begin
  if Item_Active then
  begin
    if fUseNull and (fKeyValue = fKeyNull)
    then Result := Format(LookupEqModesFilter[0], [FieldName])
    else Result := Format(LookupEqModesFilter[1], [FieldName, fKeyValue])
  end
  else Result := EmptyStr;
  DoGetFilterString(Result);
end;

{ == TRDbFilter ================================================================ }

procedure TRDbFilter.InternalInit;
begin
  fItems := TList.Create;
  fDateFmtWhere := '''mm.dd.yyyy''';
  fDateFmtFilter := ShortDateFormat;
  fKeysWhere := '';
  fKeysFilter := '';
  fOptions := [foChangeOptions, foItemActivateOnChange, foItemsDisabledActive];
  fAddBrackets := False;
  fCaseEnabled := True;
  RegisterItemsClasses;
end;

procedure TRDbFilter.InternalDone;
begin
  if Assigned(fItems) then
  begin
    DeleteItems;
    FreeAndNil(fItems);
  end;
end;

{ Items Management }

function TRDbFilter.GetItem(Index: Integer): TRDFItem;
begin
  Result := fItems[Index];
end;

procedure TRDbFilter.SetItem(Index: Integer; Value: TRDFItem);
begin
  Item[Index].Assign(Value);
  Item[Index].fFilter := Self;
end;

procedure TRDbFilter.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
  Item: TRDFItem;
begin
  for i := 0 to fItems.Count - 1 do
  begin
    Item := fItems[i];
    if Item.Owner = Root then Proc(Item);
  end;
end;

function TRDbFilter.IndexOf(Item: TRDFItem): Integer;
begin
  Result := fItems.IndexOf(Item);
end;

procedure TRDbFilter.AddItem(Item: TRDFItem);
begin
  fItems.Add(Item);
  Item.fFilter := Self;
end;

procedure TRDbFilter.RemoveItem(Item: TRDFItem);
begin
  Item.fFilter := nil;
  fItems.Remove(Item);
end;

procedure TRDbFilter.DeleteItem(Item: TRDFItem);
begin
  RemoveItem(Item);
  if not (csDestroying in Item.ComponentState) then
    Item.Free;
end;

procedure TRDbFilter.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= fItems.Count) then
    raise ERDbFilterError.CreateFmt(EListIndexError, [Index, Self.Name]);
  DeleteItem(fItems.Items[Index]);
end;

procedure TRDbFilter.DeleteItems;
begin
  while fItems.Count > 0 do
    DeleteItem(Item[fItems.Count - 1]);
end;

function TRDbFilter.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TRDbFilter.GetActiveCount: Integer;
var
  i, iCount: Integer;
begin
  Result := 0;

  iCount := fItems.Count - 1;
  for i := 0 to iCount do
    if TRDFItem(fItems[i]).Item_Active then Inc(Result);
end;

function TRDbFilter.GetFirstActiveItem: TRDFItem;
var
  i, iCount: Integer;
begin
  Result := nil;

  iCount := fItems.Count - 1;
  for i := 0 to iCount do
    if TRDFItem(fItems[i]).Item_Active then
    begin
      Result := TRDFItem(fItems[i]);
      Break;
    end;
end;

function TRDbFilter.GetItemOnField(const FieldName: string): TRDFItem;
var
  i, iCount: Integer;
begin
  Result := nil;

  if FieldName <> '' then
  begin
    iCount := fItems.Count - 1;
    for i := 0 to iCount do
      if SameText(TRDFItem(fItems[i]).fFieldName, FieldName) then
      begin
        Result := TRDFItem(fItems[i]);
        Break;
      end;
  end;
end;

{ Operations }

procedure TRDbFilter.ClearFilter;
var
  i: Integer;
begin
  fKeysWhere := EmptyStr;
  fKeysFilter := EmptyStr;
  fKeysText := EmptyStr;
  for i := 0 to fItems.Count - 1 do
    with TRDFItem(fItems.Items[i]) do
      if fEnabled then Item_Active := False;
end;

procedure TRDbFilter.InternalReset;
var
  i: Integer;
begin
  fKeysWhere := EmptyStr;
  fKeysFilter := EmptyStr;
  fKeysText := EmptyStr;
  for i := 0 to fItems.Count - 1 do
    TRDFItem(fItems.Items[i]).ResetItem;
end;

procedure TRDbFilter.ResetFilter;
begin
  InternalReset;
end;

procedure TRDbFilter.CopyToEditBuffer;
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    TRDFItem(fItems.Items[i]).CopyToEditBuffer;
end;

procedure TRDbFilter.RestoreFromEditBuffer;
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    TRDFItem(fItems.Items[i]).RestoreFromEditBuffer;
end;

function TRDbFilter.GetIniSection: string;
begin
  CheckDbLink;
  Result := AnsiUpperCase(Format(iniFilter, [OwnerName, DataSet.Name]) + GetIniSectionTag);
end;

procedure TRDbFilter.LoadItems(Ini: TMemIniFile);
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    if TRDFItem(fItems.Items[i]).Enabled then
      TRDFItem(fItems.Items[i]).ReadItemData(Ini, GetIniSection);
end;

procedure TRDbFilter.SaveItems(Ini: TMemIniFile);
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    if TRDFItem(fItems.Items[i]).Enabled then
      TRDFItem(fItems.Items[i]).SaveItemData(Ini, GetIniSection);
end;

procedure TRDbFilter.LoadData;
var
  Ini: TMemIniFile;
  strFN, strSN: string;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    if foChangeOptions in fOptions then
    begin
      strFN := GetIniFileName;
      strSN := GetIniSection;
      Ini := TMemIniFile.Create(strFN);
      try
        if Ini.ReadBool(strSN, iniRestore, foStoreItemsState in fOptions)
        then fOptions := fOptions + [foStoreItemsState]
        else fOptions := fOptions - [foStoreItemsState];
        if Ini.ReadBool(strSN, iniRebuild, foDialogOnActivate in fOptions)
        then fOptions := fOptions + [foDialogOnActivate]
        else fOptions := fOptions - [foDialogOnActivate];
        if foStoreItemsState in fOptions then
          LoadItems(Ini);
      finally
        Ini.Free;
      end;
    end;
  end;
end;

procedure TRDbFilter.SaveData;
var
  Ini: TMemIniFile;
  strFN, strSN: string;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    if (foChangeOptions in fOptions) or (foStoreItemsState in fOptions) then
    begin
      strFN := GetIniFileName;
      strSN := GetIniSection;
      Ini := TMemIniFile.Create(strFN);
      try
        if Ini.SectionExists(strSN) then Ini.EraseSection(strSN);
        if foChangeOptions in fOptions then
        begin
          Ini.WriteBool(strSN, iniRestore, foStoreItemsState in fOptions);
          Ini.WriteBool(strSN, iniRebuild, foDialogOnActivate in fOptions);
        end;
        SaveItems(Ini);
        Ini.UpdateFile;
      finally
        Ini.Free;
      end;
    end;
  end;
end;

procedure TRDbFilter.DoActivate;
begin
  if Assigned(fOnCreateItems) then
    fOnCreateItems(Self);
  inherited DoActivate;
  if (foDialogOnActivate in fOptions) and not (csLoading in ComponentState)
  then InternalShowDialog(True);
end;

function TRDbFilter.IsStoreOptions: Boolean;
begin
  Result := (foStoreItemsState in fOptions) or (foChangeOptions in fOptions);
end;

procedure TRDbFilter.SetOptions(const aValue: TRDfOptions);
begin
  if fOptions <> aValue then
  begin
    fOptions := aValue;
    if not ((csLoading in ComponentState) or (csDestroying in ComponentState))
    and (csDesigning in ComponentState) then CheckOptions;
  end;
end;

procedure TRDbFilter.LoadFromFile(const FileName: string);
var
  Ini: TMemIniFile;
begin
  if not FileExists(FileName) then
    raise ERDbFilterError.CreateFmt(EFilterFileNotFound, [FileName]);
  Ini := TMemIniFile.Create(FileName);
  try
    if not Ini.SectionExists(GetIniSection) then
      raise ERDbFilterError.CreateFmt(EFilterNotFound, [FileName, DataSet.Name]);
    LoadItems(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TRDbFilter.SaveToFile(const FileName: string);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    if not (FileExists(FileName) and Ini.SectionExists(GetIniSection))
    or (Application.MessageBox(PChar(Format(SReplaceFilter, [FileName, DataSet.Name])),
          PChar(Application.Title),
          MB_YESNO + MB_ICONQUESTION) = IDYES)
    then SaveItems(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function TRDbFilter.InternalShowDialog(const OkOnly: Boolean): Boolean;
begin
  Result := False;
  CheckDbLink;
  if CheckActive then
  begin
    with TFormDbFilter.Create(Application) do
    begin
      try
        StartWait;
        try
          CopyToEditBuffer;
          Filter := Self;
          FormStorageEnabled := not (csDesigning in Self.ComponentState);
        finally
          StopWait;
        end;
        OnlyOk := OkOnly;
        Result := ShowModal = mrOk;
        if Result then
        begin
          fKeysWhere := EmptyStr;
          fKeysFilter := EmptyStr;
        end
        else begin
          StartWait;
          try
            RestoreFromEditBuffer;
          finally
            StopWait;
          end;
        end;
      finally
        Free;
      end;
    end;
  end;
end;

function TRDbFilter.ShowDialog: Boolean;
begin
  Result := InternalShowDialog(False);
end;

function TRDbFilter.GetTextString: string;
var
  i, ActCnt, iCnt: Integer;
  Item: TRDFItem;
  ItemStr, CustStr: string;
begin
  Result := EmptyStr;
  CustStr := EmptyStr;

  if CheckActive then
  begin
    ActCnt := GetActiveCount;
    if ActCnt > 0 then
    begin
      iCnt := 0;
      for i := 0 to fItems.Count - 1 do
      begin
        Item := TRDFItem(fItems[i]);
        if Item.Item_Active then
        begin
          ItemStr := Format(STextItem, [Item.fFieldCaption, Item.GetItemText]);
          if ItemStr <> EmptyStr then
          begin
            Inc(iCnt);
            if CustStr = EmptyStr then
              CustStr := ItemStr
            else begin
              if iCnt = 2 then
                CustStr := Format(Brackets, [CustStr]);
              CustStr := CustStr + #32
                + Item.GetOperationText(CustStr <> EmptyStr) + #32
                + Format(Brackets, [ItemStr]);
            end;
          end;
        end;
      end;
    end;

    if fKeysText <> EmptyStr then
    begin
      if CustStr = EmptyStr
      then CustStr := fKeysText
      else CustStr := Format(Brackets, [fKeysText]) + #32 + LogicalOperations[loAnd] + #32 + Format(Brackets, [CustStr]);
    end;
  end;

  if CustStr = EmptyStr
  then Result := SFilterOff
  else Result := Format(SFilterOn, [CustStr]);
end;

function TRDbFilter.GetWhereString: string;
var
  i, ActCnt: Integer;
  Item: TRDFItem;
  ItemStr, CustStr: string;
begin
  Result := EmptyStr;
  CustStr := EmptyStr;

  if CheckActive then
  begin
    ActCnt := GetActiveCount;
    if ActCnt > 0 then
    begin
      for i := 0 to fItems.Count - 1 do
      begin
        Item := TRDFItem(fItems[i]);
        if Item.Item_Active then
        begin
          ItemStr := Item.GetWhereString;
          if ItemStr <> EmptyStr then
          begin
            if CustStr <> EmptyStr then
              CustStr := CustStr + LogicalOperationsSql[Item.fOperation];
            if Item.fInvert
            then CustStr := CustStr + Format(InvertItem, [ItemStr])
            else begin
              if CustStr <> EmptyStr then
                ItemStr := Format(Brackets, [ItemStr]);
              CustStr := CustStr + ItemStr;
            end;
          end;
        end;
      end;
    end;

    if fKeysWhere <> EmptyStr then
    begin
      if CustStr = EmptyStr
      then CustStr := fKeysWhere
      else CustStr := Format(Brackets, [fKeysWhere]) + LogicalOperationsSql[loAnd] + Format(Brackets, [CustStr]);
    end;
  end;

  if fAddBrackets and (CustStr <> EmptyStr)
  then Result := Format(Brackets, [CustStr])
  else Result := CustStr;
end;

function TRDbFilter.GetFilterString: string;
var
  i, ActCnt: Integer;
  Item: TRDFItem;
  ItemStr, CustStr: string;
begin
  Result := EmptyStr;
  CustStr := EmptyStr;

  if CheckActive then
  begin
    ActCnt := GetActiveCount;
    if ActCnt > 0 then
    begin
      for i := 0 to fItems.Count - 1 do
      begin
        Item := TRDFItem(fItems[i]);
        if Item.Item_Active then
        begin
          ItemStr := Item.GetFilterString;
          if ItemStr <> EmptyStr then
          begin
            if CustStr <> EmptyStr then
              CustStr := CustStr + LogicalOperationsSql[Item.fOperation];
            if Item.fInvert
            then CustStr := CustStr + Format(InvertItem, [ItemStr])
            else begin
              if CustStr <> EmptyStr then
                ItemStr := Format(Brackets, [ItemStr]);
              CustStr := CustStr + ItemStr;
            end;
          end;
        end;
      end;
    end;

    if fKeysFilter <> EmptyStr then
    begin
      if CustStr = EmptyStr
      then CustStr := fKeysFilter
      else CustStr := Format(Brackets, [fKeysFilter]) + LogicalOperationsSql[loAnd] + Format(Brackets, [CustStr]);
    end;
  end;

  if fAddBrackets and (CustStr <> EmptyStr)
  then Result := Format(Brackets, [CustStr])
  else Result := CustStr;
end;

function TRDbFilter.GetDbFilterOptions(ADefault: TFilterOptions): TFilterOptions;
var
  i: Integer;
  Item: TRDFItem;
begin
  Result := ADefault;
  if Active then
  begin
    for i := 0 to fItems.Count - 1 do
    begin
      Item := TRDFItem(fItems[i]);
      if Item.Item_Active then Result := Result + Item.GetDbFilterOptions;
    end;
  end;
end;

{ == Editor TRDbFilter ========================================================= }

resourcestring
  SFileFilter = 'Фильтры, наборы фильтров (*.flt)|*.flt';

const
  siField     = 0;
  siText      = 1;

  SFileExt    = '.flt';

procedure TFormDbFilter.InitForm;
begin
  inherited InitForm;
  ListView.OnChange := nil;
  ListView.OnChanging := nil;
end;

procedure TFormDbFilter.DoneForm;
begin
  ListView.OnChange := nil;
  ListView.OnChanging := nil;
  inherited DoneForm;
end;

procedure TFormDbFilter.SetFilter(Value: TRDbFilter);
var
  i: Integer;
begin
  if fFilter <> Value then
  begin
    StartWait;
    ListView.Items.BeginUpdate;
    ListView.OnChange := nil;
    ListView.OnChanging := nil;
    fFilter := Value;
    try
      for i := 0 to fFilter.Items.Count - 1 do
        if TRDFItem(fFilter.Items[i]).Visible then
          with ListView.Items.Add do
          begin
            Data := fFilter.Items[i];
            Checked := TRDFItem(fFilter.Items[i]).Item_Active;
            Caption := TRDFItem(fFilter.Items[i]).GetOperationText;
            SubItems.Add(TRDFItem(fFilter.Items[i]).FieldCaption);
            SubItems.Add(TRDFItem(fFilter.Items[i]).GetItemText);
          end;
    finally
      ListView.OnChange := ListViewChange;
      ListView.OnChanging := ListViewChanging;
      ListView.Items.EndUpdate;
      StopWait;
    end;
  end;
end;

procedure TFormDbFilter.ResetItemsState;
var
  i: Integer;
begin
  StartWait;
  ListView.Items.BeginUpdate;
  ListView.OnChange := nil;
  ListView.OnChanging := nil;
  try
    for i := 0 to ListView.Items.Count - 1 do
      ListView.Items[i].Caption := TRDFItem(ListView.Items[i].Data).GetOperationText;
  finally
    ListView.OnChange := ListViewChange;
    ListView.OnChanging := ListViewChanging;
    ListView.Items.EndUpdate;
    StopWait;
  end;
end;

procedure TFormDbFilter.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if TRDFItem(Item.Data).Item_Active <> Item.Checked then
  begin
    TRDFItem(Item.Data).Item_Active := Item.Checked;
    Item.SubItems[siText] := TRDFItem(Item.Data).GetItemText;
    ResetItemsState;
  end;
end;

procedure TFormDbFilter.ListViewChanging(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
begin
  AllowChange := TRDFItem(Item.Data).Enabled and not TRDFItem(Item.Data).Blocked;
end;

procedure TFormDbFilter.ListViewDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
    Item.Data := nil;
end;

procedure TFormDbFilter.CloseCancelUpdate(Sender: TObject);
begin
  CloseCancel.Enabled := not OnlyOk and IsNotWait;
end;

procedure TFormDbFilter.ClearAllUpdate(Sender: TObject);
begin
  ClearAll.Enabled := Assigned(fFilter) and (ListView.Items.Count > 0);
end;

procedure TFormDbFilter.ClearAllExecute(Sender: TObject);
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

procedure TFormDbFilter.ResetAllUpdate(Sender: TObject);
begin
  ResetAll.Enabled := Assigned(fFilter) and (ListView.Items.Count > 0);
end;

procedure TFormDbFilter.ResetAllExecute(Sender: TObject);
var
  i: Integer;
  FI: TRDFItem;
begin
  StartWait;
  ListView.Items.BeginUpdate;
  ListView.OnChange := nil;
  ListView.OnChanging := nil;
  try
    for i := 0 to ListView.Items.Count - 1 do
    begin
      FI := ListView.Items.Item[i].Data;
      FI.ResetItem;
      ListView.Items.Item[i].Checked := FI.Item_Active;
      ListView.Items.Item[i].SubItems[siText] := FI.GetItemText;
    end;
    ResetItemsState;
  finally
    ListView.OnChange := ListViewChange;
    ListView.OnChanging := ListViewChanging;
    ListView.Items.EndUpdate;
    StopWait;
  end;
end;

procedure TFormDbFilter.ActiveAllUpdate(Sender: TObject);
begin
  ActiveAll.Enabled := Assigned(fFilter) and (ListView.Items.Count > 0);
end;

procedure TFormDbFilter.ActiveAllExecute(Sender: TObject);
var
  i: Integer;
begin
  StartWait;
  try
    for i := 0 to ListView.Items.Count - 1 do
      ListView.Items[i].Checked := True;
  finally
    StopWait;
  end;
end;

procedure TFormDbFilter.ItemPropertiesUpdate(Sender: TObject);
begin
  ItemProperties.Enabled := Assigned(fFilter) and Assigned(ListView.Selected)
    and TRDFItem(ListView.Selected.Data).Enabled
    and not TRDFItem(ListView.Selected.Data).Blocked;
end;

procedure TFormDbFilter.ItemPropertiesExecute(Sender: TObject);
var
  LI: TListItem;
  FI: TRDFItem;
  R: TRect;
  P: TPoint;
begin
  LI := ListView.Selected;
  if Assigned(LI) then
  begin
    FI := TRDFItem(LI.Data);
    if (foItemActivateOnChange in Filter.Options) and not FI.Item_Active
    then FI.Item_Active := FI.AutoActivateEnabled;
    R := LI.DisplayRect(drBounds);
    P.x := R.Left + ListView.Columns[0].Width + ListView.Columns[1].Width + 1;
    P.y := R.Bottom - 1;
    P := ListView.ClientToScreen(P);
    if FI.ShowDialog(Self, P.x, P.y) then
    begin
      ListView.Items.BeginUpdate;
      try
        LI.Checked := FI.Item_Active;
        LI.SubItems[siText] := FI.GetItemText;
        ResetItemsState;
      finally
        ListView.Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TFormDbFilter.ListViewDblClick(Sender: TObject);
begin
  if ItemProperties.Enabled then ItemProperties.Execute;
end;

procedure TFormDbFilter.LoadFromFileUpdate(Sender: TObject);
begin
  LoadFromFile.Enabled := Assigned(Filter) and Assigned(Filter.DataSet)
    and (ListView.Items.Count > 0);
end;

procedure TFormDbFilter.LoadFromFileExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
  i: Integer;
  FI: TRDFItem;
begin
  Dialog := TOpenDialog.Create(Self);
  try
    Dialog.DefaultExt := SFileExt;
    Dialog.Filter := SFileFilter;
    Dialog.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    Dialog.InitialDir := ExtractFilePath(Application.ExeName);
    if Dialog.Execute then
    begin
      StartWait;
      try
        fFilter.LoadFromFile(Dialog.FileName);
        for i := 0 to ListView.Items.Count - 1 do
        begin
          FI := ListView.Items.Item[i].Data;
          ListView.Items.Item[i].Checked := FI.Item_Active;
          ListView.Items.Item[i].SubItems[siText] := FI.GetItemText;
        end;
        ResetItemsState;
      finally
        StopWait;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TFormDbFilter.SaveToFileUpdate(Sender: TObject);
begin
  SaveToFile.Enabled := Assigned(Filter) and Assigned(Filter.DataSet)
    and (ListView.Items.Count > 0);
end;

procedure TFormDbFilter.SaveToFileExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Self);
  try
    Dialog.DefaultExt := SFileExt;
    Dialog.Filter := SFileFilter;
    Dialog.Options := [ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofShareAware, ofEnableSizing];
    Dialog.InitialDir := ExtractFilePath(Application.ExeName);
    if Dialog.Execute then begin
      StartWait;
      try
        fFilter.SaveToFile(Dialog.FileName);
      finally
        StopWait;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TFormDbFilter.RestoreFilterUpdate(Sender: TObject);
begin
  RestoreFilter.Enabled := Assigned(fFilter) and fFilter.StoreInIniFile;
  RestoreFilter.Checked := foStoreItemsState in fFilter.Options;
end;

procedure TFormDbFilter.RestoreFilterExecute(Sender: TObject);
begin
  if foStoreItemsState in fFilter.Options
  then fFilter.Options := fFilter.Options - [foStoreItemsState]
  else fFilter.Options := fFilter.Options + [foStoreItemsState];
end;

procedure TFormDbFilter.RequeryFilterUpdate(Sender: TObject);
begin
  RequeryFilter.Enabled := Assigned(fFilter);
  RequeryFilter.Checked := foDialogOnActivate in fFilter.Options;
end;

procedure TFormDbFilter.RequeryFilterExecute(Sender: TObject);
begin
  if foDialogOnActivate in fFilter.Options
  then fFilter.Options := fFilter.Options - [foDialogOnActivate]
  else fFilter.Options := fFilter.Options + [foDialogOnActivate];
end;

end.
