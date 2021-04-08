
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Color DbGrid                           }
{                                                       }
{       Copyright (c) 2006-2015 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbColorGrid;

interface

uses
  Classes, SysUtils, Windows, Graphics, ImgList, Db, Grids, DbGrids;

type

  { TRDbColorGrid }

  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight, MultiSelected: Boolean) of object;

  TRDbFieldFlag = (ffSortAsc, ffSortDesc);
  TRDbColumnFlag = record
    Col: Integer;
    Flag: TRDbFieldFlag;
  end;
  TRDbColumnFlags = array of TRDbColumnFlag;

  TRDbColorGrid = class (TDBGrid)
  private
    fImages: TCustomImageList;
    fAttachImage: TImageIndex;
    fAttachField: string;
    fUseColors: Boolean;
    fInternalCalc: Boolean;
    fFlags: TRDbColumnFlags;
    fOnGetCellParams: TGetCellParamsEvent;
    procedure SetImages(Value: TCustomImageList);
    procedure SetUseColors(const aValue: Boolean);
    procedure GetCellParams(Field: TField; AFont: TFont; var Background: TColor; Highlight, MultiSelected: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateUseColors; virtual; abstract;
    procedure InternalGetCellParams(Field: TField; AFont: TFont; var Background: TColor;
      Highlight, MultiSelected: Boolean); dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure Paint; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ColumnFlagsClear;
    procedure ColumnFlagSet(const ACol: Integer; const AFlag: TRDbFieldFlag);
    procedure FieldFlagSet(const AField: string; const AFlag: TRDbFieldFlag);
    procedure FieldFlagSort(const sSort: string);
    property Canvas;
    property SelectedRows;
  published
    property Images: TCustomImageList read fImages write SetImages;
    property AttachImage: TImageIndex read fAttachImage write fAttachImage default -1;
    property AttachField: string read fAttachField write fAttachField;
    property ColorRows: Boolean read fUseColors write SetUseColors default True;
    property DefaultDrawing: Boolean read fInternalCalc write fInternalCalc default True;
    property OnGetCellParams: TGetCellParamsEvent read fOnGetCellParams write fOnGetCellParams;
  end;

  { TRDbStyledGrid }

  TRDbStyledGrid = class (TRDbColorGrid)
  private
    fFontStyle: string;
    fFontColor: string;
    fCellColor: string;
    procedure SetFontStyle(const aValue: string);
    procedure SetFontColor(const aValue: string);
    procedure SetCellColor(const aValue: string);
  protected
    procedure UpdateUseColors; override;
    procedure InternalGetCellParams(Field: TField; AFont: TFont; var Background: TColor;
      Highlight, MultiSelected: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FieldFontStyle: string read fFontStyle write SetFontStyle;
    property FieldFontColor: string read fFontColor write SetFontColor;
    property FieldCellColor: string read fCellColor write SetCellColor;
  end;

  { TRDbBalanceGrid }

  RDBGOption = (goColoredAllCells, goColoredSelectedCells, goSkipNullFields);
  RDBGOptions = set of RDBGOption;

  TFieldsIndexes = set of Byte;

  TRDbBalanceGrid = class (TRDbColorGrid)
  private
    fPositiveData: TColor;
    fNegativeData: TColor;
    fPositiveBalance: TColor;
    fNegativeBalance: TColor;
    fCosts: TColor;
    fPays: TColor;
    fColorOptions: RDBGOptions;
    fBalanceFields: string;
    fCostsFields: string;
    fPaysFields: string;
    fIndexesOk: Boolean;
    fBalanceIndexes: TFieldsIndexes;
    fCostsIndexes: TFieldsIndexes;
    fPaysIndexes: TFieldsIndexes;
    function  CheckFieldsList(const Value: string): string;
    function  CreateIndexes(const Value: string): TFieldsIndexes;
    procedure SetBalanceFields(const Value: string);
    procedure SetCostsFields(const Value: string);
    procedure SetPaysFields(const Value: string);
  protected
    procedure UpdateUseColors; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InternalGetCellParams(Field: TField; AFont: TFont; var Background: TColor;
      Highlight, MultiSelected: Boolean); override;
  published
    property ColorDataPositive: TColor read fPositiveData write fPositiveData default clWindowText;
    property ColorDataNegative: TColor read fNegativeData write fNegativeData default clRed;
    property ColorCellBalancePositive: TColor read fPositiveBalance write fPositiveBalance default $00EAFFEA;
    property ColorCellBalanceNegative: TColor read fNegativeBalance write fNegativeBalance default $00FDEBFE;
    property ColorCellCosts: TColor read fCosts write fCosts default $00FDEBFE;
    property ColorCellPays: TColor read fPays write fPays default $00EAFFEA;
    property OptionsColors: RDBGOptions read fColorOptions write fColorOptions;
    property FieldsBalance: string read fBalanceFields write SetBalanceFields;
    property FieldsCosts: string read fCostsFields write SetCostsFields;
    property FieldsPays: string read fPaysFields write SetPaysFields;
  end;

const
  ftNumericFields  = [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency];
  FieldsDelimiters = [',',';'];

implementation

uses
  Forms, RxStrUtils, RDialogs, RDbConst;

resourcestring
  SFieldNotFound = 'Поле %s в наборе данных %s не найдено!';

{ == TRDbColorGrid ============================================================= }
constructor TRDbColorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  fUseColors := True;
  fInternalCalc := False;
  fImages := nil;
  fAttachImage := -1;
  fAttachField := EmptyStr;
  SetLength(fFlags, 0);
end;

destructor TRDbColorGrid.Destroy;
begin
  SetLength(fFlags, 0);

  inherited Destroy;
end;

procedure TRDbColorGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then Images := nil;
  end;
end;

procedure TRDbColorGrid.SetImages(Value: TCustomImageList);
begin
  fImages := Value;
  if Images <> nil then Images.FreeNotification(Self);
end;

procedure TRDbColorGrid.SetUseColors(const aValue: Boolean);
begin
  if fUseColors <> aValue then
  begin
    fUseColors := aValue;
    UpdateUseColors;
    if fUseColors = aValue then Update;
  end;
end;

procedure TRDbColorGrid.InternalGetCellParams(Field: TField; AFont: TFont; var Background: TColor;
  Highlight, MultiSelected: Boolean);
begin
end;

procedure TRDbColorGrid.GetCellParams(Field: TField; AFont: TFont;
  var Background: TColor; Highlight, MultiSelected: Boolean);
begin
  if fInternalCalc then
    InternalGetCellParams(Field, AFont, Background, Highlight, MultiSelected);
  if Assigned(fOnGetCellParams) then
    fOnGetCellParams(Self, Field, AFont, Background, Highlight, MultiSelected);
end;

procedure TRDbColorGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  i: Integer;
begin
  inherited;
  if (ARow = 0) and (Length(fFlags) > 0) then
  begin
    for i := Low(fFlags) to High(fFlags) do
    begin
      if ACol = fFlags[i].Col + 1 then
      begin
        case fFlags[i].Flag of
          ffSortAsc:
            DrawFrameControl(Canvas.Handle, Rect(ARect.Right-13, ARect.Top+1, ARect.Right-1, ARect.Bottom-1), DFC_MENU, DFCS_INACTIVE+$10); // DFCS_INACTIVE+$10);
          ffSortDesc:
            DrawFrameControl(Canvas.Handle, Rect(ARect.Right-13, ARect.Top+1, ARect.Right-1, ARect.Bottom-1), DFC_MENU, DFCS_INACTIVE+$08); // DFCS_INACTIVE+$8);
        end;
      end;
    end;
  end;
end;

procedure TRDbColorGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  NewBackgrnd: TColor;
  Highlight, MultiSelect: Boolean;
  LeftPos, TopPos: Integer;
  Field: TField;

  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result := (dgMultiSelect in Options) and Assigned(Datalink) and Datalink.Active
      and Assigned(Datalink.Datasource) and Assigned(Datalink.Datasource.DataSet)
      and SelectedRows.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  end;

begin
  if Assigned(Column)
  then Field := Column.Field
  else Field := nil;
  MultiSelect := RowIsMultiSelected;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused);
  if fUseColors then
  begin
    NewBackgrnd := Canvas.Brush.Color;
    GetCellParams(Field, Canvas.Font, NewBackgrnd, Highlight, MultiSelect);
    Canvas.Brush.Color := NewBackgrnd;
  end;
  if (fAttachField <> '') and (fAttachImage > -1)
  and Assigned(Field) and Assigned(fImages)
  and (Field = DataSource.DataSet.FindField(fAttachField))
  and (Field.AsInteger > 0) then
  begin
    TopPos := Rect.Top + ((Rect.Bottom - Rect.Top + 1) - fImages.Height) div 2;
    case Column.Alignment of
      taRightJustify: LeftPos := Rect.Right - 1 - fImages.Width;
      taCenter: LeftPos := Rect.Left + (Column.Width - fImages.Width) div 2;
      else LeftPos := Rect.Left;
    end;
    Canvas.FillRect(Rect);
    fImages.Draw(Canvas, LeftPos, TopPos, fAttachImage);
  end
  else begin
    DefaultDrawColumnCell(Rect, DataCol, Column, State);
    if Columns.State = csDefault then
      inherited DrawDataCell(Rect, Field, State);
    inherited DrawColumnCell(Rect, DataCol, Column, State);
  end;
  if Highlight and not (csDesigning in ComponentState)
    and not (dgRowSelect in Options)
    and (ValidParentForm(Self).ActiveControl = Self)
  then Canvas.DrawFocusRect(Rect);
end;

procedure TRDbColorGrid.Paint;
begin
  inherited Paint;
  if not (csDesigning in ComponentState) and
    (dgRowSelect in Options) and Focused then
  begin
    Canvas.Font.Color := clWindowText;
    with Selection do
      DrawFocusRect(Canvas.Handle, BoxRect(Left, Top, Right, Bottom));
  end;
end;

function TRDbColorGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if Assigned(Datalink) and Datalink.Active and Assigned(Datalink.DataSet) then
      Result := Datalink.DataSet.MoveBy(1) <> 0;
  end;
end;

function TRDbColorGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if Assigned(Datalink) and Datalink.Active and Assigned(Datalink.DataSet) then
      Result := Datalink.DataSet.MoveBy(-1) <> 0;
  end;
end;

{ == TRDbStyledGrid ============================================================ }

constructor TRDbStyledGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFontStyle := 'FONT_STYLE';
  fFontColor := 'FONT_COLOR';
  fCellColor := 'CELL_COLOR';
  fInternalCalc := True;
end;

procedure TRDbStyledGrid.UpdateUseColors;
begin
  if fUseColors and ((Trim(fFontStyle) = EmptyStr)
  or (Trim(fFontColor) = EmptyStr) or (Trim(fCellColor) = EmptyStr))
  then fUseColors := False;
end;

procedure TRDbStyledGrid.SetFontStyle(const aValue: string);
begin
  if fFontStyle <> aValue then
  begin
    fFontStyle := aValue;
    UpdateUseColors;
    Update;
  end;
end;

procedure TRDbStyledGrid.SetFontColor(const aValue: string);
begin
  if fFontColor <> aValue then
  begin
    fFontColor := aValue;
    UpdateUseColors;
    Update;
  end;
end;

procedure TRDbStyledGrid.SetCellColor(const aValue: string);
begin
  if fCellColor <> aValue then
  begin
    fCellColor := aValue;
    UpdateUseColors;
    Update;
  end;
end;

procedure TRDbStyledGrid.InternalGetCellParams(Field: TField; AFont: TFont;
  var Background: TColor; Highlight, MultiSelected: Boolean);
var
  FontStyle, FontColor, CellColor: Integer;
  CurrStyledFld: TField;
begin
  FontStyle := -1; FontColor := -1; CellColor := -1;
  if Assigned(DataSource) and Assigned(DataSource.DataSet)
  and DataSource.DataSet.Active and not DataSource.DataSet.IsEmpty then
  begin
    CurrStyledFld := DataSource.DataSet.FindField(fFontStyle);
    if Assigned(CurrStyledFld) and not CurrStyledFld.IsNull then
      FontStyle := CurrStyledFld.AsInteger;
    CurrStyledFld := DataSource.DataSet.FindField(fFontColor);
    if Assigned(CurrStyledFld) and not CurrStyledFld.IsNull then
      FontColor := CurrStyledFld.AsInteger;
    CurrStyledFld := DataSource.DataSet.FindField(fCellColor);
    if Assigned(CurrStyledFld) and not CurrStyledFld.IsNull then
      CellColor := CurrStyledFld.AsInteger;
    if not (Highlight or MultiSelected) then
    begin
      if FontColor > -1 then
        AFont.Color := FontColor;
      if CellColor > -1 then
        Background := CellColor;
    end
    else begin
      if not Highlight then
        Background := clAppWorkSpace;
    end;
    if FontStyle > -1 then
    begin
      if (FontStyle and 1) > 0 then
        AFont.Style := AFont.Style + [fsBold];
      if (FontStyle and 2) > 0 then
        AFont.Style := AFont.Style + [fsItalic];
      if (FontStyle and 4) > 0 then
        AFont.Style := AFont.Style + [fsUnderline];
      if (FontStyle and 8) > 0 then
        AFont.Style := AFont.Style + [fsStrikeOut];
    end;
  end;
end;

{ == TRDbBalanceGrid =========================================================== }

constructor TRDbBalanceGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPositiveData := clWindowText;
  fNegativeData := clRed;
  fPositiveBalance := $00EAFFEA;
  fNegativeBalance := $00FDEBFE;
  fCosts := $00FDEBFE;
  fPays := $00EAFFEA;
  fColorOptions := [goColoredAllCells];
  fBalanceFields := '';
  fCostsFields := '';
  fPaysFields := '';
  fIndexesOk := False;
  fBalanceIndexes := [];
  fCostsIndexes := [];
  fPaysIndexes := [];
  fInternalCalc := True;
end;

procedure TRDbBalanceGrid.UpdateUseColors;
begin
end;

function TRDbBalanceGrid.CheckFieldsList(const Value: string): string;
var
  i: Integer;
  FieldName: string;
begin
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    Result := '';
    for i := 1 to WordCount(Value, FieldsDelimiters) do
    begin
      FieldName := Trim(ExtractWord(i, Value, FieldsDelimiters));
      if Assigned(DataSource.DataSet.FindField(FieldName)) then
      begin
        if Result = ''
        then Result := FieldName
        else Result := Result + ';' + FieldName;
      end
      else Application.MessageBox(PChar(Format(SFieldNotFound,
        [FieldName, DataSource.DataSet.Name])),
        PChar(Self.Name), MB_OK + MB_ICONERROR);
    end;
  end
  else Result := Value;
end;

function TRDbBalanceGrid.CreateIndexes(const Value: string): TFieldsIndexes;
var
  i: Integer;
  Field: TField;
begin
  Result := [];
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    for i := 1 to WordCount(Value, FieldsDelimiters) do
    begin
      Field := DataSource.DataSet.FindField(Trim(ExtractWord(i, Value, FieldsDelimiters)));
      if Assigned(Field) then Include(Result, Field.Index);
    end;
  end;
end;

procedure TRDbBalanceGrid.SetBalanceFields(const Value: string);
begin
  if fBalanceFields <> Value then
    fBalanceFields := CheckFieldsList(Value);
end;

procedure TRDbBalanceGrid.SetCostsFields(const Value: string);
begin
  if fCostsFields <> Value then
    fCostsFields := CheckFieldsList(Value);
end;

procedure TRDbBalanceGrid.SetPaysFields(const Value: string);
begin
  if fPaysFields <> Value then
    fPaysFields := CheckFieldsList(Value);
end;

procedure TRDbBalanceGrid.InternalGetCellParams(Field: TField; AFont: TFont;
  var Background: TColor; Highlight, MultiSelected: Boolean);
begin
  if not fIndexesOk and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    fBalanceIndexes := CreateIndexes(fBalanceFields);
    fCostsIndexes := CreateIndexes(fCostsFields);
    fPaysIndexes := CreateIndexes(fPaysFields);
    fIndexesOk := True;
  end;
  if Assigned(Field) and Assigned(DataSource) and Assigned(DataSource.DataSet)
  and DataSource.DataSet.Active and not DataSource.DataSet.IsEmpty
  and not (Highlight or MultiSelected) then
  begin
    if (Field.DataType in ftNumericFields)
    and not (Field.IsNull and (goSkipNullFields in fColorOptions)) then
    begin
      if Round(Field.AsFloat) < 0
      then AFont.Color := fNegativeData
      else AFont.Color := fPositiveData;
      // Прорисовка всех ячеек или баланса
      if (goColoredAllCells in fColorOptions)
      or ((goColoredSelectedCells in fColorOptions)
      and (Field.Index in fBalanceIndexes)) then
      begin
        if Round(Field.AsFloat) < 0
        then Background := fNegativeBalance
        else Background := fPositiveBalance;
      end;
      // Прорисовка расходов
      if (goColoredSelectedCells in fColorOptions)
      and (Field.Index in fCostsIndexes) then Background := fCosts;
      // Прорисовка доходов
      if (goColoredSelectedCells in fColorOptions)
      and (Field.Index in fPaysIndexes) then Background := fPays;
    end;
  end;
end;

procedure TRDbColorGrid.ColumnFlagsClear;
begin
  SetLength(fFlags, 0);
end;

procedure TRDbColorGrid.ColumnFlagSet(const ACol: Integer; const AFlag: TRDbFieldFlag);
var
  i: Integer;
begin
  if Length(fFlags) > 0 then
  begin
    for i := Low(fFlags) to High(fFlags) do
    begin
      if fFlags[i].Col = ACol then
      begin
        fFlags[i].Flag := AFlag;
        Exit;
      end;
    end;
  end;

  SetLength(fFlags, Length(fFlags) + 1);
  with fFlags[High(fFlags)] do
  begin
    Col := ACol;
    Flag := AFlag;
  end;
end;

procedure TRDbColorGrid.FieldFlagSet(const AField: string; const AFlag: TRDbFieldFlag);
var
  i: Integer;
begin
  for i := 0 to Columns.Count - 1 do
    if SameText(Columns[i].FieldName, AField) then
    begin
      ColumnFlagSet(i, AFlag);
      Exit;
    end;
end;

procedure TRDbColorGrid.FieldFlagSort(const sSort: string);
const
  DivChars = [',', ';'];
var
  sItem, sField, sValue: string;
  i, iCount: Integer;
begin
  if sSort <> '' then
  begin
    iCount := WordCount(sSort, DivChars);
    for i := 1 to iCount do
    begin
      sItem := Trim(ExtractWord(i, sSort, DivChars));
      sField := Trim(ExtractWord(1, sItem, [#32]));
      sValue := Trim(ExtractWord(2, sItem, [#32]));
      if SameText(sValue, 'DESC')
      then FieldFlagSet(sField, ffSortDesc)
      else FieldFlagSet(sField, ffSortAsc);
    end;
  end;
end;

end.
