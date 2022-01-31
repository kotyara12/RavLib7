unit RDbGridTuner;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplEditors, Menus, ActnList, ImgList, ComCtrls, ToolWin, RDbCustom,
  Db, DbGrids, StdCtrls, IniFiles, ExtCtrls;

type

  ERDbGridTunerError = class(Exception);

{ == TRDbGridTuner ============================================================= }

  TRDGItemData = record
    FieldName: string;
    FieldTitle: string;
    ColWidth: Integer;
    DataAlign: TAlignment;
    TitleAlign: TAlignment;
  end;

  TRDGOption = (ogStoreItemsState, ogChangeOptions);

  TRDGOptions = set of TRDGOption;

  TRDbGridTuner = class(TRDbCustomDG)
  private
    fDesignCols: TStringList;
    fViews: TStringList;
    fActiveView: Integer;
    fOptions: TRDGOptions;
    fStoreMultiSelect: Boolean;
    fOnViewChange: TNotifyEvent;

    procedure SetOptions(const AValue: TRDGOptions);
    procedure SetActiveView(const Value: Integer);

    function  LoadItem(Ini: TMemIniFile; const sSection: string;
      const iView, iIndex: Integer): string;
    procedure SaveItem(Ini: TMemIniFile; const sSection: string;
      const iView, iIndex: Integer; const sDataStr: string);
    function  PeakCols(Ini: TMemIniFile; const sSection: string;
      const iView: Integer): Boolean;
    function  LoadCols(Ini: TMemIniFile; const sSection: string;
      const iView: Integer): TStringList;
    procedure SaveCols(Ini: TMemIniFile; const sSection: string;
      const iView: Integer; const ColsData: TStringList);

    procedure DoViewChange;
  protected
    procedure InternalInit; override;
    procedure InternalDone; override;
    procedure InternalReset; override;
    function  IsStoreOptions: Boolean; override;
    function  GetIniSection: string; override;
  public
    procedure ResetColumns;

    function  GetDefaultColsData: TStringList;
    function  GetGridColsData: TStringList;
    procedure ApplyColsData(Cols: TStringList);

    procedure ViewsClear;
    function  ViewAppend(const sViewName: string; const ColsData: TStringList): Integer;
    procedure ViewUpdate(const Index: Integer; const ColsData: TStringList);
    procedure ViewUpdateActiveFromGrid;
    procedure ViewDelete(const Index: Integer);
    function  ViewColsData(const Index: Integer): TStringList;
    function  ViewActiveColsData: TStringList;

    procedure LoadData; override;
    procedure SaveData; override;

    function  ShowDialog: Boolean; override;

    property  Views: TStringList read fViews;
    property  ViewActive: Integer read fActiveView write SetActiveView default 0;
  published
    property Options: TRDGOptions read fOptions write SetOptions;
    property StoreMultiSelect: Boolean read fStoreMultiSelect write fStoreMultiSelect default True;
    property OnViewChange: TNotifyEvent read fOnViewChange write fOnViewChange;
  end;

{ ==  TFormDbGridTuner ========================================================= }

  TFormDbGridTuner = class(TEditorsTemplate)
    btnCloseOk: TToolButton;
    btnCloseCancel: TToolButton;
    SeparatorEnd: TToolButton;
    MoveUp: TAction;
    MoveDown: TAction;
    SetDefault: TAction;
    Properties: TAction;
    RestoreColumns: TAction;
    btnProperties: TToolButton;
    btnMoveUp: TToolButton;
    btnMoveDown: TToolButton;
    SeparatorEdit: TToolButton;
    itemMoveUpP: TMenuItem;
    itemMoveDownP: TMenuItem;
    divPopup2: TMenuItem;
    itemSetDefaultP: TMenuItem;
    itemPropertiesP: TMenuItem;
    divPopup3: TMenuItem;
    itemRestoreColumnsP: TMenuItem;
    divPopup4: TMenuItem;
    itemMoveUp: TMenuItem;
    itemMoveDown: TMenuItem;
    divEdit1: TMenuItem;
    itemProperties: TMenuItem;
    divEdit2: TMenuItem;
    itemSetDefault: TMenuItem;
    itemRestoreColumns: TMenuItem;
    divService1: TMenuItem;
    SeparatorView: TToolButton;
    menuViews: TMenuItem;
    ViewNew: TAction;
    ViewRename: TAction;
    ViewDelete: TAction;
    itemViewNew: TMenuItem;
    itemViewRename: TMenuItem;
    itemViewDelete: TMenuItem;
    ToolBarView: TToolBar;
    cbViews: TComboBox;
    btnSetDefault: TToolButton;
    btnViewNew: TToolButton;
    btnViewRename: TToolButton;
    btnViewDelete: TToolButton;
    SeparatorViews: TToolButton;
    procedure MoveUpUpdate(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveDownUpdate(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure SetDefaultUpdate(Sender: TObject);
    procedure SetDefaultExecute(Sender: TObject);
    procedure PropertiesUpdate(Sender: TObject);
    procedure PropertiesExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure RestoreColumnsUpdate(Sender: TObject);
    procedure RestoreColumnsExecute(Sender: TObject);
    procedure ListViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
    procedure cbViewsChange(Sender: TObject);
    procedure ViewNewUpdate(Sender: TObject);
    procedure ViewNewExecute(Sender: TObject);
    procedure ViewRenameUpdate(Sender: TObject);
    procedure ViewRenameExecute(Sender: TObject);
    procedure ViewDeleteUpdate(Sender: TObject);
    procedure ViewDeleteExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ToolBarViewResize(Sender: TObject);
  private
    fTuner: TRDbGridTuner;
    ItemChecked: TListItem;
    function  IsFieldExists(Fld: TField): Boolean;
  public
    procedure LoadFullData(const Tuner: TRDbGridTuner);
    procedure SaveFullData;
    procedure LoadColsData;
    procedure SaveColsData;
  end;

implementation

{$R *.dfm}

uses
  RxStrUtils, RVclUtils, RMsgRu, RListView, RDialogs, RDbGridTunerItem;

resourcestring
  SViewDefault        = 'По умолчанию';
  SViewCreate         = 'Создание представления';
  SViewRename         = 'Переменование представления';
  SViewDelete         = 'Удаление представления';
  SViewDeleteQuery    = 'Вы действительно хотите удалить текущее представление?';
  SViewCaption        = 'Укажите название представления:';

  SAlgLeft            = 'по левому краю';
  SAlgRight           = 'по правому краю';
  SAlgCenter          = 'по центру';

  EBadColumnDefine    = 'Ошибка загрузки описаний полей "%s": неверное описание в строке '#13'''%s''';
  EFieldNotFound      = 'Ошибка загрузки описаний полей "%s": поле "%s" не найдено!';


const
  defWidthCorrect     = 6.4;
  defAlign            = 0;

  iniGridTuner        = 'COLUMNS_%s.%s.%s';
  iniCount            = 'Count';
  iniCountView        = 'View_%d_Count';
  iniView             = 'View_%d';
  iniViewCount        = 'ViewCount';
  iniViewActive       = 'ViewActive';
  iniItem             = 'Item_%d';
  iniItemView         = 'View_%d_Item_%d';
  iniMultiSelect      = 'MultiSelect';

{ == Utilites ================================================================== }

(*
function FindDbGridColumn(Grid: TDbGrid; Fld: TField): TColumn;
var
  i, iCount: Integer;
begin
  Result := nil;

  iCount := Grid.Columns.Count - 1;
  for i := 0 to iCount do
    if Grid.Columns[i].Field = Fld then
    begin
      Result := Grid.Columns[i];
      Break;
    end;
end;
*)

function GetFieldWidth(Grid: TDbGrid; Fld: TField): Integer;
var
  TM: TTextMetric;
begin
  try
    GetTextMetrics(Grid.Canvas.Handle, TM);
    Result := Fld.DisplayWidth * (Grid.Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
  except
    Result := Trunc(Fld.DisplayWidth * defWidthCorrect);
  end;
end;

function GetColumnWidth(Col: TColumn): Integer;
begin
  Result := Col.DefaultWidth;
end;

function CreateRDGItemData(aFieldName: string; aFieldTitle: string;
  aColWidth: Integer; aDataAlign: TAlignment; aTitleAlign: TAlignment): TRDGItemData;
begin
  Result.FieldName := aFieldName;
  Result.FieldTitle := aFieldTitle;
  Result.ColWidth := aColWidth;
  Result.DataAlign := aDataAlign;
  Result.TitleAlign := aTitleAlign;
end;

function RDGItemData2String(Data: TRDGItemData): string;
begin
  Result := Data.FieldName + DelimChar
          + IntToStr(Data.ColWidth) + DelimChar
          + IntToStr(Integer(Data.DataAlign)) + DelimChar
          + IntToStr(Integer(Data.TitleAlign)) + DelimChar
          + Data.FieldTitle;
end;

function String2RDGItemData(Grid: TDbGrid; const sItemStr: string): TRDGItemData;
var
  Fld: TField;
begin
  Result.FieldName := Trim(ExtractWord(1, sItemStr, [DelimChar]));
  if Assigned(Grid) and Assigned(Grid.DataSource) and Assigned(Grid.DataSource.DataSet) then
  begin
    Fld := Grid.DataSource.DataSet.FindField(Result.FieldName);
    if Assigned(Fld) then
    begin
      Result.FieldTitle := Trim(ExtractWord(5, sItemStr, [DelimChar]));
      Result.ColWidth := StrToIntDef(Trim(ExtractWord(2, sItemStr, [DelimChar])), GetFieldWidth(Grid, Fld));
      Result.DataAlign := TAlignment(StrToIntDef(Trim(ExtractWord(3, sItemStr, [DelimChar])), Integer(Fld.Alignment)));
      Result.TitleAlign := TAlignment(StrToIntDef(Trim(ExtractWord(4, sItemStr, [DelimChar])), Integer(Fld.Alignment)));
    end
    else FillChar(Result, SizeOf(Result), #0);
  end
  else FillChar(Result, SizeOf(Result), #0);
end;

function AlignToStr(Alg: TAlignment): string;
begin
  case Alg of
    taLeftJustify: Result := SAlgLeft;
    taCenter: Result := SAlgCenter;
    taRightJustify: Result := SAlgRight;
    else Result := EmptyStr;
  end;
end;

function StrToAlign(StrAlg: string): TAlignment;
begin
  Result := taLeftJustify;
  if SameText(StrAlg, SAlgCenter) then Result := taCenter;
  if SameText(StrAlg, SAlgRight) then Result := taRightJustify;
end;

{ == TRDbGridTuner ============================================================= }

procedure TRDbGridTuner.InternalInit;
begin
  fViews := TStringList.Create;
  fDesignCols := nil;
  fStoreMultiSelect := True;
  fOptions := [ogStoreItemsState, ogChangeOptions];
end;

procedure TRDbGridTuner.InternalDone;
begin
  ViewsClear;
  fViews.Free;
  if Assigned(fDesignCols) then
    fDesignCols.Free;
end;

procedure TRDbGridTuner.InternalReset;
begin
  fDesignCols := GetGridColsData;
  ViewsClear;
  fActiveView := ViewAppend(SViewDefault, GetGridColsData);
end;

function TRDbGridTuner.IsStoreOptions: Boolean;
begin
  Result := (ogStoreItemsState in fOptions) or (ogChangeOptions in fOptions);
end;

procedure TRDbGridTuner.SetOptions(const AValue: TRDGOptions);
begin
  if fOptions <> AValue then
  begin
    fOptions := AValue;
    if not ((csLoading in ComponentState) or (csDestroying in ComponentState))
    and (csDesigning in ComponentState) then CheckOptions;
  end;
end;

function TRDbGridTuner.GetIniSection: string;
begin
  CheckDbLink;
  Result := AnsiUpperCase(Format(iniGridTuner,
    [OwnerName, DbGrid.DataSource.DataSet.Name, DbGrid.Name]) + GetIniSectionTag);
end;

procedure TRDbGridTuner.ResetColumns;
begin
  CheckDbLink;
  StartWait;
  try
    ViewUpdate(fActiveView, fDesignCols);
    ApplyColsData(ViewActiveColsData);
  finally
    StopWait;
  end;
end;

// Работа с наборами столбцов в текстовом виде ---------------------------------

procedure TRDbGridTuner.DoViewChange;
begin
  if Assigned(fOnViewChange) then
    fOnViewChange(Self);
end;

function TRDbGridTuner.GetDefaultColsData: TStringList;
var
  i, iCount: Integer;
  Fld: TField;
begin
  Result := TStringList.Create;

  if Assigned(DbGrid) and Assigned(DbGrid.DataSource) and Assigned(DbGrid.DataSource.DataSet) then
  begin
    iCount := DbGrid.DataSource.DataSet.FieldCount - 1;
    for i := 0 to iCount do
    begin
      Fld := DbGrid.DataSource.DataSet.Fields[i];
      if Fld.Visible then
      begin
        Result.Add(RDGItemData2String(CreateRDGItemData(
          Fld.FieldName,
          Fld.DisplayName,
          GetFieldWidth(DbGrid, Fld),
          Fld.Alignment,
          Fld.Alignment)));
      end;
    end;
  end;
end;

function TRDbGridTuner.GetGridColsData: TStringList;
var
  i, iCount: Integer;
begin
  Result := TStringList.Create;

  if Assigned(DbGrid) then
  begin
    iCount := DbGrid.Columns.Count - 1;
    for i := 0 to iCount do
    begin
      if Assigned(DbGrid.Columns[i].Field) then
        Result.Add(RDGItemData2String(CreateRDGItemData(
          DbGrid.Columns[i].FieldName,
          DbGrid.Columns[i].Title.Caption,
          DbGrid.Columns[i].Width,
          DbGrid.Columns[i].Alignment,
          DbGrid.Columns[i].Title.Alignment)));
    end;
  end;
end;

procedure TRDbGridTuner.ApplyColsData(Cols: TStringList);
var
  i, iCount: Integer;
  Item: TRDGItemData;
begin
  DbGrid.Columns.BeginUpdate;
  try
    DbGrid.Columns.Clear;

    iCount := Cols.Count - 1;
    for i := 0 to iCount do
    begin
      Item := String2RDGItemData(DbGrid, Cols[i]);

      if Item.FieldName <> EmptyStr then
      begin
        with DbGrid.Columns.Add do
        begin
          FieldName := Item.FieldName;
          Width := Item.ColWidth;
          Alignment := Item.DataAlign;
          Title.Caption := Item.FieldTitle;
          Title.Alignment := Item.TitleAlign;
        end;
      end;
    end;
  finally
    DbGrid.Columns.EndUpdate;
  end;
end;

procedure TRDbGridTuner.SetActiveView(const Value: Integer);
begin
  if fActiveView <> Value then
  begin
    ViewUpdateActiveFromGrid;
    fActiveView := Value;
    ApplyColsData(ViewActiveColsData);
    DoViewChange;
  end;
end;

// Представления ---------------------------------------------------------------

procedure TRDbGridTuner.ViewsClear;
var
  i, iCount: Integer;
  Buf: TStringList;
begin
  if fViews.Count > 0 then
  begin
    iCount := fViews.Count - 1;
    for i := 0 to iCount do
    begin
      Buf := TStringList(fViews.Objects[i]);
      fViews.Objects[i] := nil;
      Buf.Free;
    end;

    fViews.Clear;
  end;
end;

function TRDbGridTuner.ViewAppend(const sViewName: string; const ColsData: TStringList): Integer;
begin
  Result := fViews.AddObject(sViewName, ColsData);
end;

procedure TRDbGridTuner.ViewUpdate(const Index: Integer; const ColsData: TStringList);
begin
  if (Index >= 0) and (Index < fViews.Count) then
    TStringList(fViews.Objects[Index]).Assign(ColsData);
end;

procedure TRDbGridTuner.ViewUpdateActiveFromGrid;
begin
  ViewUpdate(fActiveView, GetGridColsData);
end;

procedure TRDbGridTuner.ViewDelete(const Index: Integer);
var
  Buf: TStringList;
begin
  if (Index >= 0) and (Index < fViews.Count) then
  begin
    Buf := TStringList(fViews.Objects[Index]);
    fViews.Objects[Index] := nil;
    Buf.Free;

    fViews.Delete(Index);
  end;
end;

function TRDbGridTuner.ViewColsData(const Index: Integer): TStringList;
begin
  Result := nil;

  if (Index >= 0) and (Index < fViews.Count) then
    Result := TStringList(fViews.Objects[Index]);
end;

function TRDbGridTuner.ViewActiveColsData: TStringList;
begin
  Result := ViewColsData(fActiveView);
end;

// Чтение и запись из INI-файла ------------------------------------------------

function TRDbGridTuner.LoadItem(Ini: TMemIniFile; const sSection: string;
  const iView, iIndex: Integer): string;
begin
  if iView > 0
  then Result := Trim(Ini.ReadString(sSection, Format(iniItemView, [iView, iIndex]), EmptyStr))
  else Result := Trim(Ini.ReadString(sSection, Format(iniItem, [iIndex]), EmptyStr));
end;

procedure TRDbGridTuner.SaveItem(Ini: TMemIniFile; const sSection: string;
  const iView, iIndex: Integer; const sDataStr: string);
begin
  if iView > 0
  then Ini.WriteString(sSection, Format(iniItemView, [iView, iIndex]), sDataStr)
  else Ini.WriteString(sSection, Format(iniItem, [iIndex]), sDataStr);
end;

function TRDbGridTuner.PeakCols(Ini: TMemIniFile; const sSection: string;
  const iView: Integer): Boolean;
begin
  if iView > 0
  then Result := Ini.ReadInteger(sSection, Format(iniCountView, [iView]), 0) > 0
  else Result := Ini.ReadInteger(sSection, iniCount, 0) > 0;
end;

function TRDbGridTuner.LoadCols(Ini: TMemIniFile; const sSection: string;
  const iView: Integer): TStringList;
var
  i, iIniCols: Integer;
  Buf: string;
  GridCols: TStringList;
begin
  Result := TStringList.Create;

  if iView > 0
  then iIniCols := Ini.ReadInteger(sSection, Format(iniCountView, [iView]), 0)
  else iIniCols := Ini.ReadInteger(sSection, iniCount, 0);

  if Assigned(DbGrid) and (iIniCols > 0) then
  begin
    for i := 1 to iIniCols do
    begin
      Buf := LoadItem(Ini, sSection, iView, i);
      if Buf <> EmptyStr then Result.Add(Buf);
    end;
  end;

  if Result.Count = 0 then
  begin
    GridCols := GetGridColsData;
    try
      Result.Assign(GridCols);
    finally
      GridCols.Free;
    end;
  end;
end;

procedure TRDbGridTuner.SaveCols(Ini: TMemIniFile; const sSection: string;
  const iView: Integer; const ColsData: TStringList);
var
  i, iCount: Integer;
begin
  iCount := ColsData.Count;

  if iView > 0
  then Ini.WriteInteger(sSection, Format(iniCountView, [iView]), iCount)
  else Ini.WriteInteger(sSection, iniCount, iCount);

  for i := 1 to iCount do
    SaveItem(Ini, sSection, iView, i, ColsData[i - 1]);
end;

procedure TRDbGridTuner.LoadData;
var
  Ini: TMemIniFile;
  sIniFileName, sSection: string;
  v, vCount: Integer;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    StartWait;
    try
      sIniFileName := GetIniFileName;
      sSection := GetIniSection;
      Ini := TMemIniFile.Create(sIniFileName);
      if Assigned(Ini) then
      begin
        try
          // Загружаем опции
          if fStoreMultiSelect then
          begin
            if Ini.ReadBool(sSection, iniMultiSelect, dgMultiSelect in DbGrid.Options)
            then DbGrid.Options := DbGrid.Options + [dgMultiSelect]
            else DbGrid.Options := DbGrid.Options - [dgMultiSelect];
          end;
          if Ini.ReadBool(sSection, iniRestore, ogStoreItemsState in fOptions)
          then fOptions := fOptions + [ogStoreItemsState]
          else fOptions := fOptions - [ogStoreItemsState];

          // Загружаем представления
          ViewsClear;
          vCount := Ini.ReadInteger(sSection, iniViewCount, 1);
          if vCount > 1 then
          begin
            // Несколько представлений
            for v := 1 to vCount do
            begin
              if PeakCols(Ini, sSection, v) // and (v <> fActiveView) ????
              then ViewAppend(Ini.ReadString(sSection, Format(iniView, [v]), SViewDefault),
                LoadCols(Ini, sSection, v))
              else ViewAppend(Ini.ReadString(sSection, Format(iniView, [v]), SViewDefault),
                LoadCols(Ini, sSection, 0));
            end;
            fActiveView := Ini.ReadInteger(sSection, iniViewActive, fActiveView + 1) - 1;
          end
          else begin
            // Одно представление
            fActiveView := ViewAppend(Ini.ReadString(sSection, Format(iniView, [1]), SViewDefault),
              LoadCols(Ini, sSection, 0));
          end;

          // Применяем активное представление к таблице
          ApplyColsData(ViewActiveColsData);
          DoViewChange;
        finally
          Ini.Free;
        end;
      end;
    finally
      StopWait;
    end;
  end;
end;

procedure TRDbGridTuner.SaveData;
var
  Ini: TMemIniFile;
  sIniFileName, sSection: string;
  v, vCount: Integer;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    StartWait;
    try
      sIniFileName := GetIniFileName;
      sSection := GetIniSection;
      Ini := TMemIniFile.Create(sIniFileName);
      if Assigned(Ini) then
      begin
        try
          // Удаляем секцию
          Ini.EraseSection(sSection);

          // Сохраняем опции
          if ogChangeOptions in fOptions then
            Ini.WriteBool(sSection, iniRestore, ogStoreItemsState in fOptions);
          if fStoreMultiSelect then
            Ini.WriteBool(sSection, iniMultiSelect, dgMultiSelect in DbGrid.Options);

          // Сохраняем представления
          vCount := fViews.Count - 1;
          Ini.WriteInteger(sSection, iniViewCount, vCount + 1);
          for v := 0 to vCount do
            Ini.WriteString(sSection, Format(iniView, [v + 1]), fViews[v]);
          Ini.WriteInteger(sSection, iniViewActive, fActiveView + 1);

          // Сохраняем "текущее" представление
          ViewUpdateActiveFromGrid;
          SaveCols(Ini, sSection, 0, ViewActiveColsData);

          // Сохраняем данные представлений
          vCount := fViews.Count - 1;
          if vCount > 0 then
          begin
            for v := 0 to vCount do
              SaveCols(Ini, sSection, v + 1, ViewColsData(v));
          end;

          // Фиксируем изменения
          Ini.UpdateFile;
        finally
          Ini.Free;
        end;
      end;
    finally
      StopWait;
    end;
  end;
end;

(*
procedure TRDbGridTuner.LoadListView(LV: TListView);
var
  i, iCount: Integer;
  Col: TColumn;
  Fld: TField;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;

    iCount := DbGrid.Columns.Count - 1;
    for i := 0 to iCount do
      with LV.Items.Add do
      begin
        Col := DbGrid.Columns[i];
        Data := Col.Field;
        Caption := Col.Title.Caption;
        Subitems.Add(Col.FieldName);
        Subitems.Add(IntToStr(Col.Width));
        Subitems.Add(AlignToStr(Col.Alignment));
        Subitems.Add(AlignToStr(Col.Title.Alignment));
        Checked := True;
      end;

    iCount := DbGrid.DataSource.DataSet.FieldCount - 1;
    for i := 0 to iCount do
    begin
      Fld := DbGrid.DataSource.DataSet.Fields[i];
      if FindDbGridColumn(DbGrid, Fld) = nil then
      begin
        with LV.Items.Add do
        begin
          Data := Fld;
          Caption := Fld.DisplayName;
          Subitems.Add(Fld.FieldName);
          Subitems.Add(IntToStr(GetFieldWidth(DbGrid, Fld)));
          Subitems.Add(AlignToStr(Fld.Alignment));
          Subitems.Add(AlignToStr(Fld.Alignment));
          Checked := False;
        end;
      end;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TRDbGridTuner.ReadListView(LV: TListView);
var
  i: Integer;
begin
  DbGrid.Columns.BeginUpdate;
  try
    DbGrid.Columns.Clear;
    for i := 0 to LV.Items.Count - 1 do
      if LV.Items[i].Checked then
        with DbGrid.Columns.Add do
        begin
          Field := LV.Items[i].Data;
          Width := StrToIntDef(Trim(LV.Items[i].Subitems[1]), GetFieldWidth(DbGrid, LV.Items[i].Data));
          Alignment := StrToAlign(LV.Items[i].Subitems[2]);
          Title.Alignment := StrToAlign(LV.Items[i].Subitems[3]);
          Title.Caption := LV.Items[i].Caption;
        end;
  finally
    DbGrid.Columns.EndUpdate;
  end;
end;
*)

function TRDbGridTuner.ShowDialog: Boolean;
begin
  Result := False;
  CheckDbLink;
  if CheckActive then
  begin
    with TFormDbGridTuner.Create(Application) do
    begin
      try
        StartWait;
        try
          ViewUpdateActiveFromGrid;
          FormStorageEnabled := not (csDesigning in Self.ComponentState);
          LoadFullData(Self);
        finally
          StopWait;
        end;
        if ShowModal = mrOk then
        begin
          StartWait;
          try
            SaveFullData;
            ApplyColsData(ViewActiveColsData);
            DoViewChange;
            Result := True;
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

{ ==  TFormDbGridTuner ========================================================= }

function TFormDbGridTuner.IsFieldExists(Fld: TField): Boolean;
var
  i, ICount: Integer;
begin
  Result := False;

  iCount := ListView.Items.Count - 1;
  for i := 0 to iCount do
    if Assigned(ListView.Items[i].Data) and (Fld = ListView.Items[i].Data) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TFormDbGridTuner.LoadFullData(const Tuner: TRDbGridTuner);
begin
  fTuner := Tuner;
  cbViews.Items.Assign(fTuner.fViews);
  cbViews.ItemIndex := fTuner.fActiveView;
  LoadColsData;
end;

procedure TFormDbGridTuner.SaveFullData;
begin
  fTuner.fViews.Assign(cbViews.Items);
  fTuner.fActiveView := cbViews.ItemIndex;
end;

procedure TFormDbGridTuner.LoadColsData;
var
  i, iCount: Integer;
  ColsData: TStringList;
  ColData: TRDGItemData;
  Fld: TField;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;

    ColsData := TStringList(cbViews.Items.Objects[cbViews.ItemIndex]);
    iCount := ColsData.Count - 1;
    for i := 0 to iCount do
    begin
      ColData := String2RDGItemData(fTuner.DbGrid, ColsData[i]);
      if ColData.FieldName <> EmptyStr then
      begin
        with ListView.Items.Add do
        begin
          Data := fTuner.DbGrid.DataSource.DataSet.FindField(ColData.FieldName);
          Caption := ColData.FieldTitle;
          Subitems.Add(ColData.FieldName);
          Subitems.Add(IntToStr(ColData.ColWidth));
          Subitems.Add(AlignToStr(ColData.DataAlign));
          Subitems.Add(AlignToStr(ColData.TitleAlign));
          Checked := True;
        end;
      end;
    end;

    iCount := fTuner.DbGrid.DataSource.DataSet.FieldCount - 1;
    for i := 0 to iCount do
    begin
      Fld := fTuner.DbGrid.DataSource.DataSet.Fields[i];
      if not IsFieldExists(Fld) then
      begin
        with ListView.Items.Add do
        begin
          Data := Fld;
          Caption := Fld.DisplayName;
          Subitems.Add(Fld.FieldName);
          Subitems.Add(IntToStr(GetFieldWidth(fTuner.DbGrid, Fld)));
          Subitems.Add(AlignToStr(Fld.Alignment));
          Subitems.Add(AlignToStr(Fld.Alignment));
          Checked := False;
        end;
      end;
    end;

    MoveUpCheckedItems(ListView);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TFormDbGridTuner.SaveColsData;
var
  i, iCount: Integer;
  ListItem: TListItem;
  ColsData: TStringList;
begin
  ColsData := TStringList(cbViews.Items.Objects[cbViews.ItemIndex]);

  ColsData.BeginUpdate;
  try
    ColsData.Clear;

    iCount := ListView.Items.Count - 1;
    for i := 0 to iCount do
    begin
      ListItem := ListView.Items[i];
      if ListItem.Checked then
        ColsData.Add(RDGItemData2String(CreateRDGItemData(
          ListItem.SubItems[0],
          ListItem.Caption,
          StrToIntDef(Trim(ListItem.Subitems[1]), GetFieldWidth(fTuner.DbGrid, ListItem.Data)),
          StrToAlign(ListItem.Subitems[2]),
          StrToAlign(ListItem.Subitems[3]))));
    end;
  finally
    ColsData.EndUpdate;
  end;
end;

procedure TFormDbGridTuner.ListViewDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
    Item.Data := nil;
end;

// Переместить элемент вверх ---------------------------------------------------
procedure TFormDbGridTuner.MoveUpUpdate(Sender: TObject);
begin
  MoveUp.Enabled := Assigned(ListView.Selected) and (ListView.Selected.Index > 0);
end;

procedure TFormDbGridTuner.MoveUpExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemUp(ListView);
    MoveUpCheckedItems(ListView);
    SaveColsData;
    ListView.SetFocus;
  finally
    ShowItemCount;
    StopWait;
  end;
end;

// Переместить элемент вниз ----------------------------------------------------
procedure TFormDbGridTuner.MoveDownUpdate(Sender: TObject);
begin
  MoveDown.Enabled := Assigned(ListView.Selected)
    and (ListView.Selected.Index < ListView.Items.Count - 1);
end;

procedure TFormDbGridTuner.MoveDownExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemDown(ListView);
    MoveUpCheckedItems(ListView);
    SaveColsData;
    ListView.SetFocus;
  finally
    ShowItemCount;
    StopWait;
  end;
end;

// Восстановить значения "по умолчанию" ----------------------------------------
procedure TFormDbGridTuner.SetDefaultUpdate(Sender: TObject);
begin
  SetDefault.Enabled := Assigned(fTuner) and Assigned(fTuner.DbGrid);
end;

procedure TFormDbGridTuner.SetDefaultExecute(Sender: TObject);
begin
  if QueryBoxStdNY(SQueryResetColumns) = IDYES then
  begin
    StartWait;
    try
      TStringList(cbViews.Items.Objects[cbViews.ItemIndex]).Assign(fTuner.fDesignCols);
      LoadColsData;
    finally
      StopWait;
    end;
  end;
end;

// Изменить настройки выделенного столбца --------------------------------------
procedure TFormDbGridTuner.PropertiesUpdate(Sender: TObject);
begin
  Properties.Enabled := Assigned(fTuner) and Assigned(fTuner.DbGrid) and Assigned(ListView.Selected);
end;

procedure TFormDbGridTuner.PropertiesExecute(Sender: TObject);
var
  R: TRect;
  P: TPoint;
begin
  with TFormDbGridTunerItem.Create(Self) do
  begin
    try
      StartWait;
      try
        // Вычисляем координаты редактора
        R := ListView.Selected.DisplayRect(drBounds);
        P.x := R.Left + ListView.Columns[0].Width + ListView.Columns[1].Width + 1;
        P.y := R.Bottom - 1;
        P := ListView.ClientToScreen(P);
        Top := P.y;
        Left := P.x;
        if Top + Height > Screen.DesktopHeight then
          Top := Screen.DesktopHeight - Height - PanelHeight;
        if Left + Width > Screen.DesktopWidth then
          Left := Screen.DesktopWidth - Width;
        // Заполняем поля
        NameComboBox.Items.Add(ListView.Selected.Caption);
        NameComboBox.Items.Add(TField(ListView.Selected.Data).DisplayName);
        NameComboBox.Text := ListView.Selected.Caption;
        PhysEdit.Text := ListView.Selected.Subitems[0];
        SpinEdit.Value := StrToIntDef(Trim(ListView.Selected.Subitems[1]),
          GetFieldWidth(fTuner.DbGrid, TField(ListView.Selected.Data)));
        DAComboBox.Items.Add(SAlgLeft);
        DAComboBox.Items.Add(SAlgRight);
        DAComboBox.Items.Add(SAlgCenter);
        DAComboBox.ItemIndex := Integer(StrToAlign(ListView.Selected.Subitems[2]));
        TAComboBox.Items.Add(SAlgLeft);
        TAComboBox.Items.Add(SAlgRight);
        TAComboBox.Items.Add(SAlgCenter);
        TAComboBox.ItemIndex := Integer(StrToAlign(ListView.Selected.Subitems[3]));
      finally
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        try
          ListView.Selected.Caption := NameComboBox.Text;
          ListView.Selected.Subitems[1] := IntToStr(SpinEdit.Value);
          ListView.Selected.Subitems[2] := AlignToStr(TAlignment(DAComboBox.ItemIndex));
          ListView.Selected.Subitems[3] := AlignToStr(TAlignment(TAComboBox.ItemIndex));
          SaveColsData;
        finally
          StopWait;
        end;
      end
    finally
      Free;
    end;
  end;
end;

procedure TFormDbGridTuner.ListViewDblClick(Sender: TObject);
begin
  if Properties.Enabled then PropertiesExecute(Sender);
end;

// Восстановить поля таблицы при следующем открытии окна -----------------------
procedure TFormDbGridTuner.RestoreColumnsUpdate(Sender: TObject);
begin
  RestoreColumns.Enabled := Assigned(fTuner) and fTuner.StoreInIniFile;
  RestoreColumns.Checked := ogStoreItemsState in fTuner.Options;
end;

procedure TFormDbGridTuner.RestoreColumnsExecute(Sender: TObject);
begin
  if ogStoreItemsState in fTuner.Options
  then fTuner.Options := fTuner.Options - [ogStoreItemsState]
  else fTuner.Options := fTuner.Options + [ogStoreItemsState];
end;

// Выталкивание отмеченных столбцов --------------------------------------------
procedure TFormDbGridTuner.ListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  HitTest: THitTests;
begin
  Item := ListView.GetItemAt(x, y);
  HitTest := ListView.GetHitTestInfoAt(x, y);
  if Assigned(Item) and (htOnStateIcon in HitTest)
  then ItemChecked := Item
  else ItemChecked := nil;
end;

procedure TFormDbGridTuner.ListViewClick(Sender: TObject);
begin
  if Assigned(ItemChecked) then
  begin
    StartWait;
    try
      ListView.Selected := ItemChecked;
      MoveUpCheckedItems(ListView);
      ScrollToSelectedItem(ListView);
      SaveColsData;
      ItemChecked := nil;
    finally
      StopWait;
    end;
  end;
end;

procedure TFormDbGridTuner.ListViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 32 then
  begin
    StartWait;
    try
      MoveUpCheckedItems(ListView);
      ScrollToSelectedItem(ListView);
      SaveColsData;
    finally
      StopWait;
    end;
  end;
end;

// Работа с представлениями ----------------------------------------------------
procedure TFormDbGridTuner.FormActivate(Sender: TObject);
begin
  inherited;

  //
end;

procedure TFormDbGridTuner.cbViewsChange(Sender: TObject);
begin
  StartWait;
  try
    LoadColsData;
  finally
    StopWait;
  end;
end;

procedure TFormDbGridTuner.ViewNewUpdate(Sender: TObject);
begin
  ViewNew.Enabled := fTuner.Active;
end;

procedure TFormDbGridTuner.ViewNewExecute(Sender: TObject);
var
  sViewName: string;
begin
  if InputQuery(SViewRename, SViewCaption, sViewName) then
  begin
    StartWait;
    try
      cbViews.ItemIndex := cbViews.Items.AddObject(sViewName, TStringList.Create);
      SaveColsData;
      cbViewsChange(nil);
    finally
      StopWait;
    end;
  end;
end;

procedure TFormDbGridTuner.ViewRenameUpdate(Sender: TObject);
begin
  ViewRename.Enabled := fTuner.Active and (cbViews.ItemIndex > -1);
end;

procedure TFormDbGridTuner.ViewRenameExecute(Sender: TObject);
var
  sViewName: string;
  iIndex: Integer;
begin
  sViewName := cbViews.Text;
  if InputQuery(SViewCreate, SViewCaption, sViewName) then
  begin
    iIndex := cbViews.ItemIndex;
    cbViews.ItemIndex := -1;
    cbViews.Items[iIndex] := sViewName;
    cbViews.ItemIndex := iIndex;
  end;
end;

procedure TFormDbGridTuner.ViewDeleteUpdate(Sender: TObject);
begin
  ViewDelete.Enabled := fTuner.Active and (cbViews.ItemIndex > -1) and (cbViews.Items.Count > 1);
end;

procedure TFormDbGridTuner.ViewDeleteExecute(Sender: TObject);
begin
  if QueryBoxYN(SViewDelete, SViewDeleteQuery) = ID_YES then
  begin
    cbViews.Items.Delete(cbViews.ItemIndex);
    cbViews.ItemIndex := 0;
    cbViewsChange(nil);
  end;
end;

procedure TFormDbGridTuner.ToolBarViewResize(Sender: TObject);
begin
  cbViews.Width := ToolBarView.ClientWidth - 4
                 - SeparatorViews.Width
                 - btnViewNew.Width
                 - btnViewRename.Width
                 - btnViewDelete.Width;
end;

end.
