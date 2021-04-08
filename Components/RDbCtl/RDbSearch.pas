unit RDbSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView,
  Db, RDbCustom, RDbCustomSearch;

type

  TRDbSearch = class(TRDbCustomFind)
  private
    FKeyField: string;
    FListFields: string;
    FDefValue: string;
    FResValue: Integer;
    FGotoRecord: Boolean;
    procedure SetKeyField(const Value: string);
    procedure SetListFields(const Value: string);
  protected
    function  IsKeyField: Boolean;
    function  GetKeyValue: Integer;
    procedure InternalInit; override;
    function  GetIniSection: string; override;
  public
    function ShowDialogEx(const DefField, DefText: string; const LoadPrm: Boolean): Boolean;
    function ShowDialog: Boolean; override;
  published
    property KeyField: string read FKeyField write SetKeyField;
    property ListFields: string read FListFields write SetListFields;
    property DefaultValue: string read FDefValue write FDefValue;
    property ResultValue: Integer read FResValue;
    property GotoToSelRecord: Boolean read FGotoRecord write FGotoRecord default True;
  end;

  TFormDbSearch = class(TDialogTemplate)
    FindBtn: TBitBtn;
    FindGroupBox: TGroupBox;
    ListViewLabel: TLabel;
    ListView: TRSortListView;
    FieldComboBoxLabel: TLabel;
    FieldComboBox: TComboBox;
    FindEditLabel: TLabel;
    FindEdit: TComboBox;
    CaseCheckBox: TCheckBox;
    PartCheckBox: TCheckBox;
    StopBtn: TBitBtn;
    procedure FormActivate(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FindBtnClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure UpdateFind(Sender: TObject);
    procedure UpdateList(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
  private
    Terminate: Boolean;
  public
    DbSearch: TRDbSearch;
    procedure CreateColumns(DS: TDataSet; FindField: TField; ListFields: string);
  end;

{ == "Быстрый" поиск =========================================================== }
function FastSeek(DS: TDataSet; const AKeyField, AFindField, AListFields, ADefaultValue: string): Integer;

implementation

{$R *.dfm}

uses
  RVclUtils, RxStrUtils;

resourcestring
  SSearchResults       = 'Найденные записи (всего найдено %d записей):';
  SSearchProgress      = 'Идет поиск (выполнено: %d%%, найдено %d записей)...';

  EFieldNotFound       = 'В наборе данных "%s" поле с именем "%s" не найдено!';

const
  iniSearch           = 'SEARCH_%s.%s';
  FmtValue            = '%s;%d;%d;%s';

  IncValue1           = 1.27;
  IncValue2           = 10;

{ == "Быстрый" поиск =========================================================== }
function FastSeek(DS: TDataSet; const AKeyField, AFindField, AListFields, ADefaultValue: string): Integer;
begin
  Result := intDisable;
  with TRDbSearch.Create(DS.Owner) do
  begin
    try
      DataSet := DS;
      KeyField := AKeyField;
      DefaultField := AFindField;
      DefaultValue := ADefaultValue;
      ListFields := AListFields;
      GotoToSelRecord := False;
      StoreInIniFile := True;
      if ShowDialog then Result := ResultValue;
    finally
      Free;
    end;
  end;
end;

{ == TRDbSearch ================================================================ }
procedure TRDbSearch.InternalInit;
begin
  inherited;
  FKeyField := EmptyStr;
  FListFields := EmptyStr;
  FResValue := intDisable;
  FGotoRecord := True;
  FHiddenFields := False;
end;

function TRDbSearch.GetIniSection: string;
begin
  CheckDbLink;
  Result := AnsiUpperCase(Format(iniSearch, [OwnerName, DataSet.Name]) + GetIniSectionTag);
end;

procedure TRDbSearch.SetKeyField(const Value: string);
begin
  if Value <> FKeyField then
  begin
    if Assigned(DataSet) then
    begin
      if (Value = EmptyStr) or Assigned(DataSet.FindField(Value))
      then FKeyField := Value
      else raise Exception.CreateFmt(EFieldNotFound, [DataSet.Name, Value]);
    end
    else FKeyField := Value;
  end;
end;

procedure TRDbSearch.SetListFields(const Value: string);
var
  i: Integer;
begin
  if Value <> FListFields then
  begin
    if Assigned(DataSet) then
    begin
      for i := 1 to WordCount(Value, chDivChars) do
        if DataSet.FindField(Trim(ExtractWord(i, Value, chDivChars))) = nil then
          raise Exception.CreateFmt(EFieldNotFound,
            [DataSet.Name, Trim(ExtractWord(i, Value, chDivChars))]);
    end;
    FListFields := Value;
  end;
end;

function TRDbSearch.IsKeyField: Boolean;
begin
  Result := Assigned(DataSet) and Assigned(DataSet.FindField(FKeyField));
end;

function TRDbSearch.GetKeyValue: Integer;
var
  KeyFld: TField;
begin
  Result := intDisable;
  if Assigned(DataSet) then
  begin
    KeyFld := DataSet.FindField(FKeyField);
    if Assigned(KeyFld) then
      Result := KeyFld.AsInteger;
  end;
end;

function TRDbSearch.ShowDialogEx(const DefField, DefText: string; const LoadPrm: Boolean): Boolean;
var
  FindFld, FindText: string;
  i, FindWord, FindCase: Integer;
begin
  Result := False;
  CheckDbLink;
  CheckActive;
  with TFormDbSearch.Create(Application) do
  begin
    try
      StartWait;
      try
        DbSearch := Self;
        // Загружаем значения по умолчанию
        FindFld := DefField;
        FindText := DefText;
        FindWord := 0;
        FindCase := 0;
        // Декодируем последнюю запись
        if LoadPrm and (Trim(FindText) = EmptyStr)
          and (FLastFind <> EmptyStr)
          and (WordCount(FLastFind, [DelimChar]) > 3) then
        begin
          FindFld := Trim(ExtractWord(1, FLastFind, [DelimChar]));
          FindWord := StrToIntDef(Trim(ExtractWord(2, FLastFind, [DelimChar])), 0);
          FindCase := StrToIntDef(Trim(ExtractWord(3, FLastFind, [DelimChar])), 0);
          for i := 4 to WordCount(FLastFind, [DelimChar]) do
          begin
            if FindText <> EmptyStr then FindText := FindText + DelimChar;
            FindText := FindText + ExtractWord(i, FLastFind, [DelimChar]);
          end;
        end;
        // Загружаем поля
        LoadKeyFieldsToCmbBox(FieldComboBox, DataSet, FindFld, True, True, UseHiddenFields);
        // Загружаем историю
        if olUseHistory in Options then FindEdit.Items.Assign(FHistory);
        // Создаем столбцы "по умолчанию" в списке
        CreateColumns(DataSet, nil, FListFields);
        // Вводим значения по умолчанию
        FindEdit.Text := FindText;
        PartCheckBox.Checked := FindWord <> 0;
        CaseCheckBox.Checked := FindCase <> 0;
      finally
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        try
          // Сохраняем данные поиска
          if PartCheckBox.Checked then FindWord := 1 else FindWord := 0;
          if CaseCheckBox.Checked then FindCase := 1 else FindCase := 0;
          FindFld := TField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).FieldName;
          FindText := FindEdit.Text;
          FLastFind := Format(FmtValue, [FindFld, FindWord, FindCase, FindText]);
          // Добавляем строку в историю
          if (olUseHistory in FOptions) and (FHistory.IndexOf(FindText) = -1) then
            FHistory.Insert(0, FindText);
          // Переходим на выбранную запись
          if Assigned(ListView.Selected) then
          begin
            if IsKeyField then
            begin
              // Записываем результат
              FResValue := TId(ListView.Selected.Data)^;
              Result := FResValue > intDisable;
              // Переходим на выбранную запись
              if FGotoRecord
              then Result := DataSet.Locate(FKeyField, FResValue, []);
            end
            else begin
              // Переходим на выбранную запись
              Result := DataSet.BookmarkValid(ListView.Selected.Data);
              if Result then
              begin
                try
                  DataSet.GotoBookmark(ListView.Selected.Data);
                except
                  Result := False;
                  raise;
                end;
              end;
            end;
          end;
        finally
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TRDbSearch.ShowDialog: Boolean;
begin
  Result := ShowDialogEx(DefaultField, DefaultValue, True);
end;

{ == TFormDbSearch ============================================================= }

procedure TFormDbSearch.FormActivate(Sender: TObject);
begin
  inherited;
  UpdateFind(Sender);
end;

procedure TFormDbSearch.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateList(Sender);
end;

procedure TFormDbSearch.UpdateFind(Sender: TObject);
begin
  FindBtn.Default := True;
  FindBtn.Enabled := FindEdit.Text <> EmptyStr;
  OkBtn.Default := False;
  OkBtn.Enabled := False;
  ButtonsPanel.TabOrder := 1;
end;

procedure TFormDbSearch.UpdateList(Sender: TObject);
begin
  FindBtn.Default := False;
  FindBtn.Enabled := False;
  OkBtn.Default := True;
  OkBtn.Enabled := ListView.Selected <> nil;
  ButtonsPanel.TabOrder := 2;
end;

procedure TFormDbSearch.CreateColumns(DS: TDataSet; FindField: TField; ListFields: string);
var
  i: Integer;
  Field: TField;

begin
  ListView.Columns.BeginUpdate;
  try
    ListView.Columns.Clear;
    if Assigned(FindField) then
    begin
      with ListView.Columns.Add do
      begin
        Tag := FindField.Index;
        Alignment := FindField.Alignment;
        Width := Trunc(ListView.Canvas.TextWidth(FindField.DisplayLabel) * IncValue1) + IncValue2;
        Caption := FindField.DisplayLabel;
      end;
    end;
    for i := 1 to WordCount(ListFields, chDivChars) do
    begin
      Field := DS.FindField(Trim(ExtractWord(i, ListFields, chDivChars)));
      if Assigned(Field) and (Field <> FindField) then
      begin
        with ListView.Columns.Add do
        begin
          Tag := Field.Index;
          Alignment := Field.Alignment;
          Width := Trunc(ListView.Canvas.TextWidth(Field.DisplayLabel) * IncValue1) + IncValue2;
          Caption := Field.DisplayLabel;
        end;
      end;
    end;
  finally
    ListView.Columns.EndUpdate;
  end;
end;

procedure TFormDbSearch.FindBtnClick(Sender: TObject);
var
  Key: TId;
  i, ItemWidth, ProgressCnt: Integer;
  ItemText: string;
  RecordOk: Boolean;
  RecordItem: TListItem;
  FindField: TField;
  CurrPos: TBookmark;
begin
  with DbSearch do
  begin
    CheckDbLink;
    CheckActive;
    // StartWait;
    // DisableControls;
    // Переключаем доступность элементов управления
    Terminate := False;
    FindGroupBox.Enabled := False;
    FindBtn.Visible := False;
    OkBtn.Enabled := False;
    CancelBtn.Enabled := False;
    StopBtn.Visible := True;
    StopBtn.Enabled := True;
    StopBtn.SetFocus;
    Application.ProcessMessages;
    try
      ListView.Items.Clear;
      FindField := TField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]);
      if Assigned(FindField) then
      begin
        // Пересоздаем столбцы и сбрасываем сортировку
        CreateColumns(DataSet, FindField, FListFields);
        ListView.SortColumn := intDisable;
        // Выполняем поиск записей
        DataSet.DisableControls;
        try
          CurrPos := DataSet.GetBookmark;
          try
            ProgressCnt := 0;
            DataSet.First;
            while not DataSet.Eof do
            begin
              Application.ProcessMessages;
              if Terminate then Break;
              if PartCheckBox.Checked then
              begin
                // Только слово целиком
                if CaseCheckBox.Checked
                then RecordOk := AnsiSameStr(FindField.DisplayText, FindEdit.Text)
                else RecordOk := AnsiSameText(FindField.DisplayText, FindEdit.Text);
              end
              else begin
                // Любая часть слова
                if CaseCheckBox.Checked
                then RecordOk := Pos(FindEdit.Text, FindField.DisplayText) > 0
                else RecordOk := Pos(AnsiUpperCase(FindEdit.Text), AnsiUpperCase(FindField.DisplayText)) > 0;
              end;
              Application.ProcessMessages;
              if Terminate then Break;
              // Если строка удовлетворяет условиям, то добавляем в список
              if RecordOk then
              begin
                RecordItem := ListView.Items.Add;
                if IsKeyField then
                begin
                  New(Key);
                  Key^ := GetKeyValue;
                  RecordItem.Data := Key;
                end
                else RecordItem.Data := DataSet.GetBookmark;
                for i := 0 to ListView.Columns.Count - 1 do
                begin
                  ItemText := DataSet.Fields[ListView.Columns[i].Tag].DisplayText;
                  ItemWidth := Trunc(ListView.Canvas.TextWidth(ItemText) * IncValue1) + IncValue2;
                  if ItemWidth > ListView.Columns[i].Width
                  then ListView.Columns[i].Width := ItemWidth;
                  if i = 0
                  then RecordItem.Caption := ItemText
                  else RecordItem.Subitems.Add(ItemText);
                end;
              end;
              // Следующая запись
              Inc(ProgressCnt);
              ListViewLabel.Caption := Format(SSearchProgress,
                [Round((ProgressCnt / DataSet.RecordCount) * 100), ListView.Items.Count]);
              Application.ProcessMessages;
              if Terminate then Break;
              DataSet.Next;
            end;
            // Выводим результат
            ListViewLabel.Caption := Format(SSearchResults, [ListView.Items.Count]);
            ListView.Update;
          finally
            try
              DataSet.GotoBookmark(CurrPos);
            except
            end;
            DataSet.FreeBookmark(CurrPos);
          end;
        finally
          DataSet.EnableControls;
        end;
      end;
    finally
      // Восстанавливаем элементы управления 
      FindGroupBox.Enabled := True;
      OkBtn.Enabled := ListView.Selected <> nil;
      CancelBtn.Enabled := True;
      StopBtn.Visible := False;
      FindBtn.Visible := True;
      // EnableControls;
      // StopWait;
    end;
  end;
  if ListView.Items.Count > 0 then
  begin
    ListView.SetFocus;
    if ListView.Selected = nil then
      ListView.Selected := ListView.TopItem;
  end
  else FindEdit.SetFocus;
end;

procedure TFormDbSearch.ListViewDblClick(Sender: TObject);
begin
  if OkBtn.Enabled then OkBtn.Click;
end;

procedure TFormDbSearch.ListViewDeletion(Sender: TObject; Item: TListItem);
var
  Key: TId;
  Bmk: TBookmark;
begin
  if Item.Data <> nil then
    with DbSearch do
    begin
      if IsKeyField then
      begin
        Key := Item.Data;
        Item.Data := nil;
        Dispose(Key);
      end
      else begin
        Bmk := Item.Data;
        Item.Data := nil;
        DataSet.FreeBookmark(Bmk);
      end;
    end;
end;

procedure TFormDbSearch.StopBtnClick(Sender: TObject);
begin
  Terminate := True;
  StopBtn.Enabled := not Terminate;
end;

procedure TFormDbSearch.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FindGroupBox.Enabled;
end;

end.
