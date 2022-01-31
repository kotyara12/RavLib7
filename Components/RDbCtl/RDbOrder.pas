unit RDbOrder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplEditors, Menus, ActnList, ImgList, ComCtrls, ToolWin, RDbCustom;

type

{ == TRDbOrder ================================================================= }

  TRDOOption = (ooStoreItemsState, ooChangeOptions);

  TRDOOptions = set of TRDOOption;

  TRDODirection = (odNone, odAsc, odDesc);

  TRDbOrder = class(TRDbCustomDS)
  private
    fOrder: string;
    fOrder_Def: string;
    fOptions: TRDOOptions;
    procedure SetOrderString(const AValue: string);
    procedure SetOptions(const AValue: TRDOOptions);
    procedure ReadListView(LV: TListView);
  protected
    procedure InternalInit; override;
    procedure InternalDone; override;
    procedure InternalReset; override;
    function  IsStoreOptions: Boolean; override;
    function  GetIniSection: string; override;
  public
    procedure ClearOrder;
    procedure ResetOrder;
    procedure LoadListView(LV: TListView; const OrderStr: string);
    procedure LoadData; override;
    procedure SaveData; override;
    function  ShowDialog: Boolean; override;
    function  GetOrderString: string;
    function  GetFieldDirection(const FieldName: string): TRDODirection;
    procedure SetFieldDirection(const FieldName: string; const Direction: TRDODirection);
    property  Item_OrderString: string read GetOrderString write SetOrderString;
  published
    property OrderString: string read fOrder_Def write fOrder_Def;
    property Options: TRDOOptions read fOptions write SetOptions;
  end;

{ == Editor TRDbOrder ========================================================== }
  TFormDbOrder = class(TEditorsTemplate)
    MoveUp: TAction;
    MoveDown: TAction;
    itemMoveUpP: TMenuItem;
    itemMoveDownP: TMenuItem;
    divPopup2: TMenuItem;
    itemMoveUp: TMenuItem;
    itemMoveDown: TMenuItem;
    AddItem: TAction;
    DelItem: TAction;
    ClearAll: TAction;
    ResetAll: TAction;
    N1: TMenuItem;
    divEdit2: TMenuItem;
    itemAddItem: TMenuItem;
    itemDelItem: TMenuItem;
    itemClearAll: TMenuItem;
    itemResetAll: TMenuItem;
    divPopup3: TMenuItem;
    divPopup4: TMenuItem;
    itemAddItemP: TMenuItem;
    itemDelItemP: TMenuItem;
    ietmClearAllP: TMenuItem;
    itemResetAllP: TMenuItem;
    RestoreOrder: TAction;
    itemRestoreOrderP: TMenuItem;
    itemRestoreOrder: TMenuItem;
    divService1: TMenuItem;
    MoveUpToolButton: TToolButton;
    MoveDownToolButton: TToolButton;
    Separator1: TToolButton;
    AddItemToolButton: TToolButton;
    DelItemToolButton: TToolButton;
    Separator2: TToolButton;
    CloseOkToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
    procedure MoveUpUpdate(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveDownUpdate(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure AddItemUpdate(Sender: TObject);
    procedure AddItemExecute(Sender: TObject);
    procedure DelItemUpdate(Sender: TObject);
    procedure DelItemExecute(Sender: TObject);
    procedure ClearAllUpdate(Sender: TObject);
    procedure ClearAllExecute(Sender: TObject);
    procedure ResetAllUpdate(Sender: TObject);
    procedure ResetAllExecute(Sender: TObject);
    procedure RestoreOrderUpdate(Sender: TObject);
    procedure RestoreOrderExecute(Sender: TObject);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
  private
    { Private declarations }
  public
    Order: TRDbOrder;
  end;

implementation

{$R *.dfm}

uses
  Db, IniFiles, RxStrUtils, RVclUtils, RListView, RDbOrderItem;

resourcestring
  SAsc                = 'по возрастанию';
  SDesc               = 'по убыванию';

  SDeleteItem         = 'Удалить поле "%s" из списка сортировки?';
  SLoadDefaults       = 'Загрузить список сортировки "по умолчанию"?';

const
  imAsc               = 11;
  imDesc              = 12;

  iniOrder            = 'ORDER_%s.%s';
  iniOrderValue       = 'OrderString';

  sqlAsc              = ' ASC';
  sqlDesc             = ' DESC';

  DivChars            = [',', ';'];
  DivChar             = ', ';

{ == Utilites ================================================================== }

function UpperPos(const Substr, S: string): Integer;
begin
  Result := Pos(AnsiUpperCase(Substr), AnsiUpperCase(S));
end;

procedure ExtractFieldName(const Item: string; out FieldName: string; out Desc: Boolean);
var
  P: Integer;
begin
  if Item = EmptyStr then
  begin
    FieldName := Item;
    Desc := False;
  end
  else begin
    P := UpperPos(sqlDesc, Item);
    if P > 0 then
    begin
      Desc := True;
      FieldName := Copy(Item, 1, P - 1);
    end
    else begin
      Desc := False;
      P := UpperPos(sqlAsc, Item);
      if P > 0
      then FieldName := Copy(Item, 1, P - 1)
      else FieldName := Item;
    end;
    FieldName := Trim(FieldName);
  end;
end;

{ == TRDbOrder ================================================================= }

procedure TRDbOrder.InternalInit;
begin
  fOrder := EmptyStr;
  fOrder_Def := EmptyStr;
  fOptions := [ooStoreItemsState, ooChangeOptions];
end;

procedure TRDbOrder.InternalDone;
begin
end;

procedure TRDbOrder.SetOrderString(const AValue: string);
var
  i: Integer;
  FldName: string;
  FldDesc: Boolean;
begin
  if fOrder <> AValue then
  begin
    CheckDbLink;
    if (AValue = EmptyStr) or ((DataSet.Fields.Count = 0) and not DataSet.Active) then
      fOrder := AValue
    else begin
      fOrder := EmptyStr;
      for i := 1 to WordCount(AValue, DivChars) do
      begin
        ExtractFieldName(Trim(ExtractWord(i, AValue, DivChars)), FldName, FldDesc);
        if (FldName <> EmptyStr) and (DataSet.FindField(FldName) <> nil) then
        begin
          if fOrder <> EmptyStr then fOrder := fOrder + DivChar;
          if FldDesc
          then fOrder := fOrder + FldName + sqlDesc
          else fOrder := fOrder + FldName + sqlAsc;
        end;
      end;
    end;
  end;
end;

function TRDbOrder.GetFieldDirection(const FieldName: string): TRDODirection;
var
  i: Integer;
  FldName: string;
  FldDesc: Boolean;
begin
  Result := odNone;
  for i := 1 to WordCount(fOrder, DivChars) do
  begin
    ExtractFieldName(Trim(ExtractWord(i, fOrder, DivChars)), FldName, FldDesc);
    if SameText(FldName, FieldName) then
    begin
      if FldDesc then Result := odDesc else Result := odAsc;
      Break;
    end;
  end;
end;

procedure TRDbOrder.SetFieldDirection(const FieldName: string; const Direction: TRDODirection);
begin
  case Direction of
    odAsc: SetOrderString(FieldName + sqlAsc);
    odDesc: SetOrderString(FieldName + sqlDesc);
  end;
end;

procedure TRDbOrder.InternalReset;
begin
  SetOrderString(fOrder_Def);
end;

procedure TRDbOrder.ClearOrder;
begin
  SetOrderString(EmptyStr);
end;

procedure TRDbOrder.ResetOrder;
begin
  InternalReset;
end;

function TRDbOrder.IsStoreOptions: Boolean;
begin
  Result := (ooStoreItemsState in fOptions) or (ooChangeOptions in fOptions);
end;

procedure TRDbOrder.SetOptions(const AValue: TRDOOptions);
begin
  if fOptions <> AValue then
  begin
    fOptions := AValue;
    if not ((csLoading in ComponentState) or (csDestroying in ComponentState))
    and (csDesigning in ComponentState) then CheckOptions;
  end;
end;

function TRDbOrder.GetIniSection: string;
begin
  CheckDbLink;
  Result := AnsiUpperCase(Format(iniOrder, [OwnerName, DataSet.Name]) + GetIniSectionTag);
end;

procedure TRDbOrder.LoadData;
var
  Ini: TMemIniFile;
  strFN, strSN: string;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    if ooChangeOptions in fOptions then
    begin
      strFN := GetIniFileName;
      strSN := GetIniSection;
      Ini := TMemIniFile.Create(strFN);
      if Assigned(Ini) then
      begin
        try
          if Ini.ReadBool(strSN, iniRestore, ooStoreItemsState in fOptions)
          then fOptions := fOptions + [ooStoreItemsState]
          else fOptions := fOptions - [ooStoreItemsState];
          if ooStoreItemsState in fOptions then
            SetOrderString(Ini.ReadString(strSN, iniOrderValue, fOrder_Def));
        finally
          Ini.Free;
        end;
      end;
    end;
  end;
end;

procedure TRDbOrder.SaveData;
var
  Ini: TMemIniFile;
  strFN, strSN: string;
begin
  if StoreInIniFile then
  begin
    CheckDbLink;
    if (ooChangeOptions in fOptions) or (ooStoreItemsState in fOptions) then
    begin
      strFN := GetIniFileName;
      strSN := GetIniSection;
      Ini := TMemIniFile.Create(strFN);
      if Assigned(Ini) then
      begin
        try
          if ooChangeOptions in fOptions then
            Ini.WriteBool(strSN, iniRestore, ooStoreItemsState in fOptions);
          Ini.WriteString(strSN, iniOrderValue, fOrder);
          Ini.UpdateFile;
        finally
          Ini.Free;
        end;
      end;
    end;
  end;
end;

procedure TRDbOrder.LoadListView(LV: TListView; const OrderStr: string);
var
  i: Integer;
  FldName: string;
  FldDesc: Boolean;
  Field: TField;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    for i := 1 to WordCount(OrderStr, DivChars) do
    begin
      ExtractFieldName(Trim(ExtractWord(i, OrderStr, DivChars)), FldName, FldDesc);
      if (FldName <> EmptyStr) then
      begin
        Field := DataSet.FindField(FldName);
        if Field <> nil then
        begin
          with LV.Items.Add do
          begin
            Data := Field;
            Caption := Field.DisplayName;
            if FldDesc then
            begin
              ImageIndex := imDesc;
              Subitems.Add(SDesc);
            end
            else begin
              ImageIndex := imAsc;
              Subitems.Add(SAsc);
            end;
          end;
        end;
      end;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TRDbOrder.ReadListView(LV: TListView);
var
  i: Integer;
  Item: TListItem;
begin
  fOrder := EmptyStr;
  for i := 0 to LV.Items.Count - 1 do
  begin
    Item := LV.Items[i];
    if fOrder <> EmptyStr then fOrder := fOrder + DivChar;
    if Item.ImageIndex = imDesc
    then fOrder := fOrder + TField(Item.Data).FieldName + sqlDesc
    else fOrder := fOrder + TField(Item.Data).FieldName + sqlAsc;
  end;
end;

function TRDbOrder.ShowDialog: Boolean;
begin
  Result := False;
  CheckDbLink;
  if CheckActive then
  begin
    with TFormDbOrder.Create(Application) do
    begin
      try
        StartWait;
        try
          Order := Self;
          FormStorageEnabled := not (csDesigning in Self.ComponentState);
          LoadListView(ListView, fOrder);
        finally
          StopWait;
        end;
        Result := ShowModal = mrOk;
        if Result then begin
          StartWait;
          try
            ReadListView(ListView);
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

function TRDbOrder.GetOrderString: string;
begin
  Result := EmptyStr;
  if CheckActive then Result := fOrder;
end;

{ == Editor TRDbOrder ========================================================== }

procedure TFormDbOrder.ListViewDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
    Item.Data := nil;
end;

// Переместить элемент вверх ---------------------------------------------------
procedure TFormDbOrder.MoveUpUpdate(Sender: TObject);
begin
  MoveUp.Enabled := Assigned(ListView.Selected) and (ListView.Selected.Index > 0)
    and IsNotWait;
end;

procedure TFormDbOrder.MoveUpExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemUp(ListView);
    ListView.SetFocus;
  finally
    ShowItemCount;
    StopWait;
  end;
end;

// Переместить элемент вниз ----------------------------------------------------
procedure TFormDbOrder.MoveDownUpdate(Sender: TObject);
begin
  MoveDown.Enabled := Assigned(ListView.Selected) and IsNotWait
    and (ListView.Selected.Index < ListView.Items.Count - 1);
end;

procedure TFormDbOrder.MoveDownExecute(Sender: TObject);
begin
  StartWait;
  try
    MoveSelectedItemDown(ListView);
    ListView.SetFocus;
  finally
    ShowItemCount;
    StopWait;
  end;
end;

// Добавить поле в список сортировки -------------------------------------------
procedure TFormDbOrder.AddItemUpdate(Sender: TObject);
begin
  AddItem.Enabled := Assigned(Order) and Assigned(Order.DataSet) and IsNotWait;
end;

procedure TFormDbOrder.AddItemExecute(Sender: TObject);
var
  Item: TListItem;
  Data: TOrderItem;

  function FindItem(F: TField): TListItem;
  var
    i: Integer;
  begin
    Result := nil;
    if F = nil then Exit;
    for i := 0 to ListView.Items.Count - 1 do
    begin
      if TField(ListView.Items[i].Data) = F then
      begin
        Result:= ListView.Items[i];
        Break;
      end;
    end;
  end;

begin
  Data.Field := Nil;
  Data.Asc := True;
  if EditOrderItem(Order.DataSet, Data) then begin
    StartWait;
    try
      Item := FindItem(Data.Field);
      if Assigned(Item) then Item.Subitems.Clear
      else begin
        Item := ListView.Items.Add;
        Item.Data := Data.Field;
        Item.Caption := Data.Field.DisplayName;
      end;
      if Data.Asc then begin
        Item.ImageIndex := imAsc;
        Item.Subitems.Add(SAsc);
      end
      else begin
        Item.ImageIndex := imDesc;
        Item.Subitems.Add(SDesc);
      end;
      ListView.Selected := Item;
      ShowItemCount;
    finally
      StopWait;
    end;
  end;
end;

// Удалить выделенное поле из списка сортировки --------------------------------
procedure TFormDbOrder.DelItemUpdate(Sender: TObject);
begin
  DelItem.Enabled := Assigned(ListView.Selected) and IsNotWait;
end;

procedure TFormDbOrder.DelItemExecute(Sender: TObject);
var
  Idx: Integer;
begin
  if Application.MessageBox(PChar(Format(SDeleteItem,
     [ListView.Selected.Caption])), PChar(Application.Title),
     MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = IDYES then
  begin
    StartWait;
    try
      Idx := ListView.Selected.Index;
      ListView.Items.Delete(Idx);
      if Idx < ListView.Items.Count
      then ListView.Selected := ListView.Items[Idx]
      else ListView.Selected := nil;
      ShowItemCount;
    finally
      StopWait;
    end;
  end;
end;

// Очистить список сортировки --------------------------------------------------
procedure TFormDbOrder.ClearAllUpdate(Sender: TObject);
begin
  ClearAll.Enabled := (ListView.Items.Count > 0) and IsNotWait;
end;

procedure TFormDbOrder.ClearAllExecute(Sender: TObject);
begin
  if Application.MessageBox('Очистить список сортировки?',
     PChar(Application.Title), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = IDYES then
  begin
    StartWait;
    try
      ListView.Items.Clear;
      ShowItemCount;
    finally
      StopWait;
    end;
  end;
end;

// Установить сортировку "по умолчанию" ----------------------------------------
procedure TFormDbOrder.ResetAllUpdate(Sender: TObject);
begin
  ResetAll.Enabled := Assigned(Order) and Assigned(Order.DataSet) and IsNotWait;
end;

procedure TFormDbOrder.ResetAllExecute(Sender: TObject);
begin
  if Application.MessageBox(PChar(SLoadDefaults), PChar(Application.Title),
    MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = IDYES then
  begin
    Order.LoadListView(ListView, Order.OrderString);
    ShowItemCount;
  end;
end;

// Автоматическая запись состояния на диск -------------------------------------
procedure TFormDbOrder.RestoreOrderUpdate(Sender: TObject);
begin
  RestoreOrder.Enabled := Assigned(Order) and Order.StoreInIniFile;
  RestoreOrder.Checked := ooStoreItemsState in Order.Options;
end;

procedure TFormDbOrder.RestoreOrderExecute(Sender: TObject);
begin
  if ooStoreItemsState in Order.Options
  then Order.Options := Order.Options - [ooStoreItemsState]
  else Order.Options := Order.Options + [ooStoreItemsState];
end;

end.
