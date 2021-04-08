unit RDFItemsEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DesignIntf, DesignEditors, DesignWindows, RDbFilter, StdCtrls, Menus, ImgList,
  ActnList, ToolWin, ComCtrls;

type
  TRDFItemsEdit = class(TDesignWindow)
    ListBox: TListBox;
    PopupMenu: TPopupMenu;
    ActionList: TActionList;
    ImageList: TImageList;
    ToolBar: TToolBar;
    AddItem: TAction;
    AddItemToolButton: TToolButton;
    Separator1: TToolButton;
    itemAdditem: TMenuItem;
    N1: TMenuItem;
    DelItem: TAction;
    DelItemToolButton: TToolButton;
    itemDelItem: TMenuItem;
    MoveUp: TAction;
    MoveDown: TAction;
    itemMoveUp: TMenuItem;
    itemMoveDown: TMenuItem;
    MoveUpToolButton: TToolButton;
    MoveDownToolButton: TToolButton;
    DelAll: TAction;
    itemClear: TMenuItem;
    DelAllToolButton: TToolButton;
    Separator2: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxClick(Sender: TObject);
    procedure AddItemUpdate(Sender: TObject);
    procedure AddItemExecute(Sender: TObject);
    procedure DelItemUpdate(Sender: TObject);
    procedure DelItemExecute(Sender: TObject);
    procedure MoveDownUpdate(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure MoveUpUpdate(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure DelAllUpdate(Sender: TObject);
    procedure DelAllExecute(Sender: TObject);
  private
  protected
    function UniqueName(Component: TComponent): string; override;
  public
    Filter: TRDbFilter;
    procedure LoadItems;
    procedure UpdateSelection;
  end;

type
  TDesigner = IDesigner;
  // TFormDesigner = IFormDesigner;

procedure EditRDbFilterItems(Designer: TDesigner; AFilter: TRDbFilter);

implementation

uses
  RDFItemCreate;

{$R *.DFM}

function FindEditor(AFilter: TRDbFilter): TRDFItemsEdit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TRDFItemsEdit then begin
      if TRDFItemsEdit(Screen.Forms[I]).Filter = AFilter then
      begin
        Result := TRDFItemsEdit(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure EditRDbFilterItems(Designer: TDesigner; AFilter: TRDbFilter);
var
  Editor: TRDFItemsEdit;
begin
  if AFilter <> nil then
  begin
    Editor := FindEditor(AFilter);
    if Editor = nil then
    begin
      Editor := TRDFItemsEdit.Create(Application);
      try
        Editor.Caption := Format('%s', [AFilter.Name]);
        Editor.Designer := Designer;
        Editor.Filter := AFilter;
        Editor.LoadItems;
        Editor.Show;
      except
        Editor.Free;
        raise;
      end;
    end
    else begin
      Editor.Caption := Format('%s', [AFilter.Name]);
      Editor.Show;
      if Editor.WindowState = wsMinimized then
        Editor.WindowState := wsNormal;
    end;
  end;
end;

{ == TRDFItemsEdit ============================================================= }

function GenerateName(Filter: TRDbFilter; FieldName: string;
  ItemClass: TRDFItemClass; Number: Integer): string;
var
  IntName: string;
begin
  if FieldName = EmptyStr then
  begin
    if ItemClass <> nil
    then IntName := Copy(ItemClass.ClassName, 2, Length(ItemClass.ClassName) - 1)
    else IntName := 'Item';
  end
  else IntName := Trim(FieldName);

  if Filter.Owner is TFrame
  then Result := Format('%s_%s_%s%d', [TFrame(Filter.Owner).Name, Filter.Name, IntName, Number])
  else Result := Format('%s_%s%d', [Filter.Name, IntName, Number]);
end;

function CreateUniqueName(Filter: TRDbFilter; const FieldName: string;
  ItemClass: TRDFItemClass; Component: TComponent): string;
var
  i: Integer;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    with Filter.Owner do
      for i := 0 to ComponentCount - 1 do
        if (Component <> Components[i]) and (CompareText(AName, TRDFItem(Components[i]).Name) = 0) then Exit;
    Result := True;
  end;

begin
  for i := 1 to MaxInt do
  begin
    Result := GenerateName(Filter, FieldName, ItemClass, i);
    if IsUnique(Result) then Exit;
  end;
end;

function TRDFItemsEdit.UniqueName(Component: TComponent): string;
begin
  Result := CreateUniqueName(Filter, TRDFItem(Component).FieldName,
    TRDFItemClass(Component.ClassType), Component);
end;

procedure TRDFItemsEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRDFItemsEdit.LoadItems;
var
  i: Integer;
  Item: TRDFItem;
begin
  ListBox.Items.Clear;
  if Filter <> nil then
  begin
    for i := 0 to Filter.Items.Count - 1 do
    begin
      Item := TRDFItem(Filter.Items[i]);
      if not (csDestroying in Item.ComponentState)
      then ListBox.Items.AddObject(Item.Name, Item);
    end;
  end;
end;

procedure TRDFItemsEdit.UpdateSelection;
var
  i: Integer;
  Item: TRDFItem;
  ComponentList: TDesignerSelections;
begin
  ComponentList := TDesignerSelections.Create;
  try
    with ListBox do
      for i := 0 to Items.Count - 1 do
        if Selected[i] then
        begin
          Item := TRDFItem(Items.Objects[i]);
          if Assigned(Item) then
            IDesignerSelections(ComponentList).Add(Item);
        end;
    if IDesignerSelections(ComponentList).Count = 0 then
      IDesignerSelections(ComponentList).Add(Filter);
  except
    ComponentList.Free;
    raise;
  end;
  SetSelection(ComponentList);
end;

procedure TRDFItemsEdit.ListBoxClick(Sender: TObject);
begin
  UpdateSelection;
end;

procedure TRDFItemsEdit.AddItemUpdate(Sender: TObject);
begin
  AddItem.Enabled := Filter <> nil;
end;

procedure TRDFItemsEdit.AddItemExecute(Sender: TObject);
var
  Item: TRDFItem;
begin
  Item := CreateNewFilterItem(Filter);
  if Item <> nil then
  begin
    Filter.AddItem(Item);
    ListBox.ItemIndex := ListBox.Items.AddObject(Item.Name, Item);
    UpdateSelection;
  end;
end;

procedure TRDFItemsEdit.DelItemUpdate(Sender: TObject);
begin
  DelItem.Enabled := ListBox.ItemIndex > -1;
end;

procedure TRDFItemsEdit.DelItemExecute(Sender: TObject);
var
  NextIndex: Integer;
begin
  if Application.MessageBox('Delete item?', PChar(Filter.Name), MB_YESNO) = IDYES then
  begin
    NextIndex := ListBox.ItemIndex - 1;
    Filter.DeleteItem(TRDFItem(ListBox.Items.Objects[ListBox.ItemIndex]));
    ListBox.Items.Delete(ListBox.ItemIndex);
    ListBox.ItemIndex := NextIndex;
  end;
  UpdateSelection;
end;

procedure TRDFItemsEdit.MoveUpUpdate(Sender: TObject);
begin
  MoveUp.Enabled := ListBox.ItemIndex > 0;
end;

procedure TRDFItemsEdit.MoveUpExecute(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := ListBox.ItemIndex - 1;
  Filter.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.ItemIndex := NewIndex;
  UpdateSelection;
end;

procedure TRDFItemsEdit.MoveDownUpdate(Sender: TObject);
begin
  MoveDown.Enabled := ListBox.ItemIndex < ListBox.Items.Count - 1;
end;

procedure TRDFItemsEdit.MoveDownExecute(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := ListBox.ItemIndex + 1;
  Filter.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.ItemIndex := NewIndex;
  UpdateSelection;
end;

procedure TRDFItemsEdit.DelAllUpdate(Sender: TObject);
begin
  DelAll.Enabled := ListBox.Items.Count > 0;
end;

procedure TRDFItemsEdit.DelAllExecute(Sender: TObject);
begin
  if Application.MessageBox('Delete ALL items?', PChar(Filter.Name), MB_YESNO) = IDYES then
  begin
    Filter.DeleteItems;
    ListBox.Items.Clear;
  end;
  UpdateSelection;
end;

end.
