unit RDUItemsEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DesignIntf, DesignEditors, DesignWindows, RDbUpdater, StdCtrls, Menus, ImgList,
  ActnList, ToolWin, ComCtrls;

type
  TRDUItemsEdit = class(TDesignWindow)
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
    Updater: TRDbUpdater;
    procedure LoadItems;
    procedure UpdateSelection;
  end;

type
  TDesigner = IDesigner;
  // TFormDesigner = IFormDesigner;

procedure EditRDbUpdaterItems(Designer: TDesigner; AUpdater: TRDbUpdater);

implementation

uses
  RDUItemCreate;

{$R *.DFM}

function FindEditor(AUpdater: TRDbUpdater): TRDUItemsEdit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TRDUItemsEdit then begin
      if TRDUItemsEdit(Screen.Forms[I]).Updater = AUpdater then
      begin
        Result := TRDUItemsEdit(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure EditRDbUpdaterItems(Designer: TDesigner; AUpdater: TRDbUpdater);
var
  Editor: TRDUItemsEdit;
begin
  if AUpdater <> nil then
  begin
    Editor := FindEditor(AUpdater);
    if Editor = nil then
    begin
      Editor := TRDUItemsEdit.Create(Application);
      try
        Editor.Caption := Format('%s', [AUpdater.Name]);
        Editor.Designer := Designer;
        Editor.Updater := AUpdater;
        Editor.LoadItems;
        Editor.Show;
      except
        Editor.Free;
        raise;
      end;
    end
    else begin
      Editor.Caption := Format('%s', [AUpdater.Name]);
      Editor.Show;
      if Editor.WindowState = wsMinimized then
        Editor.WindowState := wsNormal;
    end;
  end;
end;

{ == TRDUItemsEdit ============================================================= }

function GenerateName(Updater: TRDbUpdater; FieldName: string;
  ItemClass: TRDUItemClass; Number: Integer): string;
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
  Result := Format('%s_%s%d', [Updater.Name, IntName, Number]);
end;

function CreateUniqueName(Updater: TRDbUpdater; const FieldName: string;
  ItemClass: TRDUItemClass; Component: TComponent): string;
var
  i: Integer;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    with Updater.Owner do
      for i := 0 to ComponentCount - 1 do
        if (Component <> Components[i]) and (CompareText(AName, TRDUItem(Components[i]).Name) = 0) then Exit;
    Result := True;
  end;

begin
  for i := 1 to MaxInt do
  begin
    Result := GenerateName(Updater, FieldName, ItemClass, i);
    if IsUnique(Result) then Exit;
  end;
end;

function TRDUItemsEdit.UniqueName(Component: TComponent): string;
begin
  Result := CreateUniqueName(Updater, TRDUItem(Component).FieldName,
    TRDUItemClass(Component.ClassType), Component);
end;

procedure TRDUItemsEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRDUItemsEdit.LoadItems;
var
  i: Integer;
  Item: TRDUItem;
begin
  ListBox.Items.Clear;
  if Updater <> nil then
  begin
    for i := 0 to Updater.Items.Count - 1 do
    begin
      Item := TRDUItem(Updater.Items[i]);
      if not (csDestroying in Item.ComponentState)
      then ListBox.Items.AddObject(Item.Name, Item);
    end;
  end;
end;

procedure TRDUItemsEdit.UpdateSelection;
var
  i: Integer;
  Item: TRDUItem;
  ComponentList: TDesignerSelections;
begin
  ComponentList := TDesignerSelections.Create;
  try
    with ListBox do
      for i := 0 to Items.Count - 1 do
        if Selected[i] then
        begin
          Item := TRDUItem(Items.Objects[i]);
          if Assigned(Item) then
            IDesignerSelections(ComponentList).Add(Item);
        end;
    if IDesignerSelections(ComponentList).Count = 0 then
      IDesignerSelections(ComponentList).Add(Updater);
  except
    ComponentList.Free;
    raise;
  end;
  SetSelection(ComponentList);
end;

procedure TRDUItemsEdit.ListBoxClick(Sender: TObject);
begin
  UpdateSelection;
end;

procedure TRDUItemsEdit.AddItemUpdate(Sender: TObject);
begin
  AddItem.Enabled := Updater <> nil;
end;

procedure TRDUItemsEdit.AddItemExecute(Sender: TObject);
var
  Item: TRDUItem;
begin
  Item := CreateNewUpdaterItem(Updater);
  if Item <> nil then
  begin
    Updater.AddItem(Item);
    ListBox.ItemIndex := ListBox.Items.AddObject(Item.Name, Item);
    UpdateSelection;
  end;
end;

procedure TRDUItemsEdit.DelItemUpdate(Sender: TObject);
begin
  DelItem.Enabled := ListBox.ItemIndex > -1;
end;

procedure TRDUItemsEdit.DelItemExecute(Sender: TObject);
var
  NextIndex: Integer;
begin
  if Application.MessageBox('Delete item?', PChar(Updater.Name), MB_YESNO) = IDYES then
  begin
    NextIndex := ListBox.ItemIndex - 1;
    Updater.DeleteItem(TRDUItem(ListBox.Items.Objects[ListBox.ItemIndex]));
    ListBox.Items.Delete(ListBox.ItemIndex);
    ListBox.ItemIndex := NextIndex;
  end;
  UpdateSelection;
end;

procedure TRDUItemsEdit.MoveUpUpdate(Sender: TObject);
begin
  MoveUp.Enabled := ListBox.ItemIndex > 0;
end;

procedure TRDUItemsEdit.MoveUpExecute(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := ListBox.ItemIndex - 1;
  Updater.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.ItemIndex := NewIndex;
  UpdateSelection;
end;

procedure TRDUItemsEdit.MoveDownUpdate(Sender: TObject);
begin
  MoveDown.Enabled := ListBox.ItemIndex < ListBox.Items.Count - 1;
end;

procedure TRDUItemsEdit.MoveDownExecute(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := ListBox.ItemIndex + 1;
  Updater.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.Items.Move(ListBox.ItemIndex, NewIndex);
  ListBox.ItemIndex := NewIndex;
  UpdateSelection;
end;

procedure TRDUItemsEdit.DelAllUpdate(Sender: TObject);
begin
  DelAll.Enabled := ListBox.Items.Count > 0;
end;

procedure TRDUItemsEdit.DelAllExecute(Sender: TObject);
begin
  if Application.MessageBox('Delete ALL items?', PChar(Updater.Name), MB_YESNO) = IDYES then
  begin
    Updater.DeleteItems;
    ListBox.Items.Clear;
  end;
  UpdateSelection;
end;

end.
