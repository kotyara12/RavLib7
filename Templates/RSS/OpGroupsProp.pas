unit OpGroupsProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask,
  ComCtrls, ActnList, Menus, RavListView;

type
  TFormOpGroupsProp = class(TDbDialogTemplate)
    PageControl: TPageControl;
    DataTabSheet: TTabSheet;
    OpersTabSheet: TTabSheet;
    lblId: TLabel;
    deId: TDBEdit;
    lblName: TLabel;
    deName: TDBEdit;
    lblNotes: TLabel;
    deNotes: TDBMemo;
    LinkListView: TRSortListView;
    FreeListView: TRSortListView;
    LinkListViewLabel: TLabel;
    FreeListViewLabel: TLabel;
    AddBtn: TBitBtn;
    DelBtn: TBitBtn;
    ActionList: TActionList;
    LinkPopupMenu: TPopupMenu;
    FreePopupMenu: TPopupMenu;
    AddOper: TAction;
    DelOper: TAction;
    itemDelOperP: TMenuItem;
    itemAddOperP: TMenuItem;
    procedure AddOperUpdate(Sender: TObject);
    procedure AddOperExecute(Sender: TObject);
    procedure DelOperUpdate(Sender: TObject);
    procedure DelOperExecute(Sender: TObject);
    procedure DataTabSheetEnter(Sender: TObject);
    procedure LinkItemDeletion(Sender: TObject; Item: TListItem);
  protected
    procedure InitComponents(const EditMode: Boolean); override;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RListView, 
  BaseDbUnit, AdminVars, OpGroupsForm;

procedure TFormOpGroupsProp.LinkItemDeletion(Sender: TObject; Item: TListItem);
var
  ItemData: TId;
begin
  if Assigned(Item.Data) then
  begin
    ItemData := Item.Data;
    Item.Data := nil;
    Dispose(ItemData);
  end;
end;

{ == Установка режимов доуспности ============================================== }
procedure TFormOpGroupsProp.InitComponents(const EditMode: Boolean);
begin
  LinkListView.ParentColor := not EditMode;
  FreeListView.ParentColor := not EditMode;
end;

{ == Переключение между панелями =============================================== }
procedure TFormOpGroupsProp.DataTabSheetEnter(Sender: TObject);
begin
  deName.SetFocus;
end;

{ == Добавить операции в список разрешенных ==================================== }
procedure TFormOpGroupsProp.AddOperUpdate(Sender: TObject);
begin
  AddOper.Enabled := IsNotWait and IsEditMode and (FreeListView.SelCount > 0);
end;

procedure TFormOpGroupsProp.AddOperExecute(Sender: TObject);
begin
  StartWait;
  try
    LinkListView.ClearSelection;
    MoveSelectedListItems(FreeListView, LinkListView, imSave, imLink, imFree);
    LinkListView.SetFocus;
  finally
    StopWait;
  end;
end;

{ == Удалить операции из списка разрешенных ==================================== }
procedure TFormOpGroupsProp.DelOperUpdate(Sender: TObject);
begin
  DelOper.Enabled := IsNotWait and IsEditMode and (LinkListView.SelCount > 0);
end;

procedure TFormOpGroupsProp.DelOperExecute(Sender: TObject);
begin
  StartWait;
  try
    FreeListView.ClearSelection;
    MoveSelectedListItems(LinkListView, FreeListView, imSave, imFree, imLink);
    FreeListView.SetFocus;
  finally
    StopWait;
  end;
end;

end.
