unit UsersProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  RavListView, DBCtrls, Mask, Menus, ActnList;

type
  TFormUsersProp = class(TDbDialogTemplate)
    PageControl: TPageControl;
    DataTabSheet: TTabSheet;
    GroupsTabSheet: TTabSheet;
    OpersTabSheet: TTabSheet;
    AllOpersTabSheet: TTabSheet;
    WpTabSheet: TTabSheet;
    lblId: TLabel;
    lblName: TLabel;
    lblNotes: TLabel;
    deId: TDBEdit;
    deName: TDBEdit;
    deNotes: TDBMemo;
    OperLinkListViewLabel: TLabel;
    OperFreeListViewLabel: TLabel;
    GroupLinkListViewLabel: TLabel;
    GroupFreeListViewLabel: TLabel;
    OpersAllListViewLabel: TLabel;
    WpViewLabel: TLabel;
    LinkOperListView: TRSortListView;
    FreeOperListView: TRSortListView;
    LinkGroupListView: TRSortListView;
    FreeGroupListView: TRSortListView;
    OpersAllListView: TRSortListView;
    WpListView: TRSortListView;
    AddOperBtn: TBitBtn;
    DelOperBtn: TBitBtn;
    AddGroupBtn: TBitBtn;
    DelGroupBtn: TBitBtn;
    deFullname: TDBEdit;
    lblFullname: TLabel;
    ListDataSource: TDataSource;
    deGroups: TDBLookupComboBox;
    lblGroups: TLabel;
    ActionList: TActionList;
    AddOper: TAction;
    DelOper: TAction;
    OperLinkPopupMenu: TPopupMenu;
    itemDelOper: TMenuItem;
    OperFreePopupMenu: TPopupMenu;
    itemAddOper: TMenuItem;
    GroupLinkPopupMenu: TPopupMenu;
    itemDelGroup: TMenuItem;
    GroupFreePopupMenu: TPopupMenu;
    itemAddGroup: TMenuItem;
    AddGroup: TAction;
    DelGroup: TAction;
    NotesOperLabel: TLabel;
    NotesWpLabel: TLabel;
    StateGroupBox: TGroupBox;
    deBlocked: TDBCheckBox;
    deDeleted: TDBCheckBox;
    lblChanged: TLabel;
    deChanged: TDBEdit;
    lblCountEp: TLabel;
    deCountEp: TDBEdit;
    procedure DataTabSheetEnter(Sender: TObject);
    procedure AddOperUpdate(Sender: TObject);
    procedure AddOperExecute(Sender: TObject);
    procedure DelOperUpdate(Sender: TObject);
    procedure DelOperExecute(Sender: TObject);
    procedure AddGroupUpdate(Sender: TObject);
    procedure AddGroupExecute(Sender: TObject);
    procedure DelGroupUpdate(Sender: TObject);
    procedure DelGroupExecute(Sender: TObject);
    procedure ListItemDeletion(Sender: TObject; Item: TListItem);
  protected
    procedure InitComponents(const EditMode: Boolean); override;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RListView, 
  BaseDbUnit, AdminUnit, UsersForm;

{ == Установка режимов доуспности ============================================== }
procedure TFormUsersProp.InitComponents(const EditMode: Boolean);
begin
  LinkOperListView.ParentColor := not EditMode;
  FreeOperListView.ParentColor := not EditMode;
  LinkGroupListView.ParentColor := not EditMode;
  FreeGroupListView.ParentColor := not EditMode;
  OpersAllListView.ParentColor := not EditMode;
  WpListView.ParentColor := not EditMode;
end;

{ == Переключение между панелями =============================================== }
procedure TFormUsersProp.DataTabSheetEnter(Sender: TObject);
begin
  deName.SetFocus;
end;

{ == Добавить группы доступа в список разрешенных ============================== }
procedure TFormUsersProp.AddGroupUpdate(Sender: TObject);
begin
  AddGroup.Enabled := IsNotWait and IsEditMode and (FreeGroupListView.SelCount > 0);
end;

procedure TFormUsersProp.AddGroupExecute(Sender: TObject);
begin
  StartWait;
  try
    LinkGroupListView.ClearSelection;
    MoveSelectedListItems(FreeGroupListView, LinkGroupListView, imSave, imLink, imFree);
    LinkGroupListView.SetFocus;
  finally
    StopWait;
  end;
end;

{ == Удалить группы доступа из списка разрешенных ============================== }
procedure TFormUsersProp.DelGroupUpdate(Sender: TObject);
begin
  DelGroup.Enabled := IsNotWait and IsEditMode and (LinkGroupListView.SelCount > 0);
end;

procedure TFormUsersProp.DelGroupExecute(Sender: TObject);
begin
  StartWait;
  try
    FreeGroupListView.ClearSelection;
    MoveSelectedListItems(LinkGroupListView, FreeGroupListView, imSave, imFree, imLink);
    FreeGroupListView.SetFocus;
  finally
    StopWait;
  end;
end;

{ == Добавить операции в список разрешенных ==================================== }
procedure TFormUsersProp.AddOperUpdate(Sender: TObject);
begin
  AddOper.Enabled := IsNotWait and IsEditMode and (FreeOperListView.SelCount > 0);
end;

procedure TFormUsersProp.AddOperExecute(Sender: TObject);
begin
  StartWait;
  try
    LinkOperListView.ClearSelection;
    MoveSelectedListItems(FreeOperListView, LinkOperListView, imSave, imLink, imFree);
    LinkOperListView.SetFocus;
  finally
    StopWait;
  end;
end;

{ == Удалить операции из списка разрешенных ==================================== }
procedure TFormUsersProp.DelOperUpdate(Sender: TObject);
begin
  DelOper.Enabled := IsNotWait and IsEditMode and (LinkOperListView.SelCount > 0);
end;

procedure TFormUsersProp.DelOperExecute(Sender: TObject);
begin
  StartWait;
  try
    FreeOperListView.ClearSelection;
    MoveSelectedListItems(LinkOperListView, FreeOperListView, imSave, imFree, imLink);
    FreeOperListView.SetFocus;
  finally
    StopWait;
  end;
end;

procedure TFormUsersProp.ListItemDeletion(Sender: TObject; Item: TListItem);
var
  ItemData: TId;
begin
  if Item.Data <> nil then
  begin
    ItemData := Item.Data;
    Item.Data := nil;
    Dispose(ItemData);
  end;
end;

end.
