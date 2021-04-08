unit RDbFilterItem_LinkList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView;

type
  TFormDbFilterItem_LinkList = class(TFormDbFilterItem)
    ListViewLabel: TLabel;
    ListView: TRSortListView;
    SelectAllBtn: TBitBtn;
    UnselectAllBtn: TBitBtn;
    InvertSelectionBtn: TBitBtn;
    procedure SelectAllBtnClick(Sender: TObject);
    procedure UnselectAllBtnClick(Sender: TObject);
    procedure InvertSelectionBtnClick(Sender: TObject);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormDbFilterItem_LinkList.SelectAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
    ListView.Items[i].Checked := True;
end;

procedure TFormDbFilterItem_LinkList.UnselectAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
    ListView.Items[i].Checked := False;
end;

procedure TFormDbFilterItem_LinkList.InvertSelectionBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
    ListView.Items[i].Checked := not ListView.Items[i].Checked;
end;

procedure TFormDbFilterItem_LinkList.ListViewDeletion(Sender: TObject; Item: TListItem);
var
  ItemData: ^Integer;
begin
  if Assigned(Item.Data) then
  begin
    ItemData := Item.Data;
    Item.Data := nil;
    Dispose(ItemData);
  end;
end;

end.
