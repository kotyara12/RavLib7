unit RTreeMove;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavTreeView,
  RVclUtils;

type
  TTreeMoveOptions = (tmGroup, tmItem, tmRecord);

  TFormTreeMove = class(TDialogTemplate)
    TreeView: TRTreeView;
    procedure FormActivate(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  public
    FTargetTypes: TNodeTypes;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure ReloadTree(TV: TRTreeView; const LoadFullTree: Boolean; const LoadedTypes: TNodeTypes);
  end;

function MoveToTreeNode(TV: TRTreeView; const LoadFullTree: Boolean;
  const LoadedTypes, TargetTypes: TNodeTypes): TTreeNode;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RMsgRu, RDialogs;

function MoveToTreeNode(TV: TRTreeView; const LoadFullTree: Boolean;
  const LoadedTypes, TargetTypes: TNodeTypes): TTreeNode;
begin
  Result := nil;
  with TFormTreeMove.Create(Application.MainForm) do
  begin
    try
      TreeView.Images := TV.Images;
      FTargetTypes := TargetTypes;
      ReloadTree(TV, LoadFullTree, LoadedTypes);
      if ShowModal = mrOk then
        Result := TV.FindNode([TreeView.GetNodeType(TreeView.Selected)],
          TreeView.GetNodeId(TreeView.Selected));
    finally
      Free;
    end;
  end;
end;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TFormTreeMove.SetStyle;
begin
  inherited;
  TreeView.Color := ApplicationStyle.DataForm.TreeColor;
  FontDataToFont(ApplicationStyle.DataForm.TreeFont, TreeView.Font);
end;
{$ENDIF}

{ == Перегрузка дерева ========================================================= }
procedure TFormTreeMove.ReloadTree(TV: TRTreeView; const LoadFullTree: Boolean; const LoadedTypes: TNodeTypes);
var
  Node: TTreeNode;

  procedure AddNode(SrcNode, TrgNode: TTreeNode);
  var
    ChSrcNode, ChTrgNode: TTreeNode;
  begin
    if (LoadFullTree or (SrcNode <> TV.Selected))
    and (TV.GetNodeType(SrcNode) in LoadedTypes) then
    begin
      ChTrgNode := TreeView.CreateTypeNode(TrgNode, TV.GetNodeType(SrcNode),
        TV.GetNodeId(SrcNode), SrcNode.ImageIndex, SrcNode.SelectedIndex, SrcNode.Text, TV.GetNodeSort(SrcNode));
      ChSrcNode := SrcNode.GetFirstChild;
      while Assigned(ChSrcNode) do
      begin
        AddNode(ChSrcNode, ChTrgNode);
        ChSrcNode := SrcNode.GetNextChild(ChSrcNode);
      end;
    end;
  end;

begin
  StartWait;
  try
    TV.Items.BeginUpdate;
    try
      TreeView.Items.BeginUpdate;
      try
        TreeView.Items.Clear;
        Node := TV.Items.GetFirstNode;
        while Assigned(Node) do
        begin
          AddNode(Node, nil);
          Node := Node.GetNextSibling;
        end;
      finally
        TreeView.Items.EndUpdate;
      end;
      TreeView.SortType := TV.SortType;
      if LoadFullTree
      then TreeView.Selected := TreeView.FindNode([TV.GetNodeType(TV.Selected)], TV.GetNodeId(TV.Selected))
      else TreeView.Selected := TreeView.TopItem;
      // TreeView.FullExpand;
    finally
      TV.Items.EndUpdate;
    end;
  finally
    StopWait;
  end;
end;

{ == Выделение элемента ======================================================== }
procedure TFormTreeMove.FormActivate(Sender: TObject);
begin
  inherited;
  TreeViewChange(Sender, nil);
end;

procedure TFormTreeMove.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  OkBtn.Enabled := TreeView.GetNodeType(TreeView.Selected) in FTargetTypes;
end;

end.
