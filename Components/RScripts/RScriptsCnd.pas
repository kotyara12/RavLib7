unit RScriptsCnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ImgList, ComCtrls,
  RScripts, RScrCnst, RScrFncs,
  ActnList, Menus;

type
  TFormScriptCnd = class(TDialogTemplate)
    CmdComboBoxLabel: TLabel;
    CmdComboBox: TComboBox;
    TreeViewLabel: TLabel;
    TreeView: TTreeView;
    ImageList: TImageList;
    AddBlockBtn: TBitBtn;
    AddFunctionBtn: TBitBtn;
    PropertiesBtn: TBitBtn;
    DeleteBtn: TBitBtn;
    ActionList: TActionList;
    AddBlock: TAction;
    AddFunction: TAction;
    Properties: TAction;
    Delete: TAction;
    PopupMenu: TPopupMenu;
    itemAddBlock: TMenuItem;
    itemAddFunction: TMenuItem;
    itemProperties: TMenuItem;
    itemDelete: TMenuItem;
    procedure LoadCmdComboBox;
    procedure AddBlockUpdate(Sender: TObject);
    procedure AddBlockExecute(Sender: TObject);
    procedure AddFunctionUpdate(Sender: TObject);
    procedure AddFunctionExecute(Sender: TObject);
    procedure PropertiesUpdate(Sender: TObject);
    procedure PropertiesExecute(Sender: TObject);
    procedure DeleteUpdate(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
  private
    fEditor: TRScriptEditor;
    function  GetCmdType: TClass;
    procedure SetCmdType(const Value: TClass);
    function  GetCondition: TRFncCondition;
    procedure SetCondition(const Value: TRFncCondition);
    function  CalcImageIndex(const Inversed: Boolean; Operator: TRFncOperator): Integer;
    function  CalcInversed(const ImageIndex: Integer): Boolean;
    function  CalcOperator(const ImageIndex: Integer): TRFncOperator;
  public
    property CommandType: TClass read GetCmdType write SetCmdType;
    property Condtition: TRFncCondition read GetCondition write SetCondition;
  end;

function EditFncCondition(Editor: TRScriptEditor; var CmdClass: TClass;
  var Cnd: TRFncCondition): Boolean;

implementation

uses
  RVclUtils, RDialogs, RScriptsFnb, RScriptsFnc;

{$R *.dfm}

type
  TRFncData = ^RRFncData;
  RRFncData = packed record
    FncClass: TClass;
    Params: TRCmdParams;
  end;

resourcestring
  SErrDataIsNull   = 'Для элемента "%s" класс функции не определен!';
  SQryDeleteFnc    = 'Удалить функцию "%s"?';
  SQryDeleteFnb    = 'Удалить блок функций, включая вложенные данные?';

const
  SEmptyBlock = '(...)';

function EditFncCondition(Editor: TRScriptEditor;
  var CmdClass: TClass; var Cnd: TRFncCondition): Boolean;
begin
  with TFormScriptCnd.Create(Application.MainForm) do
  begin
    try
      fEditor := Editor;
      LoadCmdComboBox;
      CommandType := CmdClass;
      Condtition := Cnd;
      Result := ShowModal = mrOk;
      if Result then
      begin
        CmdClass := CommandType;
        Cnd := Condtition;
        // InfoBox(Condtition.GetFunctionCode);
      end;
    finally
      Free;
    end;
  end;
end;

{ TFormScriptCnd }

procedure TFormScriptCnd.LoadCmdComboBox;
var
  i: TRCmdType;
  Cmd: TRCmdCondition;
begin
  inherited;
  CmdComboBox.Items.BeginUpdate;
  try
    CmdComboBox.Items.Clear;
    for i := Low(LRCommandsCnd) to High(LRCommandsCnd) do
    begin
      Cmd := LRCommandsCnd[i].Create(fEditor);
      CmdComboBox.Items.AddObject(Cmd.GetCommandName
        + #32 + Cmd.GetCommandNote, Cmd);
    end;
  finally
    CmdComboBox.Items.EndUpdate;
  end;
end;

function TFormScriptCnd.GetCmdType: TClass;
begin
  if CmdComboBox.ItemIndex > intDisable then
    Result := CmdComboBox.Items.Objects[CmdComboBox.ItemIndex].ClassType
  else Result := nil;
end;

procedure TFormScriptCnd.SetCmdType(const Value: TClass);
var
  i: Integer;
begin
  for i := 0 to CmdComboBox.Items.Count - 1 do
    if CmdComboBox.Items.Objects[i].ClassType = Value then
    begin
      CmdComboBox.ItemIndex := i;
      Break;
    end;
end;

function TFormScriptCnd.CalcInversed(const ImageIndex: Integer): Boolean;
begin
  Result := ImageIndex in [3, 4, 5];
end;

function TFormScriptCnd.CalcOperator(const ImageIndex: Integer): TRFncOperator;
begin
  case ImageIndex of
    1, 4: Result := coOr;
    0, 3: Result := coAnd;
    else  Result := coNone;
  end;
end;

function TFormScriptCnd.CalcImageIndex(const Inversed: Boolean; Operator: TRFncOperator): Integer;
begin
  Result := -1;
  case Operator of
    coNone:
      if Inversed
      then Result := 5
      else Result := 2;
    coAnd:
      if Inversed
      then Result := 3
      else Result := 0;
    coOr:
      if Inversed
      then Result := 4
      else Result := 1;
  end;
end;

function TFormScriptCnd.GetCondition: TRFncCondition;
var
  Node: TTreeNode;

  function CreateFunction(const Node: TTreeNode): TRFncExecuted;
  var
    FncD: TRFncData;
  begin
    if Assigned(Node.Data) then
    begin
      FncD := Node.Data;
      Result := CRFncExecuted(FncD^.FncClass).Create(nil,
        CalcInversed(Node.ImageIndex), CalcOperator(Node.ImageIndex));
      Result.AddParameters(FncD^.Params);
    end
    else raise Exception.CreateFmt(SErrDataIsNull, [Node.Text]);
  end;

  procedure ProcessBlock(const RootNode: TTreeNode);
  var
    Node: TTreeNode;
  begin
    Result.BlockBegin(CalcInversed(RootNode.ImageIndex), CalcOperator(RootNode.ImageIndex));
    try
      Node := RootNode.GetFirstChild;
      while Node <> nil do
      begin
        if Assigned(Node.Data)
        then Result.AddFunction(CreateFunction(Node))
        else ProcessBlock(Node);
        Node := RootNode.GetNextChild(Node);
      end;
    finally
      Result.BlockEnd;
    end;
  end;

begin
  StartWait;
  try
    Result := TRFncCondition.Create(nil);
    Node := TreeView.Items.GetFirstNode;
    while Node <> nil do
    begin
      if Assigned(Node.Data)
      then Result.AddFunction(CreateFunction(Node))
      else ProcessBlock(Node);
      Node := Node.GetNextSibling;
    end;
  finally
    StopWait;
  end;
end;

procedure TFormScriptCnd.SetCondition(const Value: TRFncCondition);

  procedure LoadFncBlock(const OwnerNode: TTreeNode; const Block: TRFncBlock);
  var
    Node: TTreeNode;
    Data: TRFncData;
    i: Integer;
  begin
    if Assigned(Block) then
    begin
      for i := 0 to Block.Items.Count - 1 do
        if Block.Item[i] is TRFncBlock then
        begin
          if OwnerNode = nil
          then Node := TreeView.Items.Add(nil, SEmptyBlock)
          else Node := TreeView.Items.AddChild(OwnerNode, SEmptyBlock);
          Node.ImageIndex := CalcImageIndex(Block.Item[i].Inversed, Block.Item[i].Operator);
          Node.SelectedIndex := CalcImageIndex(Block.Item[i].Inversed, Block.Item[i].Operator);
          Node.Data := nil;
          LoadFncBlock(Node, TRFncBlock(Block.Item[i]));
        end
        else begin
          New(Data);
          Data^.FncClass := TRFncExecuted(Block.Item[i]).ClassType;
          Data^.Params := TRFncExecuted(Block.Item[i]).Parameters;
          if OwnerNode = nil
          then Node := TreeView.Items.Add(nil, Block.Item[i].GetFunctionText_Edit)
          else Node := TreeView.Items.AddChild(OwnerNode, Block.Item[i].GetFunctionText_Edit);
          Node.ImageIndex := CalcImageIndex(Block.Item[i].Inversed, Block.Item[i].Operator);
          Node.SelectedIndex := CalcImageIndex(Block.Item[i].Inversed, Block.Item[i].Operator);
          Node.Data := Data;
        end;
    end;
  end;

begin
  StartWait;
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    LoadFncBlock(nil, Value.TopBlock);
    TreeView.FullExpand;
  finally
    TreeView.Items.EndUpdate;
    StopWait;
  end;
end;

procedure TFormScriptCnd.AddBlockUpdate(Sender: TObject);
begin
  AddBlock.Enabled := IsNotWait;
end;

procedure TFormScriptCnd.AddBlockExecute(Sender: TObject);
var
  Node: TTreeNode;
  Inversed: Boolean;
  Operator: TRFncOperator;
begin
  Inversed := False;
  if Assigned(TreeView.Selected) then Operator := coAnd else Operator := coNone;
  if EditFnbCommand(TreeView.Selected = nil, Inversed, Operator) then
  begin
    Node := TreeView.Items.Add(TreeView.Selected, SEmptyBlock);
    Node.ImageIndex := CalcImageIndex(Inversed, Operator);
    Node.SelectedIndex := CalcImageIndex(Inversed, Operator);
    Node.Data := nil;
    TreeView.Selected := Node;
  end;
  TreeView.SetFocus;
end;

procedure TFormScriptCnd.AddFunctionUpdate(Sender: TObject);
begin
  AddFunction.Enabled := IsNotWait;
end;

procedure TFormScriptCnd.AddFunctionExecute(Sender: TObject);
var
  OwnNode, Node: TTreeNode;
  Inversed, FirstNode: Boolean;
  Operator: TRFncOperator;
  FncItem: TRFncExecuted;
  FncData: TRFncData;
  FncClass: TClass;
  FncParams: TRCmdParams;
begin
  OwnNode := nil;
  FirstNode := True;
  if Assigned(TreeView.Selected) then
  begin
    if Assigned(TreeView.Selected.Data) then
    begin
      OwnNode := TreeView.Selected.Parent;
      FirstNode := False;
    end
    else begin
      OwnNode := TreeView.Selected;
      FirstNode := not TreeView.Selected.HasChildren;
    end;
  end;
  FncClass := LRFunctions[Low(LRFunctions)];
  SetLength(FncParams, 1);
  Inversed := False;
  if FirstNode then Operator := coNone else Operator := coAnd;
  if EditFncCommand(fEditor, FirstNode, FncClass, Inversed, Operator, FncParams) then
  begin
    FncItem := CRFncExecuted(FncClass).Create(nil, Inversed, Operator);
    try
      New(FncData);
      FncData^.FncClass := FncClass;
      FncData^.Params := FncParams;
      FncItem.AddParameters(FncParams);
      if Assigned(OwnNode)
      then Node := TreeView.Items.AddChild(OwnNode, FncItem.GetFunctionText_Edit)
      else Node := TreeView.Items.Add(TreeView.Selected, FncItem.GetFunctionText_Edit);
      Node.ImageIndex := CalcImageIndex(Inversed, Operator);
      Node.SelectedIndex := CalcImageIndex(Inversed, Operator);
      Node.Data := FncData;
      TreeView.Selected := Node;
    finally
      FncItem.Free;
    end;
  end;
  TreeView.SetFocus;
end;

procedure TFormScriptCnd.PropertiesUpdate(Sender: TObject);
begin
  Properties.Enabled := IsNotWait and Assigned(TreeView.Selected);
end;

procedure TFormScriptCnd.PropertiesExecute(Sender: TObject);
var
  Inversed: Boolean;
  Operator: TRFncOperator;
  FncItem:  TRFncExecuted;
  FncClass: TClass;
  FncParams: TRCmdParams;
begin
  with TreeView.Selected do
  begin
    if Assigned(Data) then
    begin
      Inversed := CalcInversed(ImageIndex);
      Operator := CalcOperator(ImageIndex);
      FncClass := TRFncData(Data)^.FncClass;
      FncParams := TRFncData(Data)^.Params;
      if EditFncCommand(fEditor, GetPrevSibling = nil, FncClass,
        Inversed, Operator, FncParams) then
      begin
        TRFncData(Data)^.FncClass := FncClass;
        TRFncData(Data)^.Params := FncParams;
        FncItem := CRFncExecuted(FncClass).Create(nil, Inversed, Operator);
        try
          FncItem.AddParameters(FncParams);
          Text := FncItem.GetFunctionText_Edit;
          ImageIndex := CalcImageIndex(Inversed, Operator);
          SelectedIndex := CalcImageIndex(Inversed, Operator);
        finally
          FncItem.Free;
        end;
      end;
    end
    else begin
      Inversed := CalcInversed(ImageIndex);
      Operator := CalcOperator(ImageIndex);
      if EditFnbCommand(GetPrevSibling = nil, Inversed, Operator) then
      begin
        ImageIndex := CalcImageIndex(Inversed, Operator);
        SelectedIndex := CalcImageIndex(Inversed, Operator);
      end;
    end;
  end;
  TreeView.SetFocus;
end;

procedure TFormScriptCnd.TreeViewDblClick(Sender: TObject);
begin
  if Properties.Enabled then PropertiesExecute(Sender);
end;

procedure TFormScriptCnd.DeleteUpdate(Sender: TObject);
begin
  Delete.Enabled := IsNotWait and Assigned(TreeView.Selected);
end;

procedure TFormScriptCnd.DeleteExecute(Sender: TObject);
var
  QryText: string;
begin
  if Assigned(TreeView.Selected) then
  begin
    if Assigned(TreeView.Selected.Data)
    then QryText := Format(SQryDeleteFnc, [TreeView.Selected.Text])
    else QryText := Format(SQryDeleteFnb, [TreeView.Selected.Text]);
    if DeleteQueryText(QryText) then
    begin
      if TreeView.Selected.HasChildren then TreeView.Selected.DeleteChildren;
      TreeView.Selected.Delete;
    end;
  end;
  TreeView.SetFocus;
end;

end.

