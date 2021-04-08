unit RDbTreeUtils;

interface

uses
  RavTreeView, Db;

{ == "Стандартная" загрузка данных в дерево ==================================== }
procedure LoadTreeDbOwned(TreeView: TRIdTreeView; DataSet: TDataSet;
  const ImageIndex, SelectedIndex: Integer; const LoadNotes, ExpandRoot, ExpandNodes: Boolean);

implementation

uses
  SysUtils, ComCtrls, Forms, RDbConst, RStrUtils, RProgress;

{ == "Стандартная" загрузка данных в дерево ==================================== }
procedure LoadTreeDbOwned(TreeView: TRIdTreeView; DataSet: TDataSet;
  const ImageIndex, SelectedIndex: Integer; const LoadNotes, ExpandRoot, ExpandNodes: Boolean);
var
  Node: TTreeNode;
  Name: string;
  Bmk: TBookmark;

  procedure LoadSubNodes(Node: TTreeNode);
  var
    SubNode: TTreeNode;
    SubBmk: TBookmark;
  begin
    DataSet.Filter := Format(fltOwnerId, [TreeView.GetNodeId(Node)]);
    DataSet.FindFirst;
    while DataSet.Found do
    begin
      SubBmk := DataSet.GetBookmark;
      try
        if LoadNotes
        then Name := AddNotes(DataSet.FieldByName(fnNAME).AsString, DataSet.FieldByName(fnNOTES).AsString)
        else Name := DataSet.FieldByName(fnNAME).AsString;
        SubNode := TreeView.AddChildNode(Node, DataSet.FieldByName(fnID).AsInteger,
            ImageIndex, SelectedIndex, Name);
        Application.ProcessMessages;
        LoadSubNodes(SubNode);
        DataSet.Filter := Format(fltOwnerId, [TreeView.GetNodeId(Node)]);
      finally
        try
          DataSet.GotoBookmark(SubBmk);
        except
        end;
        DataSet.FreeBookmark(SubBmk);
      end;
      DataSet.FindNext;
      UpdateProgressStep(1);
    end;
  end;

begin
  DataSet.DisableControls;
  try
    try
      DataSet.Filtered := True;
      DataSet.Filter := fltOwnerNull;
      DataSet.FindFirst;
      while DataSet.Found do
      begin
        Bmk := DataSet.GetBookmark;
        try
          if LoadNotes
          then Name := AddNotes(DataSet.FieldByName(fnNAME).AsString, DataSet.FieldByName(fnNOTES).AsString)
          else Name := DataSet.FieldByName(fnNAME).AsString;
          Node := TreeView.AddFirstNode(DataSet.FieldByName(fnID).AsInteger,
            ImageIndex, SelectedIndex, Name);
          Application.ProcessMessages;
          LoadSubNodes(Node);
          if ExpandRoot then Node.Expand(ExpandNodes);
          DataSet.Filter := fltOwnerNull;
        finally
          try
            DataSet.GotoBookmark(Bmk);
          except
          end;
          DataSet.FreeBookmark(Bmk);
        end;
        DataSet.FindNext;
        UpdateProgressStep(1);
      end;
    finally
      DataSet.Filter := EmptyStr;
      DataSet.Filtered := False;
    end;
  finally
    DataSet.EnableControls;
  end;
end;

end.
