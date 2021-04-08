unit RDbFldListEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RDbFilter, StdCtrls, Menus, ImgList, ActnList, ToolWin, ComCtrls, Db, RDbEditor;

type
  TRFldListEditor = class(TForm)
    ListView: TListView;
    ToolBar: TToolBar;
    OkButton: TToolButton;
    CancelButton: TToolButton;
    ImageList: TImageList;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    fEditor: TRDbCustomEditor;
    procedure LoadList(const FieldsList: string; const OnlyDbFields: Boolean);
    function  SaveList: string;
  end;

function EditFieldsList(Editor: TRDbCustomEditor; var FieldsList: string; const OnlyDbFields: Boolean): Boolean;

implementation

{$R *.dfm}

uses
  RxStrUtils;

const
  DivChar  = ';';
  DivChars = [DivChar,#13,','];

function EditFieldsList(Editor: TRDbCustomEditor; var FieldsList: string; const OnlyDbFields: Boolean): Boolean;
begin
  with TRFldListEditor.Create(Application) do
  begin
    try
      fEditor := Editor;
      Caption := Format('%s', [Editor.Name]);
      LoadList(FieldsList, OnlyDbFields);
      Result := ShowModal = mrOk;
      if Result then
        FieldsList := SaveList;
    finally
      Free;
    end;
  end;
end;

{ TRFldListEditor }

procedure TRFldListEditor.LoadList(const FieldsList: string; const OnlyDbFields: Boolean);
var
  i: Integer;
begin
  ListView.Items.Clear;
  if Assigned(fEditor) and Assigned(fEditor.DataSet) then
  begin
    for i := 0 to fEditor.DataSet.Fields.Count - 1 do
      if not OnlyDbFields or (fEditor.DataSet.Fields[i].FieldKind = fkData) then
      begin
        with ListView.Items.Add do
        begin
          Caption := fEditor.DataSet.Fields[i].FieldName;
          Checked := IsWordPresent(Caption, FieldsList, DivChars);
        end;
      end;
  end;
end;

function TRFldListEditor.SaveList: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Checked then
    begin
      if Result = EmptyStr
      then Result := ListView.Items[i].Caption
      else Result := Result + DivChar + ListView.Items[i].Caption;
    end;
end;

procedure TRFldListEditor.OkButtonClick(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

procedure TRFldListEditor.CancelButtonClick(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

end.
