unit RDbPrint;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, Db, StdActns,
  ActnList, Menus, ImgList;

type
  TFormPrintRecord = class(TDialogTemplate)
    SaveBtn: TBitBtn;
    RichEdit: TRichEdit;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    ImageList: TImageList;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    EditCopy: TEditCopy;
    EditSelectAll: TEditSelectAll;
    itemEditCopy: TMenuItem;
    divPopup1: TMenuItem;
    itemEditSelectAll: TMenuItem;
    procedure OkBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
  public
    procedure GenerateDbRecordReport(DS: TDataSet; const ShowHidden: Boolean);
    procedure GenerateListViewReport(LI: TListItem);
  end;

procedure PrintCurrentRecord(DS: TDataSet);
procedure PrintCurrentItem(LI: TListItem);

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu, RExHandlers;

procedure PrintCurrentRecord(DS: TDataSet);
var
  State: Integer;
begin
  State := QueryBoxStdNYC(SQueryVisibleFields);
  if State in [ID_YES, ID_NO] then begin
    with TFormPrintRecord.Create(Application.MainForm) do
    begin
      try
        GenerateDbRecordReport(DS, State = ID_YES);
        ShowModal;
      finally
        Free;
      end;
    end;
  end;
end;

procedure PrintCurrentItem(LI: TListItem);
begin
  with TFormPrintRecord.Create(Application.MainForm) do
  begin
    try
      GenerateListViewReport(LI);
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TFormPrintRecord.GenerateDbRecordReport(DS: TDataSet; const ShowHidden: Boolean);
var
  i: Integer;
begin
  try
    StartWait;
    ShowInStatusBar(SMsgGenerateReport);
    RichEdit.Lines.BeginUpdate;
    try
      RichEdit.Lines.Clear;
      for i := 0 to DS.FieldCount - 1 do
        if not DS.Fields[i].IsBlob
        and (DS.Fields[i].DisplayText <> EmptyStr)
        and (ShowHidden or DS.Fields[i].Visible) then
        begin
          RichEdit.SelAttributes.Name := RichEdit.Font.Name;
          RichEdit.SelAttributes.Style := RichEdit.Font.Style + [fsUnderline];
          RichEdit.Lines.Add(DS.Fields[i].DisplayName + ':');
          RichEdit.SelAttributes.Name := RichEdit.Font.Name;
          RichEdit.SelAttributes.Style := RichEdit.Font.Style + [fsBold];
          RichEdit.Lines.Add(DS.Fields[i].DisplayText);
          RichEdit.Lines.Add(EmptyStr);
        end;
      RichEdit.SelStart := 0;
    finally
      RichEdit.Lines.EndUpdate;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, EErrCreateRecReport);
  end;
end;

procedure TFormPrintRecord.GenerateListViewReport(LI: TListItem);
var
  i: Integer;
  bEmpty: Boolean;
begin
  try
    StartWait;
    ShowInStatusBar(SMsgGenerateReport);
    if not Assigned(LI) then raise Exception.Create(SErrNoSelectedItem);
    RichEdit.Lines.BeginUpdate;
    try
      RichEdit.Lines.Clear;
      for i := 0 to TListView(LI.ListView).Columns.Count - 1 do
        if i <= LI.SubItems.Count then
        begin
          if i = 0
          then bEmpty := LI.Caption = EmptyStr
          else bEmpty := LI.Subitems[i - 1] = EmptyStr;
          if not bEmpty then
          begin
            RichEdit.SelAttributes.Name := RichEdit.Font.Name;
            RichEdit.SelAttributes.Style := RichEdit.Font.Style + [fsUnderline];
            RichEdit.Lines.Add(TListView(LI.ListView).Columns[i].Caption + ':');
            RichEdit.SelAttributes.Name := RichEdit.Font.Name;
            RichEdit.SelAttributes.Style := RichEdit.Font.Style + [fsBold];
            if i = 0
            then RichEdit.Lines.Add(LI.Caption)
            else RichEdit.Lines.Add(LI.Subitems[i - 1]);
            RichEdit.Lines.Add(EmptyStr);
          end;
        end;
      RichEdit.SelStart := 0;
    finally
      RichEdit.Lines.EndUpdate;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, EErrCreateRecReport);
  end;
end;

procedure TFormPrintRecord.OkBtnClick(Sender: TObject);
begin
  if PrintDialog.Execute then begin
    StartWait;
    try
      RichEdit.Print(Caption);
    finally
      StopWait;
    end;
  end;
end;

procedure TFormPrintRecord.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    StartWait;
    if SaveDialog.FilterIndex = 2 then RichEdit.PlainText := True;
    try
      RichEdit.Lines.SaveToFile(SaveDialog.FileName);
    finally
      RichEdit.PlainText := False;
      StopWait;
    end;
  end;
end;

end.
