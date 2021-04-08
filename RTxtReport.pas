unit RTxtReport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, Db, StdActns,
  ActnList, Menus, ImgList;

type
  TFormTextReport = class(TDialogTemplate)
    SaveBtn: TBitBtn;
    RichEdit: TRichEdit;
    SaveDialog: TSaveDialog;
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
    procedure AddLine(const Msg: string);
    procedure AddFmtValue(const Name, Value: string;
      const NameStyle, ValueStyle: TFontStyles;
      const Divider: string = #32);
    procedure AddFmtTable(const Title: string; Table: TDataSet;
      const ExpTag: Integer; const TitleStyle, NameStyle, ValueStyle: TFontStyles;
      const Divider: string = ' ( %d ): ');
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu, RExHandlers;

procedure TFormTextReport.AddLine(const Msg: string);
begin
  RichEdit.Lines.Add(Msg);
end;

procedure TFormTextReport.AddFmtValue(const Name, Value: string;
  const NameStyle, ValueStyle: TFontStyles; const Divider: string = #32);
var
  StPos: Integer;
begin
  StPos := Length(RichEdit.Text);
  RichEdit.SelStart := StPos;
  RichEdit.SelLength := 0;
  RichEdit.Lines.Add(Name + Divider + Value);
  RichEdit.SelStart := StPos;
  RichEdit.SelLength := Length(Name) + Length(Divider);
  RichEdit.SelAttributes.Style := NameStyle;
  RichEdit.SelStart := StPos + Length(Name) + Length(Divider);
  RichEdit.SelLength := Length(Value);
  RichEdit.SelAttributes.Style := ValueStyle;
  RichEdit.SelStart := Length(RichEdit.Text);
  RichEdit.SelLength := 0;
end;

procedure TFormTextReport.AddFmtTable(const Title: string; Table: TDataSet; const ExpTag: Integer;
  const TitleStyle, NameStyle, ValueStyle: TFontStyles; const Divider: string = ' ( %d ): ');
var
  i: Integer;
  Bmk: TBookmark;
begin
  if Table.Active and not Table.IsEmpty then
  begin
    if Title <> EmptyStr then
    begin
      RichEdit.SelAttributes.Style := TitleStyle;
      RichEdit.Lines.Add(Title);
    end;
    Table.DisableControls;
    Bmk := Table.GetBookmark;
    try
      Table.First;
      while not Table.Eof do
      begin
        if not Table.Bof then
          RichEdit.Lines.Add('---');
        for i := 0 to Table.FieldCount - 1 do
          if not Table.Fields[i].IsBlob
          and (Table.Fields[i].Tag = ExpTag)
          and (Trim(Table.Fields[i].DisplayText) <> EmptyStr) then
            AddFmtValue(Table.Fields[i].DisplayName,
              Table.Fields[i].DisplayText, NameStyle, ValueStyle,
                Format(Divider, [Table.RecNo]));
        Table.Next;
      end;
    finally
      if Assigned(Bmk) then
      begin
        try
          Table.GotoBookmark(Bmk);
        except
        end;
        Table.FreeBookmark(Bmk);
      end;
      Table.EnableControls;
    end;
  end;
end;

procedure TFormTextReport.OkBtnClick(Sender: TObject);
begin
  with TPrintDialog.Create(Self) do
  begin
    try
      if Execute then
      begin
        StartWait;
        try
          RichEdit.Print(Caption);
        finally
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormTextReport.SaveBtnClick(Sender: TObject);
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
