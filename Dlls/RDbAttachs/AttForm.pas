unit AttForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplListSimple, Menus, ImgList, ActnList, ComCtrls, ToolWin,
  RavListView, DB, ADODB, RRssAttachs, StdCtrls, Buttons, ExtCtrls;

type
  TFormAttachs = class(TSimpleListTemplate)
    RefreshToolButton: TToolButton;
    SepClose: TToolButton;
    CloseCancelToolButton: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    AddFile: TAction;
    DelFile: TAction;
    OpenFile: TAction;
    SaveAsFile: TAction;
    AddFileToolButton: TToolButton;
    DelFileToolButton: TToolButton;
    OpenFileToolButton: TToolButton;
    SaveAsFileToolButton: TToolButton;
    SepAdd: TToolButton;
    SepOpen: TToolButton;
    itemAddFile: TMenuItem;
    itemDelFile: TMenuItem;
    divEdit2: TMenuItem;
    itemOpenFile: TMenuItem;
    itemSaveAsFile: TMenuItem;
    divEdit3: TMenuItem;
    itemAddFileP: TMenuItem;
    itemDelFileP: TMenuItem;
    divPopupFile: TMenuItem;
    itemOpenFileP: TMenuItem;
    itemSaveAsFileP: TMenuItem;
    divPopupOpen: TMenuItem;
    procedure AddFileUpdate(Sender: TObject);
    procedure AddFileExecute(Sender: TObject);
    procedure DelFileUpdate(Sender: TObject);
    procedure DelFileExecute(Sender: TObject);
    procedure OpenFileUpdate(Sender: TObject);
    procedure OpenFileExecute(Sender: TObject);
    procedure SaveAsFileUpdate(Sender: TObject);
    procedure SaveAsFileExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
  protected
    function  GetDetailName: string; override;
    procedure InitForm; override;
    procedure FreeForm; override;
    procedure LogOnOpen; override;
    procedure LogOnClose; override;
    function  LoadDataForm: Boolean; override;
  public
    fAttachList: TRssDbAttachments;
    procedure ShowItemsCount; override;
  end;

function ShowAttachsList(AttachList: TRssDbAttachments): Boolean;

implementation

{$R *.dfm}

uses
  RVclUtils, RWait, RDialogs, FileCtrl;

const
  imDefault = 15;

resourcestring
  SRecTitleName         = 'Вложения для записи "%s": всего %d файл(ов)';
  SRecTitleCnt          = 'Вложения: всего %d файл(ов)';

  SMsgPrepare           = 'Подготовка операции...';
  SMsgLoadFile          = 'Загрузка файла "%s"...';
  SMsgSaveFile          = 'Сохранение файла "%s"...';
  SMsgOpenFile          = 'Открытие файла "%s"...';
  SMsgDeleteFile        = 'Удаление файла "%s"...';
  SMsgDirSelect         = 'Выберите каталог для сохранения файлов';

  SQryDeleteAttachment  = 'Удалить файл "%s" из списка прикрепленных файлов (вложений)?';
  SQryDeleteAttachments = 'Удалить выделенные файлы ( %d ) из списка прикрепленных файлов (вложений)?';

function ShowAttachsList(AttachList: TRssDbAttachments): Boolean;
begin
  Result := False;
  with TFormAttachs.Create(Application) do
  begin
    try
      fAttachList := AttachList;
      Tag := fAttachList.ViewTag;
      ListView.Tag := fAttachList.EditTag;
      ShowWaitOnLoad := True;
      if LoadData then
        Result := (ShowModal = mrOk);
    finally
      Free;
    end;
  end;
end;

function TFormAttachs.GetDetailName: string;
begin
  Result := inherited GetDetailName;
  if Assigned(fAttachList) then
    Result := fAttachList.DetailName;
end;

procedure TFormAttachs.InitForm;
begin
  inherited InitForm;
  fAttachList := nil;
end;

procedure TFormAttachs.FreeForm;
begin
  fAttachList := nil;
  inherited FreeForm;
end;

procedure TFormAttachs.LogOnOpen;
begin
  // if Assigned(fAttachList) then
  //   fAttachList.Open(True);
end;

procedure TFormAttachs.LogOnClose;
begin
  // if Assigned(fAttachList) then
  //   fAttachList.Close(True);
end;

procedure TFormAttachs.ShowItemsCount;
begin
  inherited ShowItemsCount;
  if Assigned(fAttachList) then
  begin
    if fAttachList.RecordName <> EmptyStr
    then StatusBar.SimpleText := Format(SRecTitleName, [fAttachList.RecordName, fAttachList.RecordCount])
    else StatusBar.SimpleText := Format(SRecTitleCnt, [fAttachList.RecordCount]);
  end;
end;

function TFormAttachs.LoadDataForm: Boolean;
begin
  Result := Assigned(fAttachList)
    and fAttachList.LoadAttachments(ListView, imDefault);
end;

procedure TFormAttachs.AddFileUpdate(Sender: TObject);
begin
  AddFile.Enabled := IsNotWait and Assigned(fAttachList) and fAttachList.IsEditEnable;
end;

procedure TFormAttachs.AddFileExecute(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog.Execute then
  begin
    StartWait;
    ShowWaitMsg(SMsgPrepare);
    try
      for i := 0 to OpenDialog.Files.Count - 1 do
      begin
        ShowWaitMsg(Format(SMsgLoadFile, [ExtractFileName(OpenDialog.Files[i])]));
        if fAttachList.CreateAttachment(ListView, OpenDialog.Files[i], imDefault)
        then ShowItemsCount
        else Break;
      end;
    finally
      CloseWaitMsg;
      ShowItemsCount;
      StopWait;
    end;
  end;
end;

procedure TFormAttachs.DelFileUpdate(Sender: TObject);
begin
  DelFile.Enabled := IsNotWait and Assigned(fAttachList) and fAttachList.IsEditEnable
    and (ListView.SelCount > 0);
end;

procedure TFormAttachs.DelFileExecute(Sender: TObject);
var
  QryText: string;
  SelItem: TListItem;
begin
  if ListView.MultiSelect
  then QryText := Format(SQryDeleteAttachments, [ListView.SelCount])
  else QryText := Format(SQryDeleteAttachment, [ListView.Selected.Caption]);
  if DeleteQueryText(QryText) then
  begin
    StartWait;
    ShowWaitMsg(SMsgPrepare);
    try
      SelItem := ListView.Selected;
      while SelItem <> nil do
      begin
        ShowWaitMsg(Format(SMsgDeleteFile, [SelItem.Caption]));
        if not fAttachList.DeleteAttachment(SelItem) then
          SelItem.Selected := False;
        SelItem := ListView.GetNextItem(SelItem, sdAll, [isSelected]);
      end;
      ListView.DeleteSelected;
    finally
      CloseWaitMsg;
      ShowItemsCount;
      StopWait;
    end;
  end;
end;

procedure TFormAttachs.OpenFileUpdate(Sender: TObject);
begin
  OpenFile.Enabled := IsNotWait and Assigned(fAttachList) and fAttachList.Active
    and (ListView.SelCount > 0);
end;

procedure TFormAttachs.OpenFileExecute(Sender: TObject);
var
  SelItem: TListItem;
begin
  StartWait;
  ShowWaitMsg(SMsgPrepare);
  try
    SelItem := ListView.Selected;
    while SelItem <> nil do
    begin
      ShowWaitMsg(Format(SMsgOpenFile, [SelItem.Caption]));
      if not fAttachList.OpenAttachment(SelItem) then
        SelItem.Selected := False;
      SelItem := ListView.GetNextItem(SelItem, sdAll, [isSelected]);
    end;
  finally
    CloseWaitMsg;
    ShowItemsCount;
    StopWait;
  end;
end;

procedure TFormAttachs.ListViewDblClick(Sender: TObject);
begin
  if IsNotWait and Assigned(fAttachList) and fAttachList.Active and (ListView.SelCount > 0) then
    OpenFileExecute(Sender);
end;

procedure TFormAttachs.SaveAsFileUpdate(Sender: TObject);
begin
  SaveAsFile.Enabled := IsNotWait and Assigned(fAttachList) and fAttachList.Active
    and (ListView.SelCount > 0);
end;

procedure TFormAttachs.SaveAsFileExecute(Sender: TObject);
var
  SelItem: TListItem;
  DirName: string;
begin
  if ListView.SelCount > 1 then
  begin
    if SelectDirectory(SMsgDirSelect, EmptyStr, DirName) then
    begin
      StartWait;
      ShowWaitMsg(SMsgPrepare);
      try
        SelItem := ListView.Selected;
        while SelItem <> nil do
        begin
          ShowWaitMsg(Format(SMsgSaveFile, [SelItem.Caption]));
          if not fAttachList.SaveAttachment(SelItem,
              IncludeTrailingPathDelimiter(DirName) + SelItem.Caption) then
            SelItem.Selected := False;
          SelItem := ListView.GetNextItem(SelItem, sdAll, [isSelected]);
        end;
      finally
        CloseWaitMsg;
        ShowItemsCount;
        StopWait;
      end;
    end;
  end
  else begin
    SelItem := ListView.Selected;
    SaveDialog.FileName := SelItem.Caption;
    if SaveDialog.Execute then
    begin
      StartWait;
      ShowWaitMsg(Format(SMsgSaveFile, [ExtractFileName(SaveDialog.FileName)]));
      try
        fAttachList.SaveAttachment(SelItem, SaveDialog.FileName);
      finally
        CloseWaitMsg;
        ShowItemsCount;
        StopWait;
      end;
    end;
  end;
end;

end.
