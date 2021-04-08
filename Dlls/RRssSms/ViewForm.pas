unit ViewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, ComCtrls, ImgList, ActnList, ToolWin, Grids,
  DBGrids, RDbColorGrid, DB, ADODB, RDbOrder, RDbCustom, RDbFilter, RDbFind,
  RDbGridTuner, DBActns, Menus, RDbStatus, RDbCustomSearch, RDbSearch;

type
  TFormView = class(TStorageTemplate)
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    ActionList: TActionList;
    ImageList: TImageList;
    StatusBar: TStatusBar;
    DbGrid: TRDbStyledGrid;
    SM_MESSAGES: TADOQuery;
    DataSource: TDataSource;
    RDbFilter: TRDbFilter;
    RDbOrder: TRDbOrder;
    CloseForm: TAction;
    CloseFormToolButton: TToolButton;
    SM_MESSAGESID_USERS: TIntegerField;
    SM_MESSAGESNAME: TStringField;
    SM_MESSAGESFULLNAME: TStringField;
    SM_MESSAGESSENDED: TDateTimeField;
    SM_MESSAGESTITLE: TStringField;
    SM_MESSAGESADDRESS: TStringField;
    RDbGridTuner: TRDbGridTuner;
    RDbFilter_NAME: TRDFStringItem;
    RDbFilter_SENDED: TRDFDateItem;
    RDbFilter_FULLNAME: TRDFStringItem;
    RDbFilter_TITLE: TRDFStringItem;
    RDbFilter_ADDRESS: TRDFStringItem;
    DataSetFirstToolButton: TToolButton;
    DataSetPriorToolButton: TToolButton;
    DataSetNextToolButton: TToolButton;
    DataSetLastToolButton: TToolButton;
    SeparatorNav: TToolButton;
    SeparatorView: TToolButton;
    SeparatorFilter: TToolButton;
    PopupMenu: TPopupMenu;
    itemDataSetFirst: TMenuItem;
    itemDataSetPrior: TMenuItem;
    itemDataSetNext: TMenuItem;
    itemDataSetLast: TMenuItem;
    divN1: TMenuItem;
    itemCloseForm: TMenuItem;
    ReadMessageId: TAction;
    SM_MESSAGESID: TIntegerField;
    ReadMessageIdToolButton: TToolButton;
    itemReadMessageId: TMenuItem;
    divN2: TMenuItem;
    ChangeFilter: TAction;
    ChangeFilterToolButton: TToolButton;
    itemChangeFilter: TMenuItem;
    divN3: TMenuItem;
    ChangeSort: TAction;
    ChangeSortToolButton: TToolButton;
    itemChangeSort: TMenuItem;
    FindData: TAction;
    FindDataToolButton: TToolButton;
    itemFindData: TMenuItem;
    RDbStatus: TRDbStatus;
    DataSetFirst: TDataSetFirst;
    DataSetPrior: TDataSetPrior;
    DataSetNext: TDataSetNext;
    DataSetLast: TDataSetLast;
    RDbSearch: TRDbSearch;
    procedure CloseFormUpdate(Sender: TObject);
    procedure CloseFormExecute(Sender: TObject);
    procedure ReadMessageIdUpdate(Sender: TObject);
    procedure ReadMessageIdExecute(Sender: TObject);
    procedure ChangeFilterUpdate(Sender: TObject);
    procedure ChangeFilterExecute(Sender: TObject);
    procedure DbGridDblClick(Sender: TObject);
    procedure ChangeSortUpdate(Sender: TObject);
    procedure ChangeSortExecute(Sender: TObject);
    procedure FindDataUpdate(Sender: TObject);
    procedure FindDataExecute(Sender: TObject);
    procedure DataSetFirstUpdate(Sender: TObject);
    procedure DataSetLastUpdate(Sender: TObject);
    procedure DataSetFirstExecute(Sender: TObject);
    procedure DataSetPriorExecute(Sender: TObject);
    procedure DataSetNextExecute(Sender: TObject);
    procedure DataSetLastExecute(Sender: TObject);
  private
    fUserId: Integer;
    fArmId: Integer;
    procedure OpenSettings;
    procedure SaveSettings;
  public
    procedure LoadMessages;
    procedure SetStyle; override;
  end;

procedure ShowMessagesList(Db: TAdoConnection; const ArmId, UserId: Integer);

implementation

{$R *.dfm}

uses
  RVclUtils, RExHandlers, RRssConst, RSmsVars, RDbConst, RAppStyles, RFonts,
  ReadForm;

procedure ShowMessagesList(Db: TAdoConnection; const ArmId, UserId: Integer);
begin
  with TFormView.Create(Application) do
  begin
    try
      SM_MESSAGES.Connection := Db;
      fArmId := ArmId;
      fUserId := UserId;
      OpenSettings;
      LoadMessages;
      ShowModal;
      SaveSettings;
    finally
      Free;
    end;
  end;
end;

procedure TFormView.SetStyle;
begin
  inherited;
  DbGrid.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, DbGrid.Font);
end;

{ == Загрузка данных =========================================================== }
procedure TFormView.OpenSettings;
begin
  StartWait;
  try
    RDbFilter.DateFormatWhere := Dp.DateFormat;
    RDbFilter.CaseStringsEnabled := Dp.CaseEnabled;
    if not RDbFilter.Active then RDbFilter.Open;
    if not RDbOrder.Active then RDbOrder.Open;
    if not RDbSearch.Active then RDbSearch.Open;
    if not RDbGridTuner.Active then RDbGridTuner.Open;
  finally
    StopWait;
  end;
end;

procedure TFormView.SaveSettings;
begin
  StartWait;
  try
    if RDbFilter.Active then RDbFilter.Close;
    if RDbOrder.Active then RDbOrder.Close;
    if RDbSearch.Active then RDbSearch.Close;
    if RDbGridTuner.Active then RDbGridTuner.Close;
    if SM_MESSAGES.Active then SM_MESSAGES.Close;
  finally
    StopWait;
  end;
end;

procedure TFormView.LoadMessages;
var
  SqlPart: string;
begin
  StartWait;
  try
    try
      SM_MESSAGES.SQL.Clear;
      SM_MESSAGES.SQL.Add(Format(sqlReadSmsList, [fUserId, fArmId, fUserId]));
      SqlPart := RDbFilter.GetWhereString;
      if SqlPart <> EmptyStr then
        SM_MESSAGES.SQL.Add(sqlAnd + SqlPart);
      SqlPart := RDbOrder.GetOrderString;
      if SqlPart <> EmptyStr then
        SM_MESSAGES.SQL.Add(Format(sqlOrder, [SqlPart]));
      SM_MESSAGES.Open;
    except
      on E: Exception do
        HandleExcept(E, SM_MESSAGES, EErrLoadMessages);
    end;
  finally
    StopWait;
  end;
end;

{ == Закрыть окно ============================================================== }
procedure TFormView.CloseFormUpdate(Sender: TObject);
begin
  CloseForm.Enabled := IsNotWait;
end;

procedure TFormView.CloseFormExecute(Sender: TObject);
begin
  Close;
end;

{ == Просмотр сообщения ======================================================== }
procedure TFormView.ReadMessageIdUpdate(Sender: TObject);
begin
  ReadMessageId.Enabled := IsNotWait and SM_MESSAGES.Active and not SM_MESSAGES.IsEmpty;
end;

procedure TFormView.ReadMessageIdExecute(Sender: TObject);
begin
  ReadSms(SM_MESSAGES.Connection, SM_MESSAGESID.AsInteger, fUserId, True);
end;

procedure TFormView.DbGridDblClick(Sender: TObject);
begin
  if ReadMessageId.Enabled then ReadMessageIdExecute(Sender);
end;

{ == Поиск сообщения =========================================================== }
procedure TFormView.FindDataUpdate(Sender: TObject);
begin
  FindData.Enabled := IsNotWait and RDbSearch.Active and SM_MESSAGES.Active;
end;

procedure TFormView.FindDataExecute(Sender: TObject);
begin
  RDbSearch.ShowDialog;
end;

{ == Фильтр сообщений ========================================================== }
procedure TFormView.ChangeFilterUpdate(Sender: TObject);
begin
  ChangeFilter.Enabled := IsNotWait and RDbFilter.Active and SM_MESSAGES.Active;
end;

procedure TFormView.ChangeFilterExecute(Sender: TObject);
begin
  if RDbFilter.ShowDialog then LoadMessages;
end;

{ == Выбор направления сортировки сообщений ==================================== }
procedure TFormView.ChangeSortUpdate(Sender: TObject);
begin
  ChangeSort.Enabled := IsNotWait and RDbOrder.Active and SM_MESSAGES.Active;
end;

procedure TFormView.ChangeSortExecute(Sender: TObject);
begin
  if RDbOrder.ShowDialog then LoadMessages;
end;

procedure TFormView.DataSetFirstUpdate(Sender: TObject);
begin
  DataSetFirst.Enabled := SM_MESSAGES.Active and not SM_MESSAGES.Bof;
  DataSetPrior.Enabled := DataSetFirst.Enabled;
end;

procedure TFormView.DataSetLastUpdate(Sender: TObject);
begin
  DataSetLast.Enabled := SM_MESSAGES.Active and not SM_MESSAGES.Eof;
  DataSetNext.Enabled := DataSetLast.Enabled;
end;

procedure TFormView.DataSetFirstExecute(Sender: TObject);
begin
  SM_MESSAGES.First;
end;

procedure TFormView.DataSetPriorExecute(Sender: TObject);
begin
  SM_MESSAGES.Prior;
end;

procedure TFormView.DataSetNextExecute(Sender: TObject);
begin
  SM_MESSAGES.Next;
end;

procedure TFormView.DataSetLastExecute(Sender: TObject);
begin
  SM_MESSAGES.Last;
end;

end.
