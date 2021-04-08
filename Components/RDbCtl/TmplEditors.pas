unit TmplEditors;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, ComCtrls, Menus, ActnList, ImgList, ToolWin;

type
  TEditorsTemplate = class(TStorageTemplate)
    StatusBar: TStatusBar;
    ImageList: TImageList;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    CloseCancel: TAction;
    CloseOk: TAction;
    ListView: TListView;
    itemCloseOkP: TMenuItem;
    itemCloseCancelP: TMenuItem;
    divPopup1: TMenuItem;
    ShowGridLines: TAction;
    itemShowGridLinesP: TMenuItem;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuService: TMenuItem;
    menuHelp: TMenuItem;
    itemCloseOk: TMenuItem;
    itemCloseCancel: TMenuItem;
    itemShowGridLines: TMenuItem;
    HelpContext: TAction;
    AboutBox: TAction;
    itemHelpContext: TMenuItem;
    divHelp1: TMenuItem;
    itemAboutBox: TMenuItem;
    menuEdit: TMenuItem;
    procedure CloseCancelUpdate(Sender: TObject);
    procedure CloseCancelExecute(Sender: TObject);
    procedure CloseOkUpdate(Sender: TObject);
    procedure CloseOkExecute(Sender: TObject);
    procedure ShowGridLinesUpdate(Sender: TObject);
    procedure ShowGridLinesExecute(Sender: TObject);
    procedure HelpContextUpdate(Sender: TObject);
    procedure HelpContextExecute(Sender: TObject);
    procedure AboutBoxUpdate(Sender: TObject);
    procedure AboutBoxExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
    procedure ShowItemCount; virtual;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RVclUtils, RFrmStorage;

resourcestring
  SItemsCount        = 'Всего элементов: %d';

{$IFDEF STYLES}
procedure TEditorsTemplate.SetStyle;
begin
  inherited;
  CoolBar.Visible := ApplicationStyle.DataForm.TbVisible;
  StatusBar.Visible := ApplicationStyle.DataForm.SbVisible;
  ListView.Color := ApplicationStyle.DataForm.DataColor;
  FontDataToFont(ApplicationStyle.DataForm.DataFont, ListView.Font);
end;
{$ENDIF}

{ == Сохранение и восстановление размеров формы ================================ }
procedure TEditorsTemplate.LoadFormControls;
begin
  inherited;
  LoadListColumns(Self, ListView);
end;

procedure TEditorsTemplate.SaveFormControls;
begin
  inherited;
  SaveListColumns(Self, ListView);
end;

{ == Отображение количества элементов ========================================== }
procedure TEditorsTemplate.ShowItemCount;
begin
  StatusBar.SimpleText := Format(SItemsCount, [ListView.Items.Count]);
end;

procedure TEditorsTemplate.FormShow(Sender: TObject);
begin
  inherited;
  ShowItemCount;
end;

{ == Закрыть окно ============================================================== }
procedure TEditorsTemplate.CloseCancelUpdate(Sender: TObject);
begin
  CloseCancel.Enabled := IsNotWait;
end;

procedure TEditorsTemplate.CloseCancelExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

{ == Выбрать запись и закрыть окно ============================================= }
procedure TEditorsTemplate.CloseOkUpdate(Sender: TObject);
begin
  CloseOk.Enabled := IsNotWait;
end;

procedure TEditorsTemplate.CloseOkExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

{ == Переключение режима отображения линий сетки =============================== }
procedure TEditorsTemplate.ShowGridLinesUpdate(Sender: TObject);
begin
  ShowGridLines.Enabled := IsNotWait;
  ShowGridLines.Checked := ListView.GridLines;
end;

procedure TEditorsTemplate.ShowGridLinesExecute(Sender: TObject);
begin
  ListView.GridLines := not ListView.GridLines;
end;

{ == Вызов справочной информации =============================================== }
procedure TEditorsTemplate.HelpContextUpdate(Sender: TObject);
begin
  HelpContext.Enabled := FileExists(HelpFile) and (HelpKeyword <> EmptyStr)
    and IsNotWait;
end;

procedure TEditorsTemplate.HelpContextExecute(Sender: TObject);
begin
  Application.HelpKeyword(HelpKeyword);
end;

{ == Информация о модуле ======================================================= }
procedure TEditorsTemplate.AboutBoxUpdate(Sender: TObject);
begin
  AboutBox.Enabled := IsNotWait;
end;

procedure TEditorsTemplate.AboutBoxExecute(Sender: TObject);
begin
  Application.MessageBox('Rav Db Components Library for Delphi 7'#13#13 +
    'Copyright (c) 2005-2012, Разживин Александр Валерьевич.'#13 +
    'http://ravsoft2004.narod.ru, e-mail: ravsoft2004@yandex.ru.',
    'RavDbLibrary 7', MB_OK + MB_ICONINFORMATION);
end;

end.
