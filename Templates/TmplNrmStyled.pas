unit TmplNrmStyled;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplBase, TmplMdiMain, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, RMessages, ToolWin, TmplNrmMain;

type
  TNrmMainStyledTemplate = class(TNrmMainTemplate)
    menuView: TMenuItem;
    ChangeStyle: TAction;
    itemChangeStyle: TMenuItem;
    MainToolBar: TAction;
    MainStatusBar: TAction;
    divView1: TMenuItem;
    itemMainToolBar: TMenuItem;
    itemMainStatusBar: TMenuItem;
    procedure ChangeStyleUpdate(Sender: TObject);
    procedure ChangeStyleExecute(Sender: TObject);
    procedure MainToolBarUpdate(Sender: TObject);
    procedure MainToolBarExecute(Sender: TObject);
    procedure MainStatusBarUpdate(Sender: TObject);
    procedure MainStatusBarExecute(Sender: TObject);
  private
    procedure AppStyleMsgHandler(var Msg: TMessage); message WM_SETFORMSTYLE;
  protected
    procedure InitApplication; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RAppStyles, RAppStylesDll, RExHandlers, RMsgRu, RWait,
  PrjVariables;

{ == Инициализация приложения ================================================== }
procedure TNrmMainStyledTemplate.InitApplication;
begin
  inherited;
  ShowInStatusBar(SMsgSetStyles);
  try
    LoadAndSetStyles(SMainStylesFile);
  except
    on E: Exception do
      HandleExcept(E, Self, SErrSetStyles);
  end;
end;

{ == Пересылка сообщения WM_SETFORMSTYLE всем дочерним формам приложения ======= }
procedure TNrmMainStyledTemplate.AppStyleMsgHandler(var Msg: TMessage);
begin
  if Msg.WParam = ID_SETFORMSTYLE then
  begin
    Msg.Result := 1;
    StartWait;
    ShowWaitMsg(SMsgSetStyles);
    try
      SetStyle;
    finally
      CloseWaitMsg;
      StopWait;
    end;
  end;
end;

{ == Настройка интерфейса программы ============================================ }
procedure TNrmMainStyledTemplate.ChangeStyleUpdate(Sender: TObject);
begin
  ChangeStyle.Enabled := IsNotWait;
end;

procedure TNrmMainStyledTemplate.ChangeStyleExecute(Sender: TObject);
var
  Edited: Boolean;
begin
  ShowInStatusBar(SMsgChangeStyles);
  Edited := EditAppStyles;
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    SaveAndSetStyles(SMainStylesFile, Edited);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Панель инструментов главного окна ========================================= }
procedure TNrmMainStyledTemplate.MainToolBarUpdate(Sender: TObject);
begin
  MainToolBar.Enabled := IsNotWait;
  MainToolBar.Checked := ApplicationStyle.MainForm.TbVisible;
end;

procedure TNrmMainStyledTemplate.MainToolBarExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.MainForm.TbVisible := not ApplicationStyle.MainForm.TbVisible;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Строка статуса главного окна ============================================== }
procedure TNrmMainStyledTemplate.MainStatusBarUpdate(Sender: TObject);
begin
  MainStatusBar.Enabled := IsNotWait;
  MainStatusBar.Checked := ApplicationStyle.MainForm.SbVisible;
end;

procedure TNrmMainStyledTemplate.MainStatusBarExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.MainForm.SbVisible := not ApplicationStyle.MainForm.SbVisible;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

end.
