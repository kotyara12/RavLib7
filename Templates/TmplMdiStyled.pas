unit TmplMdiStyled;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplBase, TmplMdiMain, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, RMessages, ToolWin;

type
  TMdiMainStyledTemplate = class(TMdiMainTemplate)
    menuView: TMenuItem;
    ChangeStyle: TAction;
    itemChangeStyle: TMenuItem;
    MainToolBar: TAction;
    MainStatusBar: TAction;
    DataToolBar: TAction;
    DataStatusBar: TAction;
    DataInfoPanel: TAction;
    divView1: TMenuItem;
    itemMainToolBar: TMenuItem;
    itemMainStatusBar: TMenuItem;
    divView2: TMenuItem;
    itemDataToolBar: TMenuItem;
    itemDataStatusBar: TMenuItem;
    itemDataInfoPanel: TMenuItem;
    DataToolCaptions: TAction;
    itemDataToolCaptions: TMenuItem;
    DataFindPanel: TAction;
    itemDataFindPanel: TMenuItem;
    procedure ChangeStyleUpdate(Sender: TObject);
    procedure ChangeStyleExecute(Sender: TObject);
    procedure MainToolBarUpdate(Sender: TObject);
    procedure MainToolBarExecute(Sender: TObject);
    procedure MainStatusBarUpdate(Sender: TObject);
    procedure MainStatusBarExecute(Sender: TObject);
    procedure DataToolBarUpdate(Sender: TObject);
    procedure DataToolBarExecute(Sender: TObject);
    procedure DataStatusBarUpdate(Sender: TObject);
    procedure DataStatusBarExecute(Sender: TObject);
    procedure DataInfoPanelUpdate(Sender: TObject);
    procedure DataInfoPanelExecute(Sender: TObject);
    procedure DataToolCaptionsUpdate(Sender: TObject);
    procedure DataToolCaptionsExecute(Sender: TObject);
    procedure DataFindPanelUpdate(Sender: TObject);
    procedure DataFindPanelExecute(Sender: TObject);
  private
    procedure AppStyleMsgHandler(var Msg: TMessage); message WM_SETFORMSTYLE;
  protected
    function  InitApplication: Boolean; override;
  public
    function  UniqueMdiWins: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RAppStyles, RAppStylesDll, RExHandlers, RMsgRu, RWait, RDialogs,
  PrjVariables;

{ == Инициализация приложения ================================================== }
function TMdiMainStyledTemplate.InitApplication: Boolean;
begin
  Result := inherited InitApplication;
  if Result then
  begin
    ShowInStatusBar(SMsgSetStyles);
    try
      LoadAndSetStyles(SMainStylesFile);
    except
      on E: Exception do
        HandleExcept(E, Self, SErrSetStyles);
    end;
  end;
end;

function TMdiMainStyledTemplate.UniqueMdiWins: Boolean;
begin
  Result := ApplicationStyle.MainForm.MdiUnique;
end;

{ == Пересылка сообщения WM_SETFORMSTYLE всем дочерним формам приложения ======= }
procedure TMdiMainStyledTemplate.AppStyleMsgHandler(var Msg: TMessage);
var
  i: Integer;
begin
  if Msg.WParam = ID_SETFORMSTYLE then
  begin
    Msg.Result := 1;
    StartWait;
    try
      SetStyle;
      for i := 0 to MDIChildCount - 1 do
        MDIChildren[i].Perform(WM_SETFORMSTYLE, ID_SETFORMSTYLE, 0);
    finally
      StopWait;
    end;
  end;
end;

{ == Настройка интерфейса программы ============================================ }
procedure TMdiMainStyledTemplate.ChangeStyleUpdate(Sender: TObject);
begin
  ChangeStyle.Enabled := IsNotWait;
end;

procedure TMdiMainStyledTemplate.ChangeStyleExecute(Sender: TObject);
var
  Edited: Boolean;
begin
  try
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
  except
    on E: Exception do
      HandleExcept(E, Sender, SErrChangeStyle);
  end;
end;

{ == Панель инструментов главного окна ========================================= }
procedure TMdiMainStyledTemplate.MainToolBarUpdate(Sender: TObject);
begin
  MainToolBar.Enabled := IsNotWait;
  MainToolBar.Checked := ApplicationStyle.MainForm.TbVisible;
end;

procedure TMdiMainStyledTemplate.MainToolBarExecute(Sender: TObject);
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
procedure TMdiMainStyledTemplate.MainStatusBarUpdate(Sender: TObject);
begin
  MainStatusBar.Enabled := IsNotWait;
  MainStatusBar.Checked := ApplicationStyle.MainForm.SbVisible;
end;

procedure TMdiMainStyledTemplate.MainStatusBarExecute(Sender: TObject);
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

{ == Панель инструментов окна данных =========================================== }
procedure TMdiMainStyledTemplate.DataToolBarUpdate(Sender: TObject);
begin
  DataToolBar.Enabled := IsNotWait;
  DataToolBar.Checked := ApplicationStyle.DataForm.TbVisible;
end;

procedure TMdiMainStyledTemplate.DataToolBarExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.DataForm.TbVisible := not ApplicationStyle.DataForm.TbVisible;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TMdiMainStyledTemplate.DataToolCaptionsUpdate(Sender: TObject);
begin
  DataToolCaptions.Enabled := IsNotWait;
  DataToolCaptions.Checked := ApplicationStyle.DataForm.TbCaptions;
end;

procedure TMdiMainStyledTemplate.DataToolCaptionsExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.DataForm.TbCaptions := not ApplicationStyle.DataForm.TbCaptions;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Строка статуса окна данных ================================================ }
procedure TMdiMainStyledTemplate.DataStatusBarUpdate(Sender: TObject);
begin
  DataStatusBar.Enabled := IsNotWait;
  DataStatusBar.Checked := ApplicationStyle.DataForm.SbVisible;
end;

procedure TMdiMainStyledTemplate.DataStatusBarExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.DataForm.SbVisible := not ApplicationStyle.DataForm.SbVisible;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Панель детальной информации =============================================== }
procedure TMdiMainStyledTemplate.DataInfoPanelUpdate(Sender: TObject);
begin
  DataInfoPanel.Enabled := IsNotWait;
  DataInfoPanel.Checked := ApplicationStyle.DataForm.InfoPanelVisible;
end;

procedure TMdiMainStyledTemplate.DataInfoPanelExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.DataForm.InfoPanelVisible := not ApplicationStyle.DataForm.InfoPanelVisible;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Панель быстрого поиска ==================================================== }
procedure TMdiMainStyledTemplate.DataFindPanelUpdate(Sender: TObject);
begin
  DataFindPanel.Enabled := IsNotWait;
  DataFindPanel.Checked := ApplicationStyle.DataForm.FastFindPanel;
end;

procedure TMdiMainStyledTemplate.DataFindPanelExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgSaveData);
  StartWait;
  try
    ApplicationStyle.DataForm.FastFindPanel := not ApplicationStyle.DataForm.FastFindPanel;
    SaveAndSetStyles(SMainStylesFile);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

end.
