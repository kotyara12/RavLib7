unit DbNestedFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, ToolWin, Menus, DbFrame;

type
  TFrameDbNested = class(TFrame)
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    DbFirstToolButton: TToolButton;
    DbPriorToolButton: TToolButton;
    DbNextToolButton: TToolButton;
    DataSetLastToolButton: TToolButton;
    SeparatorNav: TToolButton;
    InsertToolButton: TToolButton;
    EditToolButton: TToolButton;
    DeleteToolButton: TToolButton;
    SeparatorEdit: TToolButton;
    DbFindToolButton: TToolButton;
    DataToolButton: TToolButton;
    RefreshToolButton: TToolButton;
    FrameDb: TFrameDb;
    SeparatorFind: TToolButton;
    SeparatorData: TToolButton;
  private
    function GetCustomOpenDataSet: TOpenDataSetProc;
    procedure SetCustomOpenDataSet(const Value: TOpenDataSetProc);
  public
    procedure Init;
    procedure Done;
    procedure InitDataComponents;
    procedure DoneDataComponents;
    function  OpenDataSet: Boolean;
    {$IFDEF STYLES}
    procedure SetStyle;
    {$ENDIF}
    property OnCustomOpenDataSet: TOpenDataSetProc read GetCustomOpenDataSet write SetCustomOpenDataSet;
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu, RDbState, RDbConst, RDbUtils, RDbPrint, RDbImport,
  RDbOpenDS, RDbText, RExHandlers, RClipBrd, ClipBrd, RxStrUtils, StrUtils,  
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  BaseDbUnit, TmplDbDialog;

{ TFrameDbNested }

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TFrameDbNested.SetStyle;
begin
  FrameDb.SetStyle;

  CoolBar.Visible := ApplicationStyle.DataForm.TbVisible;
  ToolBar.ShowCaptions := ApplicationStyle.DataForm.TbCaptions;
  ToolBar.ButtonHeight := 1;
  ToolBar.ButtonWidth := 1;
end;
{$ENDIF}

procedure TFrameDbNested.Init;
begin
  FrameDb.Init;
end;

procedure TFrameDbNested.Done;
begin
  FrameDb.Done;
end;

procedure TFrameDbNested.InitDataComponents;
begin
  FrameDb.InitDataComponents;
end;

procedure TFrameDbNested.DoneDataComponents;
begin
  FrameDb.DoneDataComponents;
end;

function TFrameDbNested.OpenDataSet: Boolean;
begin
  Result := FrameDb.OpenDataSet;
end;

function TFrameDbNested.GetCustomOpenDataSet: TOpenDataSetProc;
begin
  Result := FrameDb.OnCustomOpenDataSet;
end;

procedure TFrameDbNested.SetCustomOpenDataSet(const Value: TOpenDataSetProc);
begin
  FrameDb.OnCustomOpenDataSet := Value;
end;

end.
