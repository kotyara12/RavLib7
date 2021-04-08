unit TmplQuery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplQueryCustom, DB, RDbEditor, Menus, RDbStatus, RDbCustom,
  RDbGridTuner, RDbOrder, RDbFilter, RDbCustomSearch, RDbSearch, DBActns,
  ActnList, RDbPanel, Grids, DBGrids, RDbColorGrid, ExtCtrls, ComCtrls,
  ToolWin, RDbFind, RDbUpdater, StdCtrls, Buttons, Tabs;

type
  TQueryTemplate = class(TQueryCustomTemplate)
    DataSetFirstToolButton: TToolButton;
    DataSetPriorToolButton: TToolButton;
    DataSetNextToolButton: TToolButton;
    DataSetLastToolButton: TToolButton;
    SeparatorNav: TToolButton;
    DataSetFindToolButton: TToolButton;
    SeparatorFind: TToolButton;
    DataSetInsertToolButton: TToolButton;
    DataSetEditToolButton: TToolButton;
    DataSetDeleteToolButton: TToolButton;
    SeparatorEdit: TToolButton;
    DataToolButton: TToolButton;
    ReportsToolButton: TToolButton;
    SeparatorEnd: TToolButton;
    RefreshToolButton: TToolButton;
    CloseSelectToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
