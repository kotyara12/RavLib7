unit TmplDbFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplStorage, Menus, DbFrame, ComCtrls, ToolWin;

type
  TDbFrameTemplate = class(TStorageTemplate)
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    DbFrame: TFrameDb;
    MainMenu: TMainMenu;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
