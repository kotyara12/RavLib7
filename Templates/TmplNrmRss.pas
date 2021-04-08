unit TmplNrmRss;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplMdiDb, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, ToolWin, Db, AdoDb, RUserRights, TmplNrmDb;

type
  TNrmMainRssTemplate = class(TNrmMainDbTemplate)
    itemChangeUserPassword: TMenuItem;
    divFile2: TMenuItem;
    menuSms: TMenuItem;
    divFile3: TMenuItem;
    itemReadMail: TMenuItem;
    divMail1: TMenuItem;
    itemViewMail: TMenuItem;
    itemSendMail: TMenuItem;
  private
  protected
  public
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RSysUtils, RDialogs, RMsgRu, RRssConst, RExHandlers, RExHandlersDbLog,
  RDbLog, RDbConst, RDbUtils, DM_TmplBaseRss, PrjVariables;

end.
