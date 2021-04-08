unit TmplMdiRss;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplMdiDb, AppEvnts, Menus, StdActns, ActnList, ImgList,
  ComCtrls, ToolWin, Db, AdoDb, RUserRights;

type
  TMdiMainRssTemplate = class(TMdiMainDbTemplate)
    itemChangeUserPassword: TMenuItem;
    divFile2: TMenuItem;
    menuSms: TMenuItem;
    divFile3: TMenuItem;
    itemReadMail: TMenuItem;
    divMail1: TMenuItem;
    itemViewMail: TMenuItem;
    itemSendMail: TMenuItem;
  private
  public
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RSysUtils, RDialogs, RMsgRu, RRssConst,
  RDbLog, RDbConst, RDbUtils, DM_TmplBaseRss, PrjVariables;

end.
