unit UsersGroupProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask;

type
  TFormUsersGroup = class(TDbDialogTemplate)
    IDDBEditLabel: TLabel;
    IDDBEdit: TDBEdit;
    NAMEDBEditLabel: TLabel;
    NAMEDBEdit: TDBEdit;
    NOTESDBMemoLabel: TLabel;
    NOTESDBMemo: TDBMemo;
    OWNER_IDDBEditLabel: TLabel;
    OWNER_IDDBEdit: TDBEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  UsersForm;

end.
