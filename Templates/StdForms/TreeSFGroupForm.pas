unit TreeSFGroupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, StdCtrls, DBCtrls, Mask, DB, Buttons, ExtCtrls;

type
  TFormTreeGroup = class(TDbDialogTemplate)
    lblId: TLabel;
    lblOwnerId: TLabel;
    lblNameS: TLabel;
    lblNotes: TLabel;
    deId: TDBEdit;
    deOwnerId: TDBEdit;
    deNameS: TDBEdit;
    deNotes: TDBMemo;
    Label1: TLabel;
    deNameF: TDBEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

end.
