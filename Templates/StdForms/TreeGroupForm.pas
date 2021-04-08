unit TreeGroupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, StdCtrls, DBCtrls, Mask, DB, Buttons, ExtCtrls;

type
  TFormTreeGroup = class(TDbDialogTemplate)
    lblId: TLabel;
    lblOwnerId: TLabel;
    lblName: TLabel;
    lblNotes: TLabel;
    deId: TDBEdit;
    deOwnerId: TDBEdit;
    deName: TDBEdit;
    deNotes: TDBMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

end.
