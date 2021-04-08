unit DbEditorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, StdCtrls, DBCtrls, Mask, DB, Buttons, ExtCtrls;

type
  TFormDbEditor = class(TDbDialogTemplate)
    lblId: TLabel;
    lblName: TLabel;
    lblNotes: TLabel;
    deId: TDBEdit;
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
