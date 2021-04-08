unit RptNameForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask;

type
  TFormRptName = class(TDbDialogTemplate)
    lblName: TLabel;
    deName: TDBEdit;
    lblNotes: TLabel;
    deNotes: TDBMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  ReportsForm;
  
end.
