unit TreeGroupAuditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplAuditDialog, StdCtrls, DBCtrls, DB, Mask, ComCtrls, Buttons,
  ExtCtrls;

type
  TFormTreeGroupAudit = class(TDbAuditDialogTemplate)
    lblId: TLabel;
    deId: TDBEdit;
    lblOwner: TLabel;
    deOwner: TDBEdit;
    lblName: TLabel;
    deName: TDBEdit;
    lblNotes: TLabel;
    deNotes: TDBMemo;
    procedure PageControlChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  BaseDbUnit;

procedure TFormTreeGroupAudit.PageControlChange(Sender: TObject);
begin
  inherited;

  if PageControl.ActivePageIndex = 0 then
    deName.SetFocus;
end;

end.
