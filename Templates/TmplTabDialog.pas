unit TmplTabDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, ComCtrls, DB, StdCtrls, Buttons, ExtCtrls;

type
  TDbTabDialogTemplate = class(TDbDialogTemplate)
    PageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

type
  THack = class(TPageControl);

procedure TDbTabDialogTemplate.FormCreate(Sender: TObject);
begin
  inherited;

  THack(PageControl).Color := Self.Color; // ApplicationStyle.DataForm.FormColor;
end;

end.
