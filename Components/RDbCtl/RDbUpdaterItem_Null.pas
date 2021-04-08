unit RDbUpdaterItem_Null;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbUpdaterItem, StdCtrls, Buttons, ExtCtrls;

type
  TFormDbUpdaterItem_Null = class(TFormDbUpdaterItem)
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormDbUpdaterItem_Null.FormActivate(Sender: TObject);
begin
  inherited;
  // ChangeState(Sender);
end;

end.
