unit RDbFilterItem_Null;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls;

type
  TFormDbFilterItem_Null = class(TFormDbFilterItem)
    ModeRadioGroup: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDbFilterItem_Null: TFormDbFilterItem_Null;

implementation

{$R *.dfm}

end.
