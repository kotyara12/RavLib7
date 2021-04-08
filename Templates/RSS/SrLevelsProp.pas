unit SrLevelsProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbColorDialog, DB, StdCtrls, Buttons, ExtCtrls, DBCtrls,
  Mask;

type
  TFormSrLevelsProp = class(TDbColorDialogTemplate)
    lblName: TLabel;
    deName: TDBEdit;
    lblNotes: TLabel;
    deNotes: TDBMemo;
    deHidden: TDBCheckBox;
    deId: TDBEdit;
    lblId: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  AdminUnit;

end.
