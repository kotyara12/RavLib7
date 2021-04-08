unit SrOpersProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask;

type
  TFormSrOpersProp = class(TDbDialogTemplate)
    lblName: TLabel;
    deName: TDBEdit;
    lblNotes: TLabel;
    deNotes: TDBMemo;
    deId: TDBEdit;
    lblId: TLabel;
    lblNameLevels: TLabel;
    deNameLevels: TDBEdit;
    lblNotesLevels: TLabel;
    deNotesLevels: TDBMemo;
    lblHidden: TLabel;
    deHidden: TDBEdit;
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
