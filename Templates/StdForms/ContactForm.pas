unit ContactForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RXmlContacts;

type
  TFormContact = class(TDialogTemplate)
    lblContType: TLabel;
    edContType: TComboBox;
    lblContValue: TLabel;
    edContValue: TEdit;
    lblContName: TLabel;
    edContName: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    function GetContData: string;
    function GetContName: string;
    function GetContType: TContactType;
    procedure SetContData(const Value: string);
    procedure SetContName(const Value: string);
    procedure SetContType(const Value: TContactType);
  protected
  public
    property ContType: TContactType read GetContType write SetContType;
    property ContName: string read GetContName write SetContName;
    property ContData: string read GetContData write SetContData;
  end;

implementation

{$R *.dfm}

{ TFormContact }

procedure TFormContact.FormCreate(Sender: TObject);
var
  i: TContactType;
begin
  inherited;
  edContType.Items.BeginUpdate;
  try
    edContType.Items.Clear;
    for i := Low(TContactType) to High(TContactType) do
      edContType.Items.Add(SContactType[i]);
  finally
    edContType.Items.EndUpdate;
  end;
end;

function TFormContact.GetContData: string;
begin
  Result := edContValue.Text;
end;

function TFormContact.GetContName: string;
begin
  Result := edContName.Text;
end;

function TFormContact.GetContType: TContactType;
begin
  if edContType.ItemIndex in [Integer(Low(TContactType))..Integer(High(TContactType))]
  then Result := TContactType(edContType.ItemIndex)
  else Result := ctOther;
end;

procedure TFormContact.SetContData(const Value: string);
begin
  edContValue.Text := Value;
end;

procedure TFormContact.SetContName(const Value: string);
begin
  edContName.Text := Value;
end;

procedure TFormContact.SetContType(const Value: TContactType);
begin
  edContType.ItemIndex := Integer(Value);
end;


end.
