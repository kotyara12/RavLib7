unit TreeNodeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TFormTreeNode = class(TDialogTemplate)
    Memo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowTreeNode(const Node: TTreeNode);

implementation

{$R *.dfm}

procedure ShowTreeNode(const Node: TTreeNode);
begin
  with TFormTreeNode.Create(Application) do
  begin
    try
      Memo.Lines.Text := Node.Text;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

end.
