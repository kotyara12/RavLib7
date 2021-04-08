unit WpsList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView, AdoDb;

type
  TFormWps = class(TDialogTemplate)
    ListView: TRSortListView;
    SelAllButton: TButton;
    UnseclectButton: TButton;
    procedure SelAllButtonClick(Sender: TObject);
    procedure UnseclectButtonClick(Sender: TObject);
  private
    procedure LoadWpsList(Db: TAdoConnection; const OldList: string);
    function  GetWpsList(const OldList: string): string;
  end;


function GetWpsList(Db: TAdoConnection; const OldList: string): string;

implementation

{$R *.dfm}

uses
  RDbUtils, RDbConst, RVclUtils, RExHandlers, RSmsVars, RxStrUtils;

function GetWpsList(Db: TAdoConnection; const OldList: string): string;
begin
  with TFormWps.Create(Application) do
  begin
    Result := EmptyStr;
    try
      LoadWpsList(Db, OldList);
      if ShowModal = mrOk then Result := GetWpsList(OldList);
    finally
      Free;
    end;
  end;
end;

procedure TFormWps.LoadWpsList(Db: TAdoConnection; const OldList: string);
var
  Qry: TAdoQuery;
begin
  StartWait;
  try
    ListView.Items.BeginUpdate;
    try
      ListView.Items.Clear;
      Qry := nil;
      try
        try
          Qry := OpenDynamicQuery(Db, sqlWpsList);
          if DataSetIsNotEmpty(Qry) then begin
            Qry.First;
            while not Qry.Eof do
            begin
              with ListView.Items.Add do
              begin
                Caption := Format(fmtWpPrefix, [Qry.FieldByName(fnID).AsString]);
                Checked := IsAddrPresent(Caption, OldList, True);
                Subitems.Add(Qry.FieldByName(fnNAME_S).AsString);
                Subitems.Add(Qry.FieldByName(fnNAME).AsString);
              end;
              Qry.Next;
            end;
          end;
        except
          on E: Exception do
            HandleSqlExcept(E, nil, sqlWpsList, SErrLoadList);
        end;
      finally
        FreeDynamicQuery(Qry);
      end;
    finally
      ListView.Items.EndUpdate;
    end;
  finally
    StopWait;
  end;
end;

function TFormWps.GetWpsList(const OldList: string): string;
begin
  StartWait;
  try
    Result := RecreateAddrList(OldList, ListView);
  finally
    StopWait;
  end;
end;

procedure TFormWps.SelAllButtonClick(Sender: TObject);
var
  i: Integer;
begin
  StartWait;
  try
    for i := 0 to ListView.Items.Count - 1 do
      ListView.Items[i].Checked := True;
  finally
    StopWait;
  end;
end;

procedure TFormWps.UnseclectButtonClick(Sender: TObject);
var
  i: Integer;
begin
  StartWait;
  try
    for i := 0 to ListView.Items.Count - 1 do
      ListView.Items[i].Checked := False;
  finally
    StopWait;
  end;
end;

end.
