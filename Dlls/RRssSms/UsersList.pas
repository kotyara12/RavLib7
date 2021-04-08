unit UsersList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView,
  AdoDb;

type
  TFormUsers = class(TDialogTemplate)
    ListView: TRSortListView;
    SelAllButton: TButton;
    UnseclectButton: TButton;
    procedure SelAllButtonClick(Sender: TObject);
    procedure UnseclectButtonClick(Sender: TObject);
  private
    procedure LoadUserList(Db: TAdoConnection; const UserId: Integer; const OldList: string);
    function  GetUserList(const OldList: string): string;
  end;

function GetUserList(Db: TAdoConnection; const UserId: Integer; const OldList: string): string;

implementation

{$R *.dfm}

uses
  RDbUtils, RDbConst, RVclUtils, RExHandlers, RSmsVars, RxStrUtils, RDialogs;

function GetUserList(Db: TAdoConnection; const UserId: Integer; const OldList: string): string;
begin
  with TFormUsers.Create(Application) do
  begin
    Result := EmptyStr;
    try
      LoadUserList(Db, UserId, OldList);
      if ShowModal = mrOk then Result := GetUserList(OldList);
    finally
      Free;
    end;
  end;
end;

procedure TFormUsers.LoadUserList(Db: TAdoConnection; const UserId: Integer; const OldList: string);
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
          Qry := OpenDynamicQuery(Db, Format(sqlActiveUsers, [UserId]));
          if DataSetIsNotEmpty(Qry) then begin
            Qry.First;
            while not Qry.Eof do
            begin
              with ListView.Items.Add do
              begin
                Caption := Qry.FieldByName(fnNAME).AsString;
                Checked := not Qry.FieldByName(fnBLOCKED).AsBoolean
                  and IsAddrPresent(Caption, OldList, True);
                Subitems.Add(Qry.FieldByName(fnFULLNAME).AsString);
                Subitems.Add(Qry.FieldByName(fnNOTES).AsString);
              end;
              Qry.Next;
            end;
          end;
        except
          on E: Exception do
            HandleSqlExcept(E, nil, Format(sqlActiveUsers, [UserId]), SErrLoadList);
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

function TFormUsers.GetUserList(const OldList: string): string;
begin
  StartWait;
  try
    Result := RecreateAddrList(OldList, ListView);
  finally
    StopWait;
  end;
end;

procedure TFormUsers.SelAllButtonClick(Sender: TObject);
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

procedure TFormUsers.UnseclectButtonClick(Sender: TObject);
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
