unit SmsList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbSimple, Menus, ImgList, ActnList, RDbStatus, RDbCustom,
  RDbGridTuner, RDbOrder, RDbFilter, RDbCustomSearch, RDbSearch, DB,
  RDbEditor, ExtCtrls, RDbPanel, Grids, DBGrids, RDbColorGrid, ComCtrls,
  ToolWin, ADODB, StdCtrls, RDbText, Buttons;

type
  TFormSmsList = class(TSimpleDbTemplate)
    sm_messages: TADOQuery;
    sm_messagesid: TIntegerField;
    sm_messagesid_users: TIntegerField;
    sm_messagesname: TStringField;
    sm_messagesfullname: TStringField;
    sm_messagessended: TDateTimeField;
    sm_messagestitle: TStringField;
    sm_messagesaddress: TStringField;
    RDbFilter_NAME: TRDFStringItem;
    RDbFilter_SENDED: TRDFDateItem;
    RDbFilter_FULLNAME: TRDFStringItem;
    RDbFilter_TITLE: TRDFStringItem;
    RDbFilter_ADDRESS: TRDFStringItem;
    AddrLabel: TLabel;
    ADDRESSRDbText: TRDbText;
    TitleLabel: TLabel;
    TITLERDbText: TRDbText;
    CreateLabel: TLabel;
    SENDEDRDbText: TRDbText;
    NAMERDbText: TRDbText;
    Properties: TAction;
    itemProperties: TMenuItem;
    divD3: TMenuItem;
    itemPropertiesP: TMenuItem;
    divP5: TMenuItem;
    procedure PropertiesUpdate(Sender: TObject);
    procedure PropertiesExecute(Sender: TObject);
    procedure DbGridDblClick(Sender: TObject);
  protected
    function LoadDataForm: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  RDbOpenDS, RSmsVars, RVclUtils, RDialogs, ReadForm;

{ TFormSmsList }

function TFormSmsList.LoadDataForm: Boolean;
begin
  Result := OpenDS_VariableQuery(Database, sm_messages, RDbFilter, RDbOrder,
    sqlReadSmsListS, Format(sqlReadSmsListW,
      [TSmsListParams(FormData)^.UserId, TSmsListParams(FormData)^.ArmId]),
    EmptyStr, EmptyStr, True);
end;

procedure TFormSmsList.PropertiesUpdate(Sender: TObject);
begin
  Properties.Enabled := IsNotWait and sm_messages.Active and not sm_messages.IsEmpty;
end;

procedure TFormSmsList.PropertiesExecute(Sender: TObject);
begin
  ReadSms(sm_messages.Connection, sm_messagesid.AsInteger, TSmsListParams(FormData)^.UserId);
end;

procedure TFormSmsList.DbGridDblClick(Sender: TObject);
begin
  PropertiesUpdate(Sender);
  if Properties.Enabled then PropertiesExecute(Sender);
end;


end.
