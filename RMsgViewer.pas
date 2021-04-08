unit RMsgViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, ComCtrls, RavListView, Menus, StdCtrls,
  StdActns;

type
  TFormMsgViewer = class(TForm)
    ActionList: TActionList;
    MainMenu: TMainMenu;
    StatusBar: TStatusBar;
    CloseMsgs: TAction;
    itemCloseMsgs: TMenuItem;
    MsgList: TRichEdit;
    PopupMenu: TPopupMenu;
    EditCopy: TEditCopy;
    EditSelectAll: TEditSelectAll;
    itemEditCopyP: TMenuItem;
    itemEditSelectAllP: TMenuItem;
    itemEditCopy: TMenuItem;
    itemEditSelectAll: TMenuItem;
    procedure CloseMsgsUpdate(Sender: TObject);
    procedure CloseMsgsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fMsgCount: Integer;
  public
    procedure AddMessage(const MsgText: string;
      const FontColor: TColor; const FontStyle: TFontStyles);
  end;

procedure ShowMsgViewer(const Title, MsgText: string;
  const FontColor: TColor = clWindowText;
  const FontStyle: TFontStyles = []);
procedure ActivateMsgViewer;
function IsShowMsgViewer: Boolean;

implementation

{$R *.dfm}

uses
  RVclUtils;

resourcestring
  SFmtInfoCount   = 'Всего сообщений: %d';

var
  MsgViewer: TFormMsgViewer;

procedure ShowMsgViewer(const Title, MsgText: string;
  const FontColor: TColor = clWindowText;
  const FontStyle: TFontStyles = []);
begin
  if Assigned(MsgViewer) then
  begin
    with MsgViewer do
    begin
      if Title <> EmptyStr then
        Caption := Title;
      AddMessage(MsgText, FontColor, FontStyle);
      Show;
      BringToFront;
    end;
  end
  else begin
    with TFormMsgViewer.Create(Application) do
    begin
      if Title <> EmptyStr then
        Caption := Title;
      AddMessage(MsgText, FontColor, FontStyle);
      Show;
      BringToFront;
    end;
  end;
end;

procedure ActivateMsgViewer;
begin
  if Assigned(MsgViewer) then
    MsgViewer.BringToFront;
end;

function IsShowMsgViewer: Boolean;
begin
  Result := Assigned(MsgViewer);
end;

procedure TFormMsgViewer.FormCreate(Sender: TObject);
begin
  MsgViewer := Self;
  Font := Screen.MenuFont;
  fMsgCount := 0;
end;

procedure TFormMsgViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormMsgViewer.FormDestroy(Sender: TObject);
begin
  MsgViewer := nil;
end;

procedure TFormMsgViewer.AddMessage(const MsgText: string;
  const FontColor: TColor; const FontStyle: TFontStyles);
begin
  if Trim(MsgText) <> EmptyStr then
  begin
    if MsgList.Lines.Count > 0 then
      MsgList.Lines.Add(EmptyStr);
    MsgList.SelLength := 0;
    MsgList.SelStart := Length(MsgList.Text);
    MsgList.SelAttributes.Color := FontColor;
    MsgList.SelAttributes.Style := FontStyle;
    MsgList.Lines.Add(MsgText);
    Inc(fMsgCount);
  end;
  StatusBar.SimpleText := Format(SFmtInfoCount, [fMsgCount]);
end;

procedure TFormMsgViewer.CloseMsgsUpdate(Sender: TObject);
begin
  CloseMsgs.Enabled := IsNotWait;
end;

procedure TFormMsgViewer.CloseMsgsExecute(Sender: TObject);
begin
  Close;
end;

initialization
  MsgViewer := nil;

finalization
  if Assigned(MsgViewer) then
    FreeAndNil(MsgViewer);
end.
