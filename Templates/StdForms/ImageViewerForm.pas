unit ImageViewerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplData, Menus, ActnList, ComCtrls, ToolWin, ExtCtrls, Db;

type
  TFormImageViewer = class(TDataTemplate)
    btnClose: TToolButton;
    Image: TImage;
    SaveToFile: TAction;
    OpenExtViewer: TAction;
    CopyToBuffer: TAction;
    itemOpenExtViewer: TMenuItem;
    N1: TMenuItem;
    itemCopyToBuffer: TMenuItem;
    itemSaveToFile: TMenuItem;
    N2: TMenuItem;
    btnOpenExtViewer: TToolButton;
    btnSaveToFile: TToolButton;
    btnCopyToBuffer: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    itemCopyToBufferP: TMenuItem;
    itemSaveToFileP: TMenuItem;
    itemOpenExtViewerP: TMenuItem;
    procedure SaveToFileUpdate(Sender: TObject);
    procedure SaveToFileExecute(Sender: TObject);
    procedure CopyToBufferUpdate(Sender: TObject);
    procedure CopyToBufferExecute(Sender: TObject);
    procedure OpenExtViewerUpdate(Sender: TObject);
    procedure OpenExtViewerExecute(Sender: TObject);
  private
  protected
    function  LoadDataForm: Boolean; override;
  public
  end;

procedure ShowImageViewer(Field: TField; const Tag: Integer); overload;
procedure ShowImageViewer(Image: TImage; const Tag: Integer); overload;

implementation

{$R *.dfm}

uses
  Clipbrd, RVclUtils, RDialogs, RGraph2, RMdiIntf, RSysUtils;

resourcestring
  srStatusBarFmt = 'Размер изображения: %d x %d';

const
  sFileExt = '.jpg';
  sFilter  = 'JPEG images (*.jpg)|*.jpg';

procedure ShowImageViewer(Field: TField; const Tag: Integer);
begin
  if Assigned(Field) and not Field.IsNull then
    ShowModalForm(TFormImageViewer, Tag, Field);
end;

procedure ShowImageViewer(Image: TImage; const Tag: Integer);
begin
  if Assigned(Image) and Assigned(Image.Picture) then
    ShowModalForm(TFormImageViewer, Tag, Image.Picture);
end;

{ TFormImageViewer }

function TFormImageViewer.LoadDataForm: Boolean;
begin
  StartWait;
  try
    if Assigned(FormData) then
    begin
      if TObject(FormData) is TField
      then rG2_JpegImage_DbRead(Image, TField(FormData))
      else Image.Picture.Assign(TPicture(FormData));
    end;

    StatusBar.SimpleText := Format(srStatusBarFmt, [Image.Picture.Width, Image.Picture.Height]);

    Result := True;
  finally
    StopWait;
  end;
end;

procedure TFormImageViewer.SaveToFileUpdate(Sender: TObject);
begin
  SaveToFile.Enabled := IsNotWait and Assigned(Image.Picture.Graphic);
end;

procedure TFormImageViewer.SaveToFileExecute(Sender: TObject);
var
  sFileName: string;
begin
  if PromptForFileName(sFileName, sFilter, sFileExt, '', '', True) then
  begin
    StartWait;
    try
      Image.Picture.SaveToFile(sFileName);
      OpenFile32('open', sFileName, '', SW_SHOWNORMAL, False, nil, 0);
    finally
      StopWait;
    end;
  end;
end;

procedure TFormImageViewer.CopyToBufferUpdate(Sender: TObject);
begin
  CopyToBuffer.Enabled := IsNotWait and Assigned(Image.Picture.Graphic);
end;

procedure TFormImageViewer.CopyToBufferExecute(Sender: TObject);
begin
  StartWait;
  try
    Clipboard.Assign(Image.Picture);
  finally
    StopWait;
  end;
end;

procedure TFormImageViewer.OpenExtViewerUpdate(Sender: TObject);
begin
  OpenExtViewer.Enabled := IsNotWait and Assigned(Image.Picture.Graphic);
end;

procedure TFormImageViewer.OpenExtViewerExecute(Sender: TObject);
var
  sImgFile: string;
begin
  StartWait;
  try
    sImgFile := GetTempFilePath(sFileExt);
    Image.Picture.SaveToFile(sImgFile);
    OpenFile32('open', sImgFile, '', SW_SHOWNORMAL, False, nil, 0);
  finally
    StopWait;
  end;
end;

end.
