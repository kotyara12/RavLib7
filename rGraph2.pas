unit RGraph2;

interface

uses
  Windows, Graphics, ExtCtrls, Jpeg, Db;

type
  TImageResizeKind = (rkOriginal, rkTarget, rk100, rk160, rk320, rk640, rk800, rk1024, rk2048, rk4096);
  TImageResizeMode = (rmAuto, rmReduction, rmIncrease);

const
  SImageResizeKind: array [TImageResizeKind] of string = (
    'Оригинальный размер (размер изображения не изменяется)',
    'Размер изображения в редакторе записи',
    'Максимум 100 px по большей стороне',
    'Максимум 160 px по большей стороне',
    'Максимум 320 px по большей стороне',
    'Максимум 640 px по большей стороне',
    'Максимум 800 px по большей стороне',
    'Максимум 1024 px по большей стороне',
    'Максимум 2048 px по большей стороне',
    'Максимум 4096 px по большей стороне'
    );
  SImageResizeMode: array [TImageResizeMode] of string = (
    'Подогнать под заданный размер',
    'Уменьшить до заданного размера',
    'Увеличить до заданного размера');

resourcestring
  sMsgImageConvert = 'Загрузка и конвертация изображения...';
  sMsgImageDbSave  = 'Сохранение изображения в базе данных...';
  sErrImageConvert = 'Ошибка конвертации изображения!';
  sErrImageDbRead  = 'Ошибка загрузки изображения из базы данных!';
  sErrImageDbSave  = 'Ошибка сохранения изображения в базе данных!';

procedure rG2_SmoothResize(Source, Target: TBitmap);

procedure rG2_ImageResize(Source, Target: TPicture; const MaxWidth, MaxHeight: Integer;
  const Kind: TImageResizeKind; const Mode: TImageResizeMode);

procedure rG2_JpegImage_Encode(Source, Target: TPicture; const Quality: Byte);
procedure rG2_JpegImage_Decode(Source, Target: TPicture);

procedure rG2_JpegImage_Load(Image: TImage; const FileName: string; const Kind: TImageResizeKind; const Mode: TImageResizeMode);
procedure rG2_JpegImage_Open(Image: TImage; const Kind: TImageResizeKind; const Mode: TImageResizeMode; const BaseDir: string);
procedure rG2_JpegImage_Paste(Image: TImage; const Kind: TImageResizeKind; const Mode: TImageResizeMode);

function  rG2_JpegImage_DbRead(Image: TImage; Field: TField): Boolean;
procedure rG2_JpegImage_DbSave(Image: TImage; Field: TField);

implementation

uses
  RVclUtils, RDialogs, RExHandlers,
  ActiveX, SysUtils, ExtDlgs, Dialogs, Classes, Clipbrd;

procedure rG2_SmoothResize(Source, Target: TBitmap);
type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;
var
  x, y: Integer;
  xP, yP: Integer;
  xP2, yP2: Integer;
  SrcLine1, SrcLine2: pRGBArray;
  t3: Integer;
  z, z2, iz2: Integer;
  DstLine: pRGBArray;
  DstGap: Integer;
  w1, w2, w3, w4: Integer;
begin
  Source.PixelFormat := pf24Bit;
  Target.PixelFormat := pf24Bit;

  if (Source.Width = Target.Width) and (Source.Height = Target.Height) then
    Target.Assign(Source)
  else begin
    DstLine := Target.ScanLine[0];
    DstGap  := Integer(Target.ScanLine[1]) - Integer(DstLine);

    xP2 := MulDiv(pred(Source.Width), $10000, Target.Width);
    yP2 := MulDiv(pred(Source.Height), $10000, Target.Height);
    yP  := 0;

    for y := 0 to pred(Target.Height) do
    begin
      xP := 0;

      SrcLine1 := Source.ScanLine[yP shr 16];

      if (yP shr 16 < pred(Source.Height))
      then SrcLine2 := Source.ScanLine[succ(yP shr 16)]
      else SrcLine2 := Source.ScanLine[yP shr 16];

      z2  := succ(yP and $FFFF);
      iz2 := succ((not yp) and $FFFF);
      for x := 0 to pred(Target.Width) do
      begin
        t3 := xP shr 16;
        z  := xP and $FFFF;
        w2 := MulDiv(z, iz2, $10000);
        w1 := iz2 - w2;
        w4 := MulDiv(z, z2, $10000);
        w3 := z2 - w4;
        DstLine[x].rgbtRed := (SrcLine1[t3].rgbtRed * w1 +
          SrcLine1[t3 + 1].rgbtRed * w2 +
          SrcLine2[t3].rgbtRed * w3 + SrcLine2[t3 + 1].rgbtRed * w4) shr 16;
        DstLine[x].rgbtGreen :=
          (SrcLine1[t3].rgbtGreen * w1 + SrcLine1[t3 + 1].rgbtGreen * w2 +
          SrcLine2[t3].rgbtGreen * w3 + SrcLine2[t3 + 1].rgbtGreen * w4) shr 16;
        DstLine[x].rgbtBlue := (SrcLine1[t3].rgbtBlue * w1 +
          SrcLine1[t3 + 1].rgbtBlue * w2 +
          SrcLine2[t3].rgbtBlue * w3 +
          SrcLine2[t3 + 1].rgbtBlue * w4) shr 16;
        Inc(xP, xP2);
      end;
      Inc(yP, yP2);
      DstLine := pRGBArray(Integer(DstLine) + DstGap);
    end;
  end;
end;

procedure rG2_ImageResize(Source, Target: TPicture; const MaxWidth, MaxHeight: Integer;
  const Kind: TImageResizeKind; const Mode: TImageResizeMode);
var
  tBmp1, tBmp2: TBitmap;
  bNeedResize: Boolean;
  iSrcWidth, iSrcHeight: Integer;
  iTrgWidth, iTrgHeight: Integer;
  iRszWidth, iRszHeight: Integer;
begin
  if Kind = rkOriginal then
    Target.Assign(Source)
  else begin
    // Задаем размеры исходной картинки
    iSrcWidth := Source.Width;
    iSrcHeight := Source.Height;

    // Задаем предельные размеры целевой картинки в БД
    case Kind of
      rkTarget:
      begin
        iTrgWidth := MaxWidth - 1;
        iTrgHeight := MaxHeight - 1;
      end;
      rk100:
      begin
        iTrgWidth := 100;
        iTrgHeight := 100;
      end;
      rk160:
      begin
        iTrgWidth := 160;
        iTrgHeight := 160;
      end;
      rk320:
      begin
        iTrgWidth := 320;
        iTrgHeight := 320;
      end;
      rk640:
      begin
        iTrgWidth := 640;
        iTrgHeight := 640;
      end;
      rk800:
      begin
        iTrgWidth := 800;
        iTrgHeight := 800;
      end;
      rk1024:
      begin
        iTrgWidth := 1024;
        iTrgHeight := 1024;
      end;
      rk2048:
      begin
        iTrgWidth := 2048;
        iTrgHeight := 2048;
      end;
      rk4096:
      begin
        iTrgWidth := 4096;
        iTrgHeight := 4096;
      end;
      else begin
        iTrgWidth := iSrcWidth;
        iTrgHeight := iSrcHeight;
      end;
    end;

    // Вычисляем целевой размер картинки
    iRszWidth := iTrgWidth;
    iRszHeight := Trunc(iSrcHeight / (iSrcWidth / iTrgWidth));
    if iRszHeight > iTrgHeight then
    begin
      iRszHeight := iTrgHeight;
      iRszWidth := Trunc(iSrcWidth / (iSrcHeight / iTrgHeight));
    end;

    // Опеределяем необходимость изменения размеров
    bNeedResize := False;
    case Mode of
      rmAuto:
        bNeedResize := (iRszWidth <> iSrcWidth) or (iRszHeight <> iSrcHeight);
      rmReduction:
        bNeedResize := (iRszWidth < iSrcWidth) or (iRszHeight < iSrcHeight);
      rmIncrease:
        bNeedResize := (iRszWidth > iSrcWidth) or (iRszHeight > iSrcHeight);
    end;

    // Изменение размеров
    if bNeedResize then
    begin
      tBmp1 := TBitmap.Create;
      tBmp2 := TBitmap.Create;
      try
        // Загружаем картинку в Bmp1
        tBmp1.Assign(Source.Graphic);

        // Задаем новые целевые размеры
        tBmp2.Height := iRszHeight;
        tBmp2.Width := iRszWidth;

        // Изменяем размеры
        rG2_SmoothResize(tBmp1, tBmp2);

        // Выгружаем картинку из Bmp2
        Target.Assign(tBmp2);
      finally
        tBmp1.Free;
        tBmp2.Free;
      end;
    end
    else
      Target.Assign(Source);
  end;
end;

{ == JPEG ====================================================================== }

procedure rG2_JpegImage_Encode(Source, Target: TPicture; const Quality: Byte);
var
  JpgImg: TJpegImage;
begin
  JpgImg := TJpegImage.Create;
  try
    JpgImg.Assign(Source.Graphic);
    JpgImg.Performance := jpBestQuality;
    if Quality > 0 then
      JpgImg.CompressionQuality := Quality;
    JpgImg.JPEGNeeded;
    Target.Assign(JpgImg);
  finally
    JpgImg.Free;
  end;
end;

procedure rG2_JpegImage_Decode(Source, Target: TPicture);
var
  JpgImg: TJpegImage;
begin
  JpgImg := TJpegImage.Create;
  try
    JpgImg.Assign(Source.Graphic);
    JpgImg.DIBNeeded;
    Target.Bitmap.Assign(JpgImg);
  finally
    JpgImg.Free;
  end;
end;

procedure rG2_JpegImage_Load(Image: TImage; const FileName: string; const Kind: TImageResizeKind; const Mode: TImageResizeMode);
var
  SrcImage: TPicture;
begin
  try
    SrcImage := TPicture.Create;
    try
      SrcImage.LoadFromFile(FileName);

      rG2_ImageResize(SrcImage, SrcImage, Image.Width, Image.Height, Kind, Mode);
      rG2_JpegImage_Encode(SrcImage, Image.Picture, 0);
    finally
      SrcImage.Free;
    end;
  except
    on E: Exception do
    begin
      HandleExcept(E, Image, sErrImageConvert);
      Image.Picture.Graphic := nil;
    end;
  end;
end;

procedure rG2_JpegImage_Open(Image: TImage; const Kind: TImageResizeKind; const Mode: TImageResizeMode; const BaseDir: string);
var
  OpenDialog: TOpenPictureDialog;
begin
  OpenDialog := TOpenPictureDialog.Create(nil);
  try
    OpenDialog.InitialDir := BaseDir;
    OpenDialog.Options := [ofPathMustExist, ofFileMustExist, ofHideReadOnly, ofEnableSizing];
    if OpenDialog.Execute then
    begin
      StartWait;
      ShowInStatusBar(sMsgImageConvert);
      try
        rG2_JpegImage_Load(Image, OpenDialog.FileName, Kind, Mode);
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure rG2_JpegImage_Paste(Image: TImage; const Kind: TImageResizeKind; const Mode: TImageResizeMode);
var
  SrcImage: TPicture;
begin
  StartWait;
  ShowInStatusBar(sMsgImageConvert);
  try
    try
      SrcImage := TPicture.Create;
      try
        SrcImage.Assign(Clipboard);

        rG2_ImageResize(SrcImage, SrcImage, Image.Width, Image.Height, Kind, Mode);
        rG2_JpegImage_Encode(SrcImage, Image.Picture, 0);
      finally
        SrcImage.Free;
      end;
    except
      on E: Exception do
      begin
        HandleExcept(E, Image, sErrImageConvert);
        Image.Picture.Graphic := nil;
      end;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function rG2_JpegImage_DbRead(Image: TImage; Field: TField): Boolean;
var
  JpgImage: TJpegImage;
begin
  Result := False;

  StartWait;
  try
    try
      if Field.IsNull then
        Image.Picture.Graphic := nil
      else begin
        JpgImage := TJpegImage.Create;
        try
          JpgImage.Assign(Field);
          Image.Picture.Assign(JpgImage);
          Result := True;
        finally
          JpgImage.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        HandleExcept(E, Field, sErrImageDbRead);
        Image.Picture.Graphic := nil;
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure rG2_JpegImage_DbSave(Image: TImage; Field: TField);
var
  JpgImage: TJpegImage;
begin
  StartWait;
  ShowInStatusBar(sMsgImageDbSave);
  try
    try
      if Assigned(Image) and Assigned(Image.Picture) and Assigned(Image.Picture.Graphic) then
      begin
        if Image.Picture.Graphic.Empty
        then Field.Clear
        else begin
          JpgImage := TJpegImage.Create;
          try
            JpgImage.Assign(Image.Picture.Graphic);
            Field.Assign(JpgImage);
          finally
            JpgImage.Free;
          end;
        end;
      end
      else Field.Clear;
    except
      on E: Exception do
      begin
        HandleExcept(E, Field, sErrImageDbSave);
        Image.Picture.Graphic := nil;
      end;
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

end.
