unit RGraph;

interface

uses
  Windows, Graphics, ExtCtrls, Jpeg, Db;

type
  TImageResizeKind = (rkOriginal, rkTarget, rk640, rk800, rk1024, rk2048, rk4096);
  TImageResizeMode = (rmAuto, rmReduction, rmIncrease);

const
  SImageResizeKind: array [TImageResizeKind] of string = (
    'Оригинальный размер (размер изображения не изменяется)',
    'Размер изображения в редакторе записи',
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

procedure SmoothResize(Src, Dst: TBitmap);

procedure Image2Jpeg(Image: TImage);
procedure Image2JpegResize(Image: TImage; const Kind: TImageResizeKind; const Mode: TImageResizeMode);
procedure Image_DbJpegRead(Image: TImage; Field: TField);
procedure Image_DbJpegSave(Image: TImage; Field: TField);

implementation

uses
  RDialogs, SysUtils;

type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;

procedure SmoothResize(Src, Dst: TBitmap);

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
  Src.PixelFormat := pf24Bit;
  Dst.PixelFormat := pf24Bit;

  if (Src.Width = Dst.Width) and (Src.Height = Dst.Height) then
    Dst.Assign(Src)
  else begin
    DstLine := Dst.ScanLine[0];
    DstGap  := Integer(Dst.ScanLine[1]) - Integer(DstLine);

    xP2 := MulDiv(pred(Src.Width), $10000, Dst.Width);
    yP2 := MulDiv(pred(Src.Height), $10000, Dst.Height);
    yP  := 0;

    for y := 0 to pred(Dst.Height) do
    begin
      xP := 0;

      SrcLine1 := Src.ScanLine[yP shr 16];

      if (yP shr 16 < pred(Src.Height))
      then SrcLine2 := Src.ScanLine[succ(yP shr 16)]
      else SrcLine2 := Src.ScanLine[yP shr 16];

      z2  := succ(yP and $FFFF);
      iz2 := succ((not yp) and $FFFF);
      for x := 0 to pred(Dst.Width) do
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
      end; {for}
      Inc(yP, yP2);
      DstLine := pRGBArray(Integer(DstLine) + DstGap);
    end; {for}
  end; {if}
end; {SmoothResize}

procedure Image2Jpeg(Image: TImage);
var
  tJpg: TJpegImage;
  tBmp: TBitmap;
begin
  tBmp := TBitmap.Create;
  tJpg := TJpegImage.Create;
  try
    // Загружаем картинку в Bmp1
    tBmp.Assign(Image.Picture.Graphic);

    // Преобразовываем картинку в Jpeg
    tJpg.Performance := jpBestQuality;
    tJpg.Assign(tBmp);

    // Возвращаем сжатую картинку обратно
    Image.Picture.Assign(tJpg);
  finally
    tBmp.Free;
    tJpg.Free;
  end;
end;

procedure Image2JpegResize(Image: TImage; const Kind: TImageResizeKind; const Mode: TImageResizeMode);
var
  tJpg: TJpegImage;
  tBmp1, tBmp2: TBitmap;
  bNeedResize: Boolean;
  iSrcWidth, iSrcHeight: Integer;
  iTrgWidth, iTrgHeight: Integer;
  iRszWidth, iRszHeight: Integer;
begin
  tBmp1 := TBitmap.Create;
  tBmp2 := TBitmap.Create;
  tJpg := TJpegImage.Create;
  try
    if Kind = rkOriginal then
    begin
      // Исходный размер картинки
      tBmp2.Assign(Image.Picture.Graphic);
    end
    else begin
      // Загружаем картинку в Bmp1
      tBmp1.Assign(Image.Picture.Graphic);

      (*
      tBmp2.Width := Image.Width - 1;
      tBmp2.Height := Trunc(tBmp1.Height / (tBmp1.Width / Image.Width));
      if tBmp2.Height >= Image.Height then
      begin
        tBmp2.Height := Image.Height - 1;
        tBmp2.Width := Trunc(tBmp1.Width / (tBmp1.Height / Image.Height));
      end;
      *)

      // Задаем размеры исходной картинки
      iSrcWidth := tBmp1.Width;
      iSrcHeight := tBmp1.Height;

      // Задаем предельные размеры целевой картинки в БД
      case Kind of
        rkTarget:
        begin
          iTrgWidth := Image.Width - 1;
          iTrgHeight := Image.Height - 1;
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

      // Изменяем размеры картинки в Bmp2
      if bNeedResize then
      begin
         tBmp2.Height := iRszHeight;
         tBmp2.Width := iRszWidth;
         SmoothResize(tBmp1, tBmp2);
      end
      else tBmp2.Assign(tBmp1);
    end;

    // Преобразовываем картинку в Jpeg
    tJpg.Performance := jpBestQuality;
    tJpg.Assign(tBmp2);

    // Возвращаем сжатую картинку обратно
    Image.Picture.Assign(tJpg);
  finally
    if Assigned(tBmp1) then
      tBmp1.Free;
    tBmp2.Free;
    tJpg.Free;
  end;
end;

procedure Image_DbJpegRead(Image: TImage; Field: TField);
var
  tJpg: TJpegImage;
begin
  if Field.IsNull then
    Image.Picture.Graphic := nil
  else begin
    tJpg := TJpegImage.Create;
    try
      tJpg.Assign(Field);
      tJpg.DIBNeeded;
      Image.Picture.Assign(tJpg);
    finally
      tJpg.Free;
    end;
  end
end;

procedure Image_DbJpegSave(Image: TImage; Field: TField);
begin
  if Image.Picture.Graphic.Empty
  then Field.Clear
  else Field.Assign(Image.Picture.Graphic);
end;

end.
