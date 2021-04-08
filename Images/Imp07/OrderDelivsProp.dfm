inherited FormOrderDelivsProp: TFormOrderDelivsProp
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    Images = BaseData.ImageList
    inherited tsProperty: TTabSheet
      ImageIndex = 9
    end
    inherited tsAudit: TTabSheet
      ImageIndex = 39
    end
  end
  inherited DataSource: TDataSource
    DataSet = FormOrderDelivs.ORDER_DETAILS
    Left = 360
    Top = 6
  end
  object SuppDataSource: TDataSource
    AutoEdit = False
    DataSet = FormOrderDelivs.SUPP_NAMES
    Left = 390
    Top = 6
  end
end
