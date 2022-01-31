inherited FormImageViewer: TFormImageViewer
  Left = 560
  Top = 270
  Caption = #1055#1088#1086#1089#1084#1086#1090#1088' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000206F45CE207045FF207045FF207045FF207045FF207045FF207045FF2070
    45FF207045FF207045FF207146FF207246FF217347FF217446CE000000000000
    0000207045FF23AE62FF23AF63FF23AF63FF23AF63FF23AF63FF23AF63FF23AF
    63FF23AF63FF23AF63FF23AF63FF23AF63FF23AE62FF22784AFF000000000000
    0000A57C4BFF80BF7FE824AF5FFD23AF63FF23AF63FF23AF63FF23AF63FF23AF
    63FF23AF63FF23AF63FF23AF63FF23AF63FF23AF63FF237D4DFF000000000000
    0000A88250FFF0E3ACFFB7D79DEE2DB46AF924B065FF24B065FF24B065FF24B0
    64FF24B064FF24B064FF24B064FF24B064FF23AF64FF24824FFF000000000000
    0000AB8854FFF2E6ACFFF5E9B7FFD6E0B6F541B877F627B368FF27B368FF26B2
    68FF26B268FF84CF95F7B0DAA8FA2AB269FC26B267FF258652FF000000000000
    0000AF8E59FFF3E5A3FFF5E8AAFFF6EBB2FFE6E7B2FA58BF7FF229B56BFF29B5
    6BFF7FCD93F2F9F1BEFFF8EEB7FFDFE0A5FE3FB26AF6268B55FF000000000000
    0000B2945EFFF3E196FFF4E49BFFF5E69FFFF6E9A3FFEFE5A5FD72C687E67ECE
    89E6F7EBA8FFF7EAA6FFF6E8A3FFF5E69FFFF3E39BFFAE8D58FF000000000000
    0000B59A62FFF2DE8DFFECD78BFFC7AD6FFFBA9E65FFC7AE70FFEEDC94FFF5E5
    9BFFF5E59BFFF4E49AFFF4E398FFF3E195FFF2DF91FFB1935DFF000000000000
    0000B9A067FFF2D882FFCCB26FFF76C2BAFF3ED9F8FF76C2BAFFCDB372FFF4DE
    8EFFF4DE8EFFF3DD8DFFF2DD8BFFF2DB89FFF1DA86FFB59961FF000000000000
    0000BCA66BFFF4D981FFC9B06FFF3FDAF8FF38DCFFFF3FDAF8FFC9B06FFFF3D9
    81FFF3D981FFF3D981FFF3D981FFF3D881FFF2D881FFB89F66FF000000000000
    0000BFAC70FFF7DB82FFD9C178FF7DCBC0FF3FDAF9FF7DCBC0FFD9C178FFF6DA
    81FFF5DA81FFF5DA81FFF5DA81FFF5DA81FFF5DA81FFBBA56BFF000000000000
    0000C3B175FFF9DC82FFF5D981FFE0C87DFFD9C27BFFE0C87DFFF4D981FFF8DC
    82FFF8DB82FFF8DB82FFF8DB82FFF7DB82FFF7DB82FFBFAA6FFF000000000000
    0000C3B275FFFDEEC2FFFDF0C7FFFDF0C7FFFDF0C7FFFDF0C7FFFDF0C7FFFDF0
    C7FFFDF0C7FFFDF0C7FFFDF0C7FFFDF0C7FFFCEDC2FFC2B074FF000000000000
    0000C3B175CCC3B275FFC3B275FFC3B275FFC3B275FFC3B275FFC3B275FFC3B2
    75FFC3B275FFC3B275FFC3B275FFC3B275FFC3B275FFC3B175CC000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000800100008001000080010000800100008001000080010000800100008001
    0000800100008001000080010000800100008001000080010000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage [0]
    Left = 0
    Top = 40
    Width = 831
    Height = 371
    Align = alClient
    AutoSize = True
    Center = True
    PopupMenu = PopupMenu
    Proportional = True
  end
  inherited CoolBar: TCoolBar
    Height = 40
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 831
      end>
    inherited ToolBar: TToolBar
      Height = 36
      ButtonHeight = 36
      ButtonWidth = 67
      object btnOpenExtViewer: TToolButton
        Left = 0
        Top = 0
        Action = OpenExtViewer
      end
      object btnCopyToBuffer: TToolButton
        Left = 67
        Top = 0
        Action = CopyToBuffer
      end
      object ToolButton4: TToolButton
        Left = 134
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object btnSaveToFile: TToolButton
        Left = 142
        Top = 0
        Action = SaveToFile
      end
      object ToolButton5: TToolButton
        Left = 209
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object btnClose: TToolButton
        Left = 217
        Top = 0
        Action = CloseCancel
      end
    end
  end
  inherited ActionList: TActionList
    object SaveToFile: TAction [4]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Enabled = False
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077' '#1074' '#1092#1072#1081#1083#1077
      ImageIndex = 30
      ShortCut = 16467
      OnExecute = SaveToFileExecute
      OnUpdate = SaveToFileUpdate
    end
    object CopyToBuffer: TAction [5]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Enabled = False
      Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077' '#1074' '#1073#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
      ImageIndex = 42
      ShortCut = 16451
      OnExecute = CopyToBufferExecute
      OnUpdate = CopyToBufferUpdate
    end
    object OpenExtViewer: TAction [6]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088
      Enabled = False
      Hint = 
        #1055#1088#1086#1089#1084#1086#1090#1088' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1089' '#1087#1086#1084#1086#1097#1100#1102' '#1074#1085#1077#1096#1085#1077#1081' '#1087#1088#1086#1075#1088#1072#1084#1084#1099' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1080#1079#1086#1073#1088 +
        #1072#1078#1077#1085#1080#1081
      ImageIndex = 37
      ShortCut = 113
      OnExecute = OpenExtViewerExecute
      OnUpdate = OpenExtViewerUpdate
    end
  end
  inherited PopupMenu: TPopupMenu
    object itemOpenExtViewerP: TMenuItem [0]
      Action = OpenExtViewer
    end
    inherited itemFindP: TMenuItem
      Visible = False
    end
    object itemCopyToBufferP: TMenuItem [3]
      Action = CopyToBuffer
    end
    object itemSaveToFileP: TMenuItem [4]
      Action = SaveToFile
    end
    inherited menuOperationsP: TMenuItem
      Enabled = False
      OnClick = SaveToFileExecute
    end
    inherited divPopupRefresh: TMenuItem
      Visible = False
    end
  end
  inherited MainMenu: TMainMenu
    inherited menuEdit: TMenuItem
      object itemOpenExtViewer: TMenuItem [0]
        Action = OpenExtViewer
      end
      object N1: TMenuItem [1]
        Caption = '-'
      end
      object itemCopyToBuffer: TMenuItem [2]
        Action = CopyToBuffer
      end
      object itemSaveToFile: TMenuItem [3]
        Action = SaveToFile
      end
      object N2: TMenuItem [4]
        Caption = '-'
      end
    end
    inherited menuData: TMenuItem
      Visible = False
    end
    inherited menuReports: TMenuItem
      Visible = False
    end
  end
end
