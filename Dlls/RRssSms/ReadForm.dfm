inherited FormRead: TFormRead
  ActiveControl = OkBtn
  Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077
  ClientHeight = 401
  ClientWidth = 509
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 350
    Width = 509
  end
  object lblName: TLabel [1]
    Left = 12
    Top = 12
    Width = 69
    Height = 13
    Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1077#1083#1100':'
    FocusControl = deName
  end
  object lblSended: TLabel [2]
    Left = 344
    Top = 12
    Width = 87
    Height = 13
    Caption = #1042#1088#1077#1084#1103' '#1089#1086#1079#1076#1072#1085#1080#1103':'
    FocusControl = deSended
  end
  object lblTitle: TLabel [3]
    Left = 12
    Top = 60
    Width = 30
    Height = 13
    Caption = #1058#1077#1084#1072':'
    FocusControl = deTitle
  end
  object lblAddress: TLabel [4]
    Left = 12
    Top = 108
    Width = 62
    Height = 13
    Caption = #1055#1086#1083#1091#1095#1072#1090#1077#1083#1080':'
    FocusControl = deAddress
  end
  object lblMessage: TLabel [5]
    Left = 12
    Top = 156
    Width = 93
    Height = 13
    Caption = #1058#1077#1082#1089#1090' '#1089#1086#1086#1073#1097#1077#1085#1080#1103':'
    FocusControl = deMessage
  end
  inherited ButtonsPanel: TPanel
    Top = 352
    Width = 509
    TabOrder = 6
    inherited ButtonsMovedPanel: TPanel
      Left = 264
      Width = 245
      inherited OkBtn: TBitBtn
        Width = 109
        Hint = #1054#1090#1084#1077#1090#1080#1090#1100' '#1089#1086#1086#1073#1097#1077#1085#1080#1077' '#1082#1072#1082' '#1087#1088#1086#1095#1080#1090#1072#1085#1085#1086#1077' '#1080' '#1079#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
        Caption = #1055#1088#1086#1095#1080#1090#1072#1085#1086
      end
      inherited CancelBtn: TBitBtn
        Left = 124
        Width = 109
        Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086' '#1073#1077#1079' '#1080#1079#1084#1077#1085#1077#1085#1080#1103' '#1089#1090#1072#1090#1091#1089#1072' '#1089#1086#1086#1073#1097#1077#1085#1080#1103
      end
    end
  end
  object deName: TRDbText [7]
    Left = 12
    Top = 28
    Width = 101
    Height = 21
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkTile
    BevelOuter = bvSpace
    BorderStyle = sbsSunken
    DataField = 'name'
    DataSource = DataSource
    ShowAccelChar = False
    TabOrder = 0
  end
  object deFullname: TRDbText [8]
    Left = 120
    Top = 28
    Width = 213
    Height = 21
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkTile
    BevelOuter = bvSpace
    BorderStyle = sbsSunken
    DataField = 'fullname'
    DataSource = DataSource
    ShowAccelChar = False
    TabOrder = 1
  end
  object deSended: TRDbText [9]
    Left = 344
    Top = 28
    Width = 153
    Height = 21
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkTile
    BevelOuter = bvSpace
    BorderStyle = sbsSunken
    DataField = 'sended'
    DataSource = DataSource
    ShowAccelChar = False
    TabOrder = 2
  end
  object deTitle: TRDbText [10]
    Left = 12
    Top = 76
    Width = 485
    Height = 21
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkTile
    BevelOuter = bvSpace
    BorderStyle = sbsSunken
    DataField = 'title'
    DataSource = DataSource
    ShowAccelChar = False
    TabOrder = 3
  end
  object deAddress: TRDbText [11]
    Left = 12
    Top = 124
    Width = 485
    Height = 21
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkTile
    BevelOuter = bvSpace
    BorderStyle = sbsSunken
    DataField = 'address'
    DataSource = DataSource
    ShowAccelChar = False
    TabOrder = 4
  end
  object deMessage: TDBMemo [12]
    Tag = 255
    Left = 12
    Top = 172
    Width = 485
    Height = 157
    BevelInner = bvLowered
    BevelOuter = bvSpace
    DataField = 'message'
    DataSource = DataSource
    ScrollBars = ssVertical
    TabOrder = 5
  end
  inherited DataSource: TDataSource
    DataSet = sm_messages
    Left = 50
    Top = 288
  end
  object sm_messages: TADOQuery
    AutoCalcFields = False
    LockType = ltReadOnly
    ParamCheck = False
    Parameters = <>
    Prepared = True
    SQL.Strings = (
      
        'SELECT id_users, name, fullname, sended, title, address, message' +
        ' '
      
        'FROM sm_messages LEFT JOIN su_users ON sm_messages.id_users=su_u' +
        'sers.id')
    Left = 22
    Top = 288
    object sm_messagesid_users: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103'-'#1086#1090#1087#1088#1072#1074#1080#1090#1077#1083#1103
      FieldName = 'id_users'
    end
    object sm_messagesname: TStringField
      DisplayLabel = #1051#1086#1075#1080#1085' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103'-'#1086#1090#1087#1088#1072#1074#1080#1090#1077#1083#1103
      FieldName = 'name'
      Size = 16
    end
    object sm_messagesfullname: TStringField
      DisplayLabel = #1054#1090#1087#1088#1072#1074#1080#1090#1077#1083#1100' '#1089#1086#1086#1073#1097#1077#1085#1080#1103
      FieldName = 'fullname'
      Size = 64
    end
    object sm_messagessended: TDateTimeField
      Alignment = taCenter
      DisplayLabel = #1042#1088#1077#1084#1103' '#1089#1086#1079#1076#1072#1085#1080#1103' '#1089#1086#1086#1073#1097#1077#1085#1080#1103
      FieldName = 'sended'
    end
    object sm_messagestitle: TStringField
      DisplayLabel = #1058#1077#1084#1072
      FieldName = 'title'
      Size = 255
    end
    object sm_messagesaddress: TStringField
      DisplayLabel = #1057#1087#1080#1089#1086#1082' '#1087#1086#1083#1091#1095#1072#1090#1077#1083#1077#1081
      FieldName = 'address'
      Size = 255
    end
    object sm_messagesmessage: TMemoField
      DisplayLabel = #1058#1077#1082#1089#1090' '#1089#1086#1086#1073#1097#1077#1085#1080#1103
      FieldName = 'message'
      BlobType = ftMemo
    end
  end
  object sm_history: TADOQuery
    AutoCalcFields = False
    LockType = ltPessimistic
    OnPostError = sm_historyPostError
    ParamCheck = False
    Parameters = <>
    Prepared = True
    SQL.Strings = (
      'SELECT * FROM sm_history')
    Left = 78
    Top = 288
    object sm_historyid_messages: TIntegerField
      FieldName = 'id_messages'
    end
    object sm_historyid_users: TIntegerField
      FieldName = 'id_users'
    end
    object sm_historyopened: TDateTimeField
      FieldName = 'opened'
    end
    object sm_historyclosed: TDateTimeField
      FieldName = 'closed'
    end
  end
end
