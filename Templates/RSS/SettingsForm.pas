unit SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbTreeList, Menus, ActnList, ComCtrls, RavListView, RVclUtils,
  Buttons, ExtCtrls, ToolWin, Db, ADODB, ImgList, RavTreeView_Old,
  RDbEditor, RDbTree, RavTreeView, StdCtrls;

type
  TFormSettings = class(TDbTreeListTemplate)
    ss_vargroups: TADOQuery;
    ss_vargroupsid: TIntegerField;
    ss_vargroupsowner_id: TIntegerField;
    ss_vargroupsname: TStringField;
    ss_vargroupsnotes: TStringField;
    ImageList: TImageList;
    procedure TreeOpenDataSets(Sender: TObject;
      var Completed: Boolean);
    procedure TreeGetEditRights(Sender: TObject;
      const NodeType: TNodeType; const Mode: TEditMode;
      var Enable: Boolean);
    (* procedure ListViewDeletion(Sender: TObject; Item: TListItem); *)
  protected
    // Загрузка данных
    function  LoadDataNode(const TreeActive: Boolean; Node: TTreeNode): Boolean; override;
    // Редактирование данных
    {$IFDEF RSS}
    function  GetDetailEditTag: Integer; override;
    {$ENDIF}
    function  GetDetailName: string; override;
    function  DetailOpenEnabled: Boolean; override;
    procedure DetailEdit; override;
  public
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF RSS}
  RDbLog, OprList, AdminUnit, AdminVars, RRssConst,
  {$ENDIF}
  RMsgRu, RDialogs, RAppStyles, RExHandlers, RDbState,
  RDbConst, RDbUtils, RDbSettings, RListView, RDbListView,
  BaseDbUnit, SettingsProp;

resourcestring
  rsSettingsCaption        = 'Параметр системы %s';
  rsErrLoadSysSettings     = 'Ошибка загрузки списка параметров системы!';

const
  sFloatFmt        = '%.4f';

{ == Загрузка данных для дерева ================================================ }
procedure TFormSettings.TreeOpenDataSets(Sender: TObject; var Completed: Boolean);
begin
  Completed := BaseData.OpenDataSet(SS_VARGROUPS, False, {$IFDEF RSS} tagEditSysSettings {$ELSE} 0 {$ENDIF});
end;

{ == Загрузка данных таблицы =================================================== }
function TFormSettings.LoadDataNode(const TreeActive: Boolean; Node: TTreeNode): Boolean;
const
  sqlFullLoad      = 'SELECT id, name, type, def_value, value_int, value_real, value_char FROM vss_settings ORDER BY id';
  sqlSelectionLoad = 'SELECT id, name, type, def_value, value_int, value_real, value_char FROM vss_settings WHERE id_groups in (%s) ORDER BY id';
var
  Qry: TAdoQuery;
  SqlText: string;
  Id: TId;
begin
  Result := True;
  Qry := nil;
  try
    try
      if TreeActive then begin
        if Assigned(Node)
        then SqlText := Format(sqlSelectionLoad, [TreeView.GetIdList(Node, [ntGroup])])
        else SqlText := Format(sqlSelectionLoad, [SNullListValue]);
      end
      else SqlText := sqlFullLoad;
      Qry := OpenDynamicQuery(BaseData.acDb, SqlText);
      if DataSetIsOpen(Qry) then
      begin
        ListView.Items.Clear;
        Qry.First;
        while not Qry.Eof do
        begin
          with ListView.Items.Add do
          begin
            New(Id);
            Id^ := Qry.FieldByName(fnID).AsInteger;
            Data := Id;
            Caption := Qry.FieldByName(fnID).AsString;
            Subitems.Add(Qry.FieldByName(fnNAME).AsString);
            {$IFDEF RSS}
            if (Id^ < sidSecurityMin) or orViewSecurity then
            begin
              if (Id^ < sidSecurityMin) or orEditSecurity
              then ImageIndex := Qry.FieldByName(fnTYPE).AsInteger
              else ImageIndex := tidReadOnly;
              Subitems.Add(Qry.FieldByName(fnDEF_VALUE).AsString);
              case ImageIndex of
                tidReadOnly, tidInteger:
                  Subitems.Add(Qry.FieldByName(fnVALUE_INT).AsString);
                tidString, tidFileName, tidFilePath:
                  Subitems.Add(Qry.FieldByName(fnVALUE_CHAR).AsString);
                tidReal:
                  Subitems.Add(Format(sFloatFmt, [Qry.FieldByName(fnVALUE_REAL).AsFloat]));
                tidDate:
                  Subitems.Add(DateToStr(Qry.FieldByName(fnVALUE_REAL).AsFloat));
                tidDateTime:
                  Subitems.Add(TimeToStr(Qry.FieldByName(fnVALUE_REAL).AsFloat));
                tidBoolean:
                  if Qry.FieldByName(fnVALUE_INT).AsInteger > 0
                  then Subitems.Add(SBooleanOn)
                  else Subitems.Add(SBooleanOff);
              end;
            end
            else begin
              ImageIndex := tidReadOnly;
              Subitems.Add(SViewNotEnabled);
              Subitems.Add(SViewNotEnabled);
            end;
            {$ELSE}
            ImageIndex := Qry.FieldByName(fnTYPE).AsInteger;
            Subitems.Add(Qry.FieldByName(fnDEF_VALUE).AsString);
            case ImageIndex of
              tidReadOnly, tidInteger:
                Subitems.Add(Qry.FieldByName(fnVALUE_INT).AsString);
              tidString, tidFileName, tidFilePath:
                Subitems.Add(Qry.FieldByName(fnVALUE_CHAR).AsString);
              tidReal:
                Subitems.Add(Format(sFloatFmt, [Qry.FieldByName(fnVALUE_REAL).AsFloat]));
              tidDate:
                Subitems.Add(DateToStr(Qry.FieldByName(fnVALUE_REAL).AsFloat));
              tidDateTime:
                Subitems.Add(TimeToStr(Qry.FieldByName(fnVALUE_REAL).AsFloat));
              tidBoolean:
                if Qry.FieldByName(fnVALUE_INT).AsInteger > 0
                then Subitems.Add(SBooleanOn)
                else Subitems.Add(SBooleanOff);
            end;
            {$ENDIF}
          end;
          Qry.Next;
        end;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleSqlExcept(E, Qry, SqlText, rsErrLoadSysSettings);
      end;
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

{ == Редактирование дерева ===================================================== }
procedure TFormSettings.TreeGetEditRights(Sender: TObject;
  const NodeType: TNodeType; const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := False;
end;

{ == Редактирование данных ===================================================== }
{$IFDEF RSS}
function TFormSettings.GetDetailEditTag: Integer;
begin
  if not Assigned(ListView.Selected)
    or (GetItemId(ListView.Selected) < sidSecurityMin)
  then Result := tagEditSysSettings
  else Result := tagEditSecurity;
end;
{$ENDIF}

function TFormSettings.GetDetailName: string;
begin
  Result := tnSysSettings;
end;

function TFormSettings.DetailOpenEnabled: Boolean;
begin
  Result := {$IFDEF RSS}orEditSysSettings and {$ENDIF}(ListView.Selected <> nil)
    and (ListView.Selected.ImageIndex <> tidReadOnly);
end;

procedure TFormSettings.DetailEdit;
begin
  if not Assigned(ListView.Selected) then
    raise Exception.Create(SErrNoSelectedItem);

  with TFormSettingsProp.Create(Self) do
  begin
    try
      StartWait;
      ShowInStatusBar(SMsgPrepareOperation);
      try
        Caption := GetEditorCaption(Format(rsSettingsCaption, [ListView.Selected.Caption]), dsEdit);
        // NameEditLabel.Caption := NameEditLabel.Caption + ListView.Selected.Caption;
        edName.Text := ListView.Selected.Subitems[0];
        edDefault.Text := ListView.Selected.Subitems[1];
        case ListView.Selected.ImageIndex of
          tidInteger:
          begin
            edInt.Visible := True;
            edInt.Value := ReadDbSysInteger(BaseData.acDb, GetItemId(ListView.Selected),
              StrToIntDef(ListView.Selected.Subitems[2], 0));
          end;
          tidReal:
          begin
            edReal.Visible := True;
            edReal.Value := ReadDbSysFloat(BaseData.acDb, GetItemId(ListView.Selected),
              StrToFloatDef(ListView.Selected.Subitems[2], 0));
          end;
          tidString:
          begin
            edChar.Visible := True;
            edChar.Width := 505;
            edChar.Text := ReadDbSysString(BaseData.acDb, GetItemId(ListView.Selected),
              ListView.Selected.Subitems[2]);
          end;
          tidDate:
          begin
            edDateTime.Visible := True;
            edDateTime.Kind := dtkDate;
            edDateTime.Date := ReadDbSysDate(BaseData.acDb, GetItemId(ListView.Selected),
              Date);
          end;
          tidDateTime:
          begin
            edDateTime.Visible := True;
            edDateTime.Kind := dtkTime;
            edDateTime.Time := ReadDbSysTime(BaseData.acDb, GetItemId(ListView.Selected),
              Time);
          end;
          tidBoolean:
          begin
            edBoolean.Visible := True;
            if ReadDbSysBoolean(BaseData.acDb, GetItemId(ListView.Selected), False)
            then edBoolean.ItemIndex := 0
            else edBoolean.ItemIndex := 1;
          end;
          tidFileName:
          begin
            edChar.Visible := True;
            btnOpen.Visible := True;
            btnOpen.Tag := 0;
            edChar.Text := ReadDbSysString(BaseData.acDb, GetItemId(ListView.Selected),
              ListView.Selected.Subitems[2]);
          end;
          tidFilePath:
          begin
            edChar.Visible := True;
            btnOpen.Visible := True;
            btnOpen.Tag := 1;
            edChar.Text := ReadDbSysString(BaseData.acDb, GetItemId(ListView.Selected),
              ListView.Selected.Subitems[2]);
          end;
        end;
      finally
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        ShowInStatusBar(SMsgSaveDataWait);
        try
          case ListView.Selected.ImageIndex of
            tidInteger:
            begin
              SaveDbSysInteger(BaseData.acDb, GetItemId(ListView.Selected), edInt.Value);
              ListView.Selected.Subitems[2] := IntToStr(edInt.Value);
            end;
            tidReal:
            begin
              SaveDbSysFloat(BaseData.acDb, GetItemId(ListView.Selected), edReal.Value);
              ListView.Selected.Subitems[2] := Format(sFloatFmt, [edReal.Value]);
            end;
            tidString, tidFileName, tidFilePath:
            begin
              SaveDbSysString(BaseData.acDb, GetItemId(ListView.Selected), edChar.Text);
              ListView.Selected.Subitems[2] := edChar.Text;
            end;
            tidDate:
            begin
              SaveDbSysDate(BaseData.acDb, GetItemId(ListView.Selected), edDateTime.Date);
              ListView.Selected.Subitems[2] := DateToStr(edDateTime.Date);
            end;
            tidDateTime:
            begin
              SaveDbSysTime(BaseData.acDb, GetItemId(ListView.Selected), edDateTime.Time);
              ListView.Selected.Subitems[2] := TimeToStr(edDateTime.Time);
            end;
            tidBoolean:
            begin
              SaveDbSysBoolean(BaseData.acDb, GetItemId(ListView.Selected), edBoolean.ItemIndex = 0);
              ListView.Selected.Subitems[2] := edBoolean.Text;
            end;
          end;
          {$IFDEF RSS}
          AddToDbLog(GetDetailEditTag, Format(SLogSetSystemParameter,
            [GetItemId(ListView.Selected), ListView.Selected.Subitems[2]]));
          {$ENDIF}
        finally
          ShowInStatusBar(EmptyStr);
          StopWait;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

end.
