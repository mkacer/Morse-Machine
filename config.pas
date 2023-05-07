
Unit config;

{$mode objfpc}{$H+}{$codepage utf8}

Interface

Uses 
Forms, Controls, Classes, SysUtils, XMLConf;

Procedure SetConfigFileName( Const filename: String );
Function GetValueFromConfigFile(Const SectionName, ValueName, Default: UnicodeString): UnicodeString;
Function GetValueFromConfigFile(Const SectionName, ValueName: UnicodeString; Default: Integer): Integer;
Function GetValueFromConfigFile(Const SectionName, ValueName: UnicodeString; Default: Boolean): Boolean;
Procedure SaveValueToConfigFile(Const SectionName, ValueName, Value: UnicodeString);
Procedure SaveValueToConfigFile(Const SectionName, ValueName: UnicodeString; Value: Integer);
Procedure SaveValueToConfigFile(Const SectionName, ValueName: UnicodeString; Value: Boolean);
Procedure GetFormPositionFromConfigFile(Const SectionName, ValueName: UnicodeString; AForm: TForm);
Procedure WriteFormPositionToConfigFile(Const SectionName, ValueName: UnicodeString; AForm: TForm);

Implementation

Var 
  XMLConfig: TXMLConfig;

Const 
  sValue = '/Value';

Procedure SetConfigFileName( Const filename: String );
Begin
  XMLConfig := TXMLConfig.Create(Nil);
  XMLConfig.Filename := filename;
End;

Function GetValueFromConfigFile(Const SectionName, ValueName, Default: UnicodeString): UnicodeString;
Begin
  Result := XMLConfig.GetValue(SectionName+'/'+ValueName+sValue, Default);
End;

Function GetValueFromConfigFile(Const SectionName, ValueName: UnicodeString; Default: Integer): Integer;
Begin
  Result := XMLConfig.GetValue(SectionName+'/'+ValueName+sValue, Default);
End;

Function GetValueFromConfigFile(Const SectionName, ValueName: UnicodeString; Default: Boolean): Boolean;
Begin
  Result := XMLConfig.GetValue(SectionName+'/'+ValueName+sValue, Default);
End;

Procedure SaveValueToConfigFile(Const SectionName, ValueName, Value: UnicodeString);
Begin
  XMLConfig.SetValue(SectionName+'/'+ValueName+sValue, Value);
End;

Procedure SaveValueToConfigFile(Const SectionName, ValueName: UnicodeString; Value: Integer);
Begin
  XMLConfig.SetValue(SectionName+'/'+ValueName+sValue, Value);
End;

Procedure SaveValueToConfigFile(Const SectionName, ValueName: UnicodeString; Value: Boolean);
Begin
  XMLConfig.SetValue(SectionName+'/'+ValueName+sValue, Value);
End;

Procedure GetFormPositionFromConfigFile(Const SectionName, ValueName: UnicodeString; AForm: TForm);

Var 
  ws: TWindowState;
  t, l, w: Integer;
Begin
  ws := TWindowState(XMLConfig.GetValue(SectionName+'/'+ValueName+'/WindowState',Ord(wsNormal)));
  t := XMLConfig.GetValue(SectionName+'/'+ValueName+'/Top',AForm.Top);
  l := XMLConfig.GetValue(SectionName+'/'+ValueName+'/Left',AForm.Left);
  If t < Screen.Height Then AForm.Top := t;
  If l < Screen.Width Then AForm.Left := l;
  If AForm.BorderStyle = bsSizeable Then
    Begin
      w := XMLConfig.GetValue(SectionName+'/'+ValueName+'/Width',AForm.Width);
      If w > 40 Then AForm.Width := w;
      AForm.Height := XMLConfig.GetValue(SectionName+'/'+ValueName+'/Height',AForm.Height);;
    End;
  Application.ProcessMessages;
  If ws = wsMaximized Then AForm.WindowState := ws;
End;

Procedure WriteFormPositionToConfigFile(Const SectionName, ValueName: UnicodeString; AForm: TForm);

Var 
  ws: TWindowState;
Begin
  ws := AForm.WindowState;
  If ws = wsNormal Then
    Begin
      XMLConfig.SetValue(SectionName+'/'+ValueName+'/Top',AForm.Top);
      XMLConfig.SetValue(SectionName+'/'+ValueName+'/Left',AForm.Left);
      If AForm.BorderStyle = bsSizeable Then
        Begin
          XMLConfig.SetValue(SectionName+'/'+ValueName+'/Width',AForm.Width);
          XMLConfig.SetValue(SectionName+'/'+ValueName+'/Height',AForm.Height);
        End;
    End;
  XMLConfig.SetValue(SectionName+'/'+ValueName+'/WindowState',Ord(ws));
End;

initialization
XMLConfig := Nil;

finalization
If XMLConfig <> Nil Then
  XMLConfig.Destroy;

End.
