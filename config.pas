unit config;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Forms, Controls, Classes, SysUtils, XMLConf;

procedure SetConfigFileName(const filename: string);
function GetValueFromConfigFile(const SectionName, ValueName, Default: unicodestring):
  unicodestring;
function GetValueFromConfigFile(const SectionName, ValueName: unicodestring;
  Default: integer): integer;
function GetValueFromConfigFile(const SectionName, ValueName: unicodestring;
  Default: boolean): boolean;
procedure SaveValueToConfigFile(const SectionName, ValueName, Value: unicodestring);
procedure SaveValueToConfigFile(const SectionName, ValueName: unicodestring; Value: integer);
procedure SaveValueToConfigFile(const SectionName, ValueName: unicodestring; Value: boolean);
procedure GetFormPositionFromConfigFile(const SectionName, ValueName: unicodestring; AForm: TForm);
procedure WriteFormPositionToConfigFile(const SectionName, ValueName: unicodestring; AForm: TForm);

implementation

var
  XMLConfig: TXMLConfig;

const
  sValue = '/Value';

procedure SetConfigFileName(const filename: string);
  begin
    XMLConfig := TXMLConfig.Create(nil);
    XMLConfig.Filename := filename;
  end;

function GetValueFromConfigFile(const SectionName, ValueName, Default: unicodestring):
unicodestring;
  begin
    Result := XMLConfig.GetValue(SectionName + '/' + ValueName + sValue, Default);
  end;

function GetValueFromConfigFile(const SectionName, ValueName: unicodestring;
  Default: integer): integer;
  begin
    Result := XMLConfig.GetValue(SectionName + '/' + ValueName + sValue, Default);
  end;

function GetValueFromConfigFile(const SectionName, ValueName: unicodestring;
  Default: boolean): boolean;
  begin
    Result := XMLConfig.GetValue(SectionName + '/' + ValueName + sValue, Default);
  end;

procedure SaveValueToConfigFile(const SectionName, ValueName, Value: unicodestring);
  begin
    XMLConfig.SetValue(SectionName + '/' + ValueName + sValue, Value);
  end;

procedure SaveValueToConfigFile(const SectionName, ValueName: unicodestring; Value: integer);
  begin
    XMLConfig.SetValue(SectionName + '/' + ValueName + sValue, Value);
  end;

procedure SaveValueToConfigFile(const SectionName, ValueName: unicodestring; Value: boolean);
  begin
    XMLConfig.SetValue(SectionName + '/' + ValueName + sValue, Value);
  end;

procedure GetFormPositionFromConfigFile(const SectionName, ValueName: unicodestring; AForm: TForm);

  var
    ws:      TWindowState;
    t, l, w: integer;
  begin
    ws := TWindowState(XMLConfig.GetValue(SectionName + '/' + ValueName +
      '/WindowState', Ord(wsNormal)));
    t  := XMLConfig.GetValue(SectionName + '/' + ValueName + '/Top', AForm.Top);
    l  := XMLConfig.GetValue(SectionName + '/' + ValueName + '/Left', AForm.Left);
    if t < Screen.Height then
      AForm.Top := t;
    if l < Screen.Width then
      AForm.Left := l;
    if AForm.BorderStyle = bsSizeable then
      begin
      w := XMLConfig.GetValue(SectionName + '/' + ValueName + '/Width', AForm.Width);
      if w > 40 then
        AForm.Width := w;
      AForm.Height  := XMLConfig.GetValue(SectionName + '/' + ValueName + '/Height', AForm.Height);
      end;
    Application.ProcessMessages;
    if ws = wsMaximized then
      AForm.WindowState := ws;
  end;

procedure WriteFormPositionToConfigFile(const SectionName, ValueName: unicodestring; AForm: TForm);

  var
    ws: TWindowState;
  begin
    ws := AForm.WindowState;
    if ws = wsNormal then
      begin
      XMLConfig.SetValue(SectionName + '/' + ValueName + '/Top', AForm.Top);
      XMLConfig.SetValue(SectionName + '/' + ValueName + '/Left', AForm.Left);
      if AForm.BorderStyle = bsSizeable then
        begin
        XMLConfig.SetValue(SectionName + '/' + ValueName + '/Width', AForm.Width);
        XMLConfig.SetValue(SectionName + '/' + ValueName + '/Height', AForm.Height);
        end;
      end;
    XMLConfig.SetValue(SectionName + '/' + ValueName + '/WindowState', Ord(ws));
  end;

initialization
  XMLConfig := nil;

finalization
  if XMLConfig <> nil then
    XMLConfig.Destroy;

end.
