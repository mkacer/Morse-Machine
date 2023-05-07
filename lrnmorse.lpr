program lrnmorse;

(*  Copyright (c) 2003 - 2006 Julian Moss, G4ILO
    Released under the GNU General Public License Version 2
    (see http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt)      *)

{$mode objfpc}{$H+}{$codepage utf8}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, main, config, codechars, mmErrMsg;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='Morse Machine';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

