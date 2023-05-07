
Unit main;


(*  Based on Teach Morse Code Copyright (c) 1978 Howard Cunningham
    and MorseTest Copyright (c) 2003 - 2006 Julian Moss, G4ILO
    Released under the GNU General Public License Version 2
    (see http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt)      *)

{$mode objfpc}{$H+}{$codepage utf8}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
ExtCtrls, ComCtrls,
// Windows-specific stuff
Windows, MMSystem, mmErrMsg;


Const 
  sample_rate = 8000;
  sine_table_samples = 1 shl 15;
  // number of samples in sine table
  max_buffer_samples = 16000;
  // reasonable size of output buffer (< 64K)
  open_error = 'Error opening waveform audio!';
  mem_error = 'Error allocating memory!';

Type 
  audio_sample = -32767..32767;
  // for 16-bit audio

Type 
  PSineTable = ^TSineTable;
  // sine value store
  TSineTable = array [0..sine_table_samples-1] Of audio_sample;

  PBuffer = ^TBuffer;
  // output buffer type
  TBuffer = array [0..max_buffer_samples-1] Of audio_sample;

  { TfrmMain }

  TfrmMain = Class(TForm)
    Control: TButton;
    cbLetters: TCheckBox;
    Graph: TPanel;
    lbK: TLabel;
    lbquest: TLabel;
    lbF: TLabel;
    lb5: TLabel;
    lbI: TLabel;
    lbT: TLabel;
    lb3: TLabel;
    lbS: TLabel;
    lb6: TLabel;
    lbD: TLabel;
    lbequal: TLabel;
    lb2: TLabel;
    lbH: TLabel;
    lbU: TLabel;
    lbE: TLabel;
    lbX: TLabel;
    lbdot: TLabel;
    lbslash: TLabel;
    lbJ: TLabel;
    lbL: TLabel;
    lbR: TLabel;
    lb9: TLabel;
    lbG: TLabel;
    lbY: TLabel;
    lbW: TLabel;
    lbO: TLabel;
    lb8: TLabel;
    lbA: TLabel;
    lbP: TLabel;
    lbB: TLabel;
    lbN: TLabel;
    lbV: TLabel;
    lbQ: TLabel;
    pbQ: TProgressBar;
    pb7: TProgressBar;
    pbP: TProgressBar;
    pbW: TProgressBar;
    pbdot: TProgressBar;
    pbL: TProgressBar;
    pbR: TProgressBar;
    pbA: TProgressBar;
    pbM: TProgressBar;
    pb6: TProgressBar;
    pbB: TProgressBar;
    pbslash: TProgressBar;
    pbZ: TProgressBar;
    pbX: TProgressBar;
    pbD: TProgressBar;
    pbequal: TProgressBar;
    pbY: TProgressBar;
    pbC: TProgressBar;
    pbK: TProgressBar;
    pbN: TProgressBar;
    pb2: TProgressBar;
    pb3: TProgressBar;
    pbquest: TProgressBar;
    pbG: TProgressBar;
    pbF: TProgressBar;
    pbU: TProgressBar;
    pb4: TProgressBar;
    pb5: TProgressBar;
    pbV: TProgressBar;
    pbH: TProgressBar;
    pbS: TProgressBar;
    pbI: TProgressBar;
    pbT: TProgressBar;
    pbE: TProgressBar;
    pb0: TProgressBar;
    pb9: TProgressBar;
    pb8: TProgressBar;
    pbO: TProgressBar;
    pb1: TProgressBar;
    pbJ: TProgressBar;
    lb7: TLabel;
    lbZ: TLabel;
    lb0: TLabel;
    lb1: TLabel;
    lbM: TLabel;
    lbC: TLabel;
    lb4: TLabel;
    Sent: TPanel;
    cbSymbols: TCheckBox;
    cbNumbers: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    tbCharSpeed: TTrackBar;
    tbPitch: TTrackBar;
    Timer: TTimer;
    Procedure ControlClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: UnicodeChar);
    Procedure SpeedChange(Sender: TObject);
    Procedure PitchChange(Sender: TObject);
    Procedure CharsChange(Sender: TObject);
    Procedure SetChar(c: Char; enable: Boolean; err: Integer);
    Procedure TimerTimer(Sender: TObject);
    Procedure WaveOutProc(hwo: HWAVEOUT; uMsg: UINT; dwParam1, dwParam2: DWORD);
    Private 
    { private declarations }
      angle: integer;
      // current sine wave angle
      sine_table: PSineTable;
      // sine-wave values are pre-stored in this array
      hWave_hdr1: PWaveHdr;
      // wave headers
      hBuffer1: PWaveHdr;
      buffer_bytes: integer;
      // max number of bytes in each output buffer
      freq: integer;
      hWave_out: HWaveOut;
      // handle to wave out device
      pcm: TWaveFormatEx;
      // wave format descriptor
      key_down: boolean;
      Procedure Send(ch: char);
      Procedure StopSending;
      Procedure do_sine_table;
      Procedure fill_buffer_with_sinewave (bfr: PBuffer;  Var index, samples: integer);
      Procedure fill_buffer_with_silence (bfr: pBuffer;  Var index, samples: integer);
      Function fill_buffer_with_char (bfr: pBuffer; ch: char): integer;
      Procedure write_next_buffer (header: PWaveHdr; ch: char);
    Public 
    { public declarations }
      Procedure ShowGraph;
      Function SelectLetter: Char;
      Procedure Weight (Var old: dword; new: integer);
      Procedure Grade(told: Integer);
  End;

Var 
  frmMain: TfrmMain;

Implementation

{ TfrmMain }

Uses 
config, codechars;

Const 
  sConfigRoot = 'Settings';
  sChars = 'Q7ZG098O1JPW.LRAM6B/XD=YCKN23?FU45VHSITE';
  good = 0;
(* error rate bounds *)
  bad = 255;
  overall = 0;

Var 
  dotlength: integer;
  codestr: UnicodeString;
  displaystr:String;
  num: integer;
(* size of current alphabet *)
  charsent: UnicodeChar;
  charsentat: dword;
  waittime: dword = 3000;
  error: array [0..96] Of dword;

Function Rand: integer;
Begin
  Rand := Trunc(Random * 32768.0);
End;

Procedure waveOutPrc(hwo: HWAVEOUT; uMsg: UINT; dwInstance, dwParam1, dwParam2: DWORD);
stdcall;
Begin
  frmMain.WaveOutProc(hwo, uMsg, dwParam1, dwParam2)
End;

Procedure TfrmMain.FormCreate(Sender: TObject);

Var 
  NonClientMetrics: TNonClientMetrics;
  open_status: MMRESULT;
  i: Integer;
Begin
  // set window font to system message text font
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  If SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) Then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
  // set form icon
  Icon.Handle := ExtractIcon(Handle,PChar(Application.ExeName),0);
  // set the configuration file
  SetConfigFileName(ChangeFileExt(Application.ExeName,'.ini'));
  // set form position and persistent values
  GetFormPositionFromConfigFile(sConfigRoot,'WindowPos',frmMain);
  tbCharSpeed.Position := GetValueFromConfigFile(sConfigRoot,'CharSpeed',20);
  tbPitch.Position := GetValueFromConfigFile(sConfigRoot,'Pitch',800);
  cbLetters.Checked := GetValueFromConfigFile(sConfigRoot,'Letters',1) = 1;
  cbNumbers.Checked := GetValueFromConfigFile(sConfigRoot,'Numbers',1) = 1;
  cbSymbols.Checked := GetValueFromConfigFile(sConfigRoot,'Symbols',0) = 1;

  // get the memory required for wave header
  GetMem(hWave_hdr1, SizeOf(WaveHdr));

  // estimate of reasonable output buffer size
  buffer_bytes := 2 * max_buffer_samples;

  // get the memory required for output buffer
  GetMem(hBuffer1, buffer_bytes);

  hWave_out := 0;
  // get the memory for the sine-wave table
  GetMem(sine_table, SizeOf(TSineTable));

  // set other state variables
  key_down := false;

  error[overall] := bad * 30 Div 100;
  For i := 1 To 96 Do
    error[i] := bad;

  Randomize;

  SpeedChange(Sender);
  PitchChange(Sender);
  CharsChange(Sender);

  // fill in the TWaveFormatEx structure with our wave details
  With pcm Do
    Begin
      wFormatTag := wave_Format_PCM;
      // it's PCM data
      nChannels := 1;
      // mono
      nSamplesPerSec := sample_rate;
      // set the 44.1KHz rate
      nAvgBytesPerSec := 2 * sample_rate;
      // two bytes per sample
      nBlockAlign := 2;
      // for mono 16-bit audio
      wBitsPerSample := 16;
      // 16-bit audio
      cbSize := 0;
    End;

  // try and open the wave device for our format of wave data
  open_status := waveOutOpen(@hWave_out, WAVE_MAPPER, @pcm,
   DWORD(@WaveOutPrc), DWORD(@Self), CALLBACK_FUNCTION);
  If open_status <> MMSYSERR_NOERROR Then
    Begin
      hWave_out := 0;
      MessageDlg(open_error + #13#10 + translate_mm_error(open_status), mtWarning, [mbOK], 0);
    End;

End;

Procedure TfrmMain.ControlClick(Sender: TObject);
Begin
  If Timer.Enabled Then
    Begin
      // stop sending
      Timer.Enabled := false;
      // wait for sending to stop
      StopSending;
      // re-enable controls
      Control.Caption := 'Resume';
      cbLetters.Enabled := true;
      cbNumbers.Enabled := true;
      cbSymbols.Enabled := true;
      tbCharSpeed.Enabled := true;
      tbPitch.Enabled := true;
    End
  Else
    Begin
      // disable controls
      cbLetters.Enabled := false;
      cbNumbers.Enabled := false;
      cbSymbols.Enabled := false;
      tbCharSpeed.Enabled := false;
      tbPitch.Enabled := false;
      // build sine table for the selected frequency
      do_sine_table;
      // select the first character
      charsent := SelectLetter;
      // start sending
      Timer.Enabled := true;
      Control.Caption := 'Pause';
    End;
End;

Procedure TfrmMain.FormDestroy(Sender: TObject);
Begin
  StopSending;
  WriteFormPositionToConfigFile(sConfigRoot,'WindowPos',frmMain);
  SaveValueToConfigFile(sConfigRoot,'CharSpeed',tbCharSpeed.Position);
  SaveValueToConfigFile(sConfigRoot,'Pitch',tbPitch.Position);
  SaveValueToConfigFile(sConfigRoot,'Letters',Ord(cbLetters.Checked));
  SaveValueToConfigFile(sConfigRoot,'Numbers',Ord(cbNumbers.Checked));
  SaveValueToConfigFile(sConfigRoot,'Symbols',Ord(cbSymbols.Checked));
  FreeMem(sine_table, SizeOf(TSineTable));
  FreeMem(hBuffer1, buffer_bytes);
  FreeMem(hWave_hdr1, SizeOf(WaveHdr));
  If hWave_out <> 0 Then
    waveOutClose(hWave_out);
End;

Procedure TfrmMain.FormKeyPress(Sender: TObject; Var Key: UnicodeChar);
Begin
  If Key In ['0'..'9','A'..'Z','a'..'z','.','/','=','?'] Then
    Begin
      // display character typed
      If Length(displaystr) > 48 Then displaystr := '';
      displaystr := displaystr + AnsiString(Uppercase(Key));
      Sent.Caption := displaystr;
      // adjust the waiting time
      Weight(waittime, 2 * (GetTickCount - charsentat));
      If waittime > 5000 Then waittime := 5000;
      If Uppercase(Key) = charsent Then
        Begin
          Grade(good);
          // choose a new character
          charsent := SelectLetter;
        End
      Else
        Grade(bad);
      ShowGraph;
      // force character to be sent
      charsentat := 0;
    End;
End;

Procedure TfrmMain.SpeedChange(Sender: TObject);

Var 
  s: Integer;
Begin
  s := tbCharSpeed.Position;
  dotlength := ((sample_rate * 12) Div 10) Div s;
  tbCharSpeed.Hint := IntToStr(s) + ' wpm';
End;

Procedure TfrmMain.PitchChange(Sender: TObject);
Begin
  freq := tbPitch.Position;
  freq := (freq + 25) Div 50 * 50;
  tbPitch.Position := freq;
  Application.ProcessMessages;
  tbPitch.Hint := IntToStr(freq) + ' Hz';
End;

Procedure TfrmMain.CharsChange(Sender: TObject);

Var 
  i: Integer;
Begin
  If Not (cbLetters.Checked Or cbNumbers.Checked Or cbSymbols.Checked) Then
    Begin
      cbLetters.Checked := true;
      Application.ProcessMessages;
    End;
  codestr := sChars;
  If Not cbLetters.Checked Then
    Begin
      i := 1;
      Repeat
        If codestr[i] In ['A'..'Z'] Then
          Delete(codestr,i,1)
        Else
          Inc(i);
      Until i > Length(codestr);
    End;
  If Not cbNumbers.Checked Then
    Begin
      i := 1;
      Repeat
        If codestr[i] In ['0'..'9'] Then
          Delete(codestr,i,1)
        Else
          Inc(i);
      Until i > Length(codestr);
    End;
  If Not cbSymbols.Checked Then
    Begin
      i := 1;
      Repeat
        If codestr[i] In ['.','/','=','?'] Then
          Delete(codestr,i,1)
        Else
          Inc(i);
      Until i > Length(codestr);
    End;
  num := Length(codestr) Div 10 + 1;
  ShowGraph;
End;

Procedure TfrmMain.SetChar(c: Char; enable: Boolean; err: Integer);

Var 
  n, n1, n2:String;
  pb, lb: TComponent;
Begin
  Case c Of 
    '.': n := 'dot';
    '/': n := 'slash';
    '=': n := 'equal';
    '?': n := 'quest';
    Else
      n := c;
  End;
  n1 := 'pb'+n;
  n2 := 'lb'+n;
  pb := FindComponent(n1);
  lb := FindComponent(n2);
  If pb <> Nil Then
    With TProgressBar(pb) Do
      Begin
        Visible := enable;
        Position := err;
      End;
  If lb <> Nil Then
    TLabel(lb).Enabled := enable;
End;

Procedure TfrmMain.TimerTimer(Sender: TObject);

Var 
  t: DWord;
Begin
  t := GetTickCount();
  If (charsentat + waittime) <= t Then
    Begin
      If charsentat > 0 Then
        Begin
          // timeout occurred, display char that was sent
          If Length(displaystr) > 48 Then displaystr := '';
          displaystr := displaystr + AnsiString(charsent);
          Sent.Caption := displaystr;
        End;
      // send character
      charsentat := t;
      Send(charsent);
    End;
End;

Procedure TfrmMain.ShowGraph;

Var 
  i: Integer;
  a: UnicodeString;
  c: Char;
Begin
  a := Copy(codestr,1,num);
  For i := 1 To Length(sChars) Do
    Begin
      c := sChars[i];
      SetChar(c, Pos(c,a) > 0, error[Ord(sChars[i])]);
    End;
End;

Function TfrmMain.SelectLetter: Char;


(*  Select a test letter from the current alphabet with selection probability
    proportional to the current error rate function.   *)

Var 
  letter, sum: integer;
Begin
  sum := 0;
  For letter := 1 To num Do
    sum := sum + error[Ord(codestr[letter])] + 1;
  sum := Rand Mod sum;
  letter := num + 1;
  Repeat
    letter := letter - 1;
    sum := sum - error[Ord(codestr[letter])] - 1;
  Until sum <= 0;
  Result := codestr[letter];
End;

Procedure TfrmMain.Grade(told: Integer);


(*  Adjust individual and overall error rate estimations.  If both are
    sufficiently low, increase the alphabet size.   *)

Var 
  i: Integer;
  increase: Boolean;
Begin
  Weight(error[overall], told);
  Weight(error[Ord(charsent)], told);
  If error [overall] < 0.30 * bad Then
    Begin
      If error[overall] < 0.10 * bad Then
        Weight(error[Ord(charsent)], told);
(* twice *)
      increase := true;
      For i := 1 To num Do
        If error[Ord(charsent)] > 0.40 * bad Then increase := false;
      If increase And (num < Length(codestr)) Then num := num + 1;
    End;
End;

Procedure TfrmMain.Weight (Var old: dword; new: integer);


(*  Adjust old value by a weighted average with a new value.  This
    approximates the damping of an r-c lowpass filter.   *)

Const 
  percent = 0.125;
Begin
  old := round ((1.0-percent) * old + percent * new);
End;


Procedure TfrmMain.Send(ch: char);
Begin
  If key_down Then Exit;

  With hWave_hdr1^ Do
    Begin
      lpData := pChar(hBuffer1);
      // pointer to the data
      dwBufferLength := 0;
      // fill in size later
      dwBytesRecorded := 0;
      dwUser := 0;
      dwFlags := 0;
      dwLoops := 1;
      // just a single loop
      lpNext := Nil;
      reserved := 0;
    End;
  hWave_hdr1^.dwBufferLength := buffer_bytes;
  // actual buffer sizes
  // now fill the buffer and write it to the wave output
  write_next_buffer(hWave_hdr1,ch);
  // ready to go
  key_down := true;

End;

Procedure TfrmMain.StopSending;
Begin
  If key_down Then
    Begin
      Repeat
        Application.ProcessMessages;
      Until Not key_down;
    End
End;

Procedure TfrmMain.WaveOutProc(hwo: HWAVEOUT; uMsg: UINT; dwParam1, dwParam2: DWORD);

Var 
  free_header: pWaveHdr;
Begin
  Case uMsg Of 
    WOM_OPEN  : ;
    // this message is never received
    WOM_CLOSE : ;
    // this message is never received
    WOM_DONE  :
                Begin
                  // handle the wave out done message by writing the next buffer, if required
                  // point to wave header just completed, i.e. the next free buffer
                  free_header := pWaveHdr(dwParam1);
                  waveOutUnprepareHeader(hWave_out, free_header, SizeOf(WaveHdr));
                  key_down := false;
                End;
  End
End;

Procedure TfrmMain.write_next_buffer(header: pWaveHdr; ch: char);
Begin
  With header^ Do
    dwBufferLength := fill_buffer_with_char (pBuffer(lpData), ch);
  // write the buffer and bump the number written
  waveOutPrepareHeader (hWave_out, header, SizeOf(WaveHdr));
  waveOutWrite (hWave_out, header, SizeOf(WaveHdr));
End;

Procedure TfrmMain.do_sine_table;

Var 
  i: 0..sine_table_samples - 1;
  y: extended;
Begin
  For i := 0 To sine_table_samples - 1 Do
    Begin
      y := round (32767.0 * sin (2.0* i * Pi / sine_table_samples));
      sine_table^ [i] := round (y);
    End;
End;

Procedure TfrmMain.fill_buffer_with_sinewave (bfr: pBuffer;  Var index, samples: integer);

Const 
  fract_bits = 15;

Var 
  sample: audio_sample;
  d_angle: integer;
  // 32-bit number, with 14 fractional bits, i.e. 17.15
  i,j,max_angle: integer;
Begin
  angle := 0;
  // compute the angular step per sample corresponding to the desired frequency
  d_angle := round ((sine_table_samples shl fract_bits) * (freq / sample_rate));
  // this is the maximum number of samples in the sine table
  max_angle := (sine_table_samples shl fract_bits) - 1;
  For i := 1 To samples Do
    Begin
      sample := sine_table^[angle shr fract_bits];
      // get current sine value
      If i < 32 Then
        For j := i To 32 Do
          sample := (sample * 8) Div 9;
      If i > (samples - 32) Then
        For j := samples-32 To i Do
          sample := (sample * 8) Div 9;
      bfr^ [index] := sample;
      Inc (index);
      // bump the buffer pointer
      Inc (angle, d_angle);
      // bump the angle
      angle := angle And max_angle;
      // wrap to 360 degrees
    End;
End;

Procedure TfrmMain.fill_buffer_with_silence (bfr: pBuffer;  Var index, samples: integer);

Var 
  sample: integer;
Begin
  For sample := 0 To samples - 1 Do
    Begin
      bfr^ [index] := 0;
      // store it in the caller's buffer
      Inc (index);
      // bump the buffer pointer
    End;
End;

Function TfrmMain.fill_buffer_with_char (bfr: pBuffer; ch: char): integer;
// This procedure fills a  buffer with a Morse character.

Var 
  sample, chunk_samples, c, i: integer;
  mc: String;
Begin
  sample := 0;
  c := Ord(ch);
  If (c >= 32) And (c <= 90) Then
    Begin
      mc := code[c];
      i := 1;
      While i <= Length(mc) Do
        Begin
          Case mc[i] Of 
            '.':
                 Begin
                   chunk_samples := dotlength;
                   fill_buffer_with_sinewave(bfr, sample, chunk_samples);
                   fill_buffer_with_silence(bfr, sample, dotlength);
                 End;
            '-':
                 Begin
                   chunk_samples := dotlength * 3;
                   fill_buffer_with_sinewave(bfr, sample, chunk_samples);
                   fill_buffer_with_silence(bfr, sample, dotlength);
                 End;
            Else
              chunk_samples := dotlength * 2;
            fill_buffer_with_silence(bfr, sample, chunk_samples);
          End;
          Inc(i);
        End;
    End;
  chunk_samples := dotlength * 2;
  fill_buffer_with_silence(bfr, sample, chunk_samples);
  Result := sample * 2;
  // no of bytes (2 per sample)
End;

initialization
  {$I main.lrs}

End.
