unit main;

(*  Based on Teach Morse Code Copyright (c) 1978 Howard Cunningham
    and MorseTest Copyright (c) 2003 - 2006 Julian Moss, G4ILO
    Released under the GNU General Public License Version 2
    (see http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt)      *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  // Windows-specific stuff
  Windows, MMSystem, mmErrMsg;


const
  sample_rate = 8000;
  sine_table_samples = 1 shl 15;     // number of samples in sine table
  max_buffer_samples = 16000;        // reasonable size of output buffer (< 64K)
  open_error = 'Error opening waveform audio!';
  mem_error = 'Error allocating memory!';

type
  audio_sample = -32767..32767;       // for 16-bit audio

type
  PSineTable = ^TSineTable;          // sine value store
  TSineTable = array [0..sine_table_samples-1] of audio_sample;

	PBuffer = ^TBuffer;                // output buffer type
  TBuffer = array [0..max_buffer_samples-1] of audio_sample;

  { TfrmMain }

  TfrmMain = class(TForm)
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
    procedure ControlClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure SpeedChange(Sender: TObject);
    procedure PitchChange(Sender: TObject);
    procedure CharsChange(Sender: TObject);
    procedure SetChar(c: Char; enable: Boolean; err: Integer);
    procedure TimerTimer(Sender: TObject);
    procedure WaveOutProc(hwo: HWAVEOUT; uMsg: UINT; dwParam1, dwParam2: DWORD);
  private
    { private declarations }
    angle: integer;          // current sine wave angle
    sine_table: PSineTable;  // sine-wave values are pre-stored in this array
    hWave_hdr1: PWaveHdr;   // wave headers
		hBuffer1: PWaveHdr;
    buffer_bytes: integer;   // max number of bytes in each output buffer
		freq: integer;
		hWave_out: HWaveOut;     // handle to wave out device
    pcm: TWaveFormatEx;      // wave format descriptor
		key_down: boolean;
    procedure Send(ch: char);
    procedure StopSending;
    procedure do_sine_table;
    procedure fill_buffer_with_sinewave (bfr: PBuffer;  var index, samples: integer);
		procedure fill_buffer_with_silence (bfr: pBuffer;  var index, samples: integer);
		function fill_buffer_with_char (bfr: pBuffer; ch: char): integer;
    procedure write_next_buffer (header: PWaveHdr; ch: char);
  public
    { public declarations }
    procedure ShowGraph;
    function SelectLetter: Char;
    procedure Weight (var old: dword; new: integer);
    procedure Grade(told: Integer);
  end; 

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

uses
  config, codechars;

const
  sConfigRoot = 'Settings';
  sChars = 'Q7ZG098O1JPW.LRAM6B/XD=YCKN23?FU45VHSITE';
  good = 0;          (* error rate bounds *)
  bad = 255;
  overall = 0;
  
var
	dotlength: integer;
  codestr, displaystr: string;
  num: integer;      (* size of current alphabet *)
  charsent: char;
  charsentat: dword;
  waittime: dword = 3000;
  error: array [0..96] of dword;

function Rand: integer;
begin
  Rand := Trunc(Random * 32768.0);
end;

procedure waveOutPrc(hwo: HWAVEOUT; uMsg: UINT; dwInstance, dwParam1, dwParam2: DWORD); stdcall;
begin
  frmMain.WaveOutProc(hwo, uMsg, dwParam1, dwParam2)
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
	NonClientMetrics: TNonClientMetrics;
  open_status: MMRESULT;
  i: Integer;
begin
	// set window font to system message text font
	NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
	if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
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

  error[overall] := bad * 30 div 100;
  for i := 1 to 96 do
    error[i] := bad;

	Randomize;

	SpeedChange(Sender);
	PitchChange(Sender);
  CharsChange(Sender);
  
  // fill in the TWaveFormatEx structure with our wave details
  with pcm do
  begin
    wFormatTag := wave_Format_PCM;         // it's PCM data
    nChannels := 1;                        // mono
    nSamplesPerSec := sample_rate;         // set the 44.1KHz rate
    nAvgBytesPerSec := 2 * sample_rate;    // two bytes per sample
    nBlockAlign := 2;                      // for mono 16-bit audio
    wBitsPerSample := 16;                  // 16-bit audio
    cbSize := 0;
  end;

  // try and open the wave device for our format of wave data
  open_status := waveOutOpen(@hWave_out, WAVE_MAPPER, @pcm, DWORD(@WaveOutPrc), Integer(Self), CALLBACK_FUNCTION);
  if open_status <> MMSYSERR_NOERROR then
  begin
    hWave_out := 0;
    MessageDlg(open_error + #13#10 + translate_mm_error(open_status), mtWarning, [mbOK], 0);
  end;

end;

procedure TfrmMain.ControlClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
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
  end
  else
  begin
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
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
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
  if hWave_out <> 0 then
    waveOutClose(hWave_out);
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['0'..'9','A'..'Z','a'..'z','.','/','=','?'] then
  begin
    // display character typed
    if Length(displaystr) > 48 then displaystr := '';
    displaystr := displaystr + Uppercase(Key);
    Sent.Caption:= displaystr;
    // adjust the waiting time
    Weight(waittime, 2 * (GetTickCount - charsentat));
    if waittime > 5000 then waittime := 5000;
    if Uppercase(Key) = charsent then
    begin
      Grade(good);
      // choose a new character
      charsent := SelectLetter;
    end
    else
      Grade(bad);
    ShowGraph;
    // force character to be sent
    charsentat := 0;
  end;
end;

procedure TfrmMain.SpeedChange(Sender: TObject);
var
	s: Integer;
begin
	s := tbCharSpeed.Position;
	dotlength := ((sample_rate * 12) div 10) div s;
  tbCharSpeed.Hint := IntToStr(s) + ' wpm';
end;

procedure TfrmMain.PitchChange(Sender: TObject);
begin
	freq := tbPitch.Position;
  freq := (freq + 25) div 50 * 50;
  tbPitch.Position := freq;
  Application.ProcessMessages;
  tbPitch.Hint := IntToStr(freq) + ' Hz';
end;

procedure TfrmMain.CharsChange(Sender: TObject);
var
  i: Integer;
begin
  if not (cbLetters.Checked or cbNumbers.Checked or cbSymbols.Checked) then
  begin
    cbLetters.Checked := true;
    Application.ProcessMessages;
  end;
  codestr := sChars;
  if not cbLetters.Checked then
  begin
    i := 1;
    repeat
      if codestr[i] in ['A'..'Z'] then
        Delete(codestr,i,1)
      else
        Inc(i);
     until i > Length(codestr);
  end;
  if not cbNumbers.Checked then
  begin
    i := 1;
    repeat
      if codestr[i] in ['0'..'9'] then
        Delete(codestr,i,1)
      else
        Inc(i);
     until i > Length(codestr);
  end;
  if not cbSymbols.Checked then
  begin
    i := 1;
    repeat
      if codestr[i] in ['.','/','=','?'] then
        Delete(codestr,i,1)
      else
        Inc(i);
     until i > Length(codestr);
  end;
  num := Length(codestr) div 10 + 1;
  ShowGraph;
end;

procedure TfrmMain.SetChar(c: Char; enable: Boolean; err: Integer);
var
  n, n1, n2: string;
  pb, lb: TComponent;
begin
  case c of
  '.': n := 'dot';
  '/': n := 'slash';
  '=': n := 'equal';
  '?': n := 'quest';
  else
    n := c;
  end;
  n1 := 'pb'+n;
  n2 := 'lb'+n;
  pb := FindComponent(n1);
  lb := FindComponent(n2);
  if pb <> nil then
    with TProgressBar(pb) do
    begin
      Visible := enable;
      Position := err;
    end;
  if lb <> nil then
    TLabel(lb).Enabled := enable;
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
var
  t: DWord;
begin
  t := GetTickCount();
  if (charsentat + waittime) <= t then
  begin
    if charsentat > 0 then
    begin
      // timeout occurred, display char that was sent
      if Length(displaystr) > 48 then displaystr := '';
      displaystr := displaystr + charsent;
      Sent.Caption:= displaystr;
    end;
    // send character
    charsentat := t;
    Send(charsent);
  end;
end;

procedure TfrmMain.ShowGraph;
var
  i: Integer;
  a: string;
  c: Char;
begin
  a := Copy(codestr,1,num);
  for i := 1 to Length(sChars) do
  begin
    c := sChars[i];
    SetChar(c, Pos(c,a) > 0, error[Ord(sChars[i])]);
  end;
end;

function TfrmMain.SelectLetter: Char;

(*  Select a test letter from the current alphabet with selection probability
    proportional to the current error rate function.   *)

var
  letter, sum: integer;
begin
  sum := 0;
  for letter := 1 to num do
    sum := sum + error[Ord(codestr[letter])] + 1;
  sum := Rand mod sum;
  letter := num + 1;
  repeat
    letter := letter - 1;
    sum := sum - error[Ord(codestr[letter])] - 1;
  until sum <= 0;
  Result := codestr[letter];
end;

procedure TfrmMain.Grade(told: Integer);

(*  Adjust individual and overall error rate estimations.  If both are
    sufficiently low, increase the alphabet size.   *)
var
  i: Integer;
  increase: Boolean;
begin
  Weight(error[overall], told);
  Weight(error[Ord(charsent)], told);
  if error [overall] < 0.30 * bad then
  begin
    if error[overall] < 0.10 * bad then
      Weight(error[Ord(charsent)], told);  (* twice *)
    increase := true;
    for i := 1 to num do
      if error[Ord(charsent)] > 0.40 * bad then increase := false;
    if increase and (num < Length(codestr)) then num := num + 1;
  end;
end;

procedure TfrmMain.Weight (var old: dword; new: integer);

(*  Adjust old value by a weighted average with a new value.  This
    approximates the damping of an r-c lowpass filter.   *)

const
  percent = 0.125;
begin
  old := round ((1.0-percent) * old + percent * new);
end;


procedure TfrmMain.Send(ch: char);
begin
  if key_down then Exit;

  with hWave_hdr1^ do
  begin
    lpData := pChar(hBuffer1);     // pointer to the data
    dwBufferLength := 0;           // fill in size later
    dwBytesRecorded := 0;
    dwUser := 0;
    dwFlags := 0;
    dwLoops := 1;                  // just a single loop
    lpNext := nil;
    reserved := 0;
  end;
  hWave_hdr1^.dwBufferLength := buffer_bytes;     // actual buffer sizes
  // now fill the buffer and write it to the wave output
  write_next_buffer(hWave_hdr1,ch);
  // ready to go
	key_down := true;

end;

procedure TfrmMain.StopSending;
begin
	if key_down then
	begin
    repeat
			Application.ProcessMessages;
    until not key_down;
  end
end;

procedure TfrmMain.WaveOutProc(hwo: HWAVEOUT; uMsg: UINT; dwParam1, dwParam2: DWORD);
var
  free_header: pWaveHdr;
begin
  case uMsg of
    WOM_OPEN  : ; // this message is never received
    WOM_CLOSE : ; // this message is never received
    WOM_DONE  : begin
                  // handle the wave out done message by writing the next buffer, if required
                  // point to wave header just completed, i.e. the next free buffer
                  free_header := pWaveHdr(dwParam1);
                  waveOutUnprepareHeader(hWave_out, free_header, SizeOf(WaveHdr));
                  key_down := false;
                end;
  end
end;

procedure TfrmMain.write_next_buffer(header: pWaveHdr; ch: char);
begin
  with header^ do
		dwBufferLength := fill_buffer_with_char (pBuffer(lpData), ch);
  // write the buffer and bump the number written
  waveOutPrepareHeader (hWave_out, header, SizeOf(WaveHdr));
  waveOutWrite (hWave_out, header, SizeOf(WaveHdr));
end;

procedure TfrmMain.do_sine_table;
var
	i: 0..sine_table_samples - 1;
	y: extended;
begin
	for i := 0 to sine_table_samples - 1 do
	begin
		y := round (32767.0 * sin (2.0* i * Pi / sine_table_samples));
		sine_table^ [i] := round (y);
	end;
end;

procedure TfrmMain.fill_buffer_with_sinewave (bfr: pBuffer;  var index, samples: integer);
const
  fract_bits = 15;
var
  sample: audio_sample;
  d_angle: integer;      // 32-bit number, with 14 fractional bits, i.e. 17.15
	i,j,max_angle: integer;
begin
	angle := 0;
  // compute the angular step per sample corresponding to the desired frequency
	d_angle := round ((sine_table_samples shl fract_bits) * (freq / sample_rate));
  // this is the maximum number of samples in the sine table
	max_angle := (sine_table_samples shl fract_bits) - 1;
  for i := 1 to samples do
  begin
  	sample := sine_table^[angle shr fract_bits];// get current sine value
    if i < 32 then
			for j := i to 32 do
      	sample := (sample * 8) div 9;
    if i > (samples - 32) then
			for j := samples-32 to i do
				sample := (sample * 8) div 9;
		bfr^ [index] := sample;
    Inc (index);                               // bump the buffer pointer
    Inc (angle, d_angle);                      // bump the angle
		angle := angle and max_angle;              // wrap to 360 degrees
  end;
end;

procedure TfrmMain.fill_buffer_with_silence (bfr: pBuffer;  var index, samples: integer);
var
  sample: integer;
begin
  for sample := 0 to samples - 1 do
	begin
    bfr^ [index] := 0;                         // store it in the caller's buffer
    Inc (index);                               // bump the buffer pointer
  end;
end;

function TfrmMain.fill_buffer_with_char (bfr: pBuffer; ch: char): integer;
// This procedure fills a  buffer with a Morse character.
var
  sample, chunk_samples, c, i: integer;
	mc: string;
begin
	sample := 0;
  c := Ord(ch);
  if (c >= 32) and (c <= 90) then
  begin
  	mc := code[c];
    i := 1;
    while i <= Length(mc) do
    begin
    	case mc[i] of
      '.':	begin
							chunk_samples := dotlength;
			      	fill_buffer_with_sinewave(bfr, sample, chunk_samples);
							fill_buffer_with_silence(bfr, sample, dotlength);
						end;
			'-':	begin
							chunk_samples := dotlength * 3;
							fill_buffer_with_sinewave(bfr, sample, chunk_samples);
							fill_buffer_with_silence(bfr, sample, dotlength);
						end;
			else
				chunk_samples := dotlength * 2;
				fill_buffer_with_silence(bfr, sample, chunk_samples);
			end;
			Inc(i);
		end;
	end;
	chunk_samples := dotlength * 2;
	fill_buffer_with_silence(bfr, sample, chunk_samples);
	Result := sample * 2; // no of bytes (2 per sample)
end;

initialization
  {$I main.lrs}

end.

