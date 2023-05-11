unit
mmErrMsg;

{$mode objfpc}{$H+}{$J-}

interface

function translate_mm_error(error_number: word): PChar;

implementation

uses
  mmSystem;

function translate_mm_error(error_number: word): PChar;
  begin
    case error_number of
      mmsyserr_NoError:
        Result := 'no error';
      mmsyserr_Error:
        Result := 'unspecified error';
      mmsyserr_BadDeviceID:
        Result := 'device ID out of range';
      mmsyserr_NotEnabled:
        Result := 'driver failed enable';
      mmsyserr_Allocated:
        Result := 'device already allocated';
      mmsyserr_InvalHandle:
        Result := 'device handle is invalid';
      mmsyserr_NoDriver:
        Result := 'no device driver present';
      mmsyserr_NoMem:
        Result := ' memory allocation error';
      mmsyserr_NotSupported:
        Result := 'function isn''t supported';
      mmsyserr_BadErrNum:
        Result := 'error value out of range';
      mmsyserr_InvalFlag:
        Result := 'invalid flag passed';
      mmsyserr_InvalParam:
        Result := 'invalid parameter passed';
      waverr_BadFormat:
        Result := 'unsupported wave format';
      waverr_StillPlaying:
        Result := 'still something playing';
      waverr_Unprepared:
        Result := 'header not prepared';
      waverr_Sync:
        Result := 'device is synchronous';
      midierr_Unprepared:
        Result := 'header not prepared';
      midierr_StillPlaying:
        Result := 'still something playing';
      midierr_NoMap:
        Result := 'no current map';
      midierr_NotReady:
        Result := 'hardware is still busy';
      midierr_NoDevice:
        Result := 'port no longer connected';
      midierr_InvalidSetup:
        Result := 'invalid setup';
      timerr_NoCanDo:
        Result := 'request not completed';
      timerr_Struct:
        Result := 'time struct size';
      joyerr_Parms:
        Result := 'bad parameters';
      joyerr_NoCanDo:
        Result := 'request not completed';
      joyerr_Unplugged:
        Result := 'joystick is unplugged';
      mmioerr_FileNotFound:
        Result := 'file not found';
      mmioerr_OutOfMemory:
        Result := 'out of memory';
      mmioerr_CannotOpen:
        Result := 'cannot open';
      mmioerr_CannotClose:
        Result := 'cannot close';
      mmioerr_CannotRead:
        Result := 'cannot read';
      mmioerr_CannotWrite:
        Result := 'cannot write';
      mmioerr_CannotSeek:
        Result := 'cannot seek';
      mmioerr_CannotExpand:
        Result := 'cannot expand file';
      mmioerr_ChunkNotFound:
        Result := 'chunk not found';
      mmioerr_Unbuffered:
        Result := 'file is unbuffered';
        {   midierr_LastError: Result := 'last error in range'; }
        {   timerr_NoError: Result := 'no error'; }
        {   joyerr_NoError: Result := 'no error'; }
      else
        Result := 'invalid parameter passed to error translation routine!';
      end;
  end;

begin
end.
