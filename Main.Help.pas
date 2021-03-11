{*******************************************************}
{ YAGE: Yet Another Global Encoder                      }
{ Unit: Main.Help                                       }
{ Copyright(c) 2021 Alexey Anisimov                     }
{ Contact email: softlight@ya.ru                        }
{*******************************************************}

unit Main.Help;

{$MODE delphiunicode}

interface

type

  TLogMsgType = (ok, info, warn, error, time);

  procedure WriteAppTitle(const AppName: string; const AppDescription: string; const AppVer: string;
                          const Copyright: string);
  procedure WriteAppTitleShort(const AppName: string; const AppVer: string);
  procedure WriteAppHelp(const AppName: string; const UsageString: string);
  procedure WriteAppLog(const LogMsgType: TLogMsgType; const Msg: string);
  procedure WriteAppStartTime(const StartTime: TDateTime);
  procedure WriteAppStopTime(const StartTime: TDateTime;
                              const StopTime: TDateTime; const ElapsedTime: Int64);

resourcestring

  rs_StartMsg = '%s/%s: %s / v.%s';
  rs_StartMsgShort = '%s v.%s';
  rs_LogMsgOk = ' ok ';
  rs_LogMsgInfo = 'info';
  rs_LogMsgWarn = 'warn';
  rs_LogMsgErr = 'err!';
  rs_LogMsgTime = 'time';

implementation

uses
  Sysutils, Windows, Classes,
  Helper.Console;

procedure WriteAppHelp(const AppName: string; const UsageString: string);
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  List.Add('');
  List.Add(Format(UsageString, [LowerCase(AppName)]));
  List.Add('');
  List.Add('Keys:');
  List.Add('[/from:Encoding] - source file encoding');
  List.Add('    valid values:');
  List.Add('      - "oem" (default value), "utf8", "unicode"');
  List.Add('[/to:Encoding] - destination file encoding');
  List.Add('    valid values:');
  List.Add('      - "oem", "utf8" (default value), "unicode"');
  List.Add('[/file:ResultFileName] - destination file name');
  List.Add('[/excel] - use COM to convert Excel');
  List.Add('[/log] - show execution details');
  List.Add('');

  for i := 0 to List.Count - 1 do
    Console.WriteLn(List.Strings[i]);

  List.Free;
end;

procedure WriteAppTitle(const AppName: string; const AppDescription: string; const AppVer: string;
                        const Copyright: string);
begin
  Console.SetColor(FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY);
  Console.Write(AppName);
  Console.SetColor();
  Console.WriteLn(Format(': %s - v.%s', [AppDescription, AppVer]));
  Console.WriteLn(Copyright);
end;

procedure WriteAppTitleShort(const AppName: string; const AppVer: string);
begin
  Console.SetColor(FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY);
  Console.Write(AppName);
  Console.SetColor();
  Console.WriteLn(Format(' v.%s', [AppVer]));
end;

procedure WriteAppLog(const LogMsgType: TLogMsgType; const Msg: string);
begin
  Console.SetColor();
  Console.Write('[');
  case LogMsgType of
    ok: begin
      Console.SetColor(FOREGROUND_BLUE);
      Console.Write(rs_LogMsgOk);
    end;
    info: begin
      Console.SetColor(FOREGROUND_GREEN);
      Console.Write(rs_LogMsgInfo);
    end;
    warn: begin
      Console.SetColor(FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY);
      Console.Write(rs_LogMsgWarn);
    end;
    error: begin
      Console.SetColor(FOREGROUND_RED or FOREGROUND_INTENSITY);
      Console.Write(rs_LogMsgErr);
    end;
    time: begin
      Console.SetColor(FOREGROUND_GREEN or FOREGROUND_BLUE);
      Console.Write(rs_LogMsgTime);
    end;
  end;
  Console.SetColor();
  Console.Write(']');
  Console.SetColor();
  Console.WriteLn(' ' + Msg);
end;

procedure WriteAppStartTime(const StartTime: TDateTime);
begin
  WriteAppLog(time, Format('Start: %s / %s', [
    FormatDateTime('dd:mm:yy', Trunc(StartTime)),
    FormatDateTime('hh:nn:ss', Frac(StartTime))]));
end;

procedure WriteAppStopTime(const StartTime: TDateTime;
                            const StopTime: TDateTime; const ElapsedTime: Int64);
begin
  WriteAppLog(time, Format('Stop: %s / %s', [
    FormatDateTime('dd:mm:yy', Trunc(StopTime)),
    FormatDateTime('hh:nn:ss', Frac(StopTime))]));
  WriteAppLog(info, Format('Elapsed time: %s ms', [IntToStr(ElapsedTime)]));
end;

end.
