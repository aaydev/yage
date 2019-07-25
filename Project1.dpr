{*******************************************************}
{ YAGE: Yet Another Global Encoder                      }
{ Unit: Project                                         }
{ Copyright(c) 2019 Alexey Anisimov                     }
{ Contact email: softlight@ya.ru                        }
{*******************************************************}

program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.DateUtils,
  Main.Exec in 'Main.Exec.pas',
  Main.Help in 'Main.Help.pas',
  Helper.Console in 'Helper.Console.pas',
  Helper.Singleton in 'Helper.Singleton.pas',
  Common.ShellFileSupport in 'Common.ShellFileSupport.pas';

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  try
    App.Init;
    App.StartTime := Now;

    if App.CheckParams then
    begin
      if App.Log then
        WriteAppStartTime(App.StartTime);

      App.Execute;

      App.StopTime := Now;
      App.ElapsedTime := MilliSecondsBetween(App.StartTime, App.StopTime);
      if App.Log then
        WriteAppStopTime(App.StartTime, App.StopTime, App.ElapsedTime);
    end;
  except
    on E: Exception do
    begin
      WriteAppLog(error, Format('%s: %s', [E.ClassName, E.Message]));
      App.StopTime := Now;
      App.ElapsedTime := MilliSecondsBetween(App.StartTime, App.StopTime);
      if App.Log then
        WriteAppStopTime(App.StartTime, App.StopTime, App.ElapsedTime);
    end;
  end;

  App.Finish;
  System.ExitCode := App.ExitCode;
end.
