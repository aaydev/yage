{*******************************************************}
{ YAGE: Yet Another Global Encoder                      }
{ Unit: Helper.Console                                  }
{ Copyright(c) 2019 Alexey Anisimov                     }
{ Contact email: softlight@ya.ru                        }
{*******************************************************}

unit Helper.Console;

interface

uses
  Winapi.Windows,
  Helper.Singleton;

type

  TConsole = class(TSingleton)
  protected
    constructor Create; override;
  public
    destructor Destroy; override;
  private
    FHandle: Integer;
    FInfo: TConsoleScreenBufferInfo;
  public
    procedure WriteLn(const Msg: string); overload;
    procedure WriteLn; overload;
    procedure Write(const Msg: string);
    procedure Cls(const Attr: Word); overload;
    procedure Cls; overload;
    procedure SetColor(const Attr: Word); overload;
    procedure SetColor; overload;
  end;

  function Console: TConsole;

implementation

{ TConsole }

constructor TConsole.Create;
begin
  inherited Create;
  // Init settings here
  FHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(FHandle, FInfo);
end;

destructor TConsole.Destroy;
begin
  // Save settings here
  inherited Destroy;
end;

procedure TConsole.WriteLn(const Msg: string);
begin
  System.WriteLn(MSg);
end;

procedure TConsole.WriteLn;
begin
  System.WriteLn;
end;

procedure TConsole.Write(const Msg: string);
begin
  System.Write(MSg);
end;

procedure TConsole.Cls(const Attr: Word);
var
  CharsWritten: DWORD;
  CharsToWrite: DWORD;
begin
  GetConsoleScreenBufferInfo(FHandle, FInfo);
  CharsToWrite := FInfo.dwSize.X * FInfo.dwSize.Y;
  SetColor(Attr);
  FInfo.dwCursorPosition.X := 0;
  FInfo.dwCursorPosition.Y := 0;
  FillConsoleOutputCharacter(FHandle, ' ', CharsToWrite, FInfo.dwCursorPosition, CharsWritten);
  Assert(CharsToWrite = CharsWritten);
  FillConsoleOutputAttribute(FHandle, Attr, CharsToWrite, FInfo.dwCursorPosition, CharsWritten);
  Assert(CharsToWrite = CharsWritten);
  FInfo.dwCursorPosition.X := 0;
  FInfo.dwCursorPosition.Y := 0;
  SetConsoleCursorPosition(FHandle, FInfo.dwCursorPosition);
end;

procedure TConsole.Cls;
begin
  Cls(FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;

procedure TConsole.SetColor(const Attr: Word);
begin
  SetConsoleTextAttribute(FHandle, Attr);
end;

procedure TConsole.SetColor;
begin
  SetConsoleTextAttribute(FHandle, FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;

function Console: TConsole;
begin
  Result := TConsole.GetInstance;
end;

end.
