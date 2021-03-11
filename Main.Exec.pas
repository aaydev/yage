{*******************************************************}
{ YAGE: Yet Another Global Encoder                      }
{ Unit: Main.Exec                                       }
{ Copyright(c) 2021 Alexey Anisimov                     }
{ Contact email: softlight@ya.ru                        }
{*******************************************************}

unit Main.Exec;

{$MODE delphiunicode}

interface

uses

  Classes, Variants, ActiveX, ComObj, LazUTF8,
  Helper.Singleton,
  Helper.Console,
  Main.Help;

type
  TEncodingType = (ETnone,EToem,ETutf8,ETunicode);

  TApp = class(TSingleton)
  const
    FOutputFileExt: string = '.txt';
  protected
    constructor Create; override;
  private
    FParamList: TStringList;
    FExitCode: Integer;
    FStartTime: TDateTime;
    FStopTime: TDateTime;
    FElapsedTime: Int64;

    FInputFile: string;
    FOutputFile: string;
    FConversionFile: string; // format: OEM,UNICODE

    FLog: Boolean;
    FExcel: Boolean;

    FEncodingFrom: TEncodingType;
    FEncodingTo: TEncodingType;

    FEncoding: string;
    FLanguage: string;

    FConversionTable: array [0..1, 0..199] of Cardinal;
    FConversionTableSize: Integer;

    procedure GetParamList;
    function SearchParam(const Key: string; var Value: string): Integer;

    procedure ReadConversionTable;
    procedure ConvertText(InputText: TStringList; OutputText: TStringList);
    function ConvertLine(InputText: string): string;

  public
    destructor Destroy; override;
    property ExitCode: Integer read FExitCode;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property StopTime: TDateTime read FStopTime write FStopTime;
    property ElapsedTime: Int64 read FElapsedTime write FElapsedTime;

    property InputFile: string read FInputFile;
    property OutputFile: string read FOutputFile;

    property Log: Boolean read FLog;
    property Excel: Boolean read FExcel;

    property FileLanguage: string read FLanguage;
    property FileEncoding: string read FEncoding;

    procedure Init;
    procedure WriteParams;
    function CheckParams: Boolean;
    procedure Execute;
    procedure Finish;
  end;

  function App: TApp;

resourcestring

  // application info
  rsAppName = 'YAGE';
  rsAppDescription = 'Yet Another Global Encoder';
  rsAppVer = '1.6';
  rsCopyright = 'Copyright (c) 2020 Alexey Anisimov / email: softlight@ya.ru';
  rs_UsageHelp = 'Usage: %s.exe SourceFile ConversionTableFile [keys]';

  // error messages
  rs_ErrFileNotFound = 'File "%s" not found.';
  rs_ErrParamError = 'Value for parameter "%s" = "%s" is invalid.';
  rs_ErrInvalidParam = 'Parameter "%s" is invalid.';

implementation

uses
  DateUtils, SysUtils,
  Common.ShellFileSupport;

function App: TApp;
begin
  Result := TApp.GetInstance;
end;

{ TApp }

constructor TApp.Create;
begin
  inherited Create;
  // Init settings here
  FExitCode := -1;
  FInputFile := '';
  FOutputFile := '';
  FParamList := TStringList.Create;
end;

destructor TApp.Destroy;
begin
  // Save settings here
  FParamList.Free;
  inherited Destroy;
end;

procedure TApp.GetParamList;
var
  i, n: Integer;
begin
  FParamList.Clear;
  n := ParamCount;
  for i := 0 to n do
    FParamList.Add(ParamStr(i));
end;

function TApp.SearchParam(const Key: string; var Value: string): Integer;
var
  i: Integer;
  s, skey: string;
begin
  Result := 0;
  Value := '';
  if Key = '' then
    Exit;
  if FParamList.Count = 0 then
    Exit;

  // Example: "/key:filename.ext"

  skey := LowerCase('/' + Key);
  for i := 1 to FParamList.Count do
  begin
    s := FParamList.Strings[i - 1];
    if Pos(LowerCase(skey), LowerCase(s)) = 1 then
    begin
      if Length(s) > Length(skey) then
      begin
        Value := Copy(s, Length(skey) + 1, Length(s) - Length(skey));
        if Value[1] = ':' then
          Value := Copy(Value, 2)
        else
          Value := '';
        Result := i;
      end
      else
      begin
        Value := '';
        Result := i;
      end;
    end;
    if Result <> 0 then
      Exit;
  end;
end;

procedure TApp.WriteParams;
begin
  if not FLog then
    Exit;
  WriteAppLog(info, Format('Input file: "%s"', [FInputFile]));
  WriteAppLog(info, Format('Conversion: "%s"', [FConversionFile]));
  WriteAppLog(info, Format('Output file: "%s"', [FOutputFile]));
  if FExcel then
    WriteAppLog(info, 'MS Excel mode in on!');
end;

function TApp.CheckParams: Boolean;
var
  ParamName: string;
  ParamValue: string;
begin
  Result := False;
  if FParamList.Count = 1 then
    Exit;
  // check source file
  FInputFile := PathSearchAndQualifyA(FParamList[1]);
  if not FileExists(FInputFile) then
    raise EFileNotFoundException.Create(Format(rs_ErrFileNotFound, [FInputFile]));

  // check conversion file
  FConversionFile := PathSearchAndQualifyA(FParamList[2]);
  if not FileExists(FConversionFile) then
    raise EFileNotFoundException.Create(Format(rs_ErrFileNotFound, [FConversionFile]));

  // set default values for params
  FOutputFile := FInputFile;

  FEncodingFrom := EToem;
  FEncodingTo := ETutf8;
  FLog := False;
  FExcel := False;

  ParamName := 'log';
  if SearchParam(ParamName, ParamValue) > 0 then
    FLog := True;

  ParamName := 'from';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    FEncodingFrom := ETnone;
    if LowerCase(ParamValue) = 'oem' then
      FEncodingFrom := EToem;
    if LowerCase(ParamValue) = 'utf8' then
      FEncodingFrom := ETutf8;
    if LowerCase(ParamValue) = 'unicode' then
      FEncodingFrom := ETunicode;
    if FEncodingFrom = ETnone then
      raise Exception.Create(Format(rs_ErrParamError, [ParamName, ParamValue]));
    FLanguage := ParamValue
  end;

  ParamName := 'to';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    FEncodingTo := ETnone;
    if LowerCase(ParamValue) = 'oem' then
      FEncodingTo := EToem;
    if LowerCase(ParamValue) = 'utf8' then
      FEncodingTo := ETutf8;
    if LowerCase(ParamValue) = 'unicode' then
      FEncodingTo := ETunicode;
    if FEncodingTo = ETnone then
      raise Exception.Create(Format(rs_ErrParamError, [ParamName, ParamValue]));
    FLanguage := ParamValue
  end;

  ParamName := 'file';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    FOutputFile := PathSearchAndQualifyA(ExtractFileName(ParamValue));
  end;

  ParamName := 'excel';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    FExcel := True;
  end;

  if FLog then
    WriteParams;

  Result := True;
end;

procedure TApp.Execute;
var
  InputText:TStringList;
  OutputText: TStringList;

  i: Integer;
  j: Integer;
  x: Integer;
  y: Integer;

  FS: TFileStream;

  Letter: Char;
  CharValue: Byte;
  Line: string;

  XLApp: Variant;
  WorkBook: Variant;
  WorkSheet: Variant;
begin
  InputText := TStringList.Create();
  OutputText := TStringList.Create();

  ReadConversionTable;

  if FExcel then
  begin
    OLEInitialize(nil);
    XLApp := CreateOleObject('Excel.Application');
    XLApp.DisplayAlerts := False;
    XLApp.Application.Workbooks.Open(FileName:=FInputFile, UpdateLinks:=0, ReadOnly:=False);
    XLApp.Visible := False;

    WorkBook := XLApp.WorkBooks.Item[1];
    WorkSheet := WorkBook.WorkSheets.Item[1];

    x := WorkSheet.Cells.SpecialCells(11, EmptyParam).Column;
    y := WorkSheet.Cells.SpecialCells(11, EmptyParam).Row;

    for j := 1 to y do
      for i := 1 to x do
      begin
        Line := WorkSheet.Cells[j, i].Text;
        Line := ConvertLine(Line);
        WorkSheet.Cells[j, i].Value := Line;
      end;

    if FInputFile = FOutputFile then
      WorkBook.Close(SaveChanges:=True)
    else
      WorkBook.SaveAs(FOutputFile);
    XLApp.Application.Quit;
    XLApp := Unassigned;
    OLEUnInitialize();
  end
  else
  begin
    try
      if FEncodingFrom = ETutf8 then
        InputText.LoadFromFile(FInputFile, TEncoding.UTF8);
      if FEncodingFrom = ETunicode then
        InputText.LoadFromFile(FInputFile, TEncoding.Unicode);
      if FEncodingFrom = EToem then
        InputText.LoadFromFile(FInputFile, TEncoding.ANSI);

      ReadConversionTable;
      ConvertText(InputText, OutputText);

      if FEncodingTo = ETutf8 then
        OutputText.SaveToFile(FOutputFile, TEncoding.UTF8);
      if FEncodingTo = ETunicode then
        OutputText.SaveToFile(FOutputFile, TEncoding.Unicode);

      if FEncodingTo = EToem then
      begin
        FS := TFileStream.Create(FOutputFile, fmCreate);
        for i := 0 to OutputText.Count - 1 do
        begin
          Line := OutputText.Strings[i];
          for j := 1 to Length(Line) do
          begin
            Letter := Line[j];
            CharValue := Cardinal(Letter);
            FS.Write(CharValue, 1);
          end;
          CharValue := $0d;
          FS.Write(CharValue, 1);
          CharValue := $0a;
          FS.Write(CharValue, 1);
        end;
        FreeAndNil(FS);
      end;

    except
      on E: Exception do
      begin
        FreeAndNil(OutputText);
        FreeAndNil(InputText);
        raise;
      end;
    end;
  end;
          
  FreeAndNil(OutputText);
  FreeAndNil(InputText);

  FExitCode := 0;
  WriteAppLog(ok, 'Done!');
end;

procedure TApp.Init;
begin
  GetParamList;
  Console.WriteLn();
  Console.WriteLn();
  if FParamList.Count = 1 then
  begin
    WriteAppTitle(rsAppName, rsAppDescription, rsAppVer, rsCopyright);
    WriteAppHelp(rsAppName, rs_UsageHelp);
  end
  else
    WriteAppTitleShort(rsAppName, rsAppVer);
end;

procedure TApp.Finish;
begin
  Console.SetColor();
end;

procedure TApp.ReadConversionTable;
var
  i: Integer;
  n: Integer;
  F: TextFile;
  line: string;
  val1: string;
  val2: string;
begin
  for i := 0 to 199 do
  begin
    FConversionTable[0, i] := 0;
    FConversionTable[1, i] := 0;
  end;

  // read conversion file
  Assign(F, FConversionFile);
  Reset(F);
  i := 0;
  while not Eof(F) do begin
    ReadLn(F, line);
    n := Pos(',', line);
    if n > 0 then
    begin
      val1 := Copy(line, 1, n - 1);
      val2 := Copy(line, n + 1);
      FConversionTable[0, i] := StrToInt(val1);
      FConversionTable[1, i] := StrToInt(val2);
    end;
    Inc(i);
  end;

  Close(F);
  FConversionTableSize := i - 1;
end;

procedure TApp.ConvertText(InputText: TStringList; OutputText: TStringList);
var
  i: Integer;
  j: Integer;
  k: Integer;
  Line1: string;
  Line2: string;
  Letter: Char;
  CharValue: Cardinal;
  Found: Boolean;
begin
  OutputText.Clear;
  for i := 0 to InputText.Count - 1 do
  begin
    Line1 := InputText.Strings[i];
    Line2 := '';

    for j := 1 to Length(Line1) do
    begin
      Letter := Line1[j];
      CharValue := Cardinal(Letter);

      if CharValue >= 127 then
      begin
        Found := False;
        k := 0;
        while (Found = False) and (k < FConversionTableSize) do
        begin
          if FEncodingFrom = EToem then begin
            if CharValue = FConversionTable[0, k] then
            begin
              CharValue := FConversionTable[1, k];
              Found := True;
            end;
          end
          else
          begin
            if CharValue = FConversionTable[1, k] then
            begin
              CharValue := FConversionTable[0, k];
              Found := True;
            end;
          end;

          Inc(k);
        end;

        Letter := Char(CharValue);
      end;
      Line2 := Line2 + Letter;
    end;
    OutputText.Add(Line2);
  end;
end;

function TApp.ConvertLine(InputText: string): string;
var
  j: Integer;
  k: Integer;
  Letter: Char;
  CharValue: Cardinal;
  Found: Boolean;
begin
  Result := '';
  for j := 1 to Length(InputText) do
  begin
    Letter := InputText[j];
    CharValue := Cardinal(Letter);

    if CharValue >= 127 then
    begin
      Found := False;
      k := 0;
      while (Found = False) and (k < FConversionTableSize) do
      begin
        if FEncodingFrom = EToem then begin
          if CharValue = FConversionTable[0, k] then
          begin
            CharValue := FConversionTable[1, k];
            Found := True;
          end;
        end
        else
        begin
          if CharValue = FConversionTable[1, k] then
          begin
            CharValue := FConversionTable[0, k];
            Found := True;
          end;
        end;

        Inc(k);
      end;

      Letter := Char(CharValue);
    end;
    Result := Result + Letter;
  end;
end;

end.
