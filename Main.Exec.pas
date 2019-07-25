{*******************************************************}
{ YAGE: Yet Another Global Encoder                      }
{ Unit: Main.Exec                                       }
{ Copyright(c) 2019 Alexey Anisimov                     }
{ Contact email: softlight@ya.ru                        }
{*******************************************************}

unit Main.Exec;

interface

uses
  Winapi.Windows,
  System.Classes,
  Helper.Singleton,
  Helper.Console,
  Main.Help;

type

  TApp = class(TSingleton)
  const
    FOutputFileExt: string = '.xml';
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

    FLog: Boolean;

    FEncoding: string;
    FLanguage: string;

    procedure GetParamList;
    function SearchParam(const Key: string; var Value: string): Integer;

    procedure ConvertTextGE(InputText: TStringList; OutputText: TStringList);
    procedure ConvertTextAZ(InputText: TStringList; OutputText: TStringList);

  public
    destructor Destroy; override;
    property ExitCode: Integer read FExitCode;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property StopTime: TDateTime read FStopTime write FStopTime;
    property ElapsedTime: Int64 read FElapsedTime write FElapsedTime;

    property InputFile: string read FInputFile;
    property OutputFile: string read FOutputFile;

    property Log: Boolean read FLog;

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
  rsAppVer = '1.3';
  rsCopyright = 'Copyright (c) 2019 Alexey Anisimov / Email: softlight@ya.ru';
  rs_UsageHelp = 'Usage: %s.exe SourceFile [keys]';

  // error messages
  rs_ErrFileNotFound = 'File "%s" not found.';
  rs_ErrParamError = 'Value for parameter "%s" = "%s" is invalid.';
  rs_ErrInvalidParam = 'Parameter "%s" is invalid.';

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.IOUtils,
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
  FreeAndNil(FParamList);
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
  WriteAppLog(info, Format('Output file: "%s"', [FOutputFile]));
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
  FInputFile := PathSearchAndQualify(FParamList[1]);
  if not FileExists(FInputFile) then
    raise EFileNotFoundException.Create(Format(rs_ErrFileNotFound, [FInputFile]));

  // set default values for params
  FOutputFile := FInputFile;
  FEncoding := 'urf8';
  FLanguage := 'ge';
  FLog := False;

  ParamName := 'log';
  if SearchParam(ParamName, ParamValue) > 0 then
    FLog := True;

  ParamName := 'lang';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    if (ParamValue <> 'ge') and (ParamValue <> 'az') then
      raise Exception.Create(Format(rs_ErrParamError, [ParamName, ParamValue]));
    FLanguage := ParamValue
  end;

  ParamName := 'enc';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    if (ParamValue <> 'utf8') and (ParamValue <> 'unicode') then
      raise Exception.Create(Format(rs_ErrParamError, [ParamName, ParamValue]));
    FEncoding := ParamValue
  end;

  ParamName := 'file';
  if SearchParam(ParamName, ParamValue) > 0 then
  begin
    FOutputFile := PathSearchAndQualify(System.IOUtils.TPath.GetFileName(ParamValue));
  end;

  if FLog then
    WriteParams;

  Result := True;
end;

procedure TApp.Execute;
var
  InputText:TStringList;
  OutputText: TStringList;
begin
  InputText := TStringList.Create();
  OutputText := TStringList.Create();

  try
    if FEncoding = 'utf8' then
      InputText.LoadFromFile(FInputFile, TEncoding.UTF8);
    if FEncoding = 'unicode' then
      InputText.LoadFromFile(FInputFile, TEncoding.Unicode);

    if FLanguage = 'ge' then
      ConvertTextGE(InputText, OutputText);
    if FLanguage = 'az' then
      ConvertTextAZ(InputText, OutputText);

    if FEncoding = 'utf8' then
      OutputText.SaveToFile(FOutputFile, TEncoding.UTF8);
    if FEncoding = 'unicode' then
      OutputText.SaveToFile(FOutputFile, TEncoding.Unicode);

  except
    on E: Exception do
    begin
      FreeAndNil(OutputText);
      FreeAndNil(InputText);
      raise;
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
  Console.Cls();
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

procedure TApp.ConvertTextGE(InputText: TStringList; OutputText: TStringList);
var
  i: Integer;
  j: Integer;
  Line1: string;
  Line2: string;
  Letter: Char;
  CharValue: Cardinal;
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
        case CharValue of
          247: CharValue :=  287;
          251: CharValue :=  305;
          248: CharValue :=  351;
          255: CharValue :=  601;
          240: CharValue :=  1025;
          128: CharValue :=  1040;
          129: CharValue :=  1041;
          130: CharValue :=  1042;
          131: CharValue :=  1043;
          132: CharValue :=  1044;
          133: CharValue :=  1045;
          134: CharValue :=  1046;
          135: CharValue :=  1047;
          136: CharValue :=  1048;
          137: CharValue :=  1049;
          138: CharValue :=  1050;
          139: CharValue :=  1051;
          140: CharValue :=  1052;
          141: CharValue :=  1053;
          142: CharValue :=  1054;
          143: CharValue :=  1055;
          144: CharValue :=  1056;
          145: CharValue :=  1057;
          146: CharValue :=  1058;
          147: CharValue :=  1059;
          148: CharValue :=  1060;
          149: CharValue :=  1061;
          150: CharValue :=  1062;
          151: CharValue :=  1063;
          152: CharValue :=  1064;
          153: CharValue :=  1065;
          154: CharValue :=  1066;
          155: CharValue :=  1067;
          156: CharValue :=  1068;
          157: CharValue :=  1069;
          158: CharValue :=  1070;
          159: CharValue :=  1071;
          160: CharValue :=  1072;
          162: CharValue :=  1074;
          163: CharValue :=  1075;
          164: CharValue :=  1076;
          165: CharValue :=  1077;
          166: CharValue :=  1078;
          167: CharValue :=  1079;
          168: CharValue :=  1080;
          169: CharValue :=  1081;
          170: CharValue :=  1082;
          171: CharValue :=  1083;
          172: CharValue :=  1084;
          173: CharValue :=  1085;
          174: CharValue :=  1086;
          175: CharValue :=  1087;
          230: CharValue :=  1094;
          231: CharValue :=  1095;
          232: CharValue :=  1096;
          233: CharValue :=  1097;
          234: CharValue :=  1098;
          235: CharValue :=  1099;
          236: CharValue :=  1100;
          237: CharValue :=  1101;
          238: CharValue :=  1102;
          239: CharValue :=  1103;
          241: CharValue :=  1105;
          192: CharValue :=  4304;
          193: CharValue :=  4305;
          194: CharValue :=  4306;
          195: CharValue :=  4307;
          196: CharValue :=  4308;
          197: CharValue :=  4309;
          198: CharValue :=  4310;
          200: CharValue :=  4311;
          201: CharValue :=  4312;
          202: CharValue :=  4313;
          203: CharValue :=  4314;
          204: CharValue :=  4315;
          205: CharValue :=  4316;
          207: CharValue :=  4317;
          208: CharValue :=  4318;
          209: CharValue :=  4319;
          210: CharValue :=  4320;
          211: CharValue :=  4321;
          212: CharValue :=  4322;
          214: CharValue :=  4323;
          215: CharValue :=  4324;
          216: CharValue :=  4325;
          217: CharValue :=  4326;
          218: CharValue :=  4327;
          219: CharValue :=  4328;
          220: CharValue :=  4329;
          221: CharValue :=  4330;
          222: CharValue :=  4331;
          223: CharValue :=  4332;
          224: CharValue :=  4333;
          161: CharValue :=  4334;
          225: CharValue :=  4334;
          227: CharValue :=  4335;
          228: CharValue :=  4336;
          199: CharValue :=  4337;
          206: CharValue :=  4338;
          213: CharValue :=  4339;
          226: CharValue :=  4340;
          229: CharValue :=  4341;
        end;
        Letter := Char(CharValue);
      end;
      Line2 := Line2 + Letter;
    end;
    OutputText.Add(Line2);
  end;
end;

procedure TApp.ConvertTextAZ(InputText: TStringList; OutputText: TStringList);
var
  i: Integer;
  j: Integer;
  Line1: string;
  Line2: string;
  Letter: Char;
  CharValue: Cardinal;
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
        case CharValue of
          215: CharValue := 286;
          247: CharValue := 287;
          200: CharValue := 304;
          251: CharValue := 305;
          216: CharValue := 350;
          248: CharValue := 351;
          223: CharValue := 399;
          255: CharValue := 601;
          240: CharValue := 1025;
          128: CharValue := 1040;
          129: CharValue := 1041;
          130: CharValue := 1042;
          131: CharValue := 1043;
          132: CharValue := 1044;
          133: CharValue := 1045;
          134: CharValue := 1046;
          135: CharValue := 1047;
          136: CharValue := 1048;
          137: CharValue := 1049;
          138: CharValue := 1050;
          139: CharValue := 1051;
          140: CharValue := 1052;
          141: CharValue := 1053;
          142: CharValue := 1054;
          143: CharValue := 1055;
          144: CharValue := 1056;
          145: CharValue := 1057;
          146: CharValue := 1058;
          147: CharValue := 1059;
          148: CharValue := 1060;
          149: CharValue := 1061;
          150: CharValue := 1062;
          151: CharValue := 1063;
          152: CharValue := 1064;
          153: CharValue := 1065;
          154: CharValue := 1066;
          155: CharValue := 1067;
          156: CharValue := 1068;
          157: CharValue := 1069;
          158: CharValue := 1070;
          159: CharValue := 1071;
          160: CharValue := 1072;
          161: CharValue := 1073;
          162: CharValue := 1074;
          163: CharValue := 1075;
          164: CharValue := 1076;
          165: CharValue := 1077;
          166: CharValue := 1078;
          167: CharValue := 1079;
          168: CharValue := 1080;
          169: CharValue := 1081;
          170: CharValue := 1082;
          171: CharValue := 1083;
          172: CharValue := 1084;
          173: CharValue := 1085;
          174: CharValue := 1086;
          175: CharValue := 1087;
          224: CharValue := 1088;
          225: CharValue := 1089;
          226: CharValue := 1090;
          227: CharValue := 1091;
          228: CharValue := 1092;
          229: CharValue := 1093;
          230: CharValue := 1094;
          231: CharValue := 1095;
          232: CharValue := 1096;
          233: CharValue := 1097;
          234: CharValue := 1098;
          235: CharValue := 1099;
          236: CharValue := 1100;
          237: CharValue := 1101;
          238: CharValue := 1102;
          239: CharValue := 1103;
          241: CharValue := 1105;
        end;
        Letter := Char(CharValue);
      end;
      Line2 := Line2 + Letter;
    end;
    OutputText.Add(Line2);
  end;
end;


end.
