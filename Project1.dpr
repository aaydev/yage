program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils;

var
  filename1: string;
  language: string;
  encoding: string;
  Text1, Text2: TStrings;
  CharValue: Cardinal;
  i, j: Integer;
  Letter: Char;
  Line1, Line2: string;

begin
  Writeln;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY);
  Writeln('YAGE v1.2');
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);

  if ParamCount = 0 then
  begin
    Writeln('(c) Alexey Anisimov <softlight@ya.ru>, 2019');
    Writeln('(c) incadea RUS, 2019. All rights reserved.');
    Writeln;
    Writeln('Usage: SourceFile LanguageCode Encoding');
    Halt(0)
  end;

  filename1 := ParamStr(1);
  language := LowerCase(ParamStr(2));
  encoding := LowerCase(ParamStr(3));

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN or FOREGROUND_RED);
  if ParamCount <> 3 then
  begin
    Writeln('Invalid parameters! Source File, Language Code and Encoding are required parameters.');
    Halt(1)
  end;

  if (language <> 'az') and (language <> 'ge') then
  begin
    Writeln('Invalid language code! Allowed values: "ge", "az".');
    Halt(2)
  end;

  if (encoding <> 'utf8') and (encoding <> 'unicode') then
  begin
    Writeln('Invalid encoding! Allowed values: "utf8" ,"unicode".');
    Halt(3)
  end;

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);

  Text1 := TStringList.Create();
  Text2 := TStringList.Create();

  try
    if encoding = 'utf8' then
      Text1.LoadFromFile(filename1, TEncoding.UTF8)
    else
    if encoding = 'unicode' then
      Text1.LoadFromFile(filename1, TEncoding.Unicode);

    for i := 0 to Text1.Count - 1 do
    begin
      Line1 := Text1.Strings[i];
      Line2 := '';
      for j := 1 to Length(Line1) do
      begin
        Letter := Line1[j];
        CharValue := Cardinal(Letter);
        if CharValue >= 127 then
        begin

          if language = 'ge' then
          begin
            case CharValue of
              //215: CharValue :=  286;
              247: CharValue :=  287;
              //200: CharValue :=  304;
              251: CharValue :=  305;
              //216: CharValue :=  350;
              248: CharValue :=  351;
              //223: CharValue :=  399;
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
              //161: CharValue :=  1073;
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
              //224: CharValue :=  1088;
              //225: CharValue :=  1089;
              //226: CharValue :=  1090;
              //227: CharValue :=  1091;
              //228: CharValue :=  1092;
              //229: CharValue :=  1093;
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
          end
          else

          if language = 'az' then
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
          end;

          Letter := Char(CharValue);
        end;
        Line2 := Line2 + Letter;
      end;
      Text2.Add(Line2);
    end;

    if encoding = 'utf8' then
      Text2.SaveToFile(filename1, TEncoding.UTF8)
    else
    if encoding = 'unicode' then
      Text2.SaveToFile(filename1, TEncoding.Unicode);

    FreeAndNil(Text1);
    FreeAndNil(Text2);

    Writeln(Format('File %s converted.', [filename1]));
  except
    on E: Exception do
    begin
      FreeAndNil(Text1);
      FreeAndNil(Text2);
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED);
      Writeln(E.ClassName, ': ', E.Message);
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
      halt(4);
    end;
  end;

end.
