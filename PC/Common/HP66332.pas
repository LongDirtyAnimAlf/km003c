unit hp66332;

interface

uses
  forms,classes, dialogs, sysutils, synaser, inifiles;

type

  THP66332=class(TObject)
  private
    ser               : TBlockSerial;
    FConnected        : boolean;
    FManufacturer     : string;
    FModel            : string;
    FVoltage,FCurrent : double;
    FSetCurrent       : double;
    KData             : TStringList;
    FSerport          : integer;
    FSerportName      : string;
    FSerspeed         : word;
    FPorts            : string;
  public
    constructor Create;
    destructor Destroy;override;

    procedure Connect;
    procedure DisConnect;
    procedure SetVoltage(value:real);
    procedure SetCurrentFast(value:real);
    procedure SetCurrentSlow(value:real);
    procedure SetOutput(value:boolean);
    procedure Measure;

    property  Connected:boolean read FConnected;
    property  Manufacturer:string read FManufacturer;
    property  Model:string read FModel;

    property  Voltage:double read FVoltage;
    property  Current:double read FCurrent;
    property  SerialPort: integer write FSerport;
    property  SerialPortName: string write FSerportName;
    property  SerialSpeed: word write FSerspeed;

    property Ports: string read FPorts;
  end;


implementation

const
  HPTimeout           = 1000;
  MaxCurrent          = 5;

function DeleteChars(Str: string): string;
var
  i: Integer;
begin
  i:=1;
  while i<=Length(Str) do
    if (NOT CharInSet(Str[i],['+','-','.','0'..'9','E'])) then Delete(Str, i, 1)
    else Inc(i);
  Result:=StringReplace(Str, '.', FormatSettings.DecimalSeparator, []);
  //Result:=Str;
end;

function FloatToStrWithDecimalPoint(const Value: Extended; const Format: String = ''): String;
var
  myFormatSettings: TFormatSettings;
begin
  {$ifndef FPC}
  myFormatSettings:= TFormatSettings.Create;
  {$endif}
  myFormatSettings.DecimalSeparator := '.';
  Result := FormatFloat(Format, Value, myFormatSettings);
end;

function SendData(ASer:TBlockSerial;aDataString:string):boolean;
begin
  result:=false;
  if ASer.CanWrite(HPTimeout) then
  begin
    ASer.SendString(aDataString + LF);
    result:=(ASer.LastError=0);
    //RawData:=ser.Recvstring(HPTimeout);
    //if RawData<>'=>' then Terminate;
  end;
end;

constructor THP66332.Create;
var
  Ini           : TIniFile;
begin
  inherited Create;

  FSerport       := -1;
  FSerspeed      := 9600;
  SerialPortName := '';

  FConnected:=False;

  FManufacturer:='';
  FModel:='';

  FVoltage:=0;
  FCurrent:=0;

  FSetCurrent:=0;

  ser:=TBlockSerial.Create;
  ser.ConvertLineEnd:=True;

  KData:=TStringList.Create;
  KData.StrictDelimiter:=True;
  {
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    FSerport       := Ini.ReadInteger( 'HPComport', 'Portnumber', FSerport );
    if  (NOT Ini.ValueExists( 'HPComport', 'Portnumber')) then Ini.WriteInteger( 'HPComport', 'Portnumber', FSerport );
    FSerspeed       := Ini.ReadInteger( 'HPComport', 'Portspeed', FSerspeed );
    if  (NOT Ini.ValueExists( 'HPComport', 'Portspeed')) then Ini.WriteInteger( 'HPComport', 'Portspeed', FSerspeed );
  finally
    Ini.Free;
  end;
  }
end;


procedure THP66332.Connect;
var
  aID:string;
begin
  FConnected:=False;

  ser.CloseSocket;
  if (FSerport<>-1) then
  begin
    {$ifdef MSWINDOWS}
    ser.Connect('COM'+InttoStr(FSerport));
    {$else}
    ser.Connect('/dev/ttyUSB'+InttoStr(FSerport));
    {$endif}
  end;
  if (Length(FSerportName)>0) then
  begin
    ser.Connect(FSerportName);
  end;

  Sleep(500);

  if ser.LastError<>0 then
  begin
    MessageDlg ('Sorry, HP comport error on port '+InttoStr(FSerport)+' !!'+
                  chr(13)+'Serial port error: '+ser.LastErrorDesc,
                  mtInformation, [mbOk],0);
  end
  else
  begin
    FConnected:=True;
  end;

  if Connected then
  begin
    ser.Config(FSerspeed,8,'N',SB1,false,false);
    ser.Purge;
  end;
  FConnected:=(ser.LastError=0);

  if Connected then
  begin
    SendData(ser,'*RST');
    SendData(ser,'*SRE 0');

    FManufacturer:='';
    FModel:='';
    SendData(ser,'*IDN?');
    KData.CommaText:=ser.Recvstring(HPTimeout);
    if (KData.Count>0) then FManufacturer:=KData.Strings[0]; // Hewlett-Packard
    if (KData.Count>1) then FModel:=KData.Strings[1]; // 6632B
    FConnected:=(Pos('663',FModel)=1);

    if Connected then
    begin
      sleep(200);
      SendData(ser,'REMS');
      SendData(ser,'SENS:CURR:DET DC');
      SendData(ser,'CURR 0');
      SendData(ser,'VOLT 0');
      sleep(200);
    end;
  end;

  if (NOT Connected) then ser.CloseSocket;
end;

procedure THP66332.DisConnect;
begin
  if Connected then
  begin
    SendData(ser,'OUTPUT OFF');
    SendData(ser,'ABORT');
    SendData(ser,'LOCS');
    SendData(ser,'*CLS');
    SendData(ser,'*SRE 0');
    sleep(500);
  end;
  ser.CloseSocket;
  FConnected:=False;
end;

procedure THP66332.SetVoltage(value:real);
begin
  if Connected then
  begin
    //ser.Purge;
    SendData(ser,'VOLT ' + FloatToStrWithDecimalPoint(value));
  end;
end;

procedure THP66332.SetCurrentFast(value:real);
var
  aStep:double;
begin
  if Connected then
  begin
    FSetCurrent:=value;
    SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
  end;
end;

procedure THP66332.SetCurrentSlow(value:real);
var
  aStep:double;
begin
  if Connected then
  begin
    if value=0 then
    begin
      FSetCurrent:=value;
      SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
    end
    else
    begin
      if (value>FSetCurrent) then
      begin
        //slowly rampup current : takes +/- 1 second
        aStep:=(value-FSetCurrent)/50;
        while True do
        begin
          FSetCurrent:=FSetCurrent+aStep;
          if (FSetCurrent>value) OR (FSetCurrent>MaxCurrent) then
          begin
            FSetCurrent:=value;
            SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
            break;
          end
          else
          begin
            SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
            sleep(20);
          end;
        end;
      end
      else
      begin
        FSetCurrent:=value;
        SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
      end;
    end;
  end;
end;


procedure THP66332.SetOutput(value:boolean);
begin
  if Connected then
  begin
    if value then
    begin
      SendData(ser,'OUTPUT ON');
    end
    else
    begin
      SetCurrentFast(0.001);
      SendData(ser,'OUTPUT ON');
      //SendData(ser,'OUTPUT OFF');
    end;
  end;
end;


procedure THP66332.Measure;
begin
  FVoltage:=0;
  FCurrent:=0;
  if Connected then
  begin
    SendData(ser,'MEAS:VOLT?');
    KData.CommaText:=ser.Recvstring(HPTimeout);
    if ser.LastError<>0
       then MessageDlg ('Sorry, serial port error: '+ser.LastErrorDesc, mtInformation, [mbOk],0);
    if KData.Count>0 then
    begin
      FVoltage:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
    end;

    SendData(ser,'MEAS:CURR?');
    KData.CommaText:=ser.Recvstring(HPTimeout);
    if ser.LastError<>0
       then MessageDlg ('Sorry, serial port error: '+ser.LastErrorDesc, mtInformation, [mbOk],0);
    if KData.Count>0 then
    begin
      FCurrent:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
    end;
  end;
end;


destructor THP66332.Destroy;
begin
  if (ser<>nil) then
  begin
    DisConnect;
    ser.Free;
  end;
  KData.Free;
  inherited Destroy;
end;

end.
