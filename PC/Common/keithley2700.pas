unit keithley2700;

interface

uses
  forms,classes, dialogs, windows, sysutils, synaser, inifiles;

type
  KeithleyModes   = (VoltageMode,CurrentMode,DualMode);
  KeithleySpeed   = (SlowSpeed,MediumSpeed,FastSpeed);

  TKeithley2700=class(TObject)
  private
    ser : TBlockSerial;
    FVoltage,FCurrent:double;
    FMode:KeithleyModes;
    FSpeed:KeithleySpeed;
    FRange:word;
    KData:TStringList;
    FConnected        : boolean;
    function  DeleteChars(Str: string): string;
    procedure SetMode(value:keithleymodes);
    procedure SetSpeed(value:keithleyspeed);
    procedure SetRange(value:word);
  public
    portlist:string;
    constructor Create;
    destructor Destroy;override;
    procedure Connect;
    procedure DisConnect;
    procedure Measure;
    procedure Value;
    property  Connected:boolean read FConnected;
    property  Voltage:double read FVoltage write FVoltage;
    property  Current:double read FCurrent write FCurrent;
    property  Mode:KeithleyModes read FMode write SetMode;
    property  Speed:KeithleySpeed read FSpeed write SetSpeed;
    property  Range:word read FRange write SetRange;
  end;


implementation

const
  TekTimeout = 1000;

constructor TKeithley2700.Create;
begin
  inherited Create;

  FConnected:=False;

  FVoltage:=0;
  FCurrent:=0;

  ser:=TBlockSerial.Create;
  ser.ConvertLineEnd:=True;
  portlist:=GetSerialPortNames;

  KData:=TStringList.Create;
  KData.StrictDelimiter:=True;
end;


procedure TKeithley2700.Connect;
var
  Serport       : word;
  Serspeed      : word;
  Ini           : TIniFile;
  RawData:string;
begin
  Serport       := 8;
  Serspeed      := 9600;


  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Serport       := Ini.ReadInteger( 'DMMComport', 'Portnumber', Serport );
    if  (NOT Ini.ValueExists( 'DMMComport', 'Portnumber')) then Ini.WriteInteger( 'DMMComport', 'Portnumber', Serport );
    Serspeed       := Ini.ReadInteger( 'DMMComport', 'Portspeed', Serspeed );
    if  (NOT Ini.ValueExists( 'DMMComport', 'Portspeed')) then Ini.WriteInteger( 'DMMComport', 'Portspeed', Serspeed );
  finally
    Ini.Free;
  end;

  ser.CloseSocket;
  ser.Connect('COM'+InttoStr(Serport));

  if ser.LastError<>0 then
  begin
    MessageDlg ('Sorry, Comport error on port '+InttoStr(Serport)+' !!'+
                  chr(13)+'Serial port error: '+ser.LastErrorDesc,
                  mtInformation, [mbOk],0);
    FConnected:=False;
  end else FConnected:=True;

  if Connected then
  begin
    ser.Config(Serspeed,8,'N',SB1,false,false);
    ser.Purge;
  end;

  ser.SendString('*SRE 0' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);
  //if RawData<>'=>' then Terminate;

  ser.SendString('REMS' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);
  //if RawData<>'=>' then Terminate;

  ser.SendString('HOLDCLR' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  ser.SendString('MMCLR' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  ser.SendString('RELCLR' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  ser.SendString('COMPCLR' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  ser.SendString('VDC' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  ser.SendString('AUTO' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  ser.SendString('RATE S' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);

  sleep(200);

  FMode:=VoltageMode;
  FSpeed:=SlowSpeed;
  FRange:=0;
end;

procedure TKeithley2700.SetMode(value:keithleymodes);
var
  RawData:string;
begin
  FMode:=value;

  if Connected then
  begin
    ser.Purge;

    //ser.SendString('TRIGGER 3' + Chr(13));
    //RawData:=ser.Recvstring(TekTimeout);

    if value=VoltageMode then
    begin
      ser.SendString('VDC' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
      //if RawData<>'=>' then Terminate;
    end;
    if value=CurrentMode then
    begin
      ser.SendString('ADC' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
      //if RawData<>'=>' then Terminate;
    end;
    if value=DualMode then
    begin
      ser.SendString('ADC' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
      //if RawData<>'=>' then Terminate;
      ser.SendString('VDC2' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
      //if RawData<>'=>' then Terminate;
    end;
  end;

end;

procedure TKeithley2700.SetSpeed(value:keithleyspeed);
var
  RawData:string;
begin
  FSpeed:=value;

  if Connected then
  begin
    ser.Purge;

    if value=SlowSpeed then
    begin
      ser.SendString('RATE S' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
    end;
    if value=MediumSpeed then
    begin
      ser.SendString('RATE M' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
    end;
    if value=FastSpeed then
    begin
      ser.SendString('RATE F' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
    end;
  end;

end;

procedure TKeithley2700.SetRange(value:word);
var
  RawData:string;
begin
  if value>7 then Exit;

  FRange:=value;

  if Connected then
  begin
    ser.Purge;

    if value=0
      then ser.SendString('AUTO' + Chr(13))
      else ser.SendString('RANGE ' + InttoStr(Value)+ Chr(13));
    RawData:=ser.Recvstring(TekTimeout);

  end;

end;

procedure TKeithley2700.Measure;
begin
  Voltage:=0;
  Current:=0;
  ser.SendString('MEAS?' + Chr(13));
  KData.CommaText:=ser.Recvstring(TekTimeout);
  if ser.LastError<>0
     then MessageDlg ('Sorry, serial port error: '+ser.LastErrorDesc, mtInformation, [mbOk],0);
  if KData.Count>0 then
  begin
    if FMode=VoltageMode then Voltage:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
    if FMode=CurrentMode then Current:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
    if FMode=DualMode then
    begin
      if KData.Count>1 then Voltage:=StrtoFloatDef(DeleteChars(KData.Strings[1]),0);
      Current:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
    end;
  // dummy receive !!
  end;
  KData.CommaText:=ser.Recvstring(TekTimeout);
end;

procedure TKeithley2700.Value;
begin
  Voltage:=0;
  Current:=0;
  ser.SendString('VAL?' + Chr(13));
  KData.CommaText:=ser.Recvstring(TekTimeout);
  if ser.LastError<>0
     then MessageDlg ('Sorry, serial port error: '+ser.LastErrorDesc, mtInformation, [mbOk],0);
  if FMode=VoltageMode then Voltage:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
  if FMode=CurrentMode then Current:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
  if FMode=DualMode then
  begin
    Voltage:=StrtoFloatDef(DeleteChars(KData.Strings[1]),0);
    Current:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
  end;
  // dummy receive !!
  KData.CommaText:=ser.Recvstring(TekTimeout);
end;


procedure TKeithley2700.DisConnect;
var
  RawData:string;
begin
  ser.SendString('LOCS' + Chr(13));
  RawData:=ser.Recvstring(TekTimeout);
  //ser.CloseSocket;
end;

function TKeithley2700.DeleteChars(Str: string): string;
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


destructor TKeithley2700.Destroy;
var
  RawData:string;
begin
  if Connected then
  begin
    if ser<>nil then
    begin
      {
      ser.SendString('*RST' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);
      }

      //ser.SendString('TRIGGER 1' + Chr(13));
      //RawData:=ser.Recvstring(TekTimeout);

      ser.SendString(':ABORT' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);

      ser.SendString('*CLS' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);

      ser.SendString('*SRE 0' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);

      ser.SendString('LOCS' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);

      ser.SendString('AUTO' + Chr(13));
      RawData:=ser.Recvstring(TekTimeout);

      //HOLDCLR
      //MMCLR
      //RELCLR
      //COMPCLR

      sleep(500);
      ser.CloseSocket;
      ser.Free;
    end;
  end;
  KData.Free;
  inherited Destroy;
end;

end.
