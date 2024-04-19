unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries,
  lazserial,
  fptimernew,
  dsLeds, libusb, libusboop;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnConnect: TButton;
    Button1: TButton;
    Button2: TButton;
    btnReset: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    cmboSerialPorts: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    grpVAData: TGroupBox;
    Memo1: TMemo;
    selectSampleRate: TRadioGroup;
    procedure btnConnectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grpVADataResize(Sender: TObject);
    procedure CheckTimerTimer(Sender: TObject);
    procedure DataTimerTimer(Sender: TObject);
  private
    PDOVoltageDisplay   : TdsSevenSegmentMultiDisplay;
    PDOCurrentDisplay   : TdsSevenSegmentMultiDisplay;
    RealVoltageDisplay  : TdsSevenSegmentMultiDisplay;
    RealCurrentDisplay  : TdsSevenSegmentMultiDisplay;

    ser                 : TLazSerial;
    KM003CComport       : string;

    PDTimer             : TFPTimer;
    DataTimer           : TFPTimer;

    Context             : TLibUsbContext;
    Device              : TLibUsbDevice;
    DeviceInterface     : TLibUsbInterface;
    OutEndPoint         : TLibUsbBulkOutEndpoint;
    InEndPoint          : TLibUsbBulkInEndpoint;

    procedure Connect;
    procedure DisConnect;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  usbcpd,
  km003c,
  Tools;

Const
  DevVID        = $5FC9;
  DevPID        = $0063;
  EP_IN         =  1 or LIBUSB_ENDPOINT_IN;
  EP_OUT        =  1 or LIBUSB_ENDPOINT_OUT;

function ChangeBrightness(lIn: tColor; factor:double): TColor;
var
  lR,lG,lB: byte;
begin
  lR := Red(lIn);
  lG := Green(lIn);
  lB := Blue(lIn);
  result := RGBToColor(Round(lR*factor),Round(lG*factor),Round(lB*factor));
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  CLIst:TStringList;
  CListDeatails:TStringList;
  i:integer;
  s:string;
begin
  PDOVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpVAData);
  with PDOVoltageDisplay do
  begin
    Parent:=grpVAData;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
  PDOCurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(grpVAData);
  with PDOCurrentDisplay do
  begin
    Parent:=grpVAData;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
    SignDigit:=True;
    DisplayCount:=5;
  end;

  RealVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpVAData);
  with RealVoltageDisplay do
  begin
    Parent:=grpVAData;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
  RealCurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(grpVAData);
  with RealCurrentDisplay do
  begin
    Parent:=grpVAData;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
    SignDigit:=True;
    DisplayCount:=5;
  end;

  KM003CComport:='';

  if (Length(KM003CComport)=0) then
  begin
    CLIst:=TStringList.Create;
    try
      EnumerateCOMPorts(CLIst);
      CListDeatails:=TStringList.Create;
      try
        for i:=0 to Pred(CLIst.Count) do
        begin
          CListDeatails.Delimiter:=DefaultFormatSettings.ListSeparator;
          CListDeatails.StrictDelimiter:=True;
          CListDeatails.DelimitedText:=CLIst[i];
          if (CListDeatails.Count>=4) then
          begin
            s:=CListDeatails[3];
            {$ifdef MSWINDOWS}
            s:=StringReplace(s,'COM','',[rfReplaceAll]);
            {$else}
            s:=StringReplace(s,'ttyUSB','',[rfReplaceAll]);
            {$endif MSWINDOWS}
            cmboSerialPorts.Items.Append(s);

            if (CListDeatails.Count>=5) then
            begin
              s:=CListDeatails[4];
              if (Pos('POWER-Z',s)=1) then
              begin
                KM003CComport:=CListDeatails[3];
                //break;
              end;
            end;
          end;
        end;
      finally
        CListDeatails.Free
      end;
    finally
      CLIst.Free;
    end;
  end;

  ser:=TLazSerial.Create(Self);
  ser.Async:=false;

  PDTimer:=TFPTimer.Create(Self);
  PDTimer.Enabled:=false;
  PDTimer.UseTimerThread:=false;
  PDTimer.Interval:=50;
  PDTimer.OnTimer:=@CheckTimerTimer;

  DataTimer:=TFPTimer.Create(Self);
  DataTimer.Enabled:=false;
  DataTimer.UseTimerThread:=false;
  DataTimer.Interval:=500;
  DataTimer.OnTimer:=@DataTimerTimer;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DisConnect;
end;

procedure TForm1.grpVADataResize(Sender: TObject);
begin
  PDOVoltageDisplay.Top:=2;
  PDOVoltageDisplay.Left:=5;

  PDOVoltageDisplay.Width:=(TControl(Sender).Width DIV 2)-12;
  PDOVoltageDisplay.Height:=(TControl(Sender).Height DIV 2)-12;

  PDOCurrentDisplay.Width:=PDOVoltageDisplay.Width;
  PDOCurrentDisplay.Height:=PDOVoltageDisplay.Height;
  PDOCurrentDisplay.Left:=PDOVoltageDisplay.Left;
  PDOCurrentDisplay.Top:=PDOVoltageDisplay.Top+PDOVoltageDisplay.Height;

  RealVoltageDisplay.Width:=PDOVoltageDisplay.Width;
  RealVoltageDisplay.Height:=PDOVoltageDisplay.Height;
  RealVoltageDisplay.Left:=PDOVoltageDisplay.Width+PDOVoltageDisplay.Left+12;
  RealVoltageDisplay.Top:=PDOVoltageDisplay.Top;

  RealCurrentDisplay.Width:=RealVoltageDisplay.Width;
  RealCurrentDisplay.Height:=RealVoltageDisplay.Height;
  RealCurrentDisplay.Left:=RealVoltageDisplay.Left;
  RealCurrentDisplay.Top:=PDOCurrentDisplay.Top;
end;

procedure TForm1.DataTimerTimer(Sender: TObject);
var
  sensordata            : TKM003CSensorData;
  header                : TKM003CMsgHeader;
  result_code           : LongInt;
  newdata               : packed array[0..4095] of byte;
  avalue                : double;
  signed                : pinteger;
  normal                : integer;
begin
  // Send command to get ADC data
  header.Raw:=0;
  header.Ctrl.typ:=Ord(TKM003CHeaderCommand.CMD_GET_DATA);
  header.Ctrl.att:=TKM003C_ATT_ADC;
  result_code := OutEndPoint.Send(header.Bytes , 4 ,10);

  // Receive ADC data
  FillChar(newdata,4096,0);
  result_code := InEndPoint.Recv(newdata ,4096 , 10);
  if (result_code>sizeof(TKM003CSensorData)) then result_code:=sizeof(TKM003CSensorData);
  move(newdata,sensordata,result_code);

  // Did we receive a packet from our command ?
  if ((sensordata.header.Data.typ=TKM003C_CMD_PUT_DATA) AND (sensordata.header_ext.Header.att=TKM003C_ATT_ADC)) then
  begin
    avalue:=swap(sensordata.V_bus_ori_avg)/1000000;
    PDOVoltageDisplay.Value:=avalue;
    Chart1LineSeries1.AddY(avalue);

    normal:=swap(sensordata.I_bus_ori_avg);
    signed:=@normal;
    avalue:=signed^/1000000;
    PDOCurrentDisplay.Value:=avalue;

    avalue:=swap(sensordata.V_bus_avg)/1000000;
    RealVoltageDisplay.Value:=avalue;
    Chart1LineSeries2.AddY(avalue);

    normal:=swap(sensordata.I_bus_avg);
    signed:=@normal;
    avalue:=signed^/1000000;
    RealCurrentDisplay.Value:=avalue;

    avalue:=swap(sensordata.V_bus)/1000000;
    Edit6.Text:=FloattoStr(avalue);
    Chart1LineSeries3.AddY(avalue);

    normal:=swap(sensordata.I_bus);
    signed:=@normal;
    avalue:=signed^/1000000;
    Edit7.Text:=FloattoStr(avalue);

    avalue:=swap(sensordata.V_cc1)/10000;
    Edit1.Text:=FloattoStr(avalue);
    avalue:=swap(sensordata.V_cc2)/10000;
    Edit2.Text:=FloattoStr(avalue);
    avalue:=swap(sensordata.V_dp)/10000;
    Edit3.Text:=FloattoStr(avalue);
    avalue:=swap(sensordata.V_dm)/10000;
    Edit4.Text:=FloattoStr(avalue);
    avalue:=swap(sensordata.V_dd)/10000;
    Edit5.Text:=FloattoStr(avalue);


    //with sensordata do Edit8.Text:=InttoHex(unknown_3[0])+' '+InttoHex(unknown_3[1])+' '+InttoHex(unknown_3[2])+' '+InttoHex(unknown_3[3])+' '+InttoHex(unknown_3[4])+' '+InttoHex(unknown_3[5])+' '+InttoHex(unknown_3[6])+' '+InttoHex(unknown_3[7])+' '+InttoHex(unknown_3[8]);
    //with sensordata do Edit8.Text:=InttoHex(unknown_3[0])+' '+InttoHex(unknown_3[1])+' '+InttoHex(unknown_3[2])+' '+InttoHex(unknown_3[3])+' '+InttoHex(unknown_3[4])+' '+InttoHex(unknown_3[5]);

    avalue:={swap}(sensordata.unknown_1)/1000;
    Edit8.Text:=FloattoStr(avalue);
    //with sensordata do Edit8.Text:=InttoHex(unknown_1[0])+' '+InttoHex(unknown_1[1]);


    (*
    normal:=(sensordata.temp[1]*2000 + sensordata.temp[0]*1000);
    signed:=@normal;
    avalue:=signed^/128;
    edit13.Text := FloattoStr(avalue);
    *)

    avalue:={swap}(sensordata.P0_extra)/1000;
    Edit11.Text:=FloattoStr(avalue);
    //avalue:={swap}(sensordata.P1_bus)/1000;
    //Edit12.Text:='P1_bus: '+FloattoStr(avalue);

    avalue:={swap}(sensordata.V_dp_extra)/1000;
    Edit9.Text:=FloattoStr(avalue);
    avalue:={swap}(sensordata.V_dm_extra)/1000;
    Edit10.Text:=FloattoStr(avalue);
  end;

end;

procedure TForm1.CheckTimerTimer(Sender: TObject);
var
  header                : TKM003CMsgHeader;
  header_ext            : TKM003CMsgHeader;
  PacketHeader          : TKM003CPacketHeader;

  result_code           : LongInt;
  newdata               : packed array[0..4095] of byte;
  i,j,k                 : integer;
  dataindexer           : integer;
  datasize              : integer;
  SOPPacket             : boolean;
  s                     : string;

  aSOPDHEADER           : PDHEADER;
  aSOPExtendedHeader    : PDHEADEREXTENDED;
  MSGCTRL               : TUSBPD_CONTROLMSG;
  MSGDATA               : TUSBPD_DATAMSG;
  MSGEXT                : TUSBPD_EXTENDEDMSG;
  SRCPDO                : USBC_SOURCE_PD_POWER_DATA_OBJECT;
begin
  // Send command to get PD data
  header.Raw:=0;
  header.Ctrl.typ:=Ord(TKM003CHeaderCommand.CMD_GET_DATA);
  header.Ctrl.att:=TKM003C_ATT_PD_PACKET;
  result_code := OutEndPoint.Send(header.Bytes , 4 ,10);

  // Receive PD data
  FillChar({%H-}newdata,4096,0);
  result_code := InEndPoint.Recv(newdata ,4096 , 10);

  if (result_code>0) then
  begin
    dataindexer:=0;

    for j:=0 to 3 do header.Bytes[j]:=newdata[dataindexer+j];
    Inc(dataindexer,4);
    for j:=0 to 3 do header_ext.Bytes[j]:=newdata[dataindexer+j];
    Inc(dataindexer,4);

    // Did we receive a packet from our command ?
    if ((header.Data.typ=TKM003C_CMD_PUT_DATA) AND (header_ext.Header.att=TKM003C_ATT_PD_PACKET)) then
    begin
      datasize:=header_ext.Header.size;

      // now follows 12 bytes of unknown data as answer to our request
      // real packet data starts after these 12 bytes, at byte 20

      Inc(dataindexer,12);
      Dec(datasize,12);

      while (datasize>5) do
      begin
        // Now a packet header of 6 bytes
        for j:=0 to 5 do PacketHeader.Bytes[j]:=newdata[dataindexer+j];
        Inc(dataindexer,6);
        Dec(datasize,6);

        // If bit8 is set, we seem to have a valid SOP packet.
        SOPPacket:=(PacketHeader.Data.Size.Bits[7]=1);

        // Get/set packet size by resetting SOP-bit
        PacketHeader.Data.Size.Bits[7]:=0;
        PacketHeader.Data.Size.Bits[6]:=0;

        if (datasize=0) then
        begin
          Memo1.Lines.Append('Empty data. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(PacketHeader.Data.Time.Raw))));
          break;
        end;

        if (NOT SOPPacket) then
        begin
          Memo1.Lines.Append('Non SOP data ['+InttoStr(datasize)+']. Data size: '+InttoStr(PacketHeader.Data.Size.Raw)+'. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(PacketHeader.Data.Time.Raw))));
          s:='';
          for j:=0 to Pred(PacketHeader.Data.Size.Raw) do
          begin
            s:=s+InttoHex(newdata[j+dataindexer])+' ';
            Inc(dataindexer,1);
            Dec(datasize,1);
          end;
          Memo1.Lines.Append(s);
        end;

        if SOPPacket then
        begin
          PacketHeader.Data.Size.Raw:=PacketHeader.Data.Size.Raw-5;
          // PacketHeader.Data.Size.Raw is now SOP packet in bytes

          Memo1.Lines.Append('Data size: '+InttoStr(PacketHeader.Data.Size.Raw)+'. SOP: '+InttoStr(PacketHeader.Data.SOP)+'. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(PacketHeader.Data.Time.Raw))));

          // Now a 2 byte SOP header
          for j:=0 to 1 do aSOPDHEADER.Bytes[j]:=newdata[dataindexer+j];
          Inc(dataindexer,2);
          Dec(datasize,2);
          Memo1.Lines.Append('SOP: '+GetSOPInfo(aSOPDHEADER));

          MSGCTRL:=USBPD_CONTROLMSG_RESERVED0;
          MSGDATA:=USBPD_DATAMSG_RESERVED0;
          MSGEXT:=USBPD_EXTMSG_RESERVED0;
          if ((aSOPDHEADER.Data.Number_of_Data_Objects=0) AND (aSOPDHEADER.Data.Extended=0)) then
          begin
            MSGCTRL:=TUSBPD_CONTROLMSG(aSOPDHEADER.Data.Message_Type);
          end
          else
          begin
            if (aSOPDHEADER.Data.Extended=0) then MSGDATA:=TUSBPD_DATAMSG(aSOPDHEADER.Data.Message_Type);
            if (aSOPDHEADER.Data.Extended=1) then
            begin
              MSGEXT:=TUSBPD_EXTENDEDMSG(aSOPDHEADER.Data.Message_Type);
              // Now a 2 byte extended SOP header
              for j:=0 to 1 do aSOPExtendedHeader.Bytes[j]:=newdata[dataindexer+j];
              Inc(dataindexer,2);
              Dec(datasize,2);
            end;
          end;

          if MSGDATA<>USBPD_DATAMSG_RESERVED0 then
          begin
            case MSGDATA of
              USBPD_DATAMSG_SRC_CAPABILITIES:
                begin
                  for k:=1 to aSOPDHEADER.Data.Number_of_Data_Objects do
                  begin
                    for j:=0 to 3 do SRCPDO.Bytes[j]:=newdata[dataindexer+j+(k-1)*4];
                    Memo1.Lines.Append(SRCPDOInfo(SRCPDO));
                  end;
                end;
            end;
          end;

          // Skip SOP data
          Inc(dataindexer,aSOPDHEADER.Data.Number_of_Data_Objects*4);
          Dec(datasize,aSOPDHEADER.Data.Number_of_Data_Objects*4);


        end;
        //else
        //begin
          // No SOP : quit
        //  break;
        //end;
      end;
    end;

  end;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    Connect;
    PDTimer.Enabled:=True;
    DataTimer.Enabled:=True;
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ser.WriteString(KC003CCommand[TKC003CCOMMAND.PDMOPEN].Command);
  ser.WriteString(KC003CCommand[TKC003CCOMMAND.ENTRYPD].Command);
  ser.WriteString(KC003CCommand[TKC003CCOMMAND.PDPDO].Command);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ser.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDREQSIMPLE].Command,[2]));
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  ser.WriteString(KC003CCommand[TKC003CCOMMAND.RESET].Command);
  ser.WriteString(KC003CCommand[TKC003CCOMMAND.PDMCLOSE].Command);
end;

procedure TForm1.DisConnect;
begin
  PDTimer.Enabled:=False;
  DataTimer.Enabled:=False;

  ser.Active:=False;

  if Assigned(OutEndPoint) then OutEndPoint.Destroy;
  OutEndPoint:=nil;

  if Assigned(InEndPoint) then InEndPoint.Destroy;
  InEndPoint:=nil;

  if Assigned(DeviceInterface) then DeviceInterface.Destroy;
  DeviceInterface:=nil;

  if Assigned(Device) then Device.Destroy;
  Device:=nil;

  if Assigned(Context) then Context.Destroy;
  Context:=nil;
end;

procedure TForm1.Connect;
begin
  if (Length(KM003CComport)>0) then
  begin
    ser.Active:=False;
    ser.Device:=KM003CComport;
    ser.BaudRate:=br921600;
    ser.FlowControl:=fcNone;
    ser.Parity:=pNone;
    ser.DataBits:=db8bits;
    ser.StopBits:=sbOne;
    ser.Active:=True;
  end;

  if (NOT Assigned(Context)) then Context := TLibUsbContext.Create;
  try
    if (NOT Assigned(Device)) then
    begin
      Device := TLibUsbDevice.Create(Context,DevVID,DevPID);
      if Assigned(Device) then
      begin
        Device.SetConfiguration(1);
        DeviceInterface := TLibUsbInterface.Create(Device,Device.FindInterface(0,0));
        OutEndPoint := TLibUsbBulkOutEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_OUT));
        InEndPoint := TLibUsbBulkInEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_IN));
      end;
    end;
  except
    Memo1.Lines.Append('Could not reach device');
    Device := nil;
  end;
end;

end.

