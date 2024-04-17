unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries, bits, dsLeds, libusb, libusboop,usbcpd;

type
  TMsgHeader = bitpacked record
      case integer of
          1 : (
               Ctrl : record
                 typ    : T7BITS;
                 extend : T1BITS;
                 id     : T8BITS;
                 dummy  : T1BITS;
                 att    : T15BITS;
               end;
          );
          2 : (
               Data : record
                 typ    : T7BITS;
                 extend : T1BITS;
                 id     : T8BITS;
                 dummy  : T6BITS;
                 att    : T10BITS;
               end;
          );
          3 : (
               Header : record
                 att    : T15BITS;
                 next   : T1BITS;
                 chunk  : T6BITS;
                 size   : T10BITS;
               end;
          );
          4 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          5 : (
               Bytes           : packed array[0..3] of byte;
               );
          6 : (
               Raw             : DWord;
              );
  end;

  TPowerzSensorData = packed record
    header:TMsgHeader;
    header_ext:TMsgHeader;
    //P1_bus:word;
    unknown_1: packed array [0..1] of byte;
    V_bus:dword;
    I_bus:dword;
    V_bus_avg:dword;
    I_bus_avg:dword;
    Vbus_ori_avg:dword;
    Ibus_ori_avg:dword;
    //unknown_2: packed array [0..7] of byte;
    temp: packed array [0..0] of byte;
    V_cc1:word;
    V_cc2:word;
    V_dp:word;
    V_dm:word;
    V_dd:word;
    Rate:byte;
    P0_extra:word;
    V_dp_extra:word;
    V_dm_extra:word;
  end;

  TCommand =
  (
    CMD_NONE,
    CMD_SYNC,
    CMD_CONNECT,
    CMD_DISCONNECT,
    CMD_RESET,
    CMD_ACCEPT,
    CMD_REJECT,
    CMD_FINISHED,
    CMD_JUMP_APROM,
    CMD_JUMP_DFU,
    CMD_GET_STATUS,
    CMD_ERROR,
    CMD_GET_DATA,
    CMD_GET_FILE
  );

const
  ATT_ADC             = $001;
  ATT_ADC_QUEUE       = $002;
  ATT_ADC_QUEUE_10K   = $004;
  ATT_SETTINGS        = $008;
  ATT_PD_PACKET       = $010;
  ATT_PD_STATUS       = $020;
  ATT_QC_PACKET       = $040;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    btnGetPDPacket: TButton;
    Button2: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
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
    Timer1: TTimer;
    procedure btnGetPDPacketClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure grpVADataResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

    PDOVoltageDisplay   : TdsSevenSegmentMultiDisplay;
    PDOCurrentDisplay   : TdsSevenSegmentMultiDisplay;
    RealVoltageDisplay  : TdsSevenSegmentMultiDisplay;
    RealCurrentDisplay  : TdsSevenSegmentMultiDisplay;

  public

  end;

var
  Form1: TForm1;

Const
  DevVID = $5FC9;
  DevPID = $0063;
  OutEndpoint_address = $01;
  InEndpoint_address = $81;
  EP_IN    =  1 or LIBUSB_ENDPOINT_IN;
  EP_OUT   =  1 or LIBUSB_ENDPOINT_OUT;

Var
    Context                : TLibUsbContext;
    Device                 : TLibUsbDevice;
    DeviceInterface        : TLibUsbInterface;
    OutEndPoint            : TLibUsbBulkOutEndpoint;
    InEndPoint             : TLibUsbBulkInEndpoint;
    result_code            : Integer;
    send_data              : String;
    data                   : String;
    send_command           : String;

implementation

{$R *.lfm}

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

procedure TForm1.Timer1Timer(Sender: TObject);
var
  result_code : LongInt;
  newdata:packed array[0..63] of byte;
  sensordata:TPowerzSensorData;

  avalue:double;

  signed:pinteger;
  normal:integer;
  header:TMsgHeader;
  i:integer;

begin
  // "up" is from device to host ... ;-)
  header.Raw:=0;
  header.Ctrl.typ:=Ord(TCommand.CMD_GET_DATA);
  header.Ctrl.att:=ATT_ADC;

  result_code := OutEndPoint.Send(header.Bytes , 4 ,10);
  FillChar(newdata,64,0);
  result_code := InEndPoint.Recv(newdata ,64 , 1000);

  Memo1.Lines.Append(InttoStr(result_code));

  move(newdata,sensordata,result_code);

  avalue:=swap(sensordata.V_bus)/1000000;
  PDOVoltageDisplay.Value:=avalue;
  Chart1LineSeries1.AddY(avalue);


  normal:=swap(sensordata.I_bus);
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

  avalue:=swap(sensordata.Vbus_ori_avg)/1000000;
  Edit6.Text:=FloattoStr(avalue);
  Chart1LineSeries3.AddY(avalue);


  normal:=swap(sensordata.Ibus_ori_avg);
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

  with sensordata do Edit8.Text:=InttoHex(unknown_1[0])+' '+InttoHex(unknown_1[1]);

  //*val = data->temp[1] * 2000 + data->temp[0] * 1000 / 128;

  avalue:={swap}(sensordata.P0_extra)/1000;
  Edit11.Text:=FloattoStr(avalue);
  //avalue:={swap}(sensordata.P1_bus)/1000;
  //Edit12.Text:='P1_bus: '+FloattoStr(avalue);

  avalue:={swap}(sensordata.V_dp_extra)/1000;
  Edit9.Text:=FloattoStr(avalue);
  avalue:={swap}(sensordata.V_dm_extra)/1000;
  Edit10.Text:=FloattoStr(avalue);

end;

procedure TForm1.Button1Click(Sender: TObject);
Var
  ID:Plibusb_interface_descriptor;
begin

  Memo1.Lines.Append(InttoStr(SizeOf(TPowerzSensorData)));

  Memo1.Lines.Append(InttoStr(SizeOf(TMsgHeader)));

  Context := TLibUsbContext.Create;
  Device := TLibUsbDevice.Create(Context,DevVID,DevPID);



  //ID:=Device.FindInterface;

  Device.SetConfiguration(1);
  ID:=Device.FindInterface(0,0);

  DeviceInterface := TLibUsbInterface.Create(Device,ID);


  OutEndPoint := TLibUsbBulkOutEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(OutEndpoint_address));
  InEndPoint := TLibUsbBulkInEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(InEndpoint_address));

  //Timer1.Enabled:=True;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  header:TMsgHeader;
  i,j:integer;
  WordData:TWordData;
begin
  header.Bytes[0]:=$01;
  header.Bytes[1]:=$00;
  header.Bytes[2]:=$00;
  header.Bytes[3]:=$0A;
  i:=header.Header.size;


  header.Bytes[0]:=$41;
  header.Bytes[1]:=$4F;
  header.Bytes[2]:=$02;
  header.Bytes[3]:=$05;
  i:=header.Header.size;

  header.Bytes[0]:=$10;
  header.Bytes[1]:=$00;
  header.Bytes[2]:=$00;
  header.Bytes[3]:=$16;
  i:=header.Header.size;


  WordData.Bytes[0]:=$9f;
  WordData.Bytes[1]:=$96 AND $03;

  WordData.Bytes[0]:=$87;
  WordData.Bytes[1]:=$96 AND $03;

  WordData.Bytes[0]:=$8b;
  WordData.Bytes[1]:=$98 AND $03;

  WordData.Bytes[0]:=$87;
  WordData.Bytes[1]:=$99 AND $03;

  WordData.Bytes[0]:=$87;
  WordData.Bytes[1]:=$9b AND $03;


  header.Bytes[0]:=$87;
  header.Bytes[1]:=$96;
  header.Bytes[2]:=$3d;
  header.Bytes[3]:=$1d;
  i:=header.Header.size;


  header.Bytes[0]:=$8b;
  header.Bytes[1]:=$98;
  header.Bytes[2]:=$3d;
  header.Bytes[3]:=$1d;
  i:=header.Header.size;

end;

procedure TForm1.btnGetPDPacketClick(Sender: TObject);
var
  header:TMsgHeader;
  header_ext:TMsgHeader;
  newdata:packed array[0..250] of byte;
  i,j,k:integer;

  SRCPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT;

  WordData:TWordData;
  aSOPDHEADER:PDHEADER;
  aSOPExtendedHeader:PDHEADEREXTENDED;
  packedsize:byte;

  datasize:integer;

  ByteData:TByteData;

  MSGCTRL:TUSBPD_CONTROLMSG;
  MSGDATA:TUSBPD_DATAMSG;
  MSGEXT:TUSBPD_EXTENDEDMSG;

begin
  // "up" is from device to host ... ;-)
  while true do
  begin
    header.Raw:=0;
    header.Ctrl.typ:=Ord(TCommand.CMD_GET_DATA);
    header.Ctrl.att:=ATT_PD_PACKET;

    result_code := OutEndPoint.Send(header.Bytes , 4 ,10);

    header.Raw:=0;
    FillChar(newdata,64,0);

    result_code := InEndPoint.Recv(newdata ,250 , 1000);

    if (result_code>0) then
    begin

      for i:=0 to 3 do header.Bytes[i]:=newdata[i];
      for i:=0 to 3 do header_ext.Bytes[i]:=newdata[i+4];
      datasize:=header_ext.Header.size;

      // now follows 12 bytes of unknown data as answer to our request
      // real data starts at byte 20

      Dec(datasize,12);
      i:=20;
      while (datasize>0) do
      begin
        // Now a header of 6 bytes
        // first byte = size - 2 byte SOP header
        ByteData.Raw:=newdata[i];
        if ByteData.Bits[7]=1 then
        begin
          // High bit seems to indicate SOP data
          ByteData.Bits[7]:=0;
          ByteData.Raw:=ByteData.Raw-7;
          // ByteData.Raw is now SOP packet in bytes minus header (2 bytes)

          Inc(i,6);
          Dec(datasize,6);
          // Now a 2 byte SOP
          for j:=0 to 1 do aSOPDHEADER.Bytes[j]:=newdata[i+j];
          Inc(i,2);
          Dec(datasize,2);
          Memo1.Lines.Append('SOP: '+GetSOPInfo(aSOPDHEADER));

          MSGCTRL:=USBPD_CONTROLMSG_RESERVED0;
          MSGDATA:=USBPD_DATAMSG_RESERVED0;
          MSGEXT:=USBPD_EXTMSG_RESERVED0;
          if ((aSOPDHEADER.Data.Number_of_Data_Objects=0) AND (aSOPDHEADER.Data.Extended=0)) then
          begin
            MSGCTRL:=TUSBPD_CONTROLMSG(aSOPDHEADER.Data.Message_Type);
            // Now a 2 byte extended SOP header
            for j:=0 to 1 do aSOPExtendedHeader.Bytes[j]:=newdata[i+j];
            Inc(i,2);
            Dec(datasize,2);
          end
          else
          begin
            if (aSOPDHEADER.Data.Extended=0) then MSGDATA:=TUSBPD_DATAMSG(aSOPDHEADER.Data.Message_Type);
            if (aSOPDHEADER.Data.Extended=1) then MSGEXT:=TUSBPD_EXTENDEDMSG(aSOPDHEADER.Data.Message_Type);
          end;

          if MSGDATA<>USBPD_DATAMSG_RESERVED0 then
          begin
            case MSGDATA of
              USBPD_DATAMSG_SRC_CAPABILITIES:
                begin
                  for k:=1 to aSOPDHEADER.Data.Number_of_Data_Objects do
                  begin
                    for j:=0 to 3 do SRCPDO.Bytes[j]:=newdata[i+j+(k-1)*4];
                    Memo1.Lines.Append(SRCPDOInfo(SRCPDO));
                  end;
                end;
            end;
          end;

          // Skip SOP data
          Inc(i,aSOPDHEADER.Data.Number_of_Data_Objects*4);
          Dec(datasize,aSOPDHEADER.Data.Number_of_Data_Objects*4);


        end
        else
        begin
          // No SOP : quit
          break;
        end;
      end;
    end;
    Sleep(1);
    Application.ProcessMessages;
  end;

  for i:=0 to 3 do header.Bytes[i]:=newdata[i];

  i:=0;

end;

end.

