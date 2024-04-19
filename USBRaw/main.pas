unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries,
  lazserial,
  fptimernew,
  bits, dsLeds, libusb, libusboop,usbcpd;

type
  TKC003CCOMMAND =
  (
    PDMOPEN,
    PDMCLOSE,
    PDMSETTYPE,
    ENTRYPD,
    ENTRYUFCS,
    ENTRYQC,
    ENTRYFCP,
    ENTRYSCP,
    ENTRYAFC,
    ENTRYVFCP,
    ENTRYSFCP,
    RESET,
    QC,
    QC3VOLT,
    QC3INC,
    QC3DEC,
    FCP,
    SCPVOLT,
    AFC,
    SFCP,
    VFCPVOLT,
    UFCSREQ,
    UFCSPDO,
    UFCSCMD,
    UFCSDATA,
    PDPDO,
    PDREQSIMPLE,
    PDREQNORMAL,
    PDREQEXT,
    PDCMD,
    PDDATA
  );

  TKC003CCOMMANDDATA = record
    Command:string;
    Help:string;
  end;

const
  KC003CCommand: array[TKC003CCOMMAND] of TKC003CCOMMANDDATA =
  (
    (Command: 'pdm open' ; Help: 'Start protocol trigger module.'),
    (Command: 'pdm close' ; Help: 'Exit protocol trigger module.'),
    (Command: 'pdm set type=%d,em=%d' ; Help: 'Customized PD protocol trigger. [type]: PD protocol trigger type, 0: automatic, 1: PD3.0, 2: PD3.1, 3: Proprietary PPS (two types for now). [em]: Emarker/Cable simulation, 0: off, 1: 20V5A, 2: 50V5A (EPR), LA135 6.75A'),
    (Command: 'entry pd' ; Help: 'Enter the PD protocol trigger and some of the proprietary protocols (type=2).'),
    (Command: 'entry ufcs' ; Help: 'UFCS (Universal Fast Charging Specification).'),
    (Command: 'entry qc' ; Help: 'Qualcomm QC, including QC2.0/3.0, it’ll automatically judge when triggered.'),
    (Command: 'entry fcp' ; Help: 'FCP proprietary protocol.'),
    (Command: 'entry scp' ; Help: 'SCP proprietary protocol.'),
    (Command: 'entry afc' ; Help: 'AFC proprietary protocol.'),
    (Command: 'entry vfcp' ; Help: 'VFCP proprietary protocol.'),
    (Command: 'entry sfcp' ; Help: 'SFCP proprietary protocol.'),
    (Command: 'reset' ; Help: 'Reset protocol trigger, restore to initial state: pdm open after sending.'),
    (Command: 'qc %dV' ; Help: 'Request fixed voltage of QC2.0 protocol. Example: qc 5V, qc 9V, qc 12V, qc 20V'),
    (Command: 'qc3 volt=%d' ; Help: 'Request any voltage of QC3.0, the minimum step is 200mV. Example: qc3 volt=3800, qc3 volt=19800, qc3 volt=5000'),
    (Command: 'qc3 inc=%d' ; Help: 'QC3.0 increases voltage.'),
    (Command: 'qc3 dec=%d' ; Help: 'QC3.0 reduces voltage.'),
    (Command: 'fcp %dV' ; Help: 'Request fixed voltage of FCP protocol. Example: fcp 5V, fcp 9V, fcp 12V'),
    (Command: 'scp volt=%d,cur=%d' ; Help: 'Request any voltage of SCP protocol, the minimum step is determined by the charger, the unit is mV.'),
    (Command: 'afc %dV' ; Help: 'Request fixed voltage of AFC protocol.'),
    (Command: 'sfcp %dV' ; Help: 'Request fixed voltage of SFCP protocol.'),
    (Command: 'vfcp volt=%d,cur=%d' ; Help: 'Request VFCP protocol.'),
    (Command: 'ufcs req=%d,volt=%d,cur=%d' ; Help: 'Request any voltage of UFCS protocol, the range is determined by the charger.'),
    (Command: 'ufcs pdo' ; Help: 'Get Output_Capabilities value of UFCS charger.'),
    (Command: 'ufcs cmd=%d' ; Help: 'Send control commands, please refer to the number in Table 14 of the UFCS protocol manual.'),
    (Command: 'ufcs data=%d' ; Help: 'not yet implemented.'),
    (Command: 'pd pdo' ; Help: 'Get the SourceCapabilities in the PD protocol.'),
    (Command: 'pd req=%d' ; Help: 'Request a fixed voltage without volt, take the Max current of PDO. [req] means ObjectPosition'),
    (Command: 'pd req=%d,cur=%d' ; Help: 'Request a fixed voltage without volt, if the cur value is not used, take the Max current of PDO.'),
    (Command: 'pd req=%d,volt=%d,cur=%d' ; Help: 'If you need to request PPS or AVS, request the fixed voltage with volt. If it is a fixed voltage, ignore the volt value.'),
    (Command: 'pd cmd=%d' ; Help: 'Send control command.'),
    (Command: 'pd data=%s' ; Help: 'Send data command. The first byte represents SOP, the second/third represents the header, and does not contain CRC')
  );

type
  TKM003CMsgHeader = bitpacked record
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

  TKM003CPacketHeader = bitpacked record
      case integer of
          1 : (
               Data : packed record
                 Size      : TByteData;
                 Time      : TDWordData;
                 SOP       : byte;
               end;
          );
          2 : (
               Bytes           : packed array[0..5] of byte;
          );
  end;


  TKM003CSensorData = packed record
    header:TKM003CMsgHeader;
    header_ext:TKM003CMsgHeader;
    //unknown_1: packed array [0..1] of byte;
    unknown_1: word;
    V_bus:dword;
    I_bus:dword;
    V_bus_avg:dword;
    I_bus_avg:dword;
    V_bus_ori_avg:dword;
    I_bus_ori_avg:dword;
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

  TKM003CHeaderCommand =
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
  TKM003C_CMD_HEAD            = 64;
  TKM003C_CMD_PUT_DATA        = 65;

  TKM003C_ATT_ADC             = $001;
  TKM003C_ATT_ADC_QUEUE       = $002;
  TKM003C_ATT_ADC_QUEUE_10K   = $004;
  TKM003C_ATT_SETTINGS        = $008;
  TKM003C_ATT_PD_PACKET       = $010;
  TKM003C_ATT_PD_STATUS       = $020;
  TKM003C_ATT_QC_PACKET       = $040;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConnect: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
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
    procedure Button3Click(Sender: TObject);
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

Const
  DevVID = $5FC9;
  DevPID = $0063;
  //EP_OUT = $01;
  //EP_IN = $81;
  EP_IN    =  1 or LIBUSB_ENDPOINT_IN;
  EP_OUT   =  1 or LIBUSB_ENDPOINT_OUT;

implementation

{$R *.lfm}

uses
  Tools;

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
  PDTimer.Enabled:=false;
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
  sensordata            : TKM003CSensorData;
  header                : TKM003CMsgHeader;
  header_ext            : TKM003CMsgHeader;
  PacketHeader          : TKM003CPacketHeader;

  result_code           : LongInt;
  newdata               : packed array[0..4095] of byte;
  i,j,k                 : integer;
  dataindexer           : integer;
  datasize              : integer;
  avalue                : double;
  signed                : pinteger;
  normal                : integer;
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
  ser.WriteString('pdm open');
  ser.WriteString('entry pd');
  ser.WriteString('pd pdo');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ser.WriteString('pd req=3');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s:string;
  i:integer;
begin
  ser.WriteString('pdm open');
  Memo1.Lines.Append(ser.ReadString(2000));
  Application.ProcessMessages;
  ser.WriteString('entry pd');
  Memo1.Lines.Append(ser.ReadString(2000));
  Application.ProcessMessages;
  ser.WriteString('pd pdo');
  Memo1.Lines.Append(ser.ReadString(2000));
  for i:=0 to 500 do
  begin
    Application.ProcessMessages;
    sleep(10);
  end;
  s:=Format(KC003CCommand[TKC003CCOMMAND.PDREQSIMPLE].Command,[4]);
  ser.WriteString(s);
  Memo1.Lines.Append(ser.ReadString(2000));
  Application.ProcessMessages;
  for i:=0 to 500 do
  begin
    Application.ProcessMessages;
    sleep(10);
  end;
  ser.WriteString('pdm close');
  Memo1.Lines.Append(ser.ReadString(2000));
end;

procedure TForm1.DisConnect;
begin
  OutEndPoint.Free;
  InEndPoint.Free;
  DeviceInterface.Free;
  Device.Free;
  Context.Free;

  ser.Active:=False;
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
    //ser.OnRxData:=@OnRXUSBCData;
    ser.Active:=True;
  end;

  Context := TLibUsbContext.Create;
  Device := TLibUsbDevice.Create(Context,DevVID,DevPID);
  Device.SetConfiguration(1);
  DeviceInterface := TLibUsbInterface.Create(Device,Device.FindInterface(0,0));
  OutEndPoint := TLibUsbBulkOutEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_OUT));
  InEndPoint := TLibUsbBulkInEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_IN));
end;

end.

