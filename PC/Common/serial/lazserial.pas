{ LazSerial
Serial Port Component for Lazarus 
by Jurassic Pork  03/2013 03/2021
This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. }

{ Based on }
{ SdpoSerial v0.1.4
  CopyRight (C) 2006-2010 Paulo Costa
   paco@fe.up.pt
} 
{ Synaser library  by Lukas Gebauer }
{ TcomPort component }

{ new in v0.3 version
 Add conditional macros for cpuarm rpi in lazsynaser.pas
 Hide Active property from IDE Object inspector
}

{ new in v0.4 version
  DeviceClose procedure fixed
}

{ features :
Changed :  baudrate values.
            stop bits  new value : 1.5
new event : onstatus
new property FRcvLineCRLF : if this property is true, you use RecvString
in place of RecvPacket when you read data from the port.

new procedure  ShowSetupDialog to open a port settings form :
the device combobox contain the enumerated ports.
new procedure to enumerate real serial port on linux ( in synaser).

Demo : a simulator of gps serial port + serial port receiver :
you can send NMEA frames ( GGA GLL RMC°) to the opened serial port
(start gps simulator). You can change speed and heading.
In the memo you can see what is received from  the opened serial port.
In the status bar you can see the status events.

}


unit LazSerial;

{$mode objfpc}{$H+}

interface

uses
Classes,
{$IFDEF LINUX}
  cthreads,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, lazsynaser,  LResources;


type
{$IFDEF UNIX}
  TBaudRate=(br_____0, br____50, br____75, br___110, br___134, br___150,
             br___200, br___300, br___600, br__1200, br__1800, br__2400,
             br__4800, br__9600, br_19200, br_38400, br_57600, br115200,
             br230400
   {$IFNDEF DARWIN}   // LINUX
             , br460800, br500000, br576000, br921600, br1000000, br1152000,
             br1500000, br2000000, br2500000, br3000000, br3500000, br4000000
   {$ENDIF} );
{$ELSE}      // MSWINDOWS
   TBaudRate=(br___110,br___300, br___600, br__1200, br__2400, br__4800,
           br__9600,br_14400, br_19200, br_38400,br_56000, br_57600,
           br115200,br128000, br230400,br256000, br460800, br921600);
{$ENDIF}
  TDataBits=(db8bits,db7bits,db6bits,db5bits);
  TParity=(pNone,pOdd,pEven,pMark,pSpace);
  TFlowControl=(fcNone,fcXonXoff,fcHardware);
  TStopBits=(sbOne,sbOneAndHalf,sbTwo);

  TModemSignal = (msRI,msCD,msCTS,msDSR);
  TModemSignals = Set of TModemSignal;
  TStatusEvent = procedure(Sender: TObject; Reason: THookSerialReason; const Value: string) of object;

const
{$IFDEF UNIX}
    ConstsBaud: array[TBaudRate] of integer=
    (0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800, 9600,
    19200, 38400, 57600, 115200, 230400
    {$IFNDEF DARWIN}  // LINUX
       , 460800, 500000, 576000, 921600, 1000000, 1152000, 1500000, 2000000,
       2500000, 3000000, 3500000, 4000000
    {$ENDIF}  );
{$ELSE}      // MSWINDOWS
    ConstsBaud: array[TBaudRate] of integer=
    (110, 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 38400, 56000, 57600,
    115200, 128000, 230400, 256000, 460800, 921600 );
{$ENDIF}

  ConstsBits: array[TDataBits] of integer=(8, 7 , 6, 5);
  ConstsParity: array[TParity] of char=('N', 'O', 'E', 'M', 'S');
  ConstsStopBits: array[TStopBits] of integer=(SB1,SB1AndHalf,SB2);


type
  TLazSerial = class;

  TComPortReadThread=class(TThread)
  public
    Owner: TLazSerial;
  protected
    procedure CallEvent;
    procedure Execute; override;
  end;

  { TLazSerial }

  TLazSerial = class(TComponent)
  private
    FActive: boolean;
    FSynSer: TBlockSerial;
    FDevice: string;

    FBaudRate: TBaudRate;
    FDataBits: TDataBits;
    FParity: TParity;
    FStopBits: TStopBits;
    
    FSoftflow, FHardflow: boolean;
    FFlowControl: TFlowControl;
    FRcvLineCRLF : Boolean;

    FOnRxData: TNotifyEvent;
    FOnStatus: TStatusEvent;
    ReadThread: TComPortReadThread;

    FData:string;

    FCriticalSection: TRTLCriticalSection;
    FCommandList:TStringList;

    FAsync:boolean;

    FSOP:AnsiString;
    FEOP:AnsiString;

    procedure DeviceOpen;
    procedure DeviceClose;

    procedure ComException(str: string);

  protected
    procedure SetActive(state: boolean);
    procedure SetAsync(state: boolean);

    procedure SetSOP(value:ansistring);
    procedure SetEOP(value:ansistring);

    procedure SetBaudRate(br: TBaudRate);
    procedure SetDataBits(db: TDataBits);
    procedure SetParity(pr: TParity);
    procedure SetFlowControl(fc: TFlowControl);
    procedure SetStopBits(sb: TStopBits);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WriteString(data: AnsiString);
    function ReadString(Timeout: Integer): AnsiString;

    // read pin states
    function ModemSignals: TModemSignals;
    function GetDSR: boolean;
    function GetCTS: boolean;
    function GetRing: boolean;
    function GetCarrier: boolean;

    // set pin states
//    procedure SetRTSDTR(RtsState, DtrState: boolean);
    procedure SetDTR(OnOff: boolean);
    procedure SetRTS(OnOff: boolean);
//  procedure SetBreak(OnOff: boolean);

  published
    property Active: boolean read FActive write SetActive;

    property Async: boolean read FAsync write SetAsync;

    property SOP:AnsiString read FSOP write SetSOP;
    property EOP:AnsiString read FEOP write SetEOP;

    property BaudRate: TBaudRate read FBaudRate write SetBaudRate; // default br115200;
    property DataBits: TDataBits read FDataBits write SetDataBits;
    property Parity: TParity read FParity write SetParity;
    property FlowControl: TFlowControl read FFlowControl write SetFlowControl;
    property StopBits: TStopBits read FStopBits write SetStopBits;
    
    property SynSer: TBlockSerial read FSynSer write FSynSer;
    property Device: string read FDevice write FDevice;
    property RcvLineCRLF: Boolean read FRcvLineCRLF write FRcvLineCRLF;

    property OnRxData: TNotifyEvent read FOnRxData write FOnRxData;
    property OnStatus: TStatusEvent read FOnStatus write FOnStatus;

    property Data: string read FData;
  end;


implementation

{ TLazSerial }

procedure TLazSerial.DeviceClose;
begin
  if FAsync then
  begin
    // stop capture thread
    if (ReadThread<>nil) then
    begin
      ReadThread.FreeOnTerminate:=false;
      ReadThread.Terminate;
      while (NOT ReadThread.Terminated) do
      begin
        sleep(10);
      end;
      ReadThread.Free;
      ReadThread:=nil;
    end;
  end;
  SynSer.OnStatus := nil;

  // flush device
  if (FSynSer.Handle<>INVALID_HANDLE_VALUE) then
  begin
    FSynSer.Flush;
    FSynSer.Purge;
  end;

  // close device
  if (FSynSer.Handle<>INVALID_HANDLE_VALUE) then
  begin
    FSynSer.Flush;
    FSynSer.CloseSocket;
  end;
end;

constructor TLazSerial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsync:=false;
  FSOP:='';
  FEOP:='';
  InitCriticalSection(FCriticalSection);
  FCommandList:=TStringList.Create;
  //FHandle:=-1;
  ReadThread:=nil;
  FSynSer:=TBlockSerial.Create;
  FSynSer.LinuxLock:=false;
  FHardflow:=false;
  FSoftflow:=false;
  FFlowControl:=fcNone;
  {$IFDEF LINUX}
  FDevice:='/dev/ttyS0';
  {$ELSE}
  FDevice:='COM1';
  {$ENDIF}
  FRcvLineCRLF := False;;
//  FBaudRate:=br115200;
end;

destructor TLazSerial.Destroy;
begin
  DeviceClose;
  FSynSer.Free;
  FCommandList.Free;
  DoneCriticalsection(FCriticalSection);
  inherited;
end;

procedure TLazSerial.DeviceOpen;
begin
  FSynSer.Connect(FDevice);
  if FSynSer.Handle=INVALID_HANDLE_VALUE then
    raise Exception.Create('Could not open device '+ FSynSer.Device);

  FSynSer.Config(ConstsBaud[FBaudRate],
                 ConstsBits[FDataBits],
                 ConstsParity[FParity],
                 ConstsStopBits[FStopBits],
                 FSoftflow, FHardflow);

  // Launch Thread

  if Assigned(OnStatus) then SynSer.OnStatus := OnStatus;

  if FAsync then
  begin
    if (ReadThread=nil) then
    begin
      ReadThread := TComPortReadThread.Create(true);
      ReadThread.Owner := Self;
      ReadThread.Start;
    end;
  end;
end;

procedure TLazSerial.SetActive(state: boolean);
begin
  if state=FActive then exit;
  try
    if state then
      DeviceOpen
    else
      DeviceClose;
    FActive:=state;
  except
  end;
end;

procedure TLazSerial.SetAsync(state: boolean);
begin
  if (state<>FAsync) then
  begin
    FAsync:=state;
  end;
end;

procedure TLazSerial.SetSOP(value:ansistring);
begin
  if ((value<>FSOP) AND (NOT FActive)) then
  begin
    FSOP:=value;
  end;
end;

procedure TLazSerial.SetEOP(value:ansistring);
begin
  if ((value<>FEOP) AND (NOT FActive)) then
  begin
    FEOP:=value;
  end;
end;

procedure TLazSerial.SetBaudRate(br: TBaudRate);
begin
  FBaudRate:=br;
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
end;

procedure TLazSerial.SetDataBits(db: TDataBits);
begin
  FDataBits:=db;
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
end;

procedure TLazSerial.SetFlowControl(fc: TFlowControl);
begin
  if fc=fcNone then begin
    FSoftflow:=false;
    FHardflow:=false;
  end else if fc=fcXonXoff then begin
    FSoftflow:=true;
    FHardflow:=false;
  end else if fc=fcHardware then begin
    FSoftflow:=false;
    FHardflow:=true;
  end;

  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FFlowControl:=fc;
end;

procedure TLazSerial.SetParity(pr: TParity);
begin
  FParity:=pr;
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
end;

procedure TLazSerial.SetStopBits(sb: TStopBits);
begin
  FStopBits:=sb;
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
end;

procedure TLazSerial.WriteString(data: AnsiString);
begin
  if FAsync then
  begin
    EnterCriticalSection(FCriticalSection);
    FCommandList.Append(data);
    LeaveCriticalSection(FCriticalSection);
  end
  else
  begin
    FSynSer.SendString(data);
  end;
end;

function TLazSerial.ReadString(Timeout: Integer): AnsiString;
begin
  if FAsync then
  begin
    if (NOT Assigned(FOnRxData)) then
      result:=FData
    else
      result:='';
  end
  else
  begin
    result:=FSynSer.RecvPacket(Timeout);
  end;
end;


function TLazSerial.ModemSignals: TModemSignals;
begin
  result:=[];
  if FSynSer.CTS then result := result + [ msCTS ];
  if FSynSer.carrier then result := result + [ msCD ];
  if FSynSer.ring then result := result + [ msRI ];
  if FSynSer.DSR then result := result + [ msDSR ];
end;

function TLazSerial.GetDSR: boolean;
begin
  result := FSynSer.DSR;
end;

function TLazSerial.GetCTS: boolean;
begin
  result := FSynSer.CTS;
end;

function TLazSerial.GetRing: boolean;
begin
  result := FSynSer.ring;
end;

function TLazSerial.GetCarrier: boolean;
begin
  result := FSynSer.carrier;
end;

procedure TLazSerial.SetDTR(OnOff: boolean);
begin
  FSynSer.DTR := OnOff;
end;

procedure TLazSerial.SetRTS(OnOff: boolean);
begin
  FSynSer.RTS := OnOff;
end;

procedure TLazSerial.ComException(str: string);
begin
  raise Exception.Create('ComPort error: '+str);
end;

{ TComPortReadThread }

procedure TComPortReadThread.CallEvent;
begin
  if Assigned(Owner.FOnRxData) then begin
    Owner.FOnRxData(Owner);
  end;
end;

procedure TComPortReadThread.Execute;
var
  FBuffer:ansistring;
  DataString:ansistring;
  PreAmbleCounterPos,PostAmbleCounterPos:word;
  x,y:word;
  SOPNeeded,EOPNeeded:boolean;
  DataOK:boolean;
begin
  DataString:='';
  FBuffer:='';
  SOPNeeded:=(Length(Owner.FSOP)>0);
  EOPNeeded:=(Length(Owner.FEOP)>0);

  try
    Owner.FSynSer.Purge;

    while (NOT Terminated) do
    begin

      EnterCriticalSection(Owner.FCriticalSection);
      //while ((not Terminated) AND (Owner.FCommandList.Count>0)) do
      if ((not Terminated) AND (Owner.FCommandList.Count>0)) then
      begin
        if Owner.FSynSer.CanWrite(10) then
        begin
          Owner.FSynSer.SendString(Owner.FCommandList[0]);
          Owner.FCommandList.Delete(0);
          //in most cases, we expect a read after a write ... wait for it ... ;-)
          //Owner.FSynSer.CanRead(100);
        end;
      end;
      LeaveCriticalSection(Owner.FCriticalSection);

      y:=0;
      repeat
        Inc(y);
        FBuffer:=Owner.FSynSer.RecvPacket(10);
        x:=Length(FBuffer);
        if (x>0) then DataString:=DataString+FBuffer;
        if (y>10) then break; // report at least every 100 ms in case of much data
      until ((x=0) OR (Terminated));

      x:=Length(DataString);

      while true do
      begin
        if ((x>0) AND (NOT Terminated)) then
        begin
          PreAmbleCounterPos:=0;
          if SOPNeeded then PreAmbleCounterPos:=Pos(Owner.FSOP,DataString);
          PostAmbleCounterPos:=0;
          if EOPNeeded then PostAmbleCounterPos:=Pos(Owner.FEOP,DataString);

          DataOk:=True;
          DataOK:=DataOK AND (SOPNeeded AND (PreAmbleCounterPos>0));
          DataOK:=DataOK AND (EOPNeeded AND (PostAmbleCounterPos>0));

          if DataOk then
          begin
            if (PreAmbleCounterPos=0) then PreAmbleCounterPos:=1;
            if (PostAmbleCounterPos=0) then PostAmbleCounterPos:=Length(DataString)+1;
            Owner.FData:=copy (DataString,PreAmbleCounterPos+Length(Owner.FSOP),PostAmbleCounterPos-PreAmbleCounterPos-Length(Owner.FSOP));
            Delete(DataString,1,PostAmbleCounterPos+Length(Owner.FEOP)-1);
            x:=Length(DataString);
            Synchronize(@CallEvent);
            continue;
          end;
        end
        else
        begin
          // Prevent burning of CPU
          sleep(1);
        end;
        break;
      end;

    end;
  except
    //Terminate;
  end;
end;

end.
