unit libmpsse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  FT_HANDLE=Pointer;

  TLibMPSSE = class(TObject)
  private
    Handle:FT_HANDLE;
    FNumChannels:Word;
  public
    constructor Create;
    destructor Destroy;override;
    function I2C_Init(Channel:word):boolean;
    function I2C_Close:boolean;
    function I2C_Write(DeviceAddress,DeviceRegister:Byte;ToWrite:Word;Buffer:PByte):boolean;
    function I2C_Read(DeviceAddress,DeviceRegister:Byte;ToRead:Word;Buffer:PByte):boolean;
    property NumChannels:Word read FNumChannels;
  end;

implementation

const
  MPSSE_LIBRARY_NAME                       = 'libmpsse.dll';

  I2C_DEVICE_BUFFER_SIZE                   = 256;

  I2C_TRANSFER_OPTIONS_START_BIT           = $00000001;
  I2C_TRANSFER_OPTIONS_STOP_BIT            = $00000002;
  I2C_TRANSFER_OPTIONS_BREAK_ON_NACK       = $00000004;
  I2C_TRANSFER_OPTIONS_NACK_LAST_BYTE      = $00000008;

  I2C_TRANSFER_OPTIONS_FAST_TRANSFER_BYTES = $00000010;
  I2C_TRANSFER_OPTIONS_FAST_TRANSFER_BITS  = $00000020;
  I2C_TRANSFER_OPTIONS_FAST_TRANSFER       = $00000030;

  I2C_TRANSFER_OPTIONS_NO_ADDRESS          = $00000040;

  I2C_CMD_GETDEVICEID_RD                   = $F9;
  I2C_CMD_GETDEVICEID_WR                   = $F8;

  I2C_GIVE_ACK                             = 1;
  I2C_GIVE_NACK                            = 0;

  I2C_DISABLE_3PHASE_CLOCKING              = $0001;

  I2C_ENABLE_DRIVE_ONLY_ZERO               = $0002;

type
  FT_RESULT=uint32;

  FT_STATUS_CODES = (
  	FT_OK,
  	FT_INVALID_HANDLE,
  	FT_DEVICE_NOT_FOUND,
  	FT_DEVICE_NOT_OPENED,
  	FT_IO_ERROR,
  	FT_INSUFFICIENT_RESOURCES,
  	FT_INVALID_PARAMETER,
  	FT_INVALID_BAUD_RATE,

  	FT_DEVICE_NOT_OPENED_FOR_ERASE,
  	FT_DEVICE_NOT_OPENED_FOR_WRITE,
  	FT_FAILED_TO_WRITE_DEVICE,
  	FT_EEPROM_READ_FAILED,
  	FT_EEPROM_WRITE_FAILED,
  	FT_EEPROM_ERASE_FAILED,
  	FT_EEPROM_NOT_PRESENT,
  	FT_EEPROM_NOT_PROGRAMMED,
  	FT_INVALID_ARGS,
  	FT_NOT_SUPPORTED,
  	FT_OTHER_ERROR,
  	FT_DEVICE_LIST_NOT_READY
  );

  FT_I2C_ClockRate = (
    I2C_CLOCK_STANDARD_MODE = 100000,							// 100kb/sec
    I2C_CLOCK_STANDARD_MODE_3P = 133333,						// 100kb/sec with 3-phase clocks
    I2C_CLOCK_FAST_MODE = 400000,								// 400kb/sec
    I2C_CLOCK_FAST_MODE_PLUS = 1000000, 						// 1000kb/sec
    I2C_CLOCK_HIGH_SPEED_MODE = 3400000 					    // 3.4Mb/sec
  );

  FT_DEVICE_LIST_INFO_NODE = record
      Flags: Cardinal;
      Typ: Cardinal;
      ID: Cardinal;
      LocId: DWord;
      SerialNumber: packed array [0..15] of char;
      Description: packed array [0..63] of char;
      ftHandle: FT_HANDLE;
  end;
  PFT_DEVICE_LIST_INFO_NODE = ^FT_DEVICE_LIST_INFO_NODE;

  FT_CHANNEL_CONFIG = record
  	ClockRate:FT_I2C_ClockRate;
  	LatencyTimer:uint8;
  	Options:uint32;
  end;
  PFT_CHANNEL_CONFIG = ^FT_CHANNEL_CONFIG;

function I2C_GetNumChannels(
  numChannels: puint32
): FT_Result; stdcall; external MPSSE_LIBRARY_NAME;

function I2C_GetChannelInfo (
  index: uint32;
  info:PFT_DEVICE_LIST_INFO_NODE
): FT_Result; stdcall; external MPSSE_LIBRARY_NAME;

function I2C_OpenChannel(
  index:uint32;
  handle:FT_HANDLE
): FT_Result; stdcall; external MPSSE_LIBRARY_NAME;

function I2C_CloseChannel(
  handle:FT_HANDLE
): FT_Result; stdcall; external MPSSE_LIBRARY_NAME;

function I2C_InitChannel(
  handle:FT_HANDLE;
  config:PFT_CHANNEL_CONFIG
): FT_Result; stdcall; external MPSSE_LIBRARY_NAME;

function I2C_DeviceWrite(
  handle              :FT_HANDLE;
  deviceAddress       :uint32;
  sizeToTransfer      :uint32;
  buffer              :pbyte;
  sizeTransferred     :puint32;
  options             :uint32
):FT_Result; cdecl; external MPSSE_LIBRARY_NAME;

function I2C_DeviceRead(
  handle              :FT_HANDLE;
  deviceAddress       :uint32;
  sizeToTransfer      :uint32;
  buffer              :pbyte;
  sizeTransferred     :puint32;
  options             :uint32
):FT_Result; cdecl; external MPSSE_LIBRARY_NAME;

{TLibMPSSE}

function TLibMPSSE.I2C_Init(Channel:word):boolean;
var
  //numofchannels:uint32;
  //info:FT_DEVICE_LIST_INFO_NODE;
  config:FT_CHANNEL_CONFIG;
  //result:FT_Result;
begin
  result:=false;
  if NOT Assigned(Handle) then
  begin
    //result:=I2C_GetNumChannels(@numofchannels);
    //result:=I2C_GetChannelInfo(Channel,@info);
    result:=(I2C_OpenChannel(Channel,@Handle)=Ord(FT_OK));
    if result then
    begin
      //config.ClockRate:=I2C_CLOCK_STANDARD_MODE;
      config.ClockRate:=I2C_CLOCK_FAST_MODE;
      config.LatencyTimer:=2;
      config.Options := 0;
      result:=(I2C_InitChannel(Handle,@config)=Ord(FT_OK));
    end;
  end;
end;

function TLibMPSSE.I2C_Close:boolean;
begin
  result:=false;
  if Assigned(Handle) then
  begin
    I2C_CloseChannel(Handle);
    Handle:=nil;
    result:=true;
  end;
end;

function TLibMPSSE.I2C_Write(DeviceAddress,DeviceRegister:Byte;ToWrite:Word;Buffer:PByte):boolean;
var
  NumBytes,NumBytesReturned:uint32;
  buf:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
begin
  result:=false;
  if Assigned(Handle) then
  begin
    NumBytes:=ToWrite+2;
    NumBytesReturned:=0;
    FillChar({%H-}buf,SizeOf(buf),0);
    buf[0]:=DeviceRegister;
    buf[1]:=ToWrite;
    Move(buffer^,buf[2],ToWrite);
    result:=(I2C_DeviceWrite(Handle,DeviceAddress,NumBytes,@buf[0],@NumBytesReturned,I2C_TRANSFER_OPTIONS_START_BIT OR I2C_TRANSFER_OPTIONS_STOP_BIT)=Ord(FT_OK));
  end;
end;

function TLibMPSSE.I2C_Read(DeviceAddress,DeviceRegister:Byte;ToRead:Word;Buffer:PByte):boolean;
var
  NumBytes,NumBytesReturned:uint32;
  buf:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
begin
  result:=false;
  if Assigned(Handle) then
  begin
    NumBytes:=1;
    NumBytesReturned:=0;
    FillChar({%H-}buf,SizeOf(buf),0);
    buf[0]:=DeviceRegister;
    result:=(I2C_DeviceWrite(Handle,DeviceAddress,NumBytes,@buf[0],@NumBytesReturned,(I2C_TRANSFER_OPTIONS_START_BIT {OR I2C_TRANSFER_OPTIONS_FAST_TRANSFER_BYTES}))=Ord(FT_OK));

    if result then
    begin
      NumBytes:=ToRead+1;
      NumBytesReturned:=0;
      FillChar({%H-}buf,SizeOf(buf),0);
      result:=(I2C_DeviceRead(Handle,DeviceAddress,NumBytes,@buf[0],@NumBytesReturned,(I2C_TRANSFER_OPTIONS_START_BIT OR I2C_TRANSFER_OPTIONS_STOP_BIT OR I2C_TRANSFER_OPTIONS_NACK_LAST_BYTE {OR I2C_TRANSFER_OPTIONS_FAST_TRANSFER_BYTES}))=Ord(FT_OK));
      if result then
      begin
        Move(buf[1],buffer^,ToRead);
      end;
    end;
  end;
end;

constructor TLibMPSSE.Create;
var
  Channels:uint32;
begin
  //inherited Create;
  Handle:=nil;
  I2C_GetNumChannels(@Channels);
  FNumChannels:=Channels;
end;

destructor TLibMPSSE.Destroy;
begin
  if Assigned(Handle) then I2C_CloseChannel(Handle);
  inherited Destroy;
end;

end.

