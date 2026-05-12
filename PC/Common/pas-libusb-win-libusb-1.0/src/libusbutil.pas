(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Utilities for the object-oriented wrapper for libusb.                 *
 *                                                                         *
 *   This Pascal unit is free software; you can redistribute it and/or     *
 *   modify it under the terms of a modified GNU Lesser General Public     *
 *   License (see the file COPYING.modifiedLGPL.txt).                      *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *   GNU Lesser General Public License for more details.                   *
 *                                                                         *
 ***************************************************************************)
Unit LibUsbUtil;

{$mode objfpc}{$H+}

Interface

Uses LibUsb,LibUsbOop;

Type

  { TLibUsbDeviceMatchVidPid }

  TLibUsbDeviceMatchVidPid = class(TLibUsbDeviceMatchClass)
  protected
    FContext : TLibUsbContext;
    FVid     : Word;
    FPid     : Word;
  public
    Function Match(Dev:Plibusb_device) : Boolean; override;
    Constructor Create(AContext:TLibUsbContext;AVid,APid:Word);
  End;

  { TLibUsbDeviceMatchVidPidSerial }

  TLibUsbDeviceMatchVidPidSerial = class(TLibUsbDeviceMatchVidPid)
  protected
    FSerial : String;
  public
    Function Match(Dev:Plibusb_device) : Boolean; override;
    Constructor Create(AContext:TLibUsbContext;AVid,APid:Word;ASerial:String);
  End;

  { TLibUsbInterfaceMatchVidPidSerial }

  { TLibUsbInterfaceMatchNumAlt }

  TLibUsbInterfaceMatchNumAlt = class(TLibUsbInterfaceMatchClass)
  protected
    FIntfNum    : Byte;
    FAltSetting : Byte;
  public
    Function Match(Intf:Plibusb_interface_descriptor) : Boolean; override;
    Constructor Create(AIntfNum,AAltSetting:Byte);
  End;

Function MatchEPBulkIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Function MatchEPBulkOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Function MatchEPIntrIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Function MatchEPIntrOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;

Type

  { TLibUsbDeviceIntfInfo }

  TLibUsbDeviceIntfInfo = class
  public
    // values from create
    FContext : TLibUsbContext;
    FVid     : Word;
    FPid     : Word;
    FSerial  : String;
    FConfig  : Byte;
    FIntf    : Byte;
    // values determined by Find
    FBusNumber  : Byte;
    FPortNumber : Byte;
    FPortPath   : TDynByteArray;
    FAddress    : Byte;
    FVendor     : String;
    FProduct    : String;
    FSerialDesc : String;
{$IFDEF LINUX}
    FSysDev     : String;
    FSysDevDir  : String;
    FSysIntf    : String;
    FSysIntfDir : String;
    FSysTty     : String;
    FDevTty     : String;
{$ENDIF}   // LINUX
    FDev        : Plibusb_device;
  public
    Constructor Create(AContext:TLibUsbContext;AVid,APid:Word;ASerial:String;AConfig,AIntf:Byte);
    Function Find : Plibusb_device;
    Function HasTty : String;
  End;

  { TLibUsbDeviceWithFirmware }

  TLibUsbDeviceWithFirmware = class(TLibUsbDevice)
  protected
    Procedure Configure(ADev:Plibusb_device); virtual; abstract;
  public
    Constructor Create(AContext:TLibUsbContext;AMatchUnconfigured,AMatchConfigured:TLibUsbDeviceMatchClass); overload;
  End;

Implementation
Uses SysUtils;

{ TLibUsbDeviceMatchVidPid }

Function TLibUsbDeviceMatchVidPid.Match(Dev : Plibusb_device) : Boolean;
Var DeviceDescriptor : libusb_device_descriptor;
Begin
  DeviceDescriptor := FContext.GetDeviceDescriptor(dev);
  Result := ((DeviceDescriptor.idVendor = FVid) and (DeviceDescriptor.idProduct = FPid));
End;

Constructor TLibUsbDeviceMatchVidPid.Create(AContext : TLibUsbContext; AVid, APid : Word);
Begin
  inherited Create;
  FContext := AContext;
  FVid     := AVid;
  FPid     := APid;
End;

{ TLibUsbDeviceMatchVidPidSerial }

Function TLibUsbDeviceMatchVidPidSerial.Match(Dev : Plibusb_device) : Boolean;
Begin
  Result := inherited Match(Dev) and (FContext.GetSerialNumber(Dev) = FSerial);
End;

Constructor TLibUsbDeviceMatchVidPidSerial.Create(AContext : TLibUsbContext; AVid, APid : Word; ASerial : String);
Begin
  inherited Create(AContext,AVid,APid);
  FSerial := ASerial;
End;

{ TLibUsbInterfaceMatchNumAlt }

Function TLibUsbInterfaceMatchNumAlt.Match(Intf : Plibusb_interface_descriptor) : Boolean;
Begin
  Result := ((Intf^.bInterfaceNumber = FIntfNum) and (Intf^.bAlternateSetting = FAltSetting));
End;

Constructor TLibUsbInterfaceMatchNumAlt.Create(AIntfNum, AAltSetting : Byte);
Begin
  inherited Create;
  FIntfNum    := AIntfNum;
  FAltSetting := AAltSetting;
End;

Function MatchEPBulkIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_IN) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_BULK);
End;

Function MatchEPBulkOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_OUT) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_BULK);
End;

Function MatchEPIntrIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_IN) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_INTERRUPT);
End;

Function MatchEPIntrOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_OUT) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_INTERRUPT);
End;

{ TLibUsbDeviceIntfInfo }

Constructor TLibUsbDeviceIntfInfo.Create(AContext : TLibUsbContext; AVid, APid : Word; ASerial : String; AConfig, AIntf : Byte);
Begin
  inherited Create;
  FContext := AContext;
  FPid     := APid;
  FVid     := AVid;
  FSerial  := ASerial;
  FConfig  := AConfig;
  FIntf    := AIntf;
End;

Function TLibUsbDeviceIntfInfo.Find : Plibusb_device;
Var Devs     : TLibUsbDeviceArray;
    DIdx     : Integer;
    Matcher  : TLibUsbDeviceMatchClass;
{$IFDEF LINUX}
    I        : Integer;
{$ENDIF}
Begin
  Result := Nil;
  SetLength(Devs,0);
  Matcher := TLibUsbDeviceMatchVidPid.Create(FContext, FVid, FPid);
  // find configured devices, but don't complain if non were found
  Devs := FContext.FindDevices(Matcher,false,0);
  Matcher.Free;
  WriteLn('Found ',Length(Devs),' devices:');
  For DIdx := 0 to Length(Devs)-1 do
    Begin
      FBusNumber  := FContext.GetBusNumber(Devs[DIdx]);
      FPortNumber := FContext.GetPortNumber(Devs[DIdx]);
      FPortPath   := FContext.GetPortPath(Devs[DIdx]);
      FAddress    := FContext.GetDeviceAddress(Devs[DIdx]);
      WriteLn('Bus Number: ', FBusNumber);
      WriteLn('Port Number: ', FPortNumber);
      Write('Port Path: ');
{$IFDEF LINUX}
      For I := 0 to Length(FPortPath)-1 do
        Write(FPortPath[I],' ');
      WriteLn;
      WriteLn('Device Address: ', FAddress);
      // create filename used in /sys for the USB device
      FSysDev := IntToStr(FBusNumber)+'-';
      For I := 1 to Length(FPortPath)-1 do
        FSysDev := FSysDev + IntToStr(FPortPath[I]) + '.';
      FSysDev := FSysDev + IntToStr(FPortNumber);
      WriteLn(FSysDev);
      FSysDevDir := '/sys/bus/usb/devices/'+FSysDev;
      if not DirectoryExists(FSysDevDir) then
        continue;
      FVendor  := Trim(GetFileAsString(FSysDevDir+'/manufacturer'));
      FProduct := Trim(GetFileAsString(FSysDevDir+'/product'));
      WriteLn('Vendor: ', FVendor);
      WriteLn('Product: ', FProduct);
      if FileExists(FSysDevDir+'/serial') then
        FSerialDesc := Trim(GetFileAsString(FSysDevDir+'/serial'));
      WriteLn('Serial = ',FSerialDesc, ' =?= ', FSerial);
      // check serial number if required
      if (FSerial > '') and (FSerialDesc <> FSerial) then
        continue;
      // create filename for USB interface
      FSysIntf := FSysDev+':'+IntToStr(FConfig)+'.'+IntToStr(FIntf);
      FSysIntfDir := FSysDevDir+'/'+ FSysIntf;
      WriteLn('/sys Dir = ',FSysIntfDir);
      if DirectoryExists(FSysIntfDir) then
        Begin
          WriteLn('FOUND!');
          FDev   := Devs[DIdx];
          Result := FDev;
          // the values in this object are only valid, if the return-value is non-Nil
          Exit;
        End;
{$ENDIF}
    End;
End;

Function TLibUsbDeviceIntfInfo.HasTty : String;
{$IFDEF LINUX}
Var
    SRec       : TSearchRec;
{$ENDIF}
Begin
  Result := '';
{$IFDEF LINUX}
  // check if Find was already called
  if FSysIntfDir = '' then
    raise Exception.Create('Call Find before HasTty');

  if not DirectoryExists(FSysIntfDir+'/tty') then
    Exit;
  // search for filename of the TTY device
  FSysTty := '';
  FindFirst(FSysIntfDir+'/tty/*', faDirectory, SRec);
  try
    repeat
      WriteLn('Found ',SRec.Name);
      if SRec.Name[1] = '.' then
        continue;
      if FSysTty > '' then
        Begin
          WriteLn('Strange, FSysTty is already set');
          continue;
        End;
      FSysTty := SRec.Name;
    until FindNext(SRec) <> 0;
  finally
    FindClose(SRec);
  End;
  if FSysTty = '' then
    Exit;
  FDevTty := '/dev/'+FSysTty;
  if not FileExists(FDevTty) then
    Begin
      WriteLn('Strange, ',FDevTty,' doesn''t exist.');
      Exit;
    End;
  Result := FDevTty;
{$ENDIF}
End;

{ TLibUsbDeviceWithFirmware }

Constructor TLibUsbDeviceWithFirmware.Create(AContext:TLibUsbContext;AMatchUnconfigured,AMatchConfigured:TLibUsbDeviceMatchClass);
Var Devs    : TLibUsbDeviceArray;
    Timeout : Integer;
Begin
  SetLength(Devs,0);
  // find configured devices, but don't complain if non were found
  Devs := AContext.FindDevices(AMatchConfigured,false,0);

  if (Length(Devs) = 0) and assigned(AMatchUnconfigured) then
    Begin
      // find unconfigured devices
      Devs := AContext.FindDevices(AMatchUnconfigured,false);

      if Length(Devs) > 1 then
        raise EUSBError.Create('Found too many unconfigured devices')
      else if Length(Devs) = 0 then
        Begin
          WriteLn(StdErr,'No unconfigured devices found.');  // TODO: this should only be a debug message
          Timeout := 300;
        End
      else
        Begin
          Configure(Devs[0]);
          // give it some time to ReNumerate, or at least to disconnect, otherwise
          // the following USBFindDevices would find the old device if both, the
          // unconfigured and the configured use the same IDs.
          Sleep(200);
          Timeout := 2000;
          SetLength(Devs,0);
        End;
    End
  else
    // don't even search for unconfigured devices
    Timeout := 300;

  // find configured devices
  if Length(Devs) = 0 then
    Devs := AContext.FindDevices(AMatchConfigured,false,Timeout);

  // before any exceptions, free matcher classes
  AMatchUnconfigured.Free;
  AMatchConfigured.Free;

  if Length(Devs) = 0 then
    raise EUSBError.Create('No configured devices found.')
  else if Length(Devs) > 1 then
    raise EUSBError.Create('Too many configured devices found.');

  inherited Create(AContext,Devs[0]);
End;

End.

