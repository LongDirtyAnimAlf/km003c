(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating sync I/O for control transfers.            *
 *                                                                         *
 *   This is free and unencumbered software released into the public       *
 *   domain.                                                               *
 *                                                                         *
 *   Anyone is free to copy, modify, publish, use, compile, sell, or       *
 *   distribute this software, either in source code form or as a compiled *
 *   binary, for any purpose, commercial or non-commercial, and by any     *
 *   means.                                                                *
 *                                                                         *
 *   In jurisdictions that recognize copyright laws, the author or authors *
 *   of this software dedicate any and all copyright interest in the       *
 *   software to the public domain. We make this dedication for the        *
 *   benefit of the public at large and to the detriment of our heirs and  *
 *   successors. We intend this dedication to be an overt act of           *
 *   relinquishment in perpetuity of all present and future rights to this *
 *   software under copyright law.                                         *
 *                                                                         *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *
 *   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF    *
 *   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                 *
 *   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY      *
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,  *
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE     *
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                *
 *                                                                         *
 ***************************************************************************)
Program Test2ControlSync;

{$mode objfpc}{$H+}

Uses SysUtils,LibUsb,LibUsbOop;

Const DevVID = $5FC9;
      DevPID = $0063;

Const
  TransferType : Array[0..3] of String = ('Control','Isochronous','Bulk','Interrupt');
  SynchType    : Array[0..3] of String = ('None','Async','Adaptive','Sync');
  UsageType    : Array[0..3] of String = ('Data','Feedback','Implicit','Reserved');

Function EndpointAddress2Name(bEndpointAddress:Byte) : String;
Begin
  Result := 'EP ' + IntToStr(bEndpointAddress and LIBUSB_ENDPOINT_ADDRESS_MASK);
  if bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK = LIBUSB_ENDPOINT_IN then
    Result := Result + ' IN'
  else
    Result := Result + ' OUT'
End;

Var Context   : TLibUsbContext;
    Device    : TLibUsbDevice;
    EP0       : TLibUsbDeviceControlEndpoint;
    DescrDev  : libusb_device_descriptor;
    Bus       : Byte;
    Addr      : Byte;
    DescrCfg  : Array[0..1023] of Byte;
    CfgSize   : Word;
    CfgIdx    : Word;
    DescrConf : libusb_config_descriptor;
    NumConf   : Byte;
    DescrIntf : libusb_interface_descriptor;
    DescrEP   : libusb_endpoint_descriptor;

Begin
  // create context
  Context := TLibUsbContext.Create;
  try
    Device := TLibUsbDevice.Create(Context,DevVID,DevPID);
    try
      EP0 := Device.Control;
      // get device descriptor
      if ELibUsb.Check(EP0.GetDescriptor(LIBUSB_DT_DEVICE,0,DescrDev,LIBUSB_DT_DEVICE_SIZE),'GetDescriptor') <> LIBUSB_DT_DEVICE_SIZE then
        raise Exception.Create('GetDescriptor: Didn''t get expected number of bytes');
      Bus  := TLibUsbContext.GetBusNumber    (Device.Device);
      Addr := TLibUsbContext.GetDeviceAddress(Device.Device);
      WriteLn(Format('Bus %.03d Device %.03d: ID %.04x:%.04x',[Bus,Addr,DescrDev.idVendor,DescrDev.idProduct]));
      With DescrDev do
        Begin
          WriteLn('Device Descriptor:');
          WriteLn('  bLength               ',bLength:3);
          WriteLn('  bDescriptorType       ',bDescriptorType:3);
          WriteLn('  bcdUSB              ',IntToHex(bcdUSB shr 8,1):2,'.',IntToHex(bcdUSB and $FF,2));
          WriteLn('  bDeviceClass          ',bDeviceClass:3);
          WriteLn('  bDeviceSubClass       ',bDeviceSubClass:3);
          WriteLn('  bDeviceProtocol       ',bDeviceProtocol:3);
          WriteLn('  bMaxPacketSize0       ',bMaxPacketSize0:3);
          WriteLn('  idVendor            $',IntToHex(idVendor, 4));
          WriteLn('  idProduct           $',IntToHex(idProduct,4));
          WriteLn('  bcdDevice           ',IntToHex(bcdDevice shr 8,1):2,'.',IntToHex(bcdDevice and $FF,2));
          WriteLn('  iManufacturer         ',iManufacturer:3,' ',EP0.GetString(iManufacturer));
          WriteLn('  iProduct              ',iProduct:3,' ',EP0.GetString(iProduct));
          WriteLn('  iSerialNumber         ',iSerialNumber:3,' ',EP0.GetString(iSerialNumber));
          WriteLn('  bNumConfigurations    ',bNumConfigurations:3);
        End;
      For NumConf := 0 to DescrDev.bNumConfigurations-1 do
        Begin
          // query config descriptor including all associated interface and endpoint descriptors
          CfgSize := ELibUsb.Check(EP0.GetDescriptor(LIBUSB_DT_CONFIG,0,DescrCfg,SizeOf(DescrCfg)),'GetDescriptor');
          (* The interface and endpoint descriptors cannot be queried
           * individually but are placed contiguous to the config descriptor.
           *
           * Since there is no generic data structure we could use to look at
           * the data retured from GetDescriptor, we use an array pointer
           * CfgIdx and analyze the next descriptor according to its
           * bDescriptorType and bLength fields. For all known descriptor types
           * the according libusb_*_descriptor records are used.
           *)
          CfgIdx := 0;
          While CfgIdx < CfgSize do
            Begin
              if (DescrCfg[CfgIdx+1] = LIBUSB_DT_CONFIG) and (DescrCfg[CfgIdx] = LIBUSB_DT_CONFIG_SIZE) then
                With DescrConf do
                  Begin
                    // copy config descriptor (inside "With" to save one level of indentation)
                    Move(DescrCfg[CfgIdx],DescrConf,DescrCfg[CfgIdx]);
                    Inc(CfgIdx,DescrConf.bLength);
                    WriteLn('  Configuration Descriptor:');
                    WriteLn('    bLength               ',bLength:3);
                    WriteLn('    bDescriptorType       ',bDescriptorType:3);
                    WriteLn('    wTotalLength        ',wTotalLength:5);
                    WriteLn('    bNumInterfaces        ',bNumInterfaces:3);
                    WriteLn('    bConfigurationValue   ',bConfigurationValue:3);
                    WriteLn('    iConfiguration        ',iConfiguration:3,' ',EP0.GetString(iConfiguration));
                    WriteLn('    bmAttributes          $',IntToHex(bmAttributes,2));
                    WriteLn('    MaxPower              ',MaxPower*2:3,'mA');
                  End
              else if (DescrCfg[CfgIdx+1] = LIBUSB_DT_INTERFACE) and (DescrCfg[CfgIdx] = LIBUSB_DT_INTERFACE_SIZE) then
                With DescrIntf do
                  Begin
                    // copy interface descriptor (inside "With" to save one level of indentation)
                    Move(DescrCfg[CfgIdx],DescrIntf,DescrCfg[CfgIdx]);
                    Inc(CfgIdx,DescrIntf.bLength);
                    WriteLn('    Interface Descriptor:');
                    WriteLn('      bLength               ',bLength:3);
                    WriteLn('      bDescriptorType       ',bDescriptorType:3);
                    WriteLn('      bInterfaceNumber      ',bInterfaceNumber:3);
                    WriteLn('      bAlternateSetting     ',bAlternateSetting:3);
                    WriteLn('      bNumEndpoints         ',bNumEndpoints:3);
                    WriteLn('      bInterfaceClass       ',bInterfaceClass:3);
                    WriteLn('      bInterfaceSubClass    ',bInterfaceSubClass:3);
                    WriteLn('      bInterfaceProtocol    ',bInterfaceProtocol:3);
                    WriteLn('      iInterface            ',iInterface:3,' ',EP0.GetString(iInterface));
                  End
              else if (DescrCfg[CfgIdx+1] = LIBUSB_DT_ENDPOINT) and (DescrCfg[CfgIdx] = LIBUSB_DT_ENDPOINT_SIZE) then
                With DescrEP do
                  Begin
                    // copy endpoint descriptor (inside "With" to save one level of indentation)
                    Move(DescrCfg[CfgIdx],DescrEP,DescrCfg[CfgIdx]);
                    Inc(CfgIdx,DescrEP.bLength);
                    WriteLn('      Endpoint Descriptor:');
                    WriteLn('        bLength               ',bLength:3);
                    WriteLn('        bDescriptorType       ',bDescriptorType:3);
                    WriteLn('        bEndpointAddress      $',IntToHex(bEndpointAddress,2),'  ',EndpointAddress2Name(bEndpointAddress));
                    WriteLn('        bmAttributes          ',bmAttributes:3);
                    WriteLn('          Transfer Type            ',TransferType[(bmAttributes shr 0) and $03]);
                    WriteLn('          Synch Type               ',SynchType   [(bmAttributes shr 2) and $03]);
                    WriteLn('          Usage Type               ',UsageType   [(bmAttributes shr 4) and $03]);
                    WriteLn('        wMaxPacketSize      $',IntToHex(wMaxPacketSize,4),'  1x ',wMaxPacketSize,' bytes');
                    WriteLn('        bInterval             ',bInterval:3);
                    if bLength > 7 then
                      Begin
                        WriteLn('        bRefresh              ',bRefresh:3);
                        WriteLn('        bSynchAddress         ',bSynchAddress:3);
                      End;
                  End
              else
                Begin
                  WriteLn('Unknown descriptor type $',IntToHex(DescrCfg[CfgIdx+1],2),' with length ',DescrCfg[CfgIdx]);
                  Inc(CfgIdx,DescrCfg[CfgIdx]);
                End;
            End;
        End;
    finally
      Device.Free;
    End;
  finally
    Context.Free;
  End;
End.

