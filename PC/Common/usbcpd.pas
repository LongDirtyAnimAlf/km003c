unit usbcpd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, Bits;

type
  TSUPPLY_TYPES = (Fixed,Battery,Variable,APDO);
  TAPDO_TYPES = (SPRPPS,EPRAVS,SPRAVS);

const
  SUPPLY_TYPES : array[TSUPPLY_TYPES] of string = ('Fixed','Battery','Variable','Augmented');
  //APDO_TYPES : array[TAPDO_TYPES] of string = ('Standard Programmable Power Supply','Extended Adjustable Voltage Supply','Standard Adjustable Voltage Supply');
  APDO_TYPES : array[TAPDO_TYPES] of string = ('PPS','EPRAVS','AVS');
  MAXPDO = 7;    // 1-4 FPDO, 5-7 APDO
  MAXEPRPDO = 4; // 3 FPDO, 1 APDO

type
  TSOURCEPDO = bitpacked record
      case integer of
          1 : (  FixedSupplyPdo : record
                   MaximumCurrentIn10mA     : T10BITS;
                   VoltageIn50mV            : T10BITS;
                   PeakCurrent              : T2BITS;
                   Reserved1                : T1BITS;
                   EPRModeCapable           : T1BITS;
                   UnchunkedMessagesSupport : T1BITS;
                   DataRoleSwap             : T1BITS;
                   UsbCommunicationCapable  : T1BITS;
                   UnconstrainedPower       : T1BITS;
                   UsbSuspendSupported      : T1BITS;
                   DualRolePower            : T1BITS;
                   FixedSupply              : T2BITS; // 0x00
                 end
              );
          2 : (  BatterySupplyPdo : record
                   MaximumAllowablePowerIn250mW  : T10BITS;
                   MinimumVoltageIn50mV          : T10BITS;
                   MaximumVoltageIn50mV          : T10BITS;
                   Battery                       : T2BITS;  // 0x01
                 end
              );
          3 : (  VariableSupplyNonBatteryPdo : record
                   MaximumCurrentIn10mA      : T10BITS;
                   MinimumVoltageIn50mV      : T10BITS;
                   MaximumVoltageIn50mV      : T10BITS;
                   VariableSupportNonBattery : T2BITS;  // 0x10
                 end
              );
          4 : (  SPRPPSPDO : record // SPRPPS Programmable Power Supply APDO
                   MaximumCurrentIn50mA         : T7BITS;
                   Reserved1                    : T1BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved2                    : T1BITS;
                   MaximumVoltageIn100mV        : T8BITS;
                   Reserved3                    : T2BITS;
                   PpsPowerLimited              : T1BITS;
                   AugmentedPowerDataObjectType : T2BITS;  // 0x00=SPRPPS Programmable Power Supply
                   AugmentedPowerDataObject     : T2BITS;  // 0x11
                 end
              );
          5 : (  EPRAVSPDO : record // EPRAVS Adjustable Voltage Supply APDO
                   PDPInW                       : T8BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved                     : T1BITS;
                   MaximumVoltageIn100mV        : T9BITS;
                   PeakCurrent                  : T2BITS;
                   AugmentedPowerDataObjectType : T2BITS;  // 0x01=EPRAVS Adjustable Voltage Supply
                   AugmentedPowerDataObject     : T2BITS;  // 0x11
                 end
              );
          6 : (  SPRAVSPDO : record // SPRPPS Adjustable Voltage Supply APDO
                   MaximumCurrentIn10mA15V20V   : T10BITS;
                   MaximumCurrentIn10mA9V15V    : T10BITS;
                   Reserved                     : T6BITS;
                   PeakCurrent                  : T2BITS;
                   AugmentedPowerDataObjectType : T2BITS;  // 0x10=SPRPPS Adjustable Voltage Supply
                   AugmentedPowerDataObject     : T2BITS;  // 0x11
                 end
              );
          7 : (  GenericPdo : record
                   PDO                          : T30BITS;
                   SupplyType                   : T2BITS;  // 0x00...0x10
                 end
                 );
          8 : (  GenericAPdo : record
                   PDO                          : T28BITS;
                   APOType                      : T2BITS;
                   SupplyType                   : T2BITS;   // 0x11
                 end
                 );
          9 : (
               Bits            : bitpacked array[0..31] of T1BITS;
              );
         10 : (
               Bytes           : bitpacked array[0..3] of byte;
              );
         11 : (
                NamedWords: packed record
                   LSW     : word;
                   HSW     : word;
                end
              );
         11 : (
               Raw             : DWord;
              );

  end;

  TSINKPDO = bitpacked record
      case integer of
          1 : (  FixedSupplyPdo : record
                   OperationalCurrentIn10mA : T10BITS;
                   VoltageIn50mV            : T10BITS;
                   Reserved                 : T3BITS;
                   FastRoleSwap             : T2BITS;
                   DualRoleData             : T1BITS;
                   UsbCommunicationCapable  : T1BITS;
                   UnconstrainedPower       : T1BITS;
                   HigherCapability         : T1BITS;
                   DualRolePower            : T1BITS;
                   FixedSupply              : T2BITS;  // 0x00
                 end
              );
          2 : (  BatterySupplyPdo : record
                   MaximumAllowablePowerIn250mW  : T10BITS;
                   MinimumVoltageIn50mV          : T10BITS;
                   MaximumVoltageIn50mV          : T10BITS;
                   Battery                       : T2BITS;  // 0x01
                 end
              );
          3 : (  VariableSupplyNonBatteryPdo : record
                   OperationalCurrentIn10mA  : T10BITS;
                   MinimumVoltageIn50mV      : T10BITS;
                   MaximumVoltageIn50mV      : T10BITS;
                   VariableSupportNonBattery : T2BITS;  // 0x10
                 end
              );

          4 : (  SPRPPSPDO : record
                   MaximumCurrentIn50mA         : T7BITS;
                   Reserved1                    : T1BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved2                    : T1BITS;
                   MaximumVoltageIn100mV        : T8BITS;
                   Reserved3                    : T3BITS;
                   AugmentedPowerDataObjectType : T2BITS;  // 0x00
                   AugmentedPowerDataObject     : T2BITS;  // 0x11
                 end
              );

          5 : (  EPRAVSPDO : record
                   PDPInW                       : T8BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved1                    : T1BITS;
                   MaximumVoltageIn100mV        : T9BITS;
                   Reserved2                    : T2BITS;
                   AugmentedPowerDataObjectType : T2BITS; // 0x01
                   AugmentedPowerDataObject     : T2BITS; // 0x11
                 end
              );

          6 : (  GenericPdo : record
                   PDO                          : T30BITS;
                   SupplyType                   : T2BITS;
                 end
                 );
          7 : (  GenericAPdo : record
                   PDO                          : T28BITS;
                   APOType                      : T2BITS;
                   SupplyType                   : T2BITS;   // 0x11
                 end
                 );
          8 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          9 : (
                Bytes          : bitpacked array[0..3] of byte;
               );
         10 : (
               Raw             : DWord;
              );
  end;

  TPDREQUEST = bitpacked record
      case integer of
          1 : (  STANDARD : record
                   MaximumOperatingCurrentIn10mA : T10BITS;
                   OperatingCurrentIn10mA        : T10BITS;
                   Reserved1                     : T2BITS;
                   EPRModeCapable                : T1BITS;
                   UnchunkedMessagesSupport      : T1BITS;
                   NoUSBsuspend                  : T1BITS;
                   UsbCommunicationCapable       : T1BITS;
                   CapabilityMismatch            : T1BITS;
                   GiveBackFlag                  : T1BITS;
                   ObjectPosition                : T4BITS;
                 end
              );
          2 : (  BATTERY : record
                   MaximumOperatingPowerIn250mW  : T10BITS;
                   OperatingPowerIn250mW         : T10BITS;
                   Reserved1                     : T2BITS;
                   EPRModeCapable                : T1BITS;
                   UnchunkedMessagesSupported    : T1BITS;
                   NoUSBsuspend                  : T1BITS;
                   UsbCommunicationCapable       : T1BITS;
                   CapabilityMismatch            : T1BITS;
                   GiveBackFlag                  : T1BITS;
                   ObjectPosition                : T4BITS;
                 end
              );
          3 : (  PPS : record
                   OperatingCurrentIn50mA             : T7BITS;
                   Reserved1                          : T2BITS;
                   OutputVoltageIn20mV                : T12BITS;
                   Reserved2                          : T1BITS;
                   EPRModeCapable                     : T1BITS;
                   UnchunkedExtendedMessagesSupported : T1BITS;
                   NoUSBsuspend                       : T1BITS;
                   UsbCommunicationCapable            : T1BITS;
                   CapabilityMismatch                 : T1BITS;
                   Reserved4                          : T1BITS;
                   ObjectPosition                     : T4BITS;
                 end
              );
          4 : (  AVS : record
                   OperatingCurrentIn50mA             : T7BITS;
                   Reserved1                          : T2BITS;
                   Reserved2                          : T2BITS;
                   OutputVoltageIn100mV               : T10BITS;
                   Reserved3                          : T1BITS;
                   EPRModeCapable                     : T1BITS;
                   UnchunkedExtendedMessagesSupported : T1BITS;
                   NoUSBsuspend                       : T1BITS;
                   UsbCommunicationCapable            : T1BITS;
                   CapabilityMismatch                 : T1BITS;
                   Reserved4                          : T1BITS;
                   ObjectPosition                     : T4BITS;
                 end
              );

          5 : (  GENERIC : record
                   RDODATA                            : T22BITS;
                   EPRModeCapable                     : T1BITS;
                   UnchunkedExtendedMessagesSupported : T1BITS;
                   NoUSBsuspend                       : T1BITS;
                   UsbCommunicationCapable            : T1BITS;
                   CapabilityMismatch                 : T1BITS;
                   Reserved5                          : T1BITS;
                   ObjectPosition                     : T4BITS;
                 end
                 );

          6 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          7 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          8 : (
                 NamedWords: packed record
                    LSW     : word;
                    HSW     : word;
                 end
               );
          9 : (
               Raw             : DWord;
              );
  end;

  TVDMHEADER = bitpacked record
      case integer of
          1 : (  StructuredVDM : record
                   Command                     : T5BITS;
                   Reserved1                   : T1BITS;
                   CommandType                 : T2BITS;
                   ObjectPosition              : T3BITS;
                   StructuredVDMVersionMinor   : T2BITS;
                   StructuredVDMVersionMajor   : T2BITS;
                   VDMType                     : T1BITS;
                   SVID                        : T16BITS;
                 end
              );
          2 : (  UnstructuredVDM : record
                   Reserved             : T15BITS;
                   VDMType              : T1BITS;
                   VID                  : T16BITS;
                 end
              );
          3 : (  GenericVDM : record
                   Reserved             : T15BITS;
                   VDMType              : T1BITS;
                   VID                  : T16BITS;
                 end
                 );
          4 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          5 : (
                Bytes           : bitpacked array[0..3] of byte;
               );
          6 : (
               Raw             : DWord;
              );
  end;

  TEPRMODE = bitpacked record
      case integer of
          1 :(
               Data : record
                   Reserved                       : T16BITS;
                   Data                           : T8BITS;
                   Action                         : T8BITS;
               end;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          4 : (
               Raw             : DWord;
              );
  end;


  TVDOIDHEADER = bitpacked record
      case integer of
          1 : (
                   VID                            : T16BITS;
                   Reserved                       : T5BITS;
                   ConnectorType                  : T2BITS;
                   SOPProductTypeDFP              : T3BITS;
                   ModalOperationSupported        : T1BITS;
                   SOPProductTypeUFP              : T3BITS;
                   USBCommunicationsCapableDevice : T1BITS;
                   USBCommunicationsCapableHost   : T1BITS;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          4 : (
               Raw             : DWord;
              );
  end;

  TVDOCERTSTATHEADER = bitpacked record
      case integer of
          1 : (
               XID             : T32BITS;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          4 : (
               Raw             : DWord;
              );
  end;

  TVDOPRODUCTHEADER = bitpacked record
      case integer of
          1 : (
                   bcdDevice   : T16BITS;
                   PID         : T16BITS;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          4 : (
               Raw             : DWord;
              );
  end;

  TVDOPRODUCTTYPEHEADER = bitpacked record
      case integer of
          1 : (
                 UFPVDO : record
                   USBHighestSpeed    : T3BITS;
                   AlternateModes     : T3BITS;
                   VBUSRequired       : T1BITS;
                   VCONNRequired      : T1BITS;
                   VCONNPower         : T3BITS;
                   Reserved1          : T11BITS;
                   ConnectorType      : T2BITS;
                   DeviceCapability   : T4BITS;
                   Reserved2          : T1BITS;
                   UFPVDOVersion      : T3BITS;
                 end;
              );
          2 : (
                 DFPVDO : record
                   PortNumber         : T5BITS;
                   Reserved1          : T17BITS;
                   ConnectorType      : T2BITS;
                   HostCapability     : T3BITS;
                   Reserved2          : T2BITS;
                   DFPVDOVersion      : T3BITS;
                 end;
              );
          3 : (
                 PassiveCableVDO : record
                   USBHighestSpeed    : T3BITS;
                   Reserved1          : T2BITS;
                   CurrentHandling    : T2BITS;
                   Reserved2          : T2BITS;
                   MaximumVoltage     : T2BITS;
                   CableTermination   : T2BITS;
                   CableLatency       : T4BITS;
                   EPRModeCapable     : T1BITS;
                   ConnectorType      : T2BITS;
                   Reserved3          : T1BITS;
                   VDOVersion         : T3BITS;
                   FWVersion          : T4BITS;
                   HWVersion          : T4BITS;
                 end;
              );
          4 : (
                 ActiveCableVDO : record
                   USBHighestSpeed    : T3BITS;
                   SOP2Present        : T1BITS;
                   VBUSThroughCable   : T1BITS;
                   CurrentHandling    : T2BITS;
                   SBUType            : T1BITS;
                   SBUSupported       : T1BITS;
                   MaximumVoltage     : T2BITS;
                   CableTermination   : T2BITS;
                   CableLatency       : T4BITS;
                   EPRModeCapable     : T1BITS;
                   ConnectorType      : T2BITS;
                   Reserved           : T1BITS;
                   VDOVersion         : T3BITS;
                   FWVersion          : T4BITS;
                   HWVersion          : T4BITS;
                 end;
              );
          5 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          6 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          7 : (
               Raw             : DWord;
              );
  end;

  TVDOCABLEHEADER = bitpacked record
      case integer of
          1 : (
             PassiveCableVDO : record
               USBHighestSpeed    : T3BITS;
               Reserved1          : T2BITS;
               CurrentHandling    : T2BITS;
               Reserved2          : T2BITS;
               MaximumVoltage     : T2BITS;
               CableTermination   : T2BITS;
               CableLatency       : T4BITS;
               EPRModeCapable     : T1BITS;
               Captive            : T2BITS;
               Reserved3          : T1BITS;
               VDOVersion         : T3BITS;
               FWVersion          : T4BITS;
               HWVersion          : T4BITS;
             end;
          );
          2 : (
             ActiveCableVDO : record
               USBHighestSpeed    : T3BITS;
               SOP2Present        : T1BITS;
               VBUSThroughCable   : T1BITS;
               CurrentHandling    : T2BITS;
               SBUType            : T1BITS;
               SBUSupported       : T1BITS;
               MaximumVoltage     : T2BITS;
               CableTermination   : T2BITS;
               CableLatency       : T4BITS;
               EPRModeCapable     : T1BITS;
               ConnectorType      : T2BITS;
               Reserved           : T1BITS;
               VDOVersion         : T3BITS;
               FWVersion          : T4BITS;
               HWVersion          : T4BITS;
             end;
          );
          3 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          4 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          5 : (
               Raw             : DWord;
              );
  end;

  TSOURCECAPSEXTENDED = packed record
      case byte of
          1 : (
               Data : packed record
                   VID                    : WORD;
                   PID                    : WORD;
                   XID                    : DWORD;
                   FW_Version             : BYTE;
                   HW_Version             : BYTE;
                   Voltage_Regulation     : BYTE;
                   Holdup_Time            : BYTE;
                   Compliance             : BYTE;
                   Touch_Current          : BYTE;
                   Peak_Current1          : WORD;
                   Peak_Current2          : WORD;
                   Peak_Current3          : WORD;
                   Touch_Temp             : BYTE;
                   Source_Inputs          : BYTE;
                   Batteries              : BYTE;
                   Source_PDP             : BYTE;
                 end;
              );
          2 : (
               Bytes           : bitpacked array[0..23] of byte;
               );
          3 : (
               DWords          : packed array[0..5] of TDWordData;
               );
  end;

  TSINKCAPSEXTENDED = packed record
      case byte of
          1 : (
               Data : packed record
                   VID                             : WORD;
                   PID                             : WORD;
                   XID                             : DWORD;
                   FW_Version                      : BYTE;
                   HW_Version                      : BYTE;
                   SKEDB_Version                   : BYTE;
                   Load_Step                       : BYTE;
                   Sink_Load_Characteristics       : WORD;
                   Compliance                      : BYTE;
                   Touch_Temp                      : BYTE;
                   Battery_Info                    : BYTE;
                   Sink_Modes                      : BYTE;
                   Sink_Minimum_PDP                : BYTE;
                   Sink_Operational_PDP            : BYTE;
                   Sink_Maximum_PDP                : BYTE;
                   EPR_Sink_Minimum_PDP            : BYTE;
                   EPR_Sink_Operational_PDP        : BYTE;
                   EPR_Sink_Maximum_PDP            : BYTE;
                 end;
              );
          2 : (
               Bytes           : bitpacked array[0..23] of byte;
               );
          3 : (
               DWords          : packed array[0..5] of TDWordData;
               );
  end;

  TPDHEADER = bitpacked record
      case integer of
          1 : (
               Data : record
                     Message_Type              : T5BITS;
                     Port_Data_Role            : T1BITS;
                     Specification_Revision    : T2BITS;
                     Port_Power_Role           : T1BITS;
                     MessageID                 : T3BITS;
                     Number_of_Data_Objects    : T3BITS;
                     Extended                  : T1BITS;
                 end;
              );
          2 : (
               Bits            : bitpacked array[0..15] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..1] of byte;
               );
          4 : (
               Raw             : Word;
              );
  end;

  TPDHEADEREXTENDED = bitpacked record
      case integer of
          1 : (
               Data : record
                   Data_Size                : T9BITS;
                   Reserved                 : T1BITS;
                   Request_Chunk            : T1BITS;
                   Chunk_Number             : T4BITS;
                   Chunked                  : T1BITS;
                 end;
              );
          2 : (
               Bits            : bitpacked array[0..15] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..1] of byte;
               );
          4 : (
               Raw             : Word;
              );
  end;

  TBATTSTATS = packed record
      case integer of
          1 : (
               Data : record
                     Reserved                 : T8BITS;
                     BatteryInfo              : T8BITS;
                     BatterySOC100mWh         : T16BITS;
                 end;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
  end;

  TBATTCAPS = packed record
      case byte of
          1 : (
               Data : packed record
                   VID                                       : WORD;
                   PID                                       : WORD;
                   BatteryDesignCapacity                     : WORD;
                   BatteryLastFullChargeCapacity             : WORD;
                   BatteryType                               : BYTE;
                 end;
              );
          2 : (
               Bytes           : bitpacked array[0..8] of byte;
              );
  end;

  TGBDB = bitpacked record
      case byte of
          1 : (
               Data : record
                 FixedBatteries                 : T2BITS;
                 HotSwappableBatteries          : T2BITS;
                 Reserved                       : T4BITS;
               end;
              );
          2 : (
               Raw : byte;
              );
  end;

  TSDB = bitpacked record
      case byte of
          1 : (
               Data : packed record
                   InternalTemp              : BYTE;
                   PresentInput              : BYTE;
                   PresentBatteryInput       : BYTE;
                   EventFlags                : BYTE;
                   TemperatureStatus         : BYTE;
                   PowerStatus               : BYTE;
                   PowerStateChange          : BYTE;
                 end;
              );
          2 : (
               Bytes           : bitpacked array[0..6] of byte;
              );
  end;

  TSIDO = packed record
      case integer of
          1 : (
               Data : record
                     PortReportedPDP  : T8BITS;
                     PortPresentPDP   : T8BITS;
                     PortMaximumPDP   : T8BITS;
                     Reserved         : T7BITS;
                     PortType         : T1BITS;
                 end;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          4 : (
               Raw             : DWord;
              );
  end;

  TPPSSDB = packed record
      case integer of
          1 : (
               Data : record
                     OutputVoltage20mV  : word;
                     OutputCurrent50mA  : byte;
                     RealTimeFlags      : byte;
                 end;
              );
          2 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          3 : (
               Raw             : DWord;
              );
  end;

  TEPRMDO = packed record
      case integer of
          1 : (
               Data : record
                     Action    : T8BITS;
                     Data      : T8BITS;
                     Reserved  : T16BITS;
                 end;
              );
          2 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          3 : (
               Bytes           : bitpacked array[0..3] of byte;
               );
          4 : (
               Raw             : DWord;
              );
  end;

  TECDB = packed record
       case integer of
           1 : (
                Data : record
                      MessageType  : T8BITS;
                      Data         : T8BITS;
                  end;
               );
           2 : (
                Bits            : bitpacked array[0..15] of T1BITS;
                );
           3 : (
                Bytes           : bitpacked array[0..1] of byte;
                );
           4 : (
                Raw             : Word;
               );
   end;


  TUSBPD_CONTROLMSG =
  (
    USBPD_CONTROLMSG_RESERVED0,
    USBPD_CONTROLMSG_GOODCRC,
    USBPD_CONTROLMSG_GOTOMIN,
    USBPD_CONTROLMSG_ACCEPT,
    USBPD_CONTROLMSG_REJECT,
    USBPD_CONTROLMSG_PING,
    USBPD_CONTROLMSG_PS_RDY,
    USBPD_CONTROLMSG_GET_SRC_CAP,
    USBPD_CONTROLMSG_GET_SNK_CAP,
    USBPD_CONTROLMSG_DR_SWAP,
    USBPD_CONTROLMSG_PR_SWAP,
    USBPD_CONTROLMSG_VCONN_SWAP,
    USBPD_CONTROLMSG_WAIT,
    USBPD_CONTROLMSG_SOFT_RESET,
    USBPD_CONTROLMSG_DATA_RESET,
    USBPD_CONTROLMSG_DATA_RESET_COMPLETE,
    USBPD_CONTROLMSG_NOT_SUPPORTED,
    USBPD_CONTROLMSG_GET_SRC_CAPEXT,
    USBPD_CONTROLMSG_GET_STATUS,
    USBPD_CONTROLMSG_FR_SWAP,
    USBPD_CONTROLMSG_GET_PPS_STATUS,
    USBPD_CONTROLMSG_GET_COUNTRY_CODES,
    USBPD_CONTROLMSG_GET_SNK_CAPEXT,
    USBPD_CONTROLMSG_GET_SOURCE_INFO,
    USBPD_CONTROLMSG_GET_REVISION
  );

  TUSBPD_DATAMSG =
  (
    USBPD_DATAMSG_RESERVED0,
    USBPD_DATAMSG_SRC_CAPABILITIES,
    USBPD_DATAMSG_REQUEST,
    USBPD_DATAMSG_BIST,
    USBPD_DATAMSG_SNK_CAPABILITIES,
    USBPD_DATAMSG_BATTERY_STATUS,
    USBPD_DATAMSG_ALERT,
    USBPD_DATAMSG_GET_COUNTRY_INFO,
    USBPD_DATAMSG_ENTER_USB,
    USBPD_DATAMSG_EPR_REQUEST,
    USBPD_DATAMSG_EPR_MODE,
    USBPD_DATAMSG_SOURCE_INFO,
    USBPD_DATAMSG_REVISION,
    USBPD_DATAMSG_RESERVED13,
    USBPD_DATAMSG_RESERVED14,
    USBPD_DATAMSG_VENDOR_DEFINED
  );

  TUSBPD_EXTENDEDMSG =
  (
    USBPD_EXTMSG_RESERVED0,
    USBPD_EXTMSG_SOURCE_CAPABILITIES_EXTENDED,
    USBPD_EXTMSG_STATUS,
    USBPD_EXTMSG_GET_BATTERY_CAP,
    USBPD_EXTMSG_GET_BATTERY_STATUS,
    USBPD_EXTMSG_BATTERY_CAPABILITIES,
    USBPD_EXTMSG_GET_MANUFACTURER_INFO,
    USBPD_EXTMSG_MANUFACTURER_INFO,
    USBPD_EXTMSG_SECURITY_REQUEST,
    USBPD_EXTMSG_SECURITY_RESPONSE,
    USBPD_EXTMSG_FIRMWARE_UPDATE_REQUEST,
    USBPD_EXTMSG_FIRMWARE_UPDATE_RESPONSE,
    USBPD_EXTMSG_PPS_STATUS,
    USBPD_EXTMSG_COUNTRY_INFO,
    USBPD_EXTMSG_COUNTRY_CODES,
    USBPD_EXTMSG_SINK_CAPABILITIES_EXTENDED,
    USBPD_EXTMSG_EXTENDED_CONTROL,
    USBPD_EXTMSG_EPR_SOURCE_CAPABILITIES,
    USBPD_EXTMSG_EPR_SINK_CAPABILITIES,
    USBPD_EXTMSG_RESERVED19,
    USBPD_EXTMSG_RESERVED20,
    USBPD_EXTMSG_RESERVED21,
    USBPD_EXTMSG_RESERVED22,
    USBPD_EXTMSG_RESERVED23,
    USBPD_EXTMSG_RESERVED24,
    USBPD_EXTMSG_RESERVED25,
    USBPD_EXTMSG_RESERVED26,
    USBPD_EXTMSG_RESERVED27,
    USBPD_EXTMSG_RESERVED28,
    USBPD_EXTMSG_RESERVED29,
    USBPD_EXTMSG_VENDOR_DEFINED_EXTENDED,
    USBPD_EXTMSG_RESERVED31
  );

 TVDMCommands =
 (
    Reserved,
    DiscoverIdentity,
    DiscoverSVIDs,
    DiscoverModes,
    EnterMode,
    ExitMode,
    Attention
 );

 TUSBPD_CONTROLMSG_DATA = record
   Name:string;
   Color:TColor;
 end;

 TUSBPD_SOPTYPE =
 (
   //PD_SOP_PRIME
   SOP,
   SOP1,
   SOP2,
   SOP1_DEBUG,
   SOP2_DEBUG,
   HARD_RESET,
   CABLE_RESET,
   BIST_MODE_2,
   INVALID = $FF
 );

 TUSBPD_ECMTYPE =
 (
   ECM_RESERVED,
   GETSOURCECAP,
   GETSINKCAP,
   KEEPALIVESINK,
   KEEPALIVEACK
 );

 TDataBuffer = record
    Data: array[0..511] of Byte;  // enough for max extended message
    TotalSize: Integer;
    ReceivedBytes: Integer;
    ChunkPayloadLen: integer;
    NextChunkNum: Integer;
    IsComplete: Boolean;
  end;

const
  CCNONE                          = 0;
  CC1                             = 1;
  CC2                             = 2;

  USBPD_CABLEPLUG_FROMDFPUFP      = 0;
  USBPD_PORTPOWERROLE_SNK         = USBPD_CABLEPLUG_FROMDFPUFP;
  USBPD_CABLEPLUG_FROMCABLEPLUG   = 1;
  USBPD_PORTPOWERROLE_SRC         = USBPD_CABLEPLUG_FROMCABLEPLUG;

  USBPD_EPRMDO_ACTION_ENTERSINK        = 1;
  USBPD_EPRMDO_ACTION_ENTERACK         = 2;
  USBPD_EPRMDO_ACTION_ENTERSUCCESS     = 3;
  USBPD_EPRMDO_ACTION_ENTERFAILED      = 4;
  USBPD_EPRMDO_ACTION_EXITSINKSOURCE   = 5;


  //https://github.com/JohnScotttt/witrn_pd_sniffer/blob/main/witrn_pd_sniffer.py

  TUSBPD_CONTROLMSG_DATAS : array[TUSBPD_CONTROLMSG] of TUSBPD_CONTROLMSG_DATA =
  (
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'GoodCRC'; Color : $fffde4),
    (Name : 'GotoMin'; Color : $e8a8ee),
    (Name : 'Accept'; Color : $bfffca),
    (Name : 'Reject'; Color : $7777ec),
    (Name : 'Ping'; Color : $78c1ad),
    (Name : 'PS_RDY'; Color : $d4f0ff),
    (Name : 'Get_Source_Cap'; Color : $f19a94),
    (Name : 'Get_Sink_Cap'; Color : $ffb6a0),
    (Name : 'DR_Swap'; Color : $ffbf00),
    (Name : 'PR_Swap'; Color : $e49342),
    (Name : 'VCONN_Swap'; Color : $ffa7ff),
    (Name : 'Wait'; Color : $ab8fff),
    (Name : 'Soft_Reset'; Color : $ac96da),
    (Name : 'Data_Reset'; Color : $eeeeaf),
    (Name : 'Data_Reset_Complete'; Color : $79a2db),
    (Name : 'Not_Supported'; Color : $a9a9a9),
    (Name : 'Get_Source_Cap_Extended'; Color : $6bb7bd),
    (Name : 'Get_Status'; Color : $d884d8),
    (Name : 'FR_Swap'; Color : $2f6b55),
    (Name : 'Get_PPS_Status'; Color : $008cff),
    (Name : 'Get_Country_Codes'; Color : $f1addb),
    (Name : 'Get_Sink_Cap_Extended'; Color : $7676eb),
    (Name : 'Get_Source_Info'; Color : $7a96e9),
    (Name : 'Get_Revision'; Color : $8fbc8f)
  );

  TUSBPD_DATAMSG_DATAS : array[TUSBPD_DATAMSG] of TUSBPD_CONTROLMSG_DATA =
  (
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Source_Capabilities'; Color : $ffc4ab),
    (Name : 'Request'; Color : $ffc6ff),
    (Name : 'BIST'; Color : $56DABD),
    (Name : 'Sink_Capabilities'; Color : $aab220),
    (Name : 'Battery_Status'; Color : $c0b6ff),
    (Name : 'Alert'; Color : $eeeeaf),
    (Name : 'Get_Country_Info'; Color : $e0ffff),
    (Name : 'Enter_USB'; Color : $92ba92),
    (Name : 'EPR_Request'; Color : $ffc6ff),
    (Name : 'EPR_Mode'; Color : $92ba92),
    (Name : 'Source_Info'; Color : $d4f0ff),
    (Name : 'Revision'; Color : $d2eaf0),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Vendor_Defined'; Color : $ffb2bd)
  );

  TUSBPD_EXTENDEDMSG_DATAS : array[TUSBPD_EXTENDEDMSG] of TUSBPD_CONTROLMSG_DATA =
  (
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Source_Capabilities_Extended'; Color : $d4e0b8),
    (Name : 'Status'; Color : $d8bfd8),
    (Name : 'Get_Battery_Cap'; Color : $7f90f3),
    (Name : 'Get_Battery_Status'; Color : $d0e040),
    (Name : 'Battery_Capabilities'; Color : $b4e5dd),
    (Name : 'Get_Manufacturer_Info'; Color : $b3def5),
    (Name : 'Manufacturer_Info'; Color : $c0c0c0),
    (Name : 'Security_Request'; Color : $32cd9a),
    (Name : 'Security_Response'; Color : $d670da),
    (Name : 'Firmware_Update_Request'; Color : $8cb4d2),
    (Name : 'Firmware_Update_Response'; Color : $7fff00),
    (Name : 'PPS_Status'; Color : $7CE97C),
    (Name : 'Country_Info'; Color : $cd5a6a),
    (Name : 'Country_Codes'; Color : $ebce87),
    (Name : 'Sink_Capabilities_Extended'; Color : $ee82ee),
    (Name : 'Extended_Control'; Color : $7197e9),
    (Name : 'EPR_Source_Capabilities'; Color : $d19981),
    (Name : 'EPR_Sink_Capabilities'; Color : $60a4f4),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Reserved'; Color : $7280fa),
    (Name : 'Vendor_Defined_Extended'; Color : $ffb2bd),
    (Name : 'Reserved'; Color : $7280fa)
  );


type
  TUSBPD = class
    NumberSRCPDO: dword;
    NumberSNKPDO: dword;

    NumberEPRSRCPDO: dword;
    NumberEPRSNKPDO: dword;

    ActiveSRCPDO: dword;
    ActiveSNKPDO: dword;

    SourcePDOs: array[1..MAXPDO] of TSOURCEPDO;
    SinkPDOs: array[1..MAXPDO] of TSINKPDO;

    SourceEPRPDOs: array[1..MAXEPRPDO] of TSOURCEPDO;
    SinkEPRPDOs: array[1..MAXEPRPDO] of TSINKPDO;

    SRCExtended:TSOURCECAPSEXTENDED;
    SNKExtended:TSINKCAPSEXTENDED;

    BatteryCaps:TBATTCAPS;
    BatteryStatus:TBATTSTATS;
    NBBatteries:dword;

    //GBCDB  : TGBDB;
    //GBSDB  : TGBDB;
    SDB    : TSDB;

    EPRMODE: TEPRMODE;

    SIDO   : TSIDO;
    PPSSDB : TPPSSDB;

    Cable:TVDOCABLEHEADER;

    ActiveCCIs:dword;
    PowerRole:dword;
    DefaultPower:dword;
    DataRole:dword;
    Vconn:dword;
    PDSpecRevision:dword;

    RDO:TPDREQUEST;
    RDOPosition:dword;
    RDOPositionPrevious:dword;
    RV:dword;
    RI:dword;
    RP:dword;

    VDM_Header:TVDMHEADER;
    VDO_ID:TVDOIDHEADER;
    VDO_Cert:TVDOCERTSTATHEADER;
    VDO_Product:TVDOPRODUCTHEADER;
    VDO_ProductType:TVDOPRODUCTTYPEHEADER;

    PDDATA  : TDataBuffer;

    function GetSRCPDOInfo(aSRCPDO:byte):string;
    function GetSNKPDOInfo(aSNKPDO:byte):string;

    function GetFixed5VSRCPDO:TSOURCEPDO;

    function GetRDOInfo:string;

    function GetSRCExtendedInfo:string;
    function GetSNKExtendedInfo:string;

    function GetStatusPresentInputInfo:string;
    function GetStatusTemperatureStatusInfo:string;
    function GetStatusPowerStateChangeInfo:string;
    function EPRModeInfo:string;

    function GetCableInfo:string;

    function GetVID(aVID:word):string;

    function ProcessExtendedMessage(aMSG:TUSBPD_EXTENDEDMSG; NumberOfBytes:byte; data:PByteArray):boolean;
    function ProcessDataMessage(aMSG:TUSBPD_DATAMSG; NumberOfDataObjects:byte; data:PByteArray):boolean;
    procedure ProcessRawPDMessage(HEADER: TPDHEADER; EXTHEADER: TPDHEADEREXTENDED; LOCALDATA:PByteArray);

    procedure Cleanup;
  end;

  function GetSOPInfo(aSOPHeader:TPDHEADER):string;
  function SRCPDOInfo(aPDO:TSOURCEPDO):string;
  function SNKPDOInfo(aPDO:TSINKPDO):string;

implementation


uses
  vids;

function GetSOPInfo(aSOPHeader:TPDHEADER):string;
var
  s:string;
  enumname:string;
begin
  s:='';

  // Do we have a control message
  if ((aSOPHeader.Data.Number_of_Data_Objects=0) AND (aSOPHeader.Data.Extended=0)) then
  begin
    if (aSOPHeader.Data.Message_Type>Ord(USBPD_CONTROLMSG_GET_REVISION)) then
      enumname:='Unknown control message: '
    else
    begin
      Str(TUSBPD_CONTROLMSG(aSOPHeader.Data.Message_Type),enumname);
      if TUSBPD_CONTROLMSG(aSOPHeader.Data.Message_Type)=TUSBPD_CONTROLMSG.USBPD_CONTROLMSG_GOODCRC then
      begin
        s:=s+'CRC';
      end;
    end;
    s:=s+enumname+': ';
  end
  else
  begin
    if (aSOPHeader.Data.Extended=0) then
    begin
      if (aSOPHeader.Data.Message_Type>Ord(USBPD_DATAMSG_VENDOR_DEFINED)) then
        enumname:='Unknown data message: '
      else
        Str(TUSBPD_DATAMSG(aSOPHeader.Data.Message_Type),enumname);
      s:=s+enumname+': ';
    end;
    if (aSOPHeader.Data.Extended=1) then
    begin
      if (aSOPHeader.Data.Message_Type>Ord(USBPD_EXTMSG_RESERVED31)) then
        enumname:='Unknown extended message: '
      else
        Str(TUSBPD_EXTENDEDMSG(aSOPHeader.Data.Message_Type),enumname);
      s:=s+enumname+': ';
    end;
  end;

  case aSOPHeader.Data.Port_Power_Role of
    0:s:=s+'Sink.'+#13#10;
    1:s:=s+'Source.'+#13#10;
  end;
  case aSOPHeader.Data.Specification_Revision of
    0:s:=s+'Revision 1.0'+#13#10;
    1:s:=s+'Revision 2.0'+#13#10;
    2:s:=s+'Revision 3.0'+#13#10;
  end;
  case aSOPHeader.Data.Port_Data_Role of
    0:s:=s+'UFP.'+#13#10;
    1:s:=s+'DFP.'+#13#10;
  end;
  s:=s+'Data length: '+InttoStr(aSOPHeader.Data.Number_of_Data_Objects)+#13#10;

  result:=s;
end;

function SRCPDOInfo(aPDO:TSOURCEPDO):string;
var
  aPDOType:TSUPPLY_TYPES;
  s:string;
begin
  aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.SupplyType);

  s:='Source PDO type: '+SUPPLY_TYPES[aPDOType]+'. ';

  case TSUPPLY_TYPES(aPDO.GenericPdo.SupplyType) of
    TSUPPLY_TYPES.Fixed:
    begin
      with aPDO.FixedSupplyPdo do
      begin
        s:=s+'Operational Current: '+InttoStr(MaximumCurrentIn10mA*10)+'mA. ';
        s:=s+'Voltage: '+InttoStr(VoltageIn50mV*50)+'mV';
      end;
    end;
    TSUPPLY_TYPES.Variable:
    begin
      with aPDO.VariableSupplyNonBatteryPdo do
      begin
        s:=s+'Operational Current: '+InttoStr(MaximumCurrentIn10mA*10)+'mA. ';
        s:=s+'Min voltage: '+InttoStr(MinimumVoltageIn50mV*50)+'mV';
        s:=s+'Max voltage: '+InttoStr(MaximumVoltageIn50mV*50)+'mV';
      end;
    end;
    TSUPPLY_TYPES.APDO:
    begin
      with aPDO.SPRPPSPDO do
      begin
        s:=s+'Operational Current: '+InttoStr(MaximumCurrentIn50mA*50)+'mA. ';
        s:=s+'Min voltage: '+InttoStr(MinimumVoltageIn100mV*100)+'mV';
        s:=s+'Max voltage: '+InttoStr(MaximumVoltageIn100mV*100)+'mV';
      end;
    end;
  end;

  result:=s;
end;

function SNKPDOInfo(aPDO:TSINKPDO):string;
var
  aPDOType:TSUPPLY_TYPES;
  s:string;
begin
  aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.SupplyType);

  s:='Sink PDO type: '+SUPPLY_TYPES[aPDOType]+'. ';

  case aPDOType of
    TSUPPLY_TYPES.Fixed:
    begin
      with aPDO.FixedSupplyPdo do
      begin
        s:=s+'Current: '+InttoStr(OperationalCurrentIn10mA*10)+'mA. ';
        s:=s+'Voltage: '+InttoStr(VoltageIn50mV*50)+'mV';
      end;
    end;
    TSUPPLY_TYPES.Variable:
    begin
      with aPDO.VariableSupplyNonBatteryPdo do
      begin
        s:=s+'Current: '+InttoStr(OperationalCurrentIn10mA*10)+'mA. ';
        s:=s+'Min voltage: '+InttoStr(MinimumVoltageIn50mV*50)+'mV';
        s:=s+'Max voltage: '+InttoStr(MaximumVoltageIn50mV*50)+'mV';
      end;
    end;
  end;

  result:=s;
end;

function TUSBPD.GetSRCPDOInfo(aSRCPDO:byte):string;
begin
  result:=SRCPDOInfo(SourcePDOs[aSRCPDO]);
end;

function TUSBPD.GetSNKPDOInfo(aSNKPDO:byte):string;
begin
  result:=SNKPDOInfo(SinkPDOs[aSNKPDO]);
end;

function TUSBPD.GetRDOInfo:string;
var
  s:string;
  //aPDOType:TSUPPLY_TYPES;
begin
  //aPDOType:=TSUPPLY_TYPES(RDO.GENERIC.Supply);

  s:='RDO info: ';
  s:=s+'Current: '+InttoStr(RDO.STANDARD.OperatingCurrentIn10mA*10)+'mA. ';
  s:=s+'Max current: '+InttoStr(RDO.STANDARD.MaximumOperatingCurrentIn10mA*10)+'mA. ';
  s:=s+'Position: '+InttoStr(RDO.STANDARD.ObjectPosition);
  result:=s;
end;

function TUSBPD.GetSRCExtendedInfo:string;
var
  s:string;
begin

  s:=s+'VID: '+InttoStr(SRCExtended.Data.VID)+#13#10;
  s:=s+'PID: '+InttoStr(SRCExtended.Data.PID)+#13#10;
  s:=s+'XID: '+InttoStr(SRCExtended.Data.XID)+#13#10;
  s:=s+'FW_revision: '+InttoStr(SRCExtended.Data.FW_Version)+#13#10;
  s:=s+'HW_revision: '+InttoStr(SRCExtended.Data.HW_Version)+#13#10;
  s:=s+'Voltage_regulation: '+InttoStr(SRCExtended.Data.Voltage_Regulation)+#13#10;
  s:=s+'Holdup_time: '+InttoStr(SRCExtended.Data.Holdup_Time)+#13#10;
  s:=s+'Compliance: '+InttoStr(SRCExtended.Data.Compliance)+#13#10;
  s:=s+'TouchCurrent: '+InttoStr(SRCExtended.Data.Touch_Current)+#13#10;
  s:=s+'PeakCurrent1: '+InttoStr(SRCExtended.Data.Peak_Current1)+#13#10;
  s:=s+'PeakCurrent2: '+InttoStr(SRCExtended.Data.Peak_Current2)+#13#10;
  s:=s+'PeakCurrent3: '+InttoStr(SRCExtended.Data.Peak_Current3)+#13#10;
  s:=s+'Touchtemp: '+InttoStr(SRCExtended.Data.Touch_Temp)+#13#10;
  s:=s+'Source_inputs: '+InttoStr(SRCExtended.Data.Source_Inputs)+#13#10;
  s:=s+'NbBatteries: '+InttoStr(SRCExtended.Data.Batteries)+#13#10;
  s:=s+'SourcePDP: '+InttoStr(SRCExtended.Data.Source_PDP)+#13#10;

  result:=s;
end;

function TUSBPD.GetSNKExtendedInfo:string;
var
  s:string;
begin

  s:=s+'VID: '+InttoStr(SNKExtended.Data.VID)+#13#10;
  s:=s+'PID: '+InttoStr(SNKExtended.Data.PID)+#13#10;
  s:=s+'XID: '+InttoStr(SNKExtended.Data.XID)+#13#10;
  s:=s+'FW_revision: '+InttoStr(SNKExtended.Data.FW_Version)+#13#10;
  s:=s+'HW_revision: '+InttoStr(SNKExtended.Data.HW_Version)+#13#10;

  s:=s+'Compliance: '+InttoStr(SNKExtended.Data.Compliance)+#13#10;

  s:=s+'Touchtemp: '+InttoStr(SNKExtended.Data.Touch_Temp)+#13#10;

  result:=s;
end;

function TUSBPD.GetCableInfo:string;
var
  s,t:string;
begin
  s:='Cable info.'+#13#10;

  s:=s+'USBHighestSpeed : ';
  t:='unknown';
  case Cable.ActiveCableVDO.USBHighestSpeed of
    0 : t:='[USB 2.0] only';
    1 : t:='[USB 3.2] Gen1';
    2 : t:='[USB 3.2] / [USB4] Gen2';
    3 : t:='[USB4] Gen3';
    4 : t:='[USB4] Gen4';
  end;
  s:=s+t+#13#10;

  s:=s+'CurrentHandling : ';
  t:='unknown';
  case Cable.ActiveCableVDO.CurrentHandling of
    0:t:='USB Type-C® Default Current';
    1:t:='3A';
    2:t:='5A';
  end;
  s:=s+t+#13#10;

  s:=s+'MaximumVoltage : ';
  t:='unknown';
  case Cable.ActiveCableVDO.MaximumVoltage of
    0:t:='20V';
    1:t:='30V';
    2:t:='40V';
    3:t:='50V';
  end;
  s:=s+t+#13#10;

  s:=s+'ConnectorType : ';
  t:='unknown';
  case Cable.ActiveCableVDO.ConnectorType of
    2:t:='USB Type-C®';
    3:t:='Captive';
  end;
  s:=s+t+#13#10;

  s:=s+'VBUSThroughCable : '+InttoStr(Cable.ActiveCableVDO.VBUSThroughCable)+#13#10;
  s:=s+'CableTermination : '+InttoStr(Cable.ActiveCableVDO.CableTermination)+#13#10;
  s:=s+'CableLatency : '+InttoStr(Cable.ActiveCableVDO.CableLatency)+#13#10;
  s:=s+'EPRModeCapable : '+InttoStr(Cable.ActiveCableVDO.EPRModeCapable)+#13#10;
  s:=s+'VDOVersion : '+InttoStr(Cable.ActiveCableVDO.VDOVersion)+#13#10;
  s:=s+'FWVersion : '+InttoStr(Cable.ActiveCableVDO.FWVersion)+#13#10;
  s:=s+'HWVersion : '+InttoStr(Cable.ActiveCableVDO.HWVersion)+#13#10;

  result:=s;
end;

function TUSBPD.GetFixed5VSRCPDO:TSOURCEPDO;
begin
  result:=SourcePDOs[1];
end;

procedure TUSBPD.Cleanup;
var
  i:integer;
begin
  NumberSRCPDO:=0;
  NumberSNKPDO:=0;

  NumberEPRSRCPDO:=0;
  NumberEPRSNKPDO:=0;

  ActiveSRCPDO:=0;
  ActiveSNKPDO:=0;

  for i:=1 to MAXPDO do SourcePDOs[i].Raw:=0;
  for i:=1 to MAXPDO do SinkPDOs[i].Raw:=0;

  for i:=1 to MAXEPRPDO do SourceEPRPDOs[i].Raw:=0;
  for i:=1 to MAXEPRPDO do SinkEPRPDOs[i].Raw:=0;

  RDO.Raw:=0;

  for i:=0 to Pred(Length(SRCExtended.DWords)) do SRCExtended.DWords[i].Raw:=0;
  for i:=0 to Pred(Length(SNKExtended.DWords)) do SNKExtended.DWords[i].Raw:=0;

  for i:=0 to Pred(Length(BatteryCaps.Bytes)) do BatteryCaps.Bytes[i]:=0;
  for i:=0 to Pred(Length(BatteryStatus.Bytes)) do BatteryStatus.Bytes[i]:=0;

  NBBatteries:=0;

  //GBCDB.Raw:=0;
  //GBSDB.Raw:=0;
  for i:=0 to Pred(Length(SDB.Bytes)) do SDB.Bytes[i]:=0;
  SIDO.Raw:=0;

  PPSSDB.Raw:=0;

  Cable.Raw:=0;

  ActiveCCIs:=0;
  PowerRole:=0;
  DefaultPower:=0;
  DataRole:=0;
  Vconn:=0;
  PDSpecRevision:=0;

  RDOPosition:=0;
  RDOPositionPrevious:=0;

  RV:=0;
  RI:=0;
  RP:=0;

  VDM_Header.Raw:=0;
  VDO_ID.Raw:=0;
  VDO_Cert.Raw:=0;
  VDO_Product.Raw:=0;
  VDO_ProductType.Raw:=0;

  PDDATA:=Default(TDataBuffer);
end;

function TUSBPD.GetVID(aVID:word):string;
var
  VIDIndex:integer;
begin
  result:='Unknown';
  for VIDIndex:=0 to Pred(Length(VIDLIST)) do
  begin
    if aVID=VIDLIST[VIDIndex].VID then
    begin
      result:=VIDLIST[VIDIndex].Name;
      break;
    end;
  end;
end;

function TUSBPD.ProcessExtendedMessage(aMSG:TUSBPD_EXTENDEDMSG; NumberOfBytes:byte; data:PByteArray):boolean;
var
  i,j:integer;
  aPDO: TDWordData;
  enumname:string;
begin
  result:=true;
  case aMSG of
    USBPD_EXTMSG_STATUS:
    begin
      for j:=0 to Pred(Length(SDB.Bytes)) do SDB.Bytes[j]:=data^[j];
    end;
    USBPD_EXTMSG_SOURCE_CAPABILITIES_EXTENDED:
    begin
      for j:=0 to Pred(Length(SRCExtended.Bytes)) do SRCExtended.Bytes[j]:=data^[j];
    end;
    USBPD_EXTMSG_SINK_CAPABILITIES_EXTENDED:
    begin
      for j:=0 to Pred(Length(SNKExtended.Bytes)) do SNKExtended.Bytes[j]:=data^[j];
    end;
    USBPD_EXTMSG_BATTERY_CAPABILITIES:
    begin
      for j:=0 to Pred(NumberOfBytes) do BatteryCaps.Bytes[j]:=data^[j];
    end;
    USBPD_EXTMSG_PPS_STATUS:
    begin
      for j:=0 to 3 do PPSSDB.Bytes[j]:=data^[j];
    end;
    USBPD_EXTMSG_EPR_SOURCE_CAPABILITIES:
    begin
      NumberSRCPDO:=0;
      NumberEPRSRCPDO:=0;
      for i:=1 to MAXPDO do SourcePDOs[i].Raw:=0;
      for i:=1 to MAXEPRPDO do SourceEPRPDOs[i].Raw:=0;
      i:=0;
      while (i<(NumberOfBytes DIV 4)) do
      begin
        Inc(i);
        for j:=0 to 3 do aPDO.Bytes[j]:=data^[j+(i-1)*4];
        if NumberSRCPDO=7 then
        begin
          // We have received all SPRs, so now receive the EPRs !!
          Inc(NumberEPRSRCPDO);
          SourceEPRPDOs[NumberEPRSRCPDO].Raw:=aPDO.Raw;
        end
        else
        begin
          // Receive the SPRs
          Inc(NumberSRCPDO);
          SourcePDOs[NumberSRCPDO].Raw:=aPDO.Raw;
        end;
      end;
    end;
    USBPD_EXTMSG_EPR_SINK_CAPABILITIES:
    begin
      NumberSNKPDO:=0;
      NumberEPRSNKPDO:=0;
      for i:=1 to MAXPDO do SinkPDOs[i].Raw:=0;
      for i:=1 to MAXEPRPDO do SinkEPRPDOs[i].Raw:=0;
      i:=0;
      while (i<(NumberOfBytes DIV 4)) do
      begin
        Inc(i);
        for j:=0 to 3 do aPDO.Bytes[j]:=data^[j+(i-1)*4];
        if NumberSNKPDO=7 then
        begin
          // We have received all SPRs, so now receive the EPRs !!
          Inc(NumberEPRSNKPDO);
          SinkEPRPDOs[NumberEPRSNKPDO].Raw:=aPDO.Raw;
        end
        else
        begin
          // Receive the SPRs
          Inc(NumberSNKPDO);
          SinkPDOs[NumberSNKPDO].Raw:=aPDO.Raw;
        end;
      end;
    end;
    USBPD_EXTMSG_GET_BATTERY_STATUS,USBPD_EXTMSG_EXTENDED_CONTROL,USBPD_EXTMSG_GET_BATTERY_CAP:
    begin
      // Prevent unhandled errors
      // We send these messages by ourselves.
    end;
    else
    begin
      result:=false;
    end;
  end;
end;

function TUSBPD.ProcessDataMessage(aMSG:TUSBPD_DATAMSG; NumberOfDataObjects:byte; data:PByteArray):boolean;
var
  i,j:word;
  DWordData:TDWordData;
begin
  result:=true;
  case aMSG of
    USBPD_DATAMSG_SRC_CAPABILITIES:
    begin
      NumberSRCPDO:=NumberOfDataObjects;
      for i:=1 to MAXPDO do
      begin
        if i<=NumberSRCPDO then
        begin
          for j:=0 to 3 do SourcePDOs[i].Bytes[j]:=data^[j+(i-1)*4];
        end
        else SourcePDOs[i].Raw:=0;
      end;
    end;
    USBPD_DATAMSG_SNK_CAPABILITIES:
    begin
      NumberSNKPDO:=NumberOfDataObjects;
      for i:=1 to MAXPDO do
      begin
        if i<=NumberSNKPDO then
        begin
          for j:=0 to 3 do SinkPDOs[i].Bytes[j]:=data^[j+(i-1)*4];
        end
        else SinkPDOs[i].Raw:=0;
      end;
    end;
    USBPD_DATAMSG_REQUEST:
    begin
      RDOPositionPrevious:=RDO.GENERIC.ObjectPosition;
      for j:=0 to 3 do RDO.Bytes[j]:=data^[j];
      RDOPosition:=RDO.GENERIC.ObjectPosition;
    end;
    USBPD_DATAMSG_EPR_REQUEST:
    begin
      RDOPositionPrevious:=RDO.GENERIC.ObjectPosition;
      for j:=0 to 3 do RDO.Bytes[j]:=data^[j];
      RDOPosition:=RDO.GENERIC.ObjectPosition;
      // Might be followed by a copy of the requested PDO
    end;
    USBPD_DATAMSG_BATTERY_STATUS:
    begin
      for j:=0 to 3 do BatteryStatus.Bytes[j]:=data^[j];
    end;
    USBPD_DATAMSG_SOURCE_INFO:
    begin
      for j:=0 to 3 do SIDO.Bytes[j]:=data^[j];
    end;
    USBPD_DATAMSG_EPR_MODE:
    begin
      for j:=0 to 3 do EPRMODE.Bytes[j]:=data^[j];
    end;
    USBPD_DATAMSG_VENDOR_DEFINED:
    begin
      // We might receive 1..7 Data Objects
      // The VDM Header is always the first !!

      // Get the VDM Header
      for j:=0 to 3 do DWordData.Bytes[j]:=data^[j];
      VDM_Header.Raw:=DWordData.Raw;

      if VDM_Header.GenericVDM.VDMType=1 then
      begin
        // We have received a structured VDM
        // Process all data
        i:=1;
        while (i<NumberOfDataObjects) do
        begin
          for j:=0 to 3 do DWordData.Bytes[j]:=data^[j+i*4];
          if (VDM_Header.StructuredVDM.Command=Ord(TVDMCommands.DiscoverIdentity)) then
          begin
            if (i=1) then VDO_ID.Raw:=DWordData.Raw;
            if (i=2) then VDO_Cert.Raw:=DWordData.Raw;
            if (i=3) then VDO_Product.Raw:=DWordData.Raw;
            if (i=4) then
            begin
              // Retrieve additional Product Type VDOs
              Cable.Raw:=DWordData.Raw;
            end;
          end;
          Inc(i)
        end;
      end;
    end;
    else
    begin
      result:=false;
    end;
  end;
end;

function TUSBPD.GetStatusPresentInputInfo:string;
var
  ByteData:TByteData;
begin
  result:='';
  ByteData.Raw:=SDB.Data.PresentInput;
  if ByteData.Bits[1]=1 then
  begin
    result:=result+'ExtPower';
    if ByteData.Bits[2]=0 then
      result:=result+'DC'
    else
      result:=result+'AC';
  end;
  if ByteData.Bits[3]=1 then result:=result+'IntPowerBatt';
  if ByteData.Bits[4]=1 then result:=result+'IntPowerOther';
end;

function TUSBPD.GetStatusTemperatureStatusInfo:string;
var
  TempData:byte;
begin
  result:='';
  TempData:=((SDB.Data.TemperatureStatus SHR 1) AND $03);
  case TempData of
    1:result:='Normal';
    2:result:='Warning';
    3:result:='Over';
  end;
end;

function TUSBPD.GetStatusPowerStateChangeInfo:string;
var
  TempData:byte;
  s:string;
begin
  s:='';
  TempData:=((SDB.Data.PowerStateChange) AND $07);
  case TempData of
    0:s:='Status not supported';
    1:s:='S0';
    2:s:='Modern Standby';
    3:s:='S3';
    4:s:='S4';
    5:s:='S5 (Off with battery, wake events supported)';
    6:s:='G3 (Off with no battery, wake events not supported)';
  end;
  result:=s;
  TempData:=((SDB.Data.PowerStateChange SHL 3) AND $07);
  case TempData of
    0:s:='Off LED';
    1:s:='On LED';
    2:s:='Blinking LED';
    3:s:='Breathing LED';
  end;
  result:=result+' '+s;
  result:=Trim(result);
end;

function TUSBPD.EPRModeInfo:string;
var
  TempData:byte;
  s:string;
begin
  s:='';
  TempData:=(EPRMODE.Data.Action);
  case TempData of
    1:s:='Enter. EPR Sink Operational PDP: '+InttoStr(EPRMODE.Data.Data);
    2:s:='Enter Acknowledged';
    3:s:='Enter Succeeded';
    4:
    begin
      s:='Enter Failed. ';
      case EPRMODE.Data.Data of
        0:s:=s+'Unknown cause.';
        1:s:=s+'Cable not EPR capable.';
        2:s:=s+'Source failed to become VCONN source.';
        3:s:=s+'EPR Mode Capable bit not set in RDO.';
        4:s:=s+'Source unable to enter EPR Mode at this time.';
        5:s:=s+'EPR Mode Capable bit not set in PDO.';
      end;
    end;
    5:s:='Exit';
  else
    s:='';
  end;
  result:=s;
end;


procedure TUSBPD.ProcessRawPDMessage(HEADER: TPDHEADER; EXTHEADER: TPDHEADEREXTENDED; LOCALDATA:PByteArray);
//var
//  ChunkPayloadLen: integer;
begin
  if (EXTHEADER.Data.Chunked=0) OR (HEADER.Data.Number_of_Data_Objects=0) then // will handle both unchunked and non-extended data
  begin
    // Single chunk message or not extended data - easy
    FillChar(PDDATA.Data,SizeOf(PDDATA.Data),0);
    PDDATA.NextChunkNum:=0;
    PDDATA.ReceivedBytes:=0;
    if (HEADER.Data.Extended=1) then
      PDDATA.TotalSize := EXTHEADER.Data.Data_Size
    else
      PDDATA.TotalSize := HEADER.Data.Number_of_Data_Objects*4;
    if (PDDATA.TotalSize>0) then Move(LOCALDATA^, PDDATA.Data[0], PDDATA.TotalSize);
    PDDATA.ReceivedBytes := PDDATA.TotalSize;
    PDDATA.ChunkPayloadLen := PDDATA.TotalSize;
    PDDATA.IsComplete := True;
  end
  else
  begin
    // Handle chuncked data
    PDDATA.ChunkPayloadLen := HEADER.Data.Number_of_Data_Objects * 4 - 2;   // subtract 2 bytes of Extended Header

    if EXTHEADER.Data.Request_Chunk=0 then
    begin
      // Handle only chunk data after a chunk request

      // First chunk sets the size and resets other
      if (EXTHEADER.Data.Chunk_Number=0) then
      begin
        PDDATA.TotalSize:=EXTHEADER.Data.Data_Size;
        FillChar(PDDATA.Data,SizeOf(PDDATA.Data),0);
        PDDATA.NextChunkNum:=0;
        PDDATA.ReceivedBytes:=0;
      end;

      // Check out of order or missing chunk → error handling
      if (EXTHEADER.Data.Chunk_Number<>PDDATA.NextChunkNum) then
      begin
        Raise Exception.Create('Wrong chunk number');
      end;

      // Copy only what we still need
      if PDDATA.ReceivedBytes + PDDATA.ChunkPayloadLen > PDDATA.TotalSize then
        PDDATA.ChunkPayloadLen := PDDATA.TotalSize - PDDATA.ReceivedBytes;

      Move(LOCALDATA^,
           PDDATA.Data[PDDATA.ReceivedBytes],
           PDDATA.ChunkPayloadLen);

      Inc(PDDATA.ReceivedBytes, PDDATA.ChunkPayloadLen);
      Inc(PDDATA.NextChunkNum);

      if PDDATA.ReceivedBytes >= PDDATA.TotalSize then
      begin
        PDDATA.IsComplete := True;
      end;
    end
    else
    begin
      // Handle next chuck requests
      PDDATA.ChunkPayloadLen:=0;
    end;
  end;
end;

end.
