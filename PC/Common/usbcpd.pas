unit usbcpd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Bits;

type
  TSUPPLY_TYPES = (Fixed,Battery,Variable,APDO);
  TAPDO_TYPES = (SPR,EPR);

const
  SUPPLY_TYPES : array[TSUPPLY_TYPES] of string = ('Fixed','Battery','Variable','Augmented');
  APDO_TYPES : array[TAPDO_TYPES] of string = ('SPR Programmable Power Supply','EPR Adjustable Power Supply');
  MAXPDO = 7;

type
  USBC_SOURCE_PD_POWER_DATA_OBJECT = bitpacked record
      case integer of
          1 : (  FixedSupplyPdo : record
                   MaximumCurrentIn10mA     : T10BITS;
                   VoltageIn50mV            : T10BITS;
                   PeakCurrent              : T2BITS;
                   Reserved1                : T3BITS;
                   DataRoleSwap             : T1BITS;
                   UsbCommunicationCapable  : T1BITS;
                   ExternallyPowered        : T1BITS;
                   UsbSuspendSupported      : T1BITS;
                   DualRolePower            : T1BITS;
                   FixedSupply              : T2BITS;
                 end
              );
          2 : (  BatterySupplyPdo : record
                   MaximumAllowablePowerIn250mW  : T10BITS;
                   MinimumVoltageIn50mV          : T10BITS;
                   MaximumVoltageIn50mV          : T10BITS;
                   Battery                       : T2BITS;
                 end
              );
          3 : (  VariableSupplyNonBatteryPdo : record
                   MaximumCurrentIn10mA      : T10BITS;
                   MinimumVoltageIn50mV      : T10BITS;
                   MaximumVoltageIn50mV      : T10BITS;
                   VariableSupportNonBattery : T2BITS;
                 end
              );
          4 : (  SPRPowerSupplyApdo : record
                   MaximumCurrentIn50mA         : T7BITS;
                   Reserved1                    : T1BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved2                    : T1BITS;
                   MaximumVoltageIn100mV        : T8BITS;
                   Reserved3                    : T2BITS;
                   PpsPowerLimited              : T1BITS;
                   AugmentedPowerDataObjectType : T2BITS;
                   AugmentedPowerDataObject     : T2BITS;
                 end
              );
          5 : (  EPRPowerSupplyApdo : record
                   PDPInW                       : T8BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved                     : T1BITS;
                   MaximumVoltageIn100mV        : T9BITS;
                   PeakCurrent                  : T2BITS;
                   AugmentedPowerDataObjectType : T2BITS;
                   AugmentedPowerDataObject     : T2BITS;
                 end
              );
          6 : (  GenericPdo : record
                   PDO                          : T30BITS;
                   Supply                       : T2BITS;
                 end
                 );
          7 : (  GenericAPdo : record
                   PDO                          : T28BITS;
                   APO                          : T2BITS;
                   Supply                       : T2BITS;
                 end
                 );
          8 : (
               Bits            : bitpacked array[0..31] of T1BITS;
              );
          9 : (
               Bytes           : bitpacked array[0..3] of byte;
              );
         10 : (
               Raw             : DWord;
              );

  end;

  USBC_SINK_PD_POWER_DATA_OBJECT = bitpacked record
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
                   FixedSupply              : T2BITS;
                 end
              );
          2 : (  BatterySupplyPdo : record
                   MaximumAllowablePowerIn250mW  : T10BITS;
                   MinimumVoltageIn50mV          : T10BITS;
                   MaximumVoltageIn50mV          : T10BITS;
                   Battery                       : T2BITS;
                 end
              );
          3 : (  VariableSupplyNonBatteryPdo : record
                   OperationalCurrentIn10mA  : T10BITS;
                   MinimumVoltageIn50mV      : T10BITS;
                   MaximumVoltageIn50mV      : T10BITS;
                   VariableSupportNonBattery : T2BITS;
                 end
              );

          4 : (  SPRPdo : record
                   MaximumCurrentIn50mA         : T7BITS;
                   Reserved1                    : T1BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved2                    : T1BITS;
                   MaximumVoltageIn100mV        : T8BITS;
                   Reserved3                    : T3BITS;
                   AugmentedPowerDataObjectType : T2BITS;
                   AugmentedPowerDataObject     : T2BITS;
                 end
              );

          5 : (  EPRPdo : record
                   PDPInW                       : T8BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved1                    : T1BITS;
                   MaximumVoltageIn100mV        : T9BITS;
                   Reserved2                    : T2BITS;
                   AugmentedPowerDataObjectType : T2BITS;
                   AugmentedPowerDataObject     : T2BITS;
                 end
              );

          6 : (  GenericPdo : record
                   PDO                          : T30BITS;
                   Supply                       : T2BITS;
                 end
                 );
          7 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          8 : (
                Bytes           : bitpacked array[0..3] of byte;
               );
          9 : (
               Raw             : DWord;
              );
  end;

  USBC_PD_REQUEST_DATA_OBJECT = bitpacked record
      case integer of
          1 : (  STANDARD : record
                   MaximumOperatingCurrentIn10mA : T10BITS;
                   OperatingCurrentIn10mA        : T10BITS;
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
                   PDODATA                            : T28BITS;
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
               Raw             : DWord;
              );
  end;

  USBC_VDM_HEADER = bitpacked record
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

  IDHeaderVDO = bitpacked record
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

  CertStatVDO = bitpacked record
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

  ProductVDO = bitpacked record
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

  ProductTypeVDO = bitpacked record
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

  TCableVDO = bitpacked record
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

  USBC_SOURCE_CAPABILITIES_EXTENDED_DATA_OBJECT = packed record
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

  USBC_SINK_CAPABILITIES_EXTENDED_DATA_OBJECT = packed record
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

  PDHEADER = bitpacked record
      case integer of
          1 : (
               Data : record
                     Message_Type              : T5BITS;
                     Port_Data_Role            : T1BITS;
                     Specification_Revision    : T2BITS;
                     Port_Power_Role_or_Plug   : T1BITS;
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

  PDHEADEREXTENDED = bitpacked record
      case integer of
          1 : (
               Data : record
                   Chunked                  : T1BITS;
                   Chunk_Number             : T4BITS;
                   Request_Chunk            : T1BITS;
                   Reserved                 : T1BITS;
                   Data_Size                : T9BITS;
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

  USBC_BATTERY_STATUS_DATA_OBJECT = packed record
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

  USBC_BATTERY_CAPABILITIES_DATA_OBJECT = packed record
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


const
  USBPD_SOPTYPE_SOP           = 0;
  USBPD_SOPTYPE_SOP1          = 1;
  USBPD_SOPTYPE_SOP2          = 2;
  USBPD_SOPTYPE_SOP1_DEBUG    = 3;
  USBPD_SOPTYPE_SOP2_DEBUG    = 4;
  USBPD_SOPTYPE_HARD_RESET    = 5;
  USBPD_SOPTYPE_CABLE_RESET   = 6;
  USBPD_SOPTYPE_BIST_MODE_2   = 7;
  USBPD_SOPTYPE_INVALID       = $FF;

  CCNONE                          = 0;
  CC1                             = 1;
  CC2                             = 2;

  USBPD_CABLEPLUG_FROMDFPUFP      = 0;
  USBPD_PORTPOWERROLE_SNK         = USBPD_CABLEPLUG_FROMDFPUFP;
  USBPD_CABLEPLUG_FROMCABLEPLUG   = 1;
  USBPD_PORTPOWERROLE_SRC         = USBPD_CABLEPLUG_FROMCABLEPLUG;


type
  TUSBPD = class
    NumberSRCPDO: dword;
    NumberSNKPDO: dword;

    ActiveSRCPDO: dword;
    ActiveSNKPDO: dword;

    SourcePDOs: array[1..MAXPDO] of USBC_SOURCE_PD_POWER_DATA_OBJECT;
    SinkPDOs: array[1..MAXPDO] of USBC_SINK_PD_POWER_DATA_OBJECT;

    RDO:USBC_PD_REQUEST_DATA_OBJECT;

    SRCExtended:USBC_SOURCE_CAPABILITIES_EXTENDED_DATA_OBJECT;
    SNKExtended:USBC_SINK_CAPABILITIES_EXTENDED_DATA_OBJECT;

    BatteryCaps:USBC_BATTERY_CAPABILITIES_DATA_OBJECT;
    BatteryStatus:USBC_BATTERY_STATUS_DATA_OBJECT;
    NBBatteries:dword;

    GBCDB  : TGBDB;
    GBSDB  : TGBDB;
    SDB    : TSDB;

    SIDO   : TSIDO;

    Cable:TCableVDO;

    ActiveCCIs:dword;
    PowerRole:dword;
    DefaultPower:dword;
    DataRole:dword;
    Vconn:dword;
    PDSpecRevision:dword;

    RDOPosition:dword;
    RDOPositionPrevious:dword;

    RequestedVoltage:dword;
    RequestedCurrent:dword;
    RequestedPower:dword;

    VDM_Header:USBC_VDM_HEADER;
    VDO_ID:IDHeaderVDO;
    VDO_Cert:CertStatVDO;
    VDO_Product:ProductVDO;
    VDO_ProductType:ProductTypeVDO;

    function GetSRCPDOInfo(aSRCPDO:byte):string;
    function GetSNKPDOInfo(aSNKPDO:byte):string;

    function GetFixed5VSRCPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT;

    function GetRDOInfo:string;
    function GetSRCExtendedInfo:string;
    function GetSNKExtendedInfo:string;

    function GetCableInfo:string;

    function GetVID(aVID:word):string;

    function ProcessExtendedMessage(aMSG:TUSBPD_EXTENDEDMSG; data:PByteArray):boolean;
    function ProcessDataMessage(aMSG:TUSBPD_DATAMSG; NumberOfDataObjects:byte; data:PByteArray):boolean;

    procedure Cleanup;
  end;

  function GetSOPInfo(aSOPHeader:PDHEADER):string;
  function SRCPDOInfo(aPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT):string;
  function SNKPDOInfo(aPDO:USBC_SINK_PD_POWER_DATA_OBJECT):string;

implementation


uses
  vids;

function GetSOPInfo(aSOPHeader:PDHEADER):string;
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

  case aSOPHeader.Data.Port_Power_Role_or_Plug of
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

function SRCPDOInfo(aPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT):string;
var
  aPDOType:TSUPPLY_TYPES;
  s:string;
begin
  aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

  s:='Source PDO type: '+SUPPLY_TYPES[aPDOType]+'. ';

  case TSUPPLY_TYPES(aPDO.GenericPdo.Supply) of
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
      with aPDO.SPRPowerSupplyApdo do
      begin
        s:=s+'Operational Current: '+InttoStr(MaximumCurrentIn50mA*50)+'mA. ';
        s:=s+'Min voltage: '+InttoStr(MinimumVoltageIn100mV*100)+'mV';
        s:=s+'Max voltage: '+InttoStr(MaximumVoltageIn100mV*100)+'mV';
      end;
    end;
  end;

  result:=s;
end;

function SNKPDOInfo(aPDO:USBC_SINK_PD_POWER_DATA_OBJECT):string;
var
  aPDOType:TSUPPLY_TYPES;
  s:string;
begin
  aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

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

function TUSBPD.GetFixed5VSRCPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT;
begin
  result:=SourcePDOs[1];
end;

procedure TUSBPD.Cleanup;
var
  i:integer;
begin
  NumberSRCPDO:=0;
  NumberSNKPDO:=0;

  ActiveSRCPDO:=0;
  ActiveSNKPDO:=0;

  for i:=1 to MAXPDO do SourcePDOs[i].Raw:=0;
  for i:=1 to MAXPDO do SinkPDOs[i].Raw:=0;

  RDO.Raw:=0;

  for i:=0 to Pred(Length(SRCExtended.DWords)) do SRCExtended.DWords[i].Raw:=0;
  for i:=0 to Pred(Length(SNKExtended.DWords)) do SNKExtended.DWords[i].Raw:=0;

  for i:=0 to Pred(Length(BatteryCaps.Bytes)) do BatteryCaps.Bytes[i]:=0;
  for i:=0 to Pred(Length(BatteryStatus.Bytes)) do BatteryStatus.Bytes[i]:=0;

  NBBatteries:=0;

  GBCDB.Raw:=0;
  GBSDB.Raw:=0;
  for i:=0 to Pred(Length(SDB.Bytes)) do SDB.Bytes[i]:=0;
  SIDO.Raw:=0;

  Cable.Raw:=0;

  ActiveCCIs:=0;
  PowerRole:=0;
  DefaultPower:=0;
  DataRole:=0;
  Vconn:=0;
  PDSpecRevision:=0;

  RDOPosition:=0;
  RDOPositionPrevious:=0;

  RequestedVoltage:=0;
  RequestedCurrent:=0;
  RequestedPower:=0;

  VDM_Header.Raw:=0;
  VDO_ID.Raw:=0;
  VDO_Cert.Raw:=0;
  VDO_Product.Raw:=0;
  VDO_ProductType.Raw:=0;
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

function TUSBPD.ProcessExtendedMessage(aMSG:TUSBPD_EXTENDEDMSG; data:PByteArray):boolean;
var
  j:integer;
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
      for j:=0 to Pred(Length(BatteryCaps.Bytes)) do BatteryCaps.Bytes[j]:=data^[j];
    end;
    USBPD_EXTMSG_GET_BATTERY_CAP:
    begin
      GBCDB.Raw:=data^[0];
    end;
    USBPD_EXTMSG_GET_BATTERY_STATUS:
    begin
      GBSDB.Raw:=data^[0];
    end;
    else
    begin
      result:=false;
    end;
  end;
end;

function TUSBPD.ProcessDataMessage(aMSG:TUSBPD_DATAMSG; NumberOfDataObjects:byte; data:PByteArray):boolean;
var
  i,j:integer;
  DWordData:TDWordData;
begin
  result:=true;
  case aMSG of
    USBPD_DATAMSG_SRC_CAPABILITIES:
    begin
      NumberSRCPDO:=NumberOfDataObjects;
      for i:=1 to NumberSRCPDO do
      begin
        for j:=0 to 3 do SourcePDOs[i].Bytes[j]:=data^[j+(i-1)*4];
      end;
    end;
    USBPD_DATAMSG_SNK_CAPABILITIES:
    begin
      NumberSNKPDO:=NumberOfDataObjects;
      for i:=1 to NumberSNKPDO do
      begin
        for j:=0 to 3 do SinkPDOs[i].Bytes[j]:=data^[j+(i-1)*4];
      end;
    end;
    USBPD_DATAMSG_REQUEST:
    begin
      for j:=0 to 3 do RDO.Bytes[j]:=data^[j];
    end;
    USBPD_DATAMSG_BATTERY_STATUS:
    begin
      for j:=0 to 3 do BatteryStatus.Bytes[j]:=data^[j];
    end;
    USBPD_DATAMSG_SOURCE_INFO:
    begin
      for j:=0 to 3 do SIDO.Bytes[j]:=data^[j];
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

end.
