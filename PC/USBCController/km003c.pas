unit km003c;

{$mode ObjFPC}{$H+}

(*
https://nl.aliexpress.com/item/1005005629494532.html?spm=a2g0o.detail.pcDetailTopMoreOtherSeller.1.536fuGv0uGv0Uf&gps-id=pcDetailTopMoreOtherSeller&scm=1007.40050.354490.0&scm_id=1007.40050.354490.0&scm-url=1007.40050.354490.0&pvid=bd142f29-bb8f-4ed4-a2f0-02a1a69a78da&_t=gps-id%3ApcDetailTopMoreOtherSeller%2Cscm-url%3A1007.40050.354490.0%2Cpvid%3Abd142f29-bb8f-4ed4-a2f0-02a1a69a78da%2Ctpp_buckets%3A668%232846%238109%231935&pdp_ext_f=%7B%22order%22%3A%22592%22%2C%22eval%22%3A%221%22%2C%22sceneId%22%3A%2230050%22%2C%22fromPage%22%3A%22recommend%22%7D&pdp_npi=6%40dis%21EUR%2188.36%2170.69%21%21%21689.24%21551.41%21%402103847817775523398318245e1e11%2112000033809720257%21rec%21NL%21%21ABXZ%211%210%21n_tag%3A-29910%3Bd%3A19b16f31%3Bm03_new_user%3A-29895&utparam-url=scene%3ApcDetailTopMoreOtherSeller%7Cquery_from%3A%7Cx_object_id%3A1005005629494532%7C_p_origin_prod%3A



https://github.com/nevetssf/powerz/blob/master/src/powerz/device/protocol.py

HID commands:
# Additional command attributes (sent as ATT field, shifted left 1 bit in wire format)
ATT_ADC = 0x001
ATT_PD_PACKET = 0x010
ATT_SETTINGS = 0x008

# ADC request command bytes: CMD_GET_DATA (0xC) with ATT_ADC (0x1)
# MsgHeader_TypeDef packed as little-endian: 0xC020 -> [0x0C, 0x00, 0x02, 0x00]
ADC_REQUEST = bytes([0x0C, 0x00, 0x02, 0x00])

# PD packet request: CMD_GET_DATA (0xC) with ATT_PD_PACKET (0x010)
# att=0x010, shifted left 1 = 0x020, LE bytes: 20 00
PD_PACKET_REQUEST = bytes([0x0C, 0x00, 0x20, 0x00])

# PD + ADC combined request
PD_ADC_REQUEST = bytes([0x0C, 0x00, 0x22, 0x00])

[0x0C, 0x00, 0x0A, 0x00] also returns data !!



*)


interface

uses
  bits;

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
               Status : packed record
                 type_id                 : byte;
                 TimeStamp               : packed array [0..2] of byte; // ~40ms/tick
                 vbus_mV                 : word;
                 ibus_mA                 : word;
                 cc1_mV                  : word;
                 cc2_mV                  : word;
               end;
          );
          2 : (
               Preamble : packed record
                 TimeStamp               : dword; // ms
                 vbus_mV                 : word;
                 ibus_mA                 : word;
                 cc1_mV                  : word;
                 cc2_mV                  : word;
               end;
          );
          3 : (
               Bytes           : packed array[0..11] of byte;
          );
  end;


  TKM003CEventHeader = bitpacked record
      case integer of
          1 : (
               PDData : packed record
                 Size        : TByteData;
                 TimeStamp   : TDWordData;
                 SOP         : byte;
               end;
          );
          2 : (
               EventData : packed record
                 Marker      : byte; // 0x45
                 TimeStamp   : packed array [0..2] of byte; // ~40ms/tick
                 Reserved    : byte;
                 EventCode   : byte; // 0x21 - Connect (33 decimal) ; 0x22 - Disconnect (34 decimal)
               end;
          );
          3 : (
               Bytes         : packed array[0..5] of byte;
          );
  end;

  TKM003CSensorData = packed record
    header             : TKM003CMsgHeader;
    header_ext         : TKM003CMsgHeader;
    V_bus              : dword;
    I_bus              : dword;
    V_bus_avg          : dword;
    I_bus_avg          : dword;
    V_bus_ori_avg      : dword;
    I_bus_ori_avg      : dword;
    temp               : packed array [0..1] of byte;
    V_cc1              : word;
    V_cc2              : word;
    V_dp               : word;
    V_dm               : word;
    V_dd               : word;
    Rate               : byte; // 0 = 2/s; 1 = 10/s 2 = 50/s; 3 = 1000/s
    Flags              : byte; // Status flags
    V_cc2_avg          : word;
    V_dp_avg           : word;
    V_dm_avg           : word;
  end;

  TKM003CSettingsData = packed record
    header                  : TKM003CMsgHeader;
    header_ext              : TKM003CMsgHeader;
    flags                   : dword;
    reserved01              : dword;
    sample_interval         : word; // in us
    display_brightness      : byte;
    reserved02              : byte;
    reserved03              : dword;
    thresholds              : packed array [0..7] of dword; // -1=disabled, -6=enabled
    calibration             : packed array [0..9] of dword;
    counter                 : dword;
    timestamp               : dword;
    mode_flags              : byte;
    reserved04              : packed array [0..14] of byte;
    device_name             : packed array [0..63] of char;
    checksum                : dword;
  end;

  TKM003CCtrlMessage =
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
    (*
    CMD_START_GRAPH,
    CMD_STOP_GRAPH,
    CMD_ENABLE_PDMONITOR,
    CMD_DISABLE_PDMONITOR,
    CMD_PUTDATA = $41,
    CMD_MEMORY_READ = $44,
    CMD_STREAMING_AUTH = $4C
    *)
  );

  TKM003CDataMessage =
  (
    DATA_HEAD = $40,
    DATA_PUTDATA
  );


const
  TKM003C_EVENT_HARDRESET        = $04;
  TKM003C_EVENT_ATTACHCC1        = $11;
  TKM003C_EVENT_DETACHCC1        = $12;
  TKM003C_EVENT_CONN_CONNECT     = $21;
  TKM003C_EVENT_CONN_DISCONNECT  = $22;
  TKM003C_EVENT_CONN_EVENT       = $45;
  TKM003C_EVENT_SOPERROR         = $46;
  TKM003C_EVENT_EOPERROR         = $56;
  TKM003C_EVENT_CRCERROR         = $66;
  TKM003C_EVENT_DATACOUNTERROR   = $76;
  TKM003C_EVENT_UNKNOWNERROR     = $86;
  TKM003C_EVENT_RETRIESERROR     = $96;

  TKM003C_ATT_ADC                = $001;
  TKM003C_ATT_ADC_QUEUE          = $002;
  TKM003C_ATT_ADC_QUEUE_10K      = $004;
  TKM003C_ATT_SETTINGS           = $008;
  TKM003C_ATT_PD_PACKET          = $010;
  TKM003C_ATT_PD_STATUS          = $020;
  TKM003C_ATT_QC_PACKET          = $040;
  TKM003C_ATT_TICK               = $080;
  TKM003C_ATT_TICK__             = $100;

  TKM003C_SIZE_OFFSET            = 5;

implementation

end.

