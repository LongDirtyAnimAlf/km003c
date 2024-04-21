unit km003c;

{$mode ObjFPC}{$H+}

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
    V_bus:dword;
    I_bus:dword;
    V_bus_avg:dword;
    I_bus_avg:dword;
    V_bus_ori_avg:dword;
    I_bus_ori_avg:dword;
    temp: packed array [0..1] of byte;
    V_cc1:word;
    V_cc2:word;
    V_dp:word;
    V_dm:word;
    V_dd:word;
    Rate:byte; // 0 = 2/s; 1 = 10/s 2 = 50/s; 3 = 1000/s
    Unknown1:byte;
    V_cc2_avg:word;
    V_dp_avg:word;
    V_dm_avg:word;
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

implementation

end.

