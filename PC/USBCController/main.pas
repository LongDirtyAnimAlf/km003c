unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Buttons, ComCtrls, ValEdit, TAGraph, TASeries, Types,
  libusb,
  libusboop,
  fptimernew,
  dsLeds,
  usbcpd,
  pddevice,
  stm32g0,
  hp66332,
  lazserial;

type
  TTestType = record
    Name:string;
    Current:word;
    Voltage:word;
  end;

  { TPowerbankMainForm }

  TPowerbankMainForm = class(TForm)
    btnBatteryCapabilities: TButton;
    btnBatteryStatus: TButton;
    btnConnectSTM32: TButton;
    btnConnectKC003C: TButton;
    btnGetPPSStatus: TButton;
    btnGetStatus: TButton;
    btnInit: TButton;
    btnKC003CRcvRemoteSink: TButton;
    btnKC003CRcvRemoteSource: TButton;
    btnKC003CReset: TButton;
    btnRcvRemoteSinkExt: TButton;
    btnRcvRemoteSourceExt: TButton;
    btnSetEPRKC003C: TButton;
    btnSetEPRKC003C1: TButton;
    btnSwapVconn: TButton;
    btnReset: TButton;
    btnRcvSelf: TButton;
    btnSwap: TButton;
    btnRcvRemoteSource: TButton;
    btnRcvRemoteSink: TButton;
    btnHardReset: TButton;
    btnCleanLogs: TButton;
    btnTestDischarge: TSpeedButton;
    btnVDMDiscoIndent: TButton;
    btnVDMDiscoSVID: TButton;
    btnGetSourceInfo: TButton;
    btnRcvRemoteEPRSource: TButton;
    Button1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    chkCurrentLimit: TCheckBox;
    chkgrpPDOFLags: TCheckGroup;
    chkVoltageLimit: TCheckBox;
    cmboSerialPorts: TComboBox;
    cmboSerialPorts1: TComboBox;
    CurrentEdit: TEdit;
    DisplaysPanel: TPanel;
    Edit1: TEdit;
    gridRemoteSinkPDO: TStringGrid;
    gridRemoteSinkEPRPDO: TStringGrid;
    gridRemoteSourceEPRPDO: TStringGrid;
    GroupBattery: TGroupBox;
    GroupBox1: TGroupBox;
    grpEPRPDOs: TGroupBox;
    grpTesting: TGroupBox;
    GroupExtendedSink: TGroupBox;
    GroupExtendedSource: TGroupBox;
    GroupPPS: TGroupBox;
    GroupStatus: TGroupBox;
    grpLineVoltages: TGroupBox;
    GroupLogs: TGroupBox;
    grpKC003CPDControl: TGroupBox;
    grpVAData: TGroupBox;
    editPowerStatusUSBCurrent: TEdit;
    editPowerStatusUSBContract: TEdit;
    gridRemoteSourcePDO: TStringGrid;
    grpSTM32PDControl: TGroupBox;
    grpPowerStatus: TGroupBox;
    grpPDOs: TGroupBox;
    lblCurrent: TLabel;
    lblDischargeSettings: TLabel;
    lblVoltage: TLabel;
    MemoUnhandled: TMemo;
    TestInfoMemo: TMemo;
    PageControl1: TPageControl;
    ProgressBar1: TProgressBar;
    SamplesBox: TComboBox;
    StartStopButton: TSpeedButton;
    StaticText1: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TestsBox: TComboBox;
    StoreTimer: TTimer;
    TestTimer: TTimer;
    UpdateTimer: TTimer;
    TypesBox: TComboBox;
    USBComLog: TMemo;
    USBDebugLog: TMemo;
    USBDetailsLog: TMemo;
    vleBatteryData: TValueListEditor;
    vlePPS: TValueListEditor;
    vleSinkExtended: TValueListEditor;
    vleSourceExtended: TValueListEditor;
    vleStatus: TValueListEditor;
    VoltageEdit: TEdit;
    procedure btnBatteryCapabilitiesClick({%H-}Sender: TObject);
    procedure btnBatteryStatusClick({%H-}Sender: TObject);
    procedure btnCleanLogsClick({%H-}Sender: TObject);
    procedure btnConnectKC003CClick(Sender: TObject);
    procedure btnConnectSTM32Click(Sender: TObject);
    procedure btnGetStatusClick({%H-}Sender: TObject);
    procedure btnInitClick(Sender: TObject);
    procedure btnKC003CRcvRemoteSinkClick({%H-}Sender: TObject);
    procedure btnKC003CRcvRemoteSourceClick({%H-}Sender: TObject);
    procedure btnKC003CResetClick({%H-}Sender: TObject);
    procedure btnRcvRemoteEPRSourceClick(Sender: TObject);
    procedure btnRcvRemoteSourceExtClick({%H-}Sender: TObject);
    procedure btnRcvRemoteSinkExtClick({%H-}Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnRcvSelfClick({%H-}Sender: TObject);
    procedure btnSwapClick({%H-}Sender: TObject);
    procedure btnRcvRemoteSourceClick({%H-}Sender: TObject);
    procedure btnRcvRemoteSinkClick({%H-}Sender: TObject);
    procedure btnHardResetClick(Sender: TObject);
    procedure btnSwapVconnClick({%H-}Sender: TObject);
    procedure btnTestDischargeClick(Sender: TObject);
    procedure btnVDMDiscoIndentClick({%H-}Sender: TObject);
    procedure btnVDMDiscoSVIDClick({%H-}Sender: TObject);
    procedure btnGetSourceInfoClick({%H-}Sender: TObject);
    procedure btnGetPPSStatusClick({%H-}Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DataEditKeyPress(Sender: TObject; var Key: char);
    procedure gridPDOResize(Sender: TObject);
    procedure grpVADataResize(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure StoreTimerTimer(Sender: TObject);
    procedure TestsBoxChange(Sender: TObject);
    procedure TestTimerTimer({%H-}Sender: TObject);
    procedure UpdateTimerTimer({%H-}Sender: TObject);

    procedure FormCreate({%H-}Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    LocalFS             : TFormatSettings;

    FMessageConfirmed   : boolean;

    FSTM32BoardSerial   : string;

    RealVoltageDisplay  : TdsSevenSegmentMultiDisplay;
    RealCurrentDisplay  : TdsSevenSegmentMultiDisplay;

    Vcc1VoltageDisplay  : TdsSevenSegmentMultiDisplay;
    Vcc2VoltageDisplay  : TdsSevenSegmentMultiDisplay;
    VdpVoltageDisplay   : TdsSevenSegmentMultiDisplay;
    VdmVoltageDisplay   : TdsSevenSegmentMultiDisplay;

    PowerDisplay        : TdsSevenSegmentMultiDisplay;
    EnergyDisplay       : TdsSevenSegmentMultiDisplay;

    TemperatureDisplay  : TdsSevenSegmentMultiDisplay;


    Led                 : TShape;

    FSystemActive       : boolean;

    FVoltage            : double;
    FCurrent            : double;
    FEnergy             : double;
    FPower              : double;
    FTemperature        : double;

    NumRate             : integer;
    StartTime           : TDateTime;
    LastTime            : TDateTime;



    DUT                 : TUSBPDDevice;

    HPsource            : THP66332;
    HPComport           : string;

    STM32               : TLazSerial;
    STMComport          : string;
    STM32PDController   : TUSBPDController;

    KM003C              : TLazSerial;
    KM003CComport       : string;

    PDTimer             : TFPTimer;
    DataTimer           : TFPTimer;

    Context             : TLibUsbContext;
    Device              : TLibUsbDevice;
    DeviceInterface     : TLibUsbInterface;
    OutEndPoint         : TLibUsbBulkOutEndpoint;
    InEndPoint          : TLibUsbBulkInEndpoint;

    TestTypes           : array of TTestType;
    ActiveTestType      : TTestType;
    BatteryDataFile     : string;



    procedure GridButtonClick(Sender: TObject);

    procedure SetEnable(Sender: TObject; value:boolean);
    procedure SetChartAxis({%H-}Sender:TObject);
    procedure CreateDataFile(Sender: TObject);
    procedure AllStop(Sender: TObject);
    procedure Measure;
    procedure SaveBatteryData(Elapsed:longword);

    procedure SendCommand(Port,Command:byte;DataToSend:pbyte;DataToSendLength:word;Confirmation:boolean=true);
    function  CorrectVoltage(value:double):double;
    function  CorrectCurrent(value:double):double;

    procedure OnRXUSBCData({%H-}Sender: TObject);

    procedure SetActive(value:boolean);

    procedure SetVoltage(value:double);
    function  GetVoltage:double;
    procedure SetCurrent(value:double);
    function  GetCurrent:double;
    procedure SetEnergy(value:double);
    function  GetEnergy:double;
    procedure SetPower(value:double);
    function  GetPower:double;
    procedure SetTemperature(value:double);
    function  GetTemperature:double;

    procedure SetGridSRCPDO(PDONumber:integer);
    procedure SetGridSRCEPRPDO(PDONumber:integer);

    procedure SetGridSNKPDO(PDONumber:integer);
    procedure SetGridSNKEPRPDO(PDONumber:integer);

    procedure SetBatteryData;

    function ProcessControlMessageGUI(MSGCTRL:TUSBPD_CONTROLMSG):boolean;
    function ProcessDataMessageGUI(aMSG:TUSBPD_DATAMSG):boolean;
    function ProcessExtendedMessageGUI(aMSG:TUSBPD_EXTENDEDMSG):boolean;

    procedure Connect(Sender: TObject);
    procedure DisConnect({%H-}Sender: TObject);

    procedure DataTimerTimer({%H-}Sender: TObject);
    procedure CheckTimerTimer({%H-}Sender: TObject);
  public
    Capacity                     : double;

    property SystemActive        : boolean read FSystemActive write SetActive;

    property Voltage             : double read GetVoltage write SetVoltage;
    property Current             : double read GetCurrent write SetCurrent;
    property Energy              : double read GetEnergy write SetEnergy;
    property Power               : double read GetPower write SetPower;
    property Temperature         : double read GetTemperature write SetTemperature;

    property STM32BoardSerial    : string read FSTM32BoardSerial;
  end;

var
  PowerbankMainForm: TPowerbankMainForm;

implementation

{$R *.lfm}

uses
  TAChartAxisUtils,
  TypInfo,
  IniFiles,
  DateUtils,
  LCLType,
  km003c,
  Bits,Tools;

Const
  DevVID                       = $5FC9;
  DevPID                       = $0063;
  EP_IN                        = 1 or LIBUSB_ENDPOINT_IN;
  EP_OUT                       = 1 or LIBUSB_ENDPOINT_OUT;
  ConfigUSBConfiguration       = 1;
  ConfigUSBInterface           = 0;
  ConfigUSBAltInterface        = 0;

function ChangeBrightness(lIn: tColor; factor:double): TColor;
var
  lR,lG,lB: byte;
begin
  lR := Red(lIn);
  lG := Green(lIn);
  lB := Blue(lIn);
  result := RGBToColor(Round(lR*factor),Round(lG*factor),Round(lB*factor));
end;

{ TPowerbankMainForm }

procedure TPowerbankMainForm.FormCreate(Sender: TObject);
var
  bt      : TButton;
  Rect    : TRect;
  aGrid   : TStringGrid;
  index   : Integer;
  ACol    : Integer;
  ARow    : Integer;
  Ini     : TIniFile;
  i       : integer;
  s       : string;
  CList,CListDeatails:TStringList;
begin
  LocalFS:=DefaultFormatSettings;

  LocalFS.ShortDateFormat:='dd-mm-yyyy';
  LocalFS.LongTimeFormat:='hh:nn:ss';
  LocalFS.DecimalSeparator:=',';
  LocalFS.ListSeparator:=';';

  DUT:=TUSBPDDevice.Create;
  DUT.Cleanup;

  STM32PDController:=TUSBPDController.Create;
  STM32PDController.Cleanup;

  {$ifdef DEBUG}
  USBComLog.Visible:=false;
  USBDetailsLog.Visible:=false;
  {$endif}

  gridRemoteSourcePDO.Cells[0,0]:='Source';
  gridRemoteSourcePDO.Cells[0,1]:='Type';
  gridRemoteSourcePDO.Cells[0,2]:='Current';
  gridRemoteSourcePDO.Cells[0,3]:='Voltage';
  gridRemoteSourcePDO.Cells[0,4]:='Power';

  gridRemoteSinkPDO.Cells[0,0]:='Sink';
  gridRemoteSinkPDO.Cells[0,1]:='Type';
  gridRemoteSinkPDO.Cells[0,2]:='Current';
  gridRemoteSinkPDO.Cells[0,3]:='Voltage';

  Led:=TShape.Create(GroupBox1);
  Led.Parent:=GroupBox1;
  Led.Width := 30;
  Led.Height := 30;
  Led.Brush.Color := clLime;
  Led.Shape := stCircle;

  RealVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpVAData);
  with RealVoltageDisplay do
  begin
    Parent:=grpVAData;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
    DisplayCount:=6;
  end;
  RealCurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(grpVAData);
  with RealCurrentDisplay do
  begin
    Parent:=grpVAData;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
    SignDigit:=True;
    DisplayCount:=6;
  end;

  Vcc1VoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpLineVoltages);
  with Vcc1VoltageDisplay do
  begin
    Parent:=grpLineVoltages;
    OnColor:=clAqua;
    Height:=66;
    DisplayCount:=4;
    Align:=alTop;
    Hint:='VCC1';
    ShowHint:=True;
  end;
  Vcc2VoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpLineVoltages);
  with Vcc2VoltageDisplay do
  begin
    Parent:=grpLineVoltages;
    OnColor:=clFuchsia;
    Height:=66;
    DisplayCount:=4;
    Align:=alTop;
    Hint:='VCC2';
    ShowHint:=True;
  end;

  VdpVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpLineVoltages);
  with VdpVoltageDisplay do
  begin
    Parent:=grpLineVoltages;
    OnColor:=clLime;
    Height:=66;
    DisplayCount:=4;
    Align:=alTop;
    Hint:='VD+';
    ShowHint:=True;
  end;

  VdmVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpLineVoltages);
  with VdmVoltageDisplay do
  begin
    Parent:=grpLineVoltages;
    OnColor:=clYellow;
    Height:=66;
    DisplayCount:=4;
    Align:=alTop;
    Hint:='VD-';
    ShowHint:=True;
  end;

  PowerDisplay:=TdsSevenSegmentMultiDisplay.Create(DisplaysPanel);
  with PowerDisplay do
  begin
    Parent:=DisplaysPanel;
    OnColor:=clSilver;
    OffColor:=ChangeBrightness(OnColor,0.1);
    DisplayCount:=6;
    Height:=80;

    AnchorSideLeft.Control := DisplaysPanel;
    AnchorSideTop.Control := TestInfoMemo;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := DisplaysPanel;
    AnchorSideRight.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight];
  end;


  EnergyDisplay:=TdsSevenSegmentMultiDisplay.Create(DisplaysPanel);
  with EnergyDisplay do
  begin
    Parent:=DisplaysPanel;
    OnColor:=clBlue;
    OffColor:=ChangeBrightness(OnColor,0.1);
    DisplayCount:=6;
    Height:=80;

    AnchorSideLeft.Control := DisplaysPanel;
    AnchorSideTop.Control := PowerDisplay;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := DisplaysPanel;
    AnchorSideRight.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight];
  end;

  TemperatureDisplay := TdsSevenSegmentMultiDisplay.Create(grpKC003CPDControl);
  with TemperatureDisplay do
  begin
    Parent:=grpKC003CPDControl;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
    //DisplayCount:=4;
    Height:=50;
    //Width:=btnConnectKC003C.Width;
    AnchorSideLeft.Control := btnConnectKC003C;
    AnchorSideTop.Control := btnConnectKC003C;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := btnConnectKC003C;
    AnchorSideRight.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight];
  end;


  aGrid:=gridRemoteSourcePDO;
  for ACol:=1 to Pred(aGrid.ColCount)  do
  begin
    ARow           := 5;
    Rect           := aGrid.CellRect(ACol,ARow);
    InflateRect(Rect,-4,-2);
    bt             := TButton.Create(aGrid);
    bt.Parent      := aGrid;
    bt.BoundsRect  := Rect;
    bt.Caption     := 'Select';
    bt.Name        := 'bt'+IntToStr(ACol);
    bt.Tag         := 0;
    bt.Enabled     := false;
    index          := aGrid.ComponentCount-1;
    bt             :=(aGrid.Components[index] as TButton);
    aGrid.Objects[ACol,ARow] := aGrid.Components[index];
    bt.OnMouseUp   := aGrid.OnMouseUp;
    bt.OnMouseMove := aGrid.OnMouseMove;
    bt.Visible     := true;
    bt.OnClick     := @GridButtonClick;
  end;

  aGrid:=gridRemoteSourceEPRPDO;
  for ACol:=1 to Pred(aGrid.ColCount)  do
  begin
    ARow           := 5;
    Rect           := aGrid.CellRect(ACol,ARow);
    InflateRect(Rect,-4,-2);
    bt             := TButton.Create(aGrid);
    bt.Parent      := aGrid;
    bt.BoundsRect  := Rect;
    bt.Caption     := 'Select';
    bt.Name        := 'bt'+IntToStr(ACol);
    bt.Tag         := 0;
    bt.Enabled     := false;
    index          := aGrid.ComponentCount-1;
    bt             :=(aGrid.Components[index] as TButton);
    aGrid.Objects[ACol,ARow] := aGrid.Components[index];
    bt.OnMouseUp   := aGrid.OnMouseUp;
    bt.OnMouseMove := aGrid.OnMouseMove;
    bt.Visible     := true;
    bt.OnClick     := @GridButtonClick;
  end;

  chkgrpPDOFLags.ControlStyle := chkgrpPDOFLags.ControlStyle - [csClickEvents];
  chkgrpPDOFLags.Items.Append('DRS');
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Hint:='DataRoleSwap';
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).ShowHint:=True;
  //TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Enabled:=False;
  chkgrpPDOFLags.Items.Append('UCC');
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Hint:='UsbCommunicationCapable';
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).ShowHint:=True;
  //TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Enabled:=False;
  chkgrpPDOFLags.Items.Append('EP');
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Hint:='ExternallyPowered';
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).ShowHint:=True;
  //TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Enabled:=False;
  chkgrpPDOFLags.Items.Append('USS');
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Hint:='UsbSuspendSupported';
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).ShowHint:=True;
  //TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Enabled:=False;
  chkgrpPDOFLags.Items.Append('DRP');
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Hint:='DualRolePower';
  TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).ShowHint:=True;
  //TCheckBox(chkgrpPDOFLags.Controls[Pred(chkgrpPDOFLags.Items.Count)]).Enabled:=False;


  if FileExists('types.dat')
     then TypesBox.Items.LoadFromFile('types.dat')
     else TypesBox.Items.SaveToFile('types.dat');
  if TypesBox.Items.Count=1 then
  begin
    TypesBox.ItemIndex:=0;
    TypesBox.Enabled:=False;
  end;

  if FileExists('samples.dat')
     then SamplesBox.Items.LoadFromFile('samples.dat')
     else SamplesBox.Items.SaveToFile('samples.dat');
  if SamplesBox.Items.Count=1 then
  begin
    SamplesBox.ItemIndex:=0;
    SamplesBox.Enabled:=False;
  end;


  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) );
  try
    NumRate           := Ini.ReadInteger('General', 'NumRate', 10);
    Self.Top          := ini.ReadInteger(Self.Name,'Top',Self.Top);
    Self.Left         := ini.ReadInteger(Self.Name,'Left',Self.Left);
    Self.Width        := ini.ReadInteger(Self.Name,'Width',Self.Width);
    Self.Height       := ini.ReadInteger(Self.Name,'Height',Self.Height);

    i:=1;
    while (i<MaxInt) do
    begin
      if (NOT Ini.SectionExists('Test'+InttoStr(i))) then break;
      Inc(i);
    end;
    Dec(i);

    if (i=0) then
    begin
      i:=1;
      Ini.WriteString('Test'+InttoStr(i), 'Name', 'Default');
      Ini.WriteInteger('Test'+InttoStr(i), 'Voltage', 5000);
      Ini.WriteInteger('Test'+InttoStr(i), 'Current', 1500);
    end;

    if (i>0) then
    begin
      SetLength(TestTypes,i);
      for i:=Low(TestTypes) to High(TestTypes) do
      begin
        TestTypes[i].Name:=Ini.ReadString('Test'+InttoStr(i+1), 'Name', '');
        TestTypes[i].Voltage:=Ini.ReadInteger('Test'+InttoStr(i+1), 'Voltage', 0);
        TestTypes[i].Current:=Ini.ReadInteger('Test'+InttoStr(i+1), 'Current', 0);
      end;
    end;

  finally
    Ini.Free;
  end;

  if (Length(TestTypes)>0) then
  begin
    for i:=Low(TestTypes) to High(TestTypes) do
    begin
      TestsBox.Items.Append(TestTypes[i].Name);
    end;
  end;

  {$ifdef WITHKEITHLEY}
  Tek4020:=TKeithley2700.Create;
  {$endif}
  HPsource:=THP66332.Create;





  STMComport:='';
  KM003CComport:='';

  //if ((Length(STMComport)=0) OR (Length(HPComport)=0)) then
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
              if (Pos('ST-Link',s)=1) then STMComport:=CListDeatails[3];
              if (Pos('POWER-Z',s)=1) then KM003CComport:=CListDeatails[3];
            end;
            if (Length(STMComport)=0) then if CListDeatails[2]='STMicroelectronics' then STMComport:=CListDeatails[3];
          end;
        end;
      finally
        CListDeatails.Free
      end;
    finally
      CLIst.Free;
    end;
  end;

  FSTM32BoardSerial        := 'UNKNOWN';

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) );
  try
    Self.Top          := ini.ReadInteger(Self.Name,'Top',Self.Top);
    Self.Left         := ini.ReadInteger(Self.Name,'Left',Self.Left);
    Self.Width        := ini.ReadInteger(Self.Name,'Width',Self.Width);
    Self.Height       := ini.ReadInteger(Self.Name,'Height',Self.Height);
  finally
    Ini.Free;
  end;

  STM32:=TLazSerial.Create(Self);
  STM32.Async:=True;
  STM32.SOP:=#$FD#$FD#$FD#$FD;
  STM32.EOP:=#$A5#$A5#$A5#$A5;


  KM003C:=TLazSerial.Create(Self);
  KM003C.Async:=false;

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

procedure TPowerbankMainForm.grpVADataResize(Sender: TObject);
begin
  RealVoltageDisplay.Top:=2;
  RealVoltageDisplay.Left:=5;

  RealVoltageDisplay.Width:=(TControl(Sender).Width)-6;
  RealVoltageDisplay.Height:=(TControl(Sender).Height DIV 2)-12;

  RealCurrentDisplay.Width:=RealVoltageDisplay.Width;
  RealCurrentDisplay.Height:=RealVoltageDisplay.Height;
  RealCurrentDisplay.Left:=RealVoltageDisplay.Left;
  RealCurrentDisplay.Top:=RealVoltageDisplay.Top+RealVoltageDisplay.Height;
end;

procedure TPowerbankMainForm.btnCleanLogsClick(Sender: TObject);
begin
  USBComLog.Lines.Clear;
  USBDetailsLog.Lines.Clear;
  USBDebugLog.Lines.Clear;
  MemoUnhandled.Lines.Clear;
  TestInfoMemo.Lines.Clear;
end;

procedure TPowerbankMainForm.btnConnectKC003CClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    Connect(Sender);
    PDTimer.Enabled:=True;
    DataTimer.Enabled:=True;
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TPowerbankMainForm.btnConnectSTM32Click(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    Connect(Sender);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TPowerbankMainForm.btnGetStatusClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_STATUS);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);
  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_STATUS]));
end;

procedure TPowerbankMainForm.btnInitClick(Sender: TObject);
var
  aPort:word;
begin
  if (Length(HPComport)=0) then
  begin
    aPort:=StrToIntDef(cmboSerialPorts.Text,0);
    if (aPort=0) then
    begin
      TestInfoMemo.Lines.Append('Please select serial port.');
      exit;
    end;
  end;

  TButton(Sender).Enabled:=False;
  try
    {$ifdef WITHKEITHLEY}
    TestInfoMemo.Lines.Append('Initializing DMM');
    TestInfoMemo.Invalidate;
    Tek4020.DisConnect;
    sleep(1000);
    Tek4020.Connect;
    Tek4020.Mode:=VoltageMode;
    Tek4020.Range:=3;
    Tek4020.Speed:=SlowSpeed;
    {$endif}

    if (Length(HPComport)>0) then
    begin
      TestInfoMemo.Lines.Append('Looking for HP.');
      TestInfoMemo.Invalidate;
      HPsource.DisConnect;
      sleep(1000);
      HPsource.SerialPortName:=HPComport;
      HPsource.Connect;

      if HPsource.Connected then
      begin
        TestInfoMemo.Lines.Append('Success. Connected with HP.');
        TestInfoMemo.Lines.Append('Brand: '+HPsource.Manufacturer+'.');
        TestInfoMemo.Lines.Append('Model: '+HPsource.Model+'.');
      end
      else
      begin
        TestInfoMemo.Lines.Append('HP failure.');
        TestInfoMemo.Lines.Append('Select correct port.');
      end;

      AllStop(Sender);

    end;

  finally
    TButton(Sender).Enabled:=True;
  end;
end;

procedure TPowerbankMainForm.btnKC003CRcvRemoteSinkClick(Sender: TObject);
begin
  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SNK_CAP]));
end;

procedure TPowerbankMainForm.btnKC003CRcvRemoteSourceClick(Sender: TObject);
begin
  KM003C.WriteString(KC003CCommand[TKC003CCOMMAND.PDMOPEN].Command);
  KM003C.WriteString(KC003CCommand[TKC003CCOMMAND.ENTRYPD].Command);
  KM003C.WriteString(KC003CCommand[TKC003CCOMMAND.PDPDO].Command);
end;

procedure TPowerbankMainForm.btnKC003CResetClick(Sender: TObject);
begin
  KM003C.WriteString(KC003CCommand[TKC003CCOMMAND.RESET].Command);
end;

procedure TPowerbankMainForm.btnRcvRemoteEPRSourceClick(Sender: TObject);
var
  SOPHeader:TPDHEADER;
  SOPHeaderExtended:TPDHEADEREXTENDED;
  GBDB:TGBDB;
  DWordData:TDWordData;
  Data:string;
  j:integer;
begin
  // Create Header
  SOPHeader.Raw:=0;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_EPR_SOURCE_CAPABILITIES);
  SOPHeader.Data.Specification_Revision:=2;
  SOPHeader.Data.Port_Power_Role_or_Plug:=1;
  SOPHeader.Data.Number_of_Data_Objects:=2;
  SOPHeader.Data.Extended:=1;

  // Create Extended Header
  SOPHeaderExtended.Raw:=0;
  SOPHeaderExtended.Data.Data_Size:=9;
  SOPHeaderExtended.Data.Chunked:=1;

  // Create DataObject
  DWordData.Raw:=0;
  GBDB.Raw:=0;
  DWordData.Bytes[0]:=GBDB.Raw;

  // Add SOP
  Data:=InttoHex(Ord(USBPD_SOPTYPE_SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  //Data:=Data+InttoHex(GBDB.Raw,2);
  //for j:=0 to 3 do Data:=Data+InttoHex(DWordData.Bytes[j],2);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));
end;

procedure TPowerbankMainForm.btnBatteryStatusClick(Sender: TObject);
const
  BATTNUMBER = 0;
var
  Buffer:array[0..255] of byte;
  SOPHeader:TPDHEADER;
  SOPHeaderExtended:TPDHEADEREXTENDED;
  GBDB:TGBDB;
  DWordData:TDWordData;
  Data:string;
  j:integer;
begin
  vleBatteryData.Values['SOC']:='';

  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_BAT_STATUS);
  Buffer[1]:=0;
  Buffer[2]:=0;
  Buffer[3]:=Ord(GUI_PARAM_MSG_BATTERYREF);
  Buffer[4]:=0;
  Buffer[5]:=1;
  Buffer[6]:=BATTNUMBER;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,7);


  // Create Header
  SOPHeader.Raw:=$0000;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_GET_BATTERY_STATUS);
  SOPHeader.Data.Specification_Revision:=2;
  SOPHeader.Data.Port_Power_Role_or_Plug:=1;
  SOPHeader.Data.Number_of_Data_Objects:=1;
  SOPHeader.Data.Extended:=1;

  // Create Extended Header
  SOPHeaderExtended.Raw:=$0000;
  SOPHeaderExtended.Data.Data_Size:=7;
  SOPHeaderExtended.Data.Chunked:=1;

  // Create DataObject
  DWordData.Raw:=0;
  GBDB.Raw:=0;
  DWordData.Bytes[0]:=GBDB.Raw;

  // Add SOP
  Data:=InttoHex(Ord(USBPD_SOPTYPE_SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  for j:=0 to 3 do Data:=Data+InttoHex(DWordData.Bytes[j],2);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));
end;

procedure TPowerbankMainForm.btnBatteryCapabilitiesClick(Sender: TObject);
const
  BATTNUMBER = 0;
var
  Buffer:array[0..255] of byte;
  SOPHeader:TPDHEADER;
  SOPHeaderExtended:TPDHEADEREXTENDED;
  GBDB:TGBDB;
  DWordData:TDWordData;
  Data:string;
  j:integer;
begin
  vleBatteryData.Values['VID']:='';
  vleBatteryData.Values['Type']:='';
  vleBatteryData.Values['Capacity']:='';


  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_BAT_CAPA);
  Buffer[1]:=0;
  Buffer[2]:=0;
  Buffer[3]:=Ord(GUI_PARAM_MSG_BATTERYREF);
  Buffer[4]:=0;
  Buffer[5]:=1;
  Buffer[6]:=BATTNUMBER;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,7);


  // Create Header
  SOPHeader.Raw:=0;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_GET_BATTERY_CAP);
  SOPHeader.Data.Specification_Revision:=2;
  SOPHeader.Data.Port_Power_Role_or_Plug:=1;
  SOPHeader.Data.Number_of_Data_Objects:=2;
  SOPHeader.Data.Extended:=1;

  // Create Extended Header
  SOPHeaderExtended.Raw:=0;
  SOPHeaderExtended.Data.Data_Size:=9;
  SOPHeaderExtended.Data.Chunked:=1;

  // Create DataObject
  DWordData.Raw:=0;
  GBDB.Raw:=0;
  DWordData.Bytes[0]:=GBDB.Raw;

  // Add SOP
  Data:=InttoHex(Ord(USBPD_SOPTYPE_SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add padding
  //for j:=0 to 1 do Data:=Data+InttoHex(0,2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  Data:=Data+InttoHex(GBDB.Raw,2);
  for j:=0 to 3 do Data:=Data+InttoHex(DWordData.Bytes[j],2);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));
end;

procedure TPowerbankMainForm.btnRcvRemoteSourceExtClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_SOURCE_CAPA_EXTENDED);
  Buffer[1]:=0;
  Buffer[2]:=0;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SRC_CAPEXT]));
end;

procedure TPowerbankMainForm.btnRcvRemoteSinkExtClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_SINK_CAPA_EXTENDED);
  Buffer[1]:=0;
  Buffer[2]:=0;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SNK_CAPEXT]));
end;

procedure TPowerbankMainForm.btnResetClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    SendCommand(0,Ord(DPM_RESET_REQ),nil,0);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TPowerbankMainForm.btnRcvSelfClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_PARAM_SNK_PDO);
  SendCommand(1,Ord(DPM_CONFIG_GET_REQ),Buffer,3);
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_PARAM_SRC_PDO);
  SendCommand(1,Ord(DPM_CONFIG_GET_REQ),Buffer,3);
end;

procedure TPowerbankMainForm.btnRcvRemoteSourceClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_SRC_CAPA);
  Buffer[1]:=0;
  Buffer[2]:=0;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);
end;

procedure TPowerbankMainForm.btnRcvRemoteSinkClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_SNK_CAPA);
  Buffer[1]:=0;
  Buffer[2]:=0;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);
end;

procedure TPowerbankMainForm.btnHardResetClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}Buffer,SizeOf(Buffer),0);
    Buffer[0]:=Ord(GUI_MSG_HARD_RESET);
    SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TPowerbankMainForm.btnSwapVconnClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_VCONN_SWAP);
  Buffer[1]:=0;
  Buffer[2]:=0;
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);
end;

procedure TPowerbankMainForm.btnTestDischargeClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Down then
  begin
    if (
       (HPsource.Connected)
       //AND
       //(TypesBox.ItemIndex<>-1)
       //AND
       //(SamplesBox.ItemIndex<>-1)
       AND
       (Length(ActiveTestType.Name)>0)
       )
    then
    begin
      StartTime:=NowUTC;
      LastTime:=StartTime;
      SetChartAxis(Sender);
      SetEnable(Sender,false);
      TestInfoMemo.Lines.Append('Checking selected discharge.');
      SystemActive:=True;
      HPsource.SetOutput(SystemActive);
      HPsource.SetCurrentSlow((ActiveTestType.Current/1000));
      TestTimer.Enabled:=true;
    end else TSpeedButton(Sender).Down:=False;
  end
  else
  //if (NOT TSpeedButton(Sender).Down) then
  begin
    AllStop(Sender);
    TestInfoMemo.Lines.Append('Check finished.');
  end;
end;

procedure TPowerbankMainForm.btnVDMDiscoIndentClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_VDM_DISCO_IDENT);
  Buffer[1]:=0;
  Buffer[2]:=0;
  Buffer[3]:=Ord(USBPD_SOPTYPE_SOP);
  Buffer[4]:=0;
  Buffer[5]:=1;
  Buffer[6]:=Ord(USBPD_SOPTYPE_SOP);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,7);
end;

procedure TPowerbankMainForm.btnVDMDiscoSVIDClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_VDM_DISCO_SVID);
  Buffer[1]:=0;
  Buffer[2]:=0;
  Buffer[3]:=Ord(USBPD_SOPTYPE_SOP);
  Buffer[4]:=0;
  Buffer[5]:=1;
  Buffer[6]:=Ord(USBPD_SOPTYPE_SOP1);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,7);
end;

procedure TPowerbankMainForm.btnGetSourceInfoClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_SOURCE_CAPA);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SOURCE_INFO]));
end;

procedure TPowerbankMainForm.btnGetPPSStatusClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_GET_PPS_STATUS);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);

  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_PPS_STATUS]));
end;

procedure TPowerbankMainForm.Button1Click(Sender: TObject);
begin
  KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDMSETTYPE].Command,[0,2,1]));
end;

procedure TPowerbankMainForm.btnSwapClick(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_PR_SWAP);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,3);
end;


procedure TPowerbankMainForm.DataEditKeyPress(Sender: TObject; var Key: char);
begin
  if (not CharInSet(Key,[#8, '0'..'9', '-', FormatSettings.DecimalSeparator])) then
  begin
    Key := #0;
  end
  else if (Key = FormatSettings.DecimalSeparator) and
          (Pos(Key, (Sender as TEdit).Text) > 0) then
  begin
    Key := #0;
  end;
  {
  else if (Key = '-') and
          ((Sender as TEdit).SelStart <> 0) then
  begin
    ShowMessage('Only allowed at beginning of number: ' + Key);
    Key := #0;
  end;
  }
end;

procedure TPowerbankMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Buffer:array[0..255] of byte;
  Ini : TIniFile;
begin
    if (false) then
    begin
      CloseAction :=CaNone;
    end
    else if MessageDlg ('Are you REALLY SURE you want to exit ?'+
                    chr(13)+'(This is your last change to stay with us!)',
                    mtConfirmation, [mbYes,mbNo],0)=idNo
       then CloseAction :=CaNone
       else
       begin
         SetEnable(Sender,false);

         AllStop(Sender);

         FillChar({%H-}Buffer,SizeOf(Buffer),0);
         //Buffer[0]:=Ord(GUI_INIT_HWBOARDVERSION);
         //Buffer[1]:=0;
         //Buffer[2]:=0;
         //SendCommand(0,Ord(DPM_RESET_REQ),Buffer,0, false);

         FillChar({%H-}Buffer,SizeOf(Buffer),0);
         Buffer[0]:=Ord(GUI_PARAM_MEASUREREPORTING);
         Buffer[1]:=0;
         Buffer[2]:=1;
         Buffer[3]:=0;
         SendCommand(1,Ord(DPM_CONFIG_SET_REQ),Buffer,4, false);
         SendCommand(2,Ord(DPM_CONFIG_SET_REQ),Buffer,4, false);

         Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) );
         try
           ini.WriteInteger(Self.Name,'Top',Self.Top);
           ini.WriteInteger(Self.Name,'Left',Self.Left);
           ini.WriteInteger(Self.Name,'Width',Self.Width);
           ini.WriteInteger(Self.Name,'Height',Self.Height);

           Ini.WriteInteger('General', 'NumRate', NumRate);
         finally
           Ini.Free;
         end;
         CloseAction:=caFree;
       end;
end;

procedure TPowerbankMainForm.FormDestroy(Sender: TObject);
begin
  DisConnect(Sender);
  HPsource.Free;
  DUT.Free;
  STM32PDController.Free;
end;

procedure TPowerbankMainForm.StartStopButtonClick(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
  try
    if TSpeedButton(Sender).Down then
    begin
      if (
         //(HPsource.Connected)
         //AND
         (TypesBox.ItemIndex<>-1)
         AND
         (SamplesBox.ItemIndex<>-1)
         AND
         (Length(ActiveTestType.Name)>0)
         )
      then
      begin
        SetEnable(Sender,false);

        Voltage:=0;
        Current:=0;

        Energy:=0;

        SetChartAxis(Sender);

        CreateDataFile(Sender);
        TestInfoMemo.Lines.Append('Filename: '+BatteryDataFile);

        SystemActive:=True;
        HPsource.SetOutput(SystemActive);
        HPsource.SetCurrentSlow(ActiveTestType.Current/1000);

        TestInfoMemo.Lines.Append('Test started.');

        StartTime:=NowUTC;
        LastTime:=StartTime;

        StoreTimer.Enabled:=False;
        StoreTimer.Interval:=NumRate*1000;
        StoreTimer.Enabled:=True;

        StoreTimerTimer(nil);

      end else TSpeedButton(Sender).Down:=false;
    end
    else
    begin
      TestInfoMemo.Lines.Append('Test stopped.');
    end;

    if TSpeedButton(Sender).Down then
    begin
      TSpeedButton(Sender).Caption:='Stop';
      TSpeedButton(Sender).Font.Color:=clRed;
    end
    else
    begin
      AllStop(Sender);
    end;

  finally
    TSpeedButton(Sender).Enabled:=True;
  end;
end;

procedure TPowerbankMainForm.StoreTimerTimer(Sender: TObject);
const
  STOPPERCENTAGE = 80;
var
  Elapsed:longword;
begin
  Measure;

  Chart1LineSeries1.Add(Voltage);
  Chart1LineSeries2.Add(Current);

  if SystemActive then
  begin
    // only perform the below if coming from the timer !
    if Assigned(Sender) then
    begin
      Elapsed:=MilliSecondsBetween(NowUTC,LastTime);
      LastTime:=NowUTC;

      Power:=Voltage*Current;
      Capacity:=Capacity+1000*Current*(Elapsed/3600000);
      Energy:=Energy+1000*Power*(Elapsed/3600000);

      SaveBatteryData(Elapsed);

      if (
         (chkVoltageLimit.Checked)
         AND
         (Voltage<(ActiveTestType.Voltage*(STOPPERCENTAGE/100)/1000))
         )
      then
      begin
        TestInfoMemo.Lines.Append('Voltage below threshold. Test ready.');
        AllStop(Sender);
      end;

      if (
         (chkCurrentLimit.Checked)
         AND
         (Current<(ActiveTestType.Current*(STOPPERCENTAGE/100)/1000))
         )
      then
      begin
        TestInfoMemo.Lines.Append('Current below threshold. Test ready.');
        AllStop(Sender);
      end;

    end;

  end;
end;

procedure TPowerbankMainForm.TestsBoxChange(Sender: TObject);
var
  aCombo:TComboBox;
begin
  aCombo:=nil;
  if (Sender<>nil) then aCombo:=TComboBox(Sender);
  if ((aCombo<>nil) AND (aCombo.ItemIndex<>-1)) then
  begin
    ActiveTestType.Name:=TestTypes[aCombo.ItemIndex].Name;
    ActiveTestType.Current:=TestTypes[aCombo.ItemIndex].Current;
    ActiveTestType.Voltage:=TestTypes[aCombo.ItemIndex].Voltage;
  end
  else
  begin
    ActiveTestType.Name:='';
    ActiveTestType.Current:=0;
    ActiveTestType.Voltage:=0;
  end;
  CurrentEdit.Text:=InttoStr(ActiveTestType.Current);
  VoltageEdit.Text:=InttoStr(ActiveTestType.Voltage);

  CurrentEdit.ReadOnly:=((Pos('Power',ActiveTestType.Name)=0) AND (Pos('Variable',ActiveTestType.Name)=0));
  VoltageEdit.ReadOnly:=CurrentEdit.ReadOnly;
end;

procedure TPowerbankMainForm.TestTimerTimer(Sender: TObject);
begin
  if (ProgressBar1.Position=ProgressBar1.Max) then
    ProgressBar1.Position:=1
  else
    ProgressBar1.StepIt;
end;

procedure TPowerbankMainForm.UpdateTimerTimer(Sender: TObject);
begin
  if (StartTime<>0) then
    Edit1.Text:='Running: '+InttoStr(SecondsBetween(NowUTC,StartTime))+' sec'
  else
    Edit1.Text:='CONSULAB    ' + DateTimeToStr(NowUTC);
end;

procedure TPowerbankMainForm.SetEnable(Sender: TObject; value:boolean);
begin
  if Sender<>nil then
  begin
    if (Sender<>btnInit) then btnInit.Enabled:=value;
    if (Sender<>cmboSerialPorts) then cmboSerialPorts.Enabled:=value;

    if (Sender<>StartStopButton) then StartStopButton.Enabled:=value;

    if (Sender<>btnTestDischarge) then btnTestDischarge.Enabled:=value;

    if (Sender<>TypesBox) then TypesBox.Enabled:=value;
    if (Sender<>SamplesBox) then SamplesBox.Enabled:=value;
    if (Sender<>TestsBox) then TestsBox.Enabled:=value;
    if (Sender<>CurrentEdit) then CurrentEdit.Enabled:=value;
    if (Sender<>VoltageEdit) then VoltageEdit.Enabled:=value;

    if (Sender<>chkVoltageLimit) then chkVoltageLimit.Enabled:=value;
    if (Sender<>chkCurrentLimit) then chkCurrentLimit.Enabled:=value;

    Application.ProcessMessages;
  end;
end;


procedure TPowerbankMainForm.gridPDOResize(Sender: TObject);
var
  aGrid:TStringGrid;
begin
  aGrid:=TStringGrid(Sender);
  aGrid.Height:=aGrid.DefaultRowHeight*aGrid.RowCount+4;
end;

procedure TPowerbankMainForm.SendCommand(Port,Command:byte;DataToSend:pbyte;DataToSendLength:word;Confirmation:boolean);
var
  DataBuffer:array[0..255] of byte;
  DataString:ansistring;
  x,y:byte;
begin
  if (NOT STM32.Active) then exit;

  y:=0;
  FillChar({%H-}DataBuffer,SizeOf(DataBuffer),0);
  for x:=0 to 3 do
  begin
    DataBuffer[y]:=$FD;
    Inc(y);
  end;
  DataBuffer[y]:=(byte(Command AND $1F) OR byte((PORT shl 5) AND $E0));
  Inc(y);
  DataBuffer[y]:=(DataToSendLength DIV 256);
  Inc(y);
  DataBuffer[y]:=(DataToSendLength MOD 256);
  Inc(y);
  if ((DataToSendLength>0) AND (DataToSend<>nil)) then Move(DataToSend^,DataBuffer[y],DataToSendLength);
  Inc(y,DataToSendLength);
  for x:=0 to 3 do
  begin
    DataBuffer[y]:=$A5;
    Inc(y);
  end;
  SetString(DataString, PAnsiChar(@DataBuffer[0]), y);

  if (
     (Command=Ord(DPM_INIT_REQ))
     OR
     (Command=Ord(DPM_CONFIG_GET_REQ))
     OR
     (Command=Ord(DPM_CONFIG_SET_REQ))
     OR
     (Command=Ord(DPM_MESSAGE_REQ))
     OR
     (Command=Ord(DPM_REGISTER_READ_REQ))
     OR
     (Command=Ord(DPM_REGISTER_WRITE_REQ))
     ) then
  begin
    // wait for confirmation
    // if not: repeat request
    if Confirmation then
      FMessageConfirmed:=false
    else
      FMessageConfirmed:=true;
    y:=0;
    repeat
      STM32.WriteString(DataString);
      x:=0;
      repeat
        sleep(50);
        Application.ProcessMessages;
        Inc(x);
      until (FMessageConfirmed OR (x>50));
      {$ifdef DEBUG}
      if (NOT FMessageConfirmed) then
      begin
        //USBDebugLog.Lines.Append('We have a request failure !! Resend request. '+GetEnumNameSimple(TypeInfo(MESSAGE_TYPE),Ord(Command))+' on port '+InttoStr(Port));
        //Application.ProcessMessages;
      end;
      {$endif DEBUG}
      Inc(y);
    until (FMessageConfirmed OR (y>2));
    {$ifdef DEBUG}
    if (NOT FMessageConfirmed) then
    begin
      //USBDebugLog.Lines.Append('We have a request failure !! Request NOT confirmed. '+GetEnumNameSimple(TypeInfo(MESSAGE_TYPE),Ord(Command))+' on port '+InttoStr(Port));
      //Application.ProcessMessages;
    end;
    {$endif DEBUG}
  end
  else
  begin
    STM32.WriteString(DataString);
    sleep(25);
    Application.ProcessMessages;
  end;

end;

function TPowerbankMainForm.CorrectVoltage(value:double):double;
begin
  result:=value;
end;
function TPowerbankMainForm.CorrectCurrent(value:double):double;
begin
  result:=value;
end;

procedure TPowerbankMainForm.SetVoltage(value:double);
begin
  if (FVoltage<>value) then
  begin
    FVoltage:=value;
    RealVoltageDisplay.Value:=value;
  end;
end;

function  TPowerbankMainForm.GetVoltage:double;
begin
  result:=FVoltage;
end;

procedure TPowerbankMainForm.SetCurrent(value:double);
begin
  if (FCurrent<>value) then
  begin
    FCurrent:=value;
    RealCurrentDisplay.Value:=value;
  end;
end;

function  TPowerbankMainForm.GetCurrent:double;
begin
  result:=FCurrent;
end;

procedure TPowerbankMainForm.SetEnergy(value:double);
begin
  if (FEnergy<>value) then
  begin
    FEnergy:=value;
    EnergyDisplay.Value:=value;
  end;
end;

function  TPowerbankMainForm.GetEnergy:double;
begin
  result:=FEnergy;
end;

procedure TPowerbankMainForm.SetPower(value:double);
begin
  if (FPower<>value) then
  begin
    FPower:=value;
    PowerDisplay.Value:=value;
  end;
end;

function  TPowerbankMainForm.GetPower:double;
begin
  result:=FPower;
end;

procedure TPowerbankMainForm.SetTemperature(value:double);
begin
  if (FTemperature<>value) then
  begin
    FTemperature:=value;
    TemperatureDisplay.Value:=value;
  end;
end;

function  TPowerbankMainForm.GetTemperature:double;
begin
  result:=FTemperature;
end;

procedure TPowerbankMainForm.OnRXUSBCData(Sender: TObject);
var
  i,j               : word;
  diff              : integer;
  USBData           : string;
  s                 : string;
  enumname          : string;

  MessageLengthData,MessagePortNumber:byte;
  MessageType:TMESSAGE_TYPE;
  MessageData:array of byte;

  StackMessageLength:byte;
  {%H-}StackMessageTime:DWord;
  {%H-}StackMessageUSBPort:byte;
  StackMessageIDRaw,MessageID:byte;
  StackEvent:TRACE_EVENT;
  StackMessageSOP:USBPD_SOPTYPE;

  aSOPLength:word;
  aSOPHeader:TPDHEADER;
  {%H-}aSOPExtendedHeader:TPDHEADEREXTENDED;
  aGUIMessage:GUI_TAG;
  aGUIInitMessage:GUI_INIT_TAG;
  aGUIParam:GUI_PARAM_TAG;
  aGUIPower:USBPD_POWER_NO;
  aGUILengthData:byte;

  ValidRemoteSourcePDO:boolean;
  ValidRemoteRDO:boolean;
  ValidRemoteSinkPDO:boolean;
  aLog:TMemo;
  WordData:TWordData;
  DWordData:TDWordData;
  DataIndexer:word;

  PDDataMessage:TUSBPD_DATAMSG;
  PDExtendedMessage:TUSBPD_EXTENDEDMSG;
  PDControlMessage:TUSBPD_CONTROLMSG;
  PDCADEvent:TUSBPD_CAD_EVENT;
  PDNotify:USBPD_NOTIFY;

  PDOSupplyType:TSUPPLY_TYPES;
  PDOVoltage:integer;
  PDOCurrent:integer;
begin
  //if InterLockedExchange(TimersBusy, integer(True))<>integer(True) then
  try
    USBData:=STM32.Data;

    USBDebugLog.Lines.Append('Message #'+InttoStr(USBDebugLog.Lines.Count)+': '+StrToHex(USBData));

    if (Length(USBData)=0) then
    begin
      USBDetailsLog.Lines.Append('');
      USBDetailsLog.Lines.Append('!!! EMPTY !!!');
      USBDetailsLog.Lines.Append('');
      exit;
    end;

    ValidRemoteSourcePDO:=false;
    ValidRemoteSinkPDO:=false;
    ValidRemoteRDO:=false;

    s:='';

    aLog:=USBDetailsLog;

    DataIndexer:=1;

    MessageID:=Ord(USBData[DataIndexer]);
    Inc(DataIndexer);
    MessagePortNumber:=((MessageID AND $E0) shr 5);
    MessageType:=TMESSAGE_TYPE((MessageID AND $1F));

    WordData.NamedBytes.HSB:=Ord(USBData[DataIndexer]);
    Inc(DataIndexer);
    WordData.NamedBytes.LSB:=Ord(USBData[DataIndexer]);
    Inc(DataIndexer);
    MessageLengthData:=WordData.Raw;

    if (MessageLengthData>0) then
    begin
      SetLength({%H-}MessageData,MessageLengthData);
      for i:=0 to Pred(MessageLengthData) do
      begin
        MessageData[i]:=Ord(USBData[DataIndexer]);
        Inc(DataIndexer);
      end;
    end;

    DataIndexer:=0;

    {$ifdef DEBUG}
    //s:='Raw message. Tag:'+IntToStr(MessageID)+'. Port:'+IntToStr(MessagePortNumber)+'. '+'. Length:'+IntToStr(MessageLengthData)+'. '+GetEnumNameSimple(TypeInfo(MESSAGE_TYPE),Ord(aMessage))+'. Data: ';
    //USBDebugLog.Lines.Append(s);
    {$endif}

    if MessageType in
    [
      DPM_INIT_CNF,
      DPM_CONFIG_GET_CNF,
      DPM_CONFIG_SET_CNF,
      DPM_CONFIG_REJ,
      DPM_MESSAGE_CNF,
      DPM_MESSAGE_REJ,
      DPM_REGISTER_READ_CNF,
      DPM_REGISTER_WRITE_CNF
    ]
    then
    begin
      FMessageConfirmed:=true;
    end;

    if ((MessagePortNumber=1) OR (MessagePortNumber=2)) then
    begin
      case MessageType of
        DEBUG_STACK_MESSAGE:
        begin
          aLog:=USBDebugLog;

          StackEvent:=TRACE_EVENT(MessageData[DataIndexer]);
          Inc(DataIndexer);

          DWordData.NamedBytes.LSB:=MessageData[DataIndexer];
          Inc(DataIndexer);
          DWordData.NamedBytes.HSB:=MessageData[DataIndexer];
          Inc(DataIndexer);
          DWordData.NamedBytes.USB:=MessageData[DataIndexer];
          Inc(DataIndexer);
          DWordData.NamedBytes.MSB:=MessageData[DataIndexer];
          Inc(DataIndexer);
          StackMessageTime:=DWordData.Raw;

          StackMessageUSBPort:=MessageData[DataIndexer];
          Inc(DataIndexer);
          StackMessageIDRaw:=MessageData[DataIndexer];
          StackMessageSOP:=USBPD_SOPTYPE(StackMessageIDRaw);
          Inc(DataIndexer);

          WordData.NamedBytes.HSB:=MessageData[DataIndexer];
          Inc(DataIndexer);
          WordData.NamedBytes.LSB:=MessageData[DataIndexer];
          Inc(DataIndexer);
          StackMessageLength:=WordData.Raw;


          // Reset values to default.
          aSOPHeader.Raw:=0;
          aSOPLength:=0;
          PDDataMessage:=USBPD_DATAMSG_RESERVED0;
          PDExtendedMessage:=USBPD_EXTMSG_RESERVED0;
          PDControlMessage:=USBPD_CONTROLMSG_RESERVED0;

          if ((StackEvent=USBPD_TRACE_MESSAGE_IN) OR (StackEvent=USBPD_TRACE_MESSAGE_OUT)) then
          begin
            //Check if we have a USB PD message
            if (StackMessageLength>0) then
            begin
              // Parse the header SOP header

              WordData.NamedBytes.LSB:=MessageData[DataIndexer];
              Inc(DataIndexer);
              WordData.NamedBytes.HSB:=MessageData[DataIndexer];
              Inc(DataIndexer);
              aSOPHeader.Raw:=WordData.Raw;

              GetSOPInfo(aSOPHeader);
              aSOPLength:=aSOPHeader.Data.Number_of_Data_Objects;

              // Do we have a control message
              if ((aSOPLength=0) AND (aSOPHeader.Data.Extended=0)) then
              begin
                PDControlMessage:=TUSBPD_CONTROLMSG(aSOPHeader.Data.Message_Type);
              end
              else
              begin
                if (aSOPHeader.Data.Extended=0) then PDDataMessage:=TUSBPD_DATAMSG(aSOPHeader.Data.Message_Type);
                if (aSOPHeader.Data.Extended=1) then
                begin
                  // We have an extended message !!
                  PDExtendedMessage:=TUSBPD_EXTENDEDMSG(aSOPHeader.Data.Message_Type);
                  // Get the extra header.
                  aSOPExtendedHeader.Bytes[0]:=MessageData[DataIndexer];
                  Inc(DataIndexer);
                  aSOPExtendedHeader.Bytes[1]:=MessageData[DataIndexer];
                  Inc(DataIndexer);
                end
              end;
            end;
            //s:=s+'SOP: '+GetEnumNameSimple(TypeInfo(USBPD_SOPTYPE),StackMessageIDRaw)+'. ';
            //s:=s+Copy(GetEnumNameSimple(TypeInfo(TRACE_EVENT),aStackMessageType),Length('USBPD_TRACE_')+1,MaxInt)+'. Message: '+InttoStr(aSOPMessage)+'. '+'. Objects: '+InttoStr(aSOPLength)+'. ';
          end;

          case StackEvent of
            USBPD_TRACE_MESSAGE_IN:
            begin
              aLog:=USBComLog;
              s:=s+'IN: ';

              if ((PDDataMessage=USBPD_DATAMSG_RESERVED0) AND (PDExtendedMessage=USBPD_EXTMSG_RESERVED0) AND (PDControlMessage=USBPD_CONTROLMSG_RESERVED0)) then
              begin
                MemoUnhandled.Lines.Append('UNKNOWN USBPD_TRACE_MESSAGE_IN');
              end;

              if (PDDataMessage<>USBPD_DATAMSG_RESERVED0) then
              begin
                if DUT.ProcessDataMessage(PDDataMessage,aSOPLength,@MessageData[DataIndexer]) then
                begin
                  ProcessDataMessageGUI(PDDataMessage);
                  case PDDataMessage of
                    USBPD_DATAMSG_SRC_CAPABILITIES:
                    begin
                      ValidRemoteSourcePDO:=true;
                    end;
                    USBPD_DATAMSG_SNK_CAPABILITIES:
                    begin
                      ValidRemoteSinkPDO:=true;
                    end;
                  end;
                end
                else
                begin
                  Str(PDDataMessage,enumname);
                  MemoUnhandled.Lines.Append('USBPD_TRACE_MESSAGE_IN. UNHANDLED DATA MESSAGE: '+enumname);
                end;
              end;

              // Extended messages
              if (PDExtendedMessage<>USBPD_EXTMSG_RESERVED0) then
              begin
                if DUT.ProcessExtendedMessage(PDExtendedMessage,@MessageData[DataIndexer]) then
                begin
                  ProcessExtendedMessageGUI(PDExtendedMessage);
                end
                else
                begin
                  Str(PDExtendedMessage,enumname);
                  MemoUnhandled.Lines.Append('USBPD_TRACE_MESSAGE_IN. UNHANDLED EXTENDED MESSAGE: '+enumname);
                end;
                Dec(DataIndexer,2);
              end;

              if (PDControlMessage<>USBPD_CONTROLMSG_RESERVED0) then
              begin
                ProcessControlMessageGUI(PDControlMessage);
              end;

              Inc(DataIndexer,aSOPLength*4);
            end;

            USBPD_TRACE_MESSAGE_OUT:
            begin
              aLog:=USBComLog;
              s:=s+'OUT: ';

              if ((PDDataMessage=USBPD_DATAMSG_RESERVED0) AND (PDExtendedMessage=USBPD_EXTMSG_RESERVED0) AND (PDControlMessage=USBPD_CONTROLMSG_RESERVED0)) then
              begin
                MemoUnhandled.Lines.Append('UNKNOWN USBPD_TRACE_MESSAGE_OUT');
              end;

              if (PDDataMessage<>USBPD_DATAMSG_RESERVED0) then
              begin
                if STM32PDController.ProcessDataMessage(PDDataMessage,aSOPLength,@MessageData[DataIndexer]) then
                begin
                  //ProcessDataMessageGUI(PDDataMessage);
                  case PDDataMessage of
                    USBPD_DATAMSG_SRC_CAPABILITIES:
                    begin
                      s:=s+'Source PDO'+#13#10;
                      for i:=1 to aSOPLength do
                      begin
                        s:=s+'Self Source PDO. ';
                        s:=s+STM32PDController.GetSRCPDOInfo(i);
                        if (i<aSOPLength) then s:=s+#13#10;
                      end;
                    end;
                    USBPD_DATAMSG_SNK_CAPABILITIES:
                    begin
                      s:=s+'Sink PDO'+#13#10;
                      for i:=1 to aSOPLength do
                      begin
                        s:=s+'Self Sink PDO.';
                        s:=s+STM32PDController.GetSNKPDOInfo(i);
                        if (i<=aSOPLength) then s:=s+#13#10;
                      end;
                    end;
                    USBPD_DATAMSG_REQUEST:
                    begin
                      ValidRemoteRDO:=True;
                      s:=s+'Which RDO !!??!!'+#13#10;
                      DUT.RDO.Raw:=STM32PDController.RDO.Raw;
                      s:=s+'Official Requested RDO. ';
                      s:=s+DUT.GetRDOInfo;
                    end;
                    USBPD_DATAMSG_VENDOR_DEFINED:
                    begin
                      for j:=0 to 3 do DWordData.Bytes[j]:=MessageData[DataIndexer+j];
                      STM32PDController.VDM_Header.Raw:=DWordData.Raw;
                      s:=s+'VDM header received.';
                    end;
                  end;
                end
                else
                begin
                  Str(PDDataMessage,enumname);
                  MemoUnhandled.Lines.Append('USBPD_TRACE_MESSAGE_OUT. UNHANDLED DATA MESSAGE: '+enumname);
                end;
              end;

              // Extended messages
              if (PDExtendedMessage<>USBPD_EXTMSG_RESERVED0) then
              begin
                if STM32PDController.ProcessExtendedMessage(PDExtendedMessage,@MessageData[DataIndexer]) then
                begin
                  //ProcessExtendedMessageGUI(PDExtendedMessage);
                end
                else
                begin
                  Str(PDExtendedMessage,enumname);
                  MemoUnhandled.Lines.Append('USBPD_TRACE_MESSAGE_OUT. UNHANDLED EXTENDED MESSAGE: '+enumname);
                end;
                Dec(DataIndexer,2);
              end;

              if (PDControlMessage<>USBPD_CONTROLMSG_RESERVED0) then
              begin
                ProcessControlMessageGUI(PDControlMessage);
              end;

              Inc(DataIndexer,aSOPLength*4);
            end;
            USBPD_TRACE_CADEVENT:
            begin
              aLog:=USBDetailsLog;
              s:='CAD: ';
              // Uses SOP as CAD indicator
              PDCADEvent:=TUSBPD_CAD_EVENT(StackMessageIDRaw);
              Str(PDCADEvent,enumname);
              s:=s+enumname;
              if ((PDCADEvent=USBPD_CAD_EVENT_DETACHED) OR (PDCADEvent=USBPD_CAD_EVENT_ATTACHED)) then
              begin
                DUT.Cleanup;
                ValidRemoteSinkPDO:=true;
                ValidRemoteSourcePDO:=true;
                ValidRemoteRDO:=true;
                SetGridSNKPDO(0);
                SetGridSRCPDO(0);
              end;
            end;
            USBPD_TRACE_PE_STATE:
            begin
              aLog:=USBComLog;
              s:='PE: ';
              try
                // Uses SOP as PE_STATE indicator
                s:=s+GetPEStateInfo(TPE_STATE_MESSAGES(StackMessageIDRaw));
              except
                s:=s+'Severe unknown PE message !!';
              end;
            end;
            USBPD_TRACE_NOTIF:
            begin
              s:='NOTIF: ';
              aLog:=USBComLog;
              PDNotify:=USBPD_NOTIFY(StackMessageIDRaw);
              Str(PDNotify,enumname);
              s:=s+enumname;
            end;
            USBPD_TRACE_CAD_LOW:
            begin
              s:='';
              aLog:=nil;
            end;
            USBPD_TRACE_DEBUG:
            begin
              // ALF !!!

              s:='';
              if (StackMessageSOP=USBPD_SOPTYPE_SOP1) then
              begin
                s:=s+'VCONN message';
              end;
              for j:=0 to Pred(StackMessageLength) do
              begin
                if Chr(MessageData[j+DataIndexer]) in [' '..'~'] then s:=s+Chr(MessageData[j+DataIndexer]);
              end;
              Inc(DataIndexer,StackMessageLength);
              if Pos('-- BSP_USBPD_PWR_SetRole :',s)=1 then
              begin
                // We are waiting for a device to connect
                // So, skip these messages !!
                s:='';
                aLog:=nil;
              end;
            end;
            else
            begin
              Str(StackEvent,enumname);
              MemoUnhandled.Lines.Append('USBPD_TRACE_MESSAGE: '+enumname);
              Inc(DataIndexer,StackMessageLength);
            end;
          end;
        end;
        DPM_MESSAGE_IND:
        begin
          while (DataIndexer<(MessageLengthData)) do
          begin
            aGUIMessage:=GUI_TAG(MessageData[DataIndexer]);
            Inc(DataIndexer);
            WordData.NamedBytes.HSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            WordData.NamedBytes.LSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            aGUILengthData:=WordData.Raw;

            DWordData.Raw:=0;
            for j:=0 to 3 do
            begin
              if (aGUILengthData>j) then DWordData.Bytes[j]:=MessageData[DataIndexer+j];
            end;

            case aGUIMessage of
              GUI_IND_ISCONNECTED:
              begin
                aGUIPower:=USBPD_POWER_NO(DWordData.Bytes[0]);
                Str(aGUIPower,enumname);
                s:=s+'. '+enumname;
                editPowerStatusUSBCurrent.Text:=enumname;
              end;
              GUI_IND_VIBUS_LEVEL:
              begin
                aLog:=nil;

                WordData.NamedBytes.LSB:=DWordData.NamedBytes.LSB;
                WordData.NamedBytes.HSB:=DWordData.NamedBytes.HSB;
                if (MessagePortNumber=1) then RealVoltageDisplay.Value:=CorrectVoltage(WordData.Raw/1000);

                WordData.NamedBytes.LSB:=DWordData.NamedBytes.USB;
                WordData.NamedBytes.HSB:=DWordData.NamedBytes.MSB;
                if (MessagePortNumber=1) then RealCurrentDisplay.Value:=CorrectCurrent(WordData.Raw/1000);
              end;
              GUI_IND_LISTOFRCVSRCPDO:
              begin
                if DUT.ProcessDataMessage(USBPD_DATAMSG_SRC_CAPABILITIES,(aGUILengthData DIV 4),@MessageData[DataIndexer]) then
                  ProcessDataMessageGUI(USBPD_DATAMSG_SRC_CAPABILITIES);
              end;
              GUI_IND_LISTOFRCVSNKPDO:
              begin
                if DUT.ProcessDataMessage(USBPD_DATAMSG_SNK_CAPABILITIES,(aGUILengthData DIV 4),@MessageData[DataIndexer]) then
                  ProcessDataMessageGUI(USBPD_DATAMSG_SNK_CAPABILITIES);
              end;
              GUI_IND_TIMESTAMP:
              begin
                aLog:=nil;
              end;
              GUI_IND_CABLE_VDO:
              begin
                DUT.Cable.Raw:=DWordData.Raw;
                s:=s+DUT.GetCableInfo;
              end;
              GUI_IND_CC                                : DUT.ActiveCCIs:=DWordData.Bytes[0];
              GUI_IND_POWERROLE                         : DUT.PowerRole:=DWordData.Bytes[0];
              GUI_IND_CCDEFAULTCURRENTADVERTISED        : DUT.DefaultPower:=DWordData.Bytes[0];
              GUI_IND_DATAROLE                          : DUT.DataRole:=DWordData.Bytes[0];
              GUI_IND_VCONNON                           : DUT.Vconn:=DWordData.Bytes[0];
              GUI_IND_PD_SPECREVISION                   : DUT.PDSpecRevision:=DWordData.Bytes[0];
              GUI_IND_RDOPOSITION                       : DUT.RDOPosition:=DWordData.Bytes[0];
              GUI_IND_NUMBEROFRCVSRCPDO                 : DUT.NumberSRCPDO:=DWordData.Bytes[0];
              GUI_IND_NUMBEROFRCVSNKPDO                 : DUT.NumberSNKPDO:=DWordData.Bytes[0];
              GUI_IND_NBBATTERIES                       : DUT.NBBatteries:=DWordData.Bytes[0];

              GUI_IND_PD_MESSAGENOTIF:
              begin
                PDNotify:=USBPD_NOTIFY(MessageData[DataIndexer]);
                Str(PDNotify,enumname);
                s:=s+'Notification: '+enumname;
              end;

              GUI_IND_VDM_IDENTITY:
              begin
                //if DUT.ProcessDataMessage(USBPD_DATAMSG_VENDOR_DEFINED,(aGUILengthData DIV 4),@MessageData[DataIndexer]) then
                //  ProcessDataMessageGUI(USBPD_DATAMSG_VENDOR_DEFINED);
                DUT.VDO_ID.Raw:=DWordData.Raw;
                s:=s+'Received VDO ID';
                MemoUnhandled.Lines.Append('Received VDO ID ?');
              end;
              GUI_IND_STATUS:
              begin
                if DUT.ProcessExtendedMessage(USBPD_EXTMSG_STATUS,@MessageData[DataIndexer]) then
                  ProcessExtendedMessageGUI(USBPD_EXTMSG_STATUS);
              end;
              GUI_IND_PPS:
              begin
                if DUT.ProcessExtendedMessage(USBPD_EXTMSG_PPS_STATUS,@MessageData[DataIndexer]) then
                  ProcessExtendedMessageGUI(USBPD_EXTMSG_PPS_STATUS);
              end;
              else
              begin
                Str(aGUIMessage,enumname);
                MemoUnhandled.Lines.Append('UNHANDLED GUI_IND: '+enumname);
              end;
            end;
            if aGUILengthData=0 then break;
            Inc(DataIndexer,aGUILengthData);
          end;
        end;
        DPM_INIT_CNF:
        begin
          while (DataIndexer<(MessageLengthData)) do
          begin
            aGUIInitMessage:=GUI_INIT_TAG(MessageData[DataIndexer]);
            Inc(DataIndexer);
            Str(aGUIInitMessage,enumname);
            s:=enumname+': ';

            WordData.NamedBytes.HSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            WordData.NamedBytes.LSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            aGUILengthData:=WordData.Raw;

            if (MessagePortNumber=1) then
            begin
              {$ifdef CALIBRATION}
              if aGUIInitMessage in [GUI_INIT_VOLTAGE_SLOPE,GUI_INIT_VOLTAGE_OFFSET,GUI_INIT_CURRENT_SLOPE,GUI_INIT_CURRENT_OFFSET] then
              begin
                calval:=PDouble(@MessageData[i])^;
              end;
              case aGUIInitMessage of
                GUI_INIT_VOLTAGE_SLOPE: FBoardVoltageSlope:=calval;
                GUI_INIT_VOLTAGE_OFFSET:FBoardVoltageOffset:=calval;
                GUI_INIT_CURRENT_SLOPE: FBoardCurrentSlope:=calval;
                GUI_INIT_CURRENT_OFFSET:FBoardCurrentOffset:=calval;
              end;
              {$endif}
            end;
            if aGUILengthData=0 then break;
            for j:=0 to Pred(aGUILengthData) do
            begin
              s:=s+InttoHex(MessageData[DataIndexer+j]);
            end;
            s:=s+#13#10;
            Inc(DataIndexer,aGUILengthData);
          end;
        end;
        DPM_MESSAGE_REJ,DPM_CONFIG_REJ:
        begin
          if MessageType=DPM_MESSAGE_REJ then s:=s+' Message rejected !!';
          if MessageType=DPM_CONFIG_REJ then s:=s+' Config rejected !!';
          s:=s+' Reason: '+GetEnumNameSimple(TypeInfo(USBPD_GUI_REJECT_REASON),MessageData[DataIndexer]);
          Inc(DataIndexer);
          while (DataIndexer<(MessageLengthData)) do
          begin
            s:=s+' Param: '+InttoStr(MessageData[DataIndexer]);
            Inc(DataIndexer);
          end;
        end;
        DPM_CONFIG_GET_CNF:
        begin
          while (DataIndexer<(MessageLengthData)) do
          begin
            aGUIParam:=GUI_PARAM_TAG(MessageData[DataIndexer]);
            Inc(DataIndexer);
            WordData.NamedBytes.HSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            WordData.NamedBytes.LSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            aGUILengthData:=WordData.Raw;

            case aGUIParam of
              GUI_PARAM_SNK_PDO:
              begin
                s:=s+#13#10;
                i:=1;
                // Data arrived in multiples of 4 bytes
                while (i<=(aGUILengthData DIV 4)) do
                begin
                  for j:=0 to 3 do DWordData.Bytes[j]:=MessageData[DataIndexer+j+(i-1)*4];
                  STM32PDController.SinkPDOs[i].Raw:=DWordData.Raw;
                  s:=s+'Self Sink PDO. ';
                  s:=s+STM32PDController.GetSNKPDOInfo(i);
                  s:=s+#13#10;
                  Inc(i);
                end;
              end;

              GUI_PARAM_SRC_PDO:
              begin
                s:=s+#13#10;
                i:=1;
                // Data arrived in multiples of 4 bytes
                while (i<=(aGUILengthData DIV 4)) do
                begin
                  for j:=0 to 3 do DWordData.Bytes[j]:=MessageData[DataIndexer+j+(i-1)*4];
                  STM32PDController.SourcePDOs[i].Raw:=DWordData.Raw;
                  s:=s+'Self Source PDO. ';
                  s:=s+STM32PDController.GetSRCPDOInfo(i);
                  s:=s+#13#10;
                  Inc(i);
                end;
              end;
              GUI_PARAM_SOP:
              begin
                //aLog:=nil;
              end;
              else
              begin
                Str(aGUIParam,enumname);
                MemoUnhandled.Lines.Append('UNHANDLED GUI_PARAM_TAG: '+enumname);
                s:=s+'UNHANDLED GUI_PARAM_TAG. '+enumname+': ';
                for j:=0 to Pred(aGUILengthData) do
                begin
                  s:=s+InttoHex(MessageData[DataIndexer+j])+' ';
                end;
              end;
            end;
            if aGUILengthData=0 then break;
            Inc(DataIndexer,aGUILengthData);
          end;
        end;
        DPM_CONFIG_GET_REQ:
        begin
          while (DataIndexer<(MessageLengthData)) do
          begin
            s:=s+InttoHex(MessageData[DataIndexer])+' ';
            Inc(DataIndexer);
          end;
        end;
        DPM_MESSAGE_CNF,DPM_CONFIG_SET_CNF,DPM_REGISTER_READ_CNF,DPM_REGISTER_WRITE_CNF:
        begin
        end;
        else
        begin
          s:=s+'Tag:'+IntToStr(MessageID)+'. Port:'+IntToStr(MessagePortNumber)+'. Unknown Command:'+InttoStr(Ord(MessageType));
          Str(StackEvent,enumname);
          s:=s+'Type: '+enumname+'. '+'Length:'+IntToStr(MessageLengthData);
          for i:=9 to Length(MessageData) do
          begin
            s:=s+InttoHex(MessageData[i])+' ';
          end;
          for i:=9 to Length(MessageData) do
          begin
            if Chr(MessageData[i]) in [' '..'~'] then s:=s+Chr(MessageData[i]);
          end;
        end;
      end;

      {$ifdef DEBUG}
      if (aLog<>nil) then aLog:=USBDebugLog;
      {$endif}
      if ((aLog<>nil) AND (Length(s)>0)) then aLog.Append(s);

      if (MessagePortNumber=1) then
      begin

        if (ValidRemoteRDO) then
        begin
          ValidRemoteRDO:=False;
          if (DUT.RDO.STANDARD.ObjectPosition>0) then
          begin
            gridRemoteSourcePDO.Col:=DUT.RDO.STANDARD.ObjectPosition;
            j:=DUT.RDO.GENERIC.ObjectPosition;
            PDOSupplyType:=TSUPPLY_TYPES(DUT.SourcePDOs[j].GenericPdo.Supply);

            s:='unknown';
            PDOCurrent:=0;
            PDOVoltage:=0;

            case PDOSupplyType of
              TSUPPLY_TYPES.Fixed:
              begin
                with DUT.SourcePDOs[j].FixedSupplyPdo do
                begin
                  PDOCurrent:=(MaximumCurrentIn10mA*10);
                  PDOVoltage:=(VoltageIn50mV*50);
                end;
              end;
              TSUPPLY_TYPES.Variable:
              begin
                with DUT.SourcePDOs[j].VariableSupplyNonBatteryPdo do
                begin
                  PDOCurrent:=(MaximumCurrentIn10mA*10);
                  PDOVoltage:=(MaximumVoltageIn50mV*50);
                end;
              end;
              TSUPPLY_TYPES.APDO:
              begin
                with DUT.SourcePDOs[j].SPRPowerSupplyApdo do
                begin
                  PDOCurrent:=(MaximumCurrentIn50mA*50);
                  PDOVoltage:=(MaximumVoltageIn100mV*100);
                end;
              end;
            end;
            if ((PDOVoltage>0) AND (PDOCurrent>0)) then s:=InttoStr(PDOCurrent)+'mA. Max '+InttoStr(PDOVoltage)+'mV. Pos '+InttoStr(j)+'.';
            editPowerStatusUSBContract.Text:=s+'.';
          end
          else
          begin
            gridRemoteSourcePDO.Col:=1;
            editPowerStatusUSBContract.Text:='';
          end;
        end;

        if (ValidRemoteSourcePDO) then
        begin
          ValidRemoteSourcePDO:=False;
          SetGridSRCPDO(0);
        end;
        if (ValidRemoteSinkPDO) then
        begin
          ValidRemoteSinkPDO:=False;
          SetGridSNKPDO(0);
        end;

      end;
    end;

    if (MessagePortNumber=0) then
    begin
      case MessageType of
        DPM_INIT_CNF:
        begin
          while (DataIndexer<(MessageLengthData)) do
          begin

            aGUIInitMessage:=GUI_INIT_TAG(MessageData[DataIndexer]);
            Inc(DataIndexer);
            Str(aGUIInitMessage,enumname);
            s:=enumname+': ';
            WordData.NamedBytes.HSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            WordData.NamedBytes.LSB:=MessageData[DataIndexer];
            Inc(DataIndexer);
            aGUILengthData:=WordData.Raw;

            case aGUIInitMessage of
              GUI_INIT_HWBOARDVERSION: for j:=0 to Pred(aGUILengthData) do s:=s+Chr(MessageData[DataIndexer+j]);
              GUI_INIT_HWPDTYPE:
              begin
                FSTM32BoardSerial:='';
                for j:=0 to Pred(aGUILengthData) do FSTM32BoardSerial:=FSTM32BoardSerial+Chr(MessageData[DataIndexer+j]);
                s:=s+FSTM32BoardSerial;
              end;
              GUI_INIT_FWVERSION:
              begin
                j:=aGUILengthData;
                while (j>0) do
                begin
                  if j=aGUILengthData then s:=s+'FW:';
                  if j=4 then s:=s+'. SV:';
                  s:=s+InttoHex(MessageData[DataIndexer+j-1]);
                  Dec(j);
                end;
              end;
              {$ifdef CALIBRATION}
              GUI_INIT_CALIBRATION_DATE: for j:=0 to Pred(GUILengthData) do s:=s+Chr(MessageData[DataIndexer+j]);
              {$endif}
              GUI_INIT_ORIGINAL_SETTINGS:
              begin
                if (MessageData[DataIndexer]=1) then
                  s:=s+'Original firmware settings.'
                else
                  s:=s+'Settings from flash !!';
              end;
              GUI_INIT_NBPORTMAX:s:=s+'Ports: #'+InttoStr(MessageData[DataIndexer]);
              else
              begin
                s:='';
              end;
            end;
            if aGUILengthData=0 then break;
            Inc(DataIndexer,aGUILengthData);
            if (Length(s)>0) then USBDebugLog.Lines.Append(s);
          end;
        end;
      end;
    end;

    //MemoUnhandled.Lines.Append('Received: '+InttoStr(MessageLengthData)+'. Processed: '+InttoStr(DataIndexer-4)+'.');
    diff:=(MessageLengthData-DataIndexer);
    if (diff<>0) then MemoUnhandled.Lines.Append('Diff: : '+InttoStr(diff)+'.');

  finally
    Finalize(MessageData);
  end;
end;

procedure TPowerbankMainForm.GridButtonClick(Sender: TObject);
var
  PDOVoltage      : integer;
  PDOCurrent      : integer;
  PDONumber       : byte;
  index           : integer;
  aButton         : TButton;
  Buffer          : array[0..255] of byte;
  aPDO            : TSOURCEPDO;
  aPDOType        : TSUPPLY_TYPES;
begin
  PDONumber:=0;

  if (Sender<>nil) then
  begin
    for index:=1 to gridRemoteSourcePDO.ColCount do
    begin
      aButton:=TButton(gridRemoteSourcePDO.Objects[index,5]);
      if (aButton=nil) then break;
      if (aButton.Tag<>0)
        then aButton.Enabled:=false
      else
        break;
      aButton.Invalidate;
    end;
    sleep(100);
    Application.ProcessMessages;
    aButton:=TButton(Sender);
    PDONumber:=aButton.Tag;
  end;
  try

    if (PDONumber<>0) then
    begin
      aPDO:=DUT.SourcePDOs[PDONumber];
      aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

      PDOCurrent:=0;
      PDOVoltage:=0;

      if (aPDOType=TSUPPLY_TYPES.Fixed) then
      begin
        with aPDO.FixedSupplyPdo do
        begin
          PDOCurrent:=(MaximumCurrentIn10mA*10);
          PDOVoltage:=(VoltageIn50mV*50);
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Variable) then
      begin
        with aPDO.VariableSupplyNonBatteryPdo do
        begin
          PDOCurrent:=(MaximumCurrentIn10mA*10);
          PDOVoltage:=(MaximumVoltageIn50mV*50);
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.APDO) then
      begin
        with aPDO.SPRPowerSupplyApdo do
        begin
          PDOCurrent:=(MaximumCurrentIn50mA*50);
          PDOVoltage:=(MaximumVoltageIn100mV*100);
        end;
      end;

      if ((PDOVoltage=0) OR (PDOCurrent=0)) then exit;

      USBDebugLog.Lines.Append('Requesting PDO #'+InttoStr(PDONumber)+' at '+InttoStr(PDOVoltage)+'mV and '+InttoStr(PDOCurrent)+'mA.');

      //KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDREQSIMPLE].Command,[PDONumber]));
      KM003C.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDREQEXT].Command,[PDONumber,PDOVoltage,PDOCurrent]));

      FillChar({%H-}Buffer,SizeOf(Buffer),0);
      Buffer[0]:=Ord(GUI_MSG_REQUEST);
      Buffer[1]:=0;
      Buffer[2]:=0;
      Buffer[3]:=Ord(GUI_PARAM_MSG_RDOPOSITION);
      Buffer[4]:=0;
      Buffer[5]:=1;
      Buffer[6]:=PDONumber;
      Buffer[7]:=Ord(GUI_PARAM_MSG_REQUESTEDVOLTAGE);
      Buffer[8]:=0;
      Buffer[9]:=2;
      Buffer[10]:=((PDOVoltage) MOD 256);
      Buffer[11]:=((PDOVoltage) DIV 256);
      SendCommand(1,Ord(DPM_MESSAGE_REQ),Buffer,12);
    end;

  finally
    if (Sender<>nil) then
    begin
      for index:=1 to gridRemoteSourcePDO.ColCount do
      begin
        aButton:=TButton(gridRemoteSourcePDO.Objects[index,5]);
        if (aButton=nil) then break;
        if (aButton.Tag<>0)
          then aButton.Enabled:=true
        else
          break;
      end;
    end;
  end;
end;

procedure TPowerbankMainForm.SetGridSRCPDO(PDONumber:integer);
var
  aPDO:TSOURCEPDO;
  aPDOType:TSUPPLY_TYPES;
  aPDOGrid:TStringGrid;
  s:string;
  aPDOGridColumn:integer;
  PDOIndex:integer;
begin
  aPDOGrid:=gridRemoteSourcePDO;

  for PDOIndex:=1 to MAXPDO do
  begin
    if (PDONumber>0) AND (PDONumber<>PDOIndex) then continue;

    aPDO:=DUT.SourcePDOs[PDOIndex];
    aPDOGridColumn:=PDOIndex;

    TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Enabled:=true;
    TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Tag:=(aPDOGridColumn);

    if aPDO.Raw=0 then
    begin
      aPDOGrid.Cells[aPDOGridColumn,0]:='';
      aPDOGrid.Cells[aPDOGridColumn,1]:='';
      aPDOGrid.Cells[aPDOGridColumn,2]:='';
      aPDOGrid.Cells[aPDOGridColumn,3]:='';
      aPDOGrid.Cells[aPDOGridColumn,4]:='';
      TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Enabled:=false;
      TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Tag:=0;
    end
    else
    begin
      if (PDOIndex=1) then
      begin
        chkgrpPDOFLags.Checked[0]:=(aPDO.FixedSupplyPdo.DataRoleSwap=1);
        chkgrpPDOFLags.Checked[1]:=(aPDO.FixedSupplyPdo.UsbCommunicationCapable=1);
        chkgrpPDOFLags.Checked[2]:=(aPDO.FixedSupplyPdo.ExternallyPowered=1);
        chkgrpPDOFLags.Checked[3]:=(aPDO.FixedSupplyPdo.UsbSuspendSupported=1);
        chkgrpPDOFLags.Checked[4]:=(aPDO.FixedSupplyPdo.DualRolePower=1);
      end;

      aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

      aPDOGrid.Cells[aPDOGridColumn,0]:='PDO '+InttoStr(aPDOGridColumn);
      aPDOGrid.Cells[aPDOGridColumn,1]:=SUPPLY_TYPES[aPDOType];

      if (aPDOType=TSUPPLY_TYPES.Fixed) then
      begin
        with aPDO.FixedSupplyPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumCurrentIn10mA*10)+ 'mA';
          aPDOGrid.Cells[aPDOGridColumn,3]:=InttoStr(VoltageIn50mV*50 DIV 1000)+'Volt';
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Variable) then
      begin
        with aPDO.VariableSupplyNonBatteryPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumCurrentIn10mA*10)+ 'mA';
          aPDOGrid.Cells[aPDOGridColumn,3]:=InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+'Volt';
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.APDO) then
      begin
        if (TAPDO_TYPES(aPDO.GenericAPdo.APO)=TAPDO_TYPES.SPR) then
        begin
          with aPDO.SPRPowerSupplyApdo do
          begin
            aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumCurrentIn50mA*50)+ 'mA';
            s:=InttoStr(MinimumVoltageIn100mV*100 DIV 1000)+
               '-'+
               InttoStr(MaximumVoltageIn100mV*100 DIV 1000)+
               'Volt';
            aPDOGrid.Cells[aPDOGridColumn,3]:=s;
          end;
        end;

        if (TAPDO_TYPES(aPDO.GenericAPdo.APO)=TAPDO_TYPES.EPR) then
        begin
          with aPDO.EPRPowerSupplyApdo do
          begin
            s:=InttoStr(MinimumVoltageIn100mV*100 DIV 1000)+
               '-'+
               InttoStr(MaximumVoltageIn100mV*100 DIV 1000)+
               'Volt';
            aPDOGrid.Cells[aPDOGridColumn,3]:=s;
            aPDOGrid.Cells[aPDOGridColumn,4]:=InttoStr(PDPInW)+'Watt';
          end;
        end;
      end
      else
      begin
        aPDOGrid.Cells[aPDOGridColumn,2]:='No data';
        aPDOGrid.Cells[aPDOGridColumn,3]:='No data';
        aPDOGrid.Cells[aPDOGridColumn,4]:='No data';
        TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Enabled:=false;
        TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Tag:=0;
      end;
    end;
  end;
end;

procedure TPowerbankMainForm.SetGridSRCEPRPDO(PDONumber:integer);
var
  aPDO:TSOURCEPDO;
  aPDOType:TSUPPLY_TYPES;
  aPDOGrid:TStringGrid;
  s:string;
  aPDOGridColumn:integer;
  PDOIndex:integer;
begin
  aPDOGrid:=gridRemoteSourceEPRPDO;

  for PDOIndex:=8 to 11 do
  begin
    if (PDONumber>0) AND (PDONumber<>PDOIndex) then continue;

    aPDO:=DUT.SourceEPRPDOs[PDOIndex];
    aPDOGridColumn:=PDOIndex-7;

    TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Enabled:=true;
    TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Tag:=(aPDOGridColumn);

    if aPDO.Raw=0 then
    begin
      aPDOGrid.Cells[aPDOGridColumn,0]:='';
      aPDOGrid.Cells[aPDOGridColumn,1]:='';
      aPDOGrid.Cells[aPDOGridColumn,2]:='';
      aPDOGrid.Cells[aPDOGridColumn,3]:='';
      aPDOGrid.Cells[aPDOGridColumn,4]:='';
      TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Enabled:=false;
      TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Tag:=0;
    end
    else
    begin
      if (PDOIndex=1) then
      begin
        chkgrpPDOFLags.Checked[0]:=(aPDO.FixedSupplyPdo.DataRoleSwap=1);
        chkgrpPDOFLags.Checked[1]:=(aPDO.FixedSupplyPdo.UsbCommunicationCapable=1);
        chkgrpPDOFLags.Checked[2]:=(aPDO.FixedSupplyPdo.ExternallyPowered=1);
        chkgrpPDOFLags.Checked[3]:=(aPDO.FixedSupplyPdo.UsbSuspendSupported=1);
        chkgrpPDOFLags.Checked[4]:=(aPDO.FixedSupplyPdo.DualRolePower=1);
      end;

      aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

      aPDOGrid.Cells[aPDOGridColumn,0]:='PDO '+InttoStr(aPDOGridColumn);
      aPDOGrid.Cells[aPDOGridColumn,1]:=SUPPLY_TYPES[aPDOType];

      if (aPDOType=TSUPPLY_TYPES.Fixed) then
      begin
        with aPDO.FixedSupplyPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumCurrentIn10mA*10)+ 'mA';
          aPDOGrid.Cells[aPDOGridColumn,3]:=InttoStr(VoltageIn50mV*50 DIV 1000)+'Volt';
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Variable) then
      begin
        with aPDO.VariableSupplyNonBatteryPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumCurrentIn10mA*10)+ 'mA';
          aPDOGrid.Cells[aPDOGridColumn,3]:=InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+'Volt';
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.APDO) then
      begin
        if (TAPDO_TYPES(aPDO.GenericAPdo.APO)=TAPDO_TYPES.SPR) then
        begin
          with aPDO.SPRPowerSupplyApdo do
          begin
            aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumCurrentIn50mA*50)+ 'mA';
            s:=InttoStr(MinimumVoltageIn100mV*100 DIV 1000)+
               '-'+
               InttoStr(MaximumVoltageIn100mV*100 DIV 1000)+
               'Volt';
            aPDOGrid.Cells[aPDOGridColumn,3]:=s;
          end;
        end;

        if (TAPDO_TYPES(aPDO.GenericAPdo.APO)=TAPDO_TYPES.EPR) then
        begin
          with aPDO.EPRPowerSupplyApdo do
          begin
            s:=InttoStr(MinimumVoltageIn100mV*100 DIV 1000)+
               '-'+
               InttoStr(MaximumVoltageIn100mV*100 DIV 1000)+
               'Volt';
            aPDOGrid.Cells[aPDOGridColumn,3]:=s;
            aPDOGrid.Cells[aPDOGridColumn,4]:=InttoStr(PDPInW)+'Watt';
          end;
        end;
      end
      else
      begin
        aPDOGrid.Cells[aPDOGridColumn,2]:='No data';
        aPDOGrid.Cells[aPDOGridColumn,3]:='No data';
        aPDOGrid.Cells[aPDOGridColumn,4]:='No data';
        TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Enabled:=false;
        TButton(aPDOGrid.Objects[aPDOGridColumn,5]).Tag:=0;
      end;
    end;
  end;
end;


procedure TPowerbankMainForm.SetGridSNKPDO(PDONumber:integer);
var
  aPDO:TSINKPDO;
  aPDOType:TSUPPLY_TYPES;
  aPDOGrid:TStringGrid;
  aPDOGridColumn:integer;
  PDOIndex:integer;
  s:string;
begin
  aPDOGrid:=gridRemoteSinkPDO;

  for PDOIndex:=1 to MAXPDO do
  begin
    if (PDONumber>0) AND (PDOIndex<>PDONumber) then continue;

    aPDO:=DUT.SinkPDOs[PDOIndex];
    aPDOGridColumn:=PDOIndex;

    if aPDO.Raw=0 then
    begin
      aPDOGrid.Cells[aPDOGridColumn,0]:='';
      aPDOGrid.Cells[aPDOGridColumn,1]:='';
      aPDOGrid.Cells[aPDOGridColumn,2]:='';
      aPDOGrid.Cells[aPDOGridColumn,3]:='';
    end
    else
    begin
      aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

      aPDOGrid.Cells[aPDOGridColumn,0]:='PDO '+InttoStr(aPDOGridColumn);
      aPDOGrid.Cells[aPDOGridColumn,1]:=SUPPLY_TYPES[aPDOType];


      if (aPDOType=TSUPPLY_TYPES.Fixed) then
      begin
        with aPDO.FixedSupplyPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(OperationalCurrentIn10mA*10)+ 'mA';
          aPDOGrid.Cells[aPDOGridColumn,3]:=InttoStr(VoltageIn50mV*50 DIV 1000)+'Volt';
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Battery) then
      begin
        with aPDO.BatterySupplyPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumAllowablePowerIn250mW*250)+ 'mW';
          s:=InttoStr(MinimumVoltageIn50mV*50 DIV 1000)+
             '-'+
             InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+
             'Volt';
          aPDOGrid.Cells[aPDOGridColumn,3]:=s;
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Variable) then
      begin
        with aPDO.VariableSupplyNonBatteryPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(OperationalCurrentIn10mA*10)+ 'mA';
          s:=InttoStr(MinimumVoltageIn50mV*50 DIV 1000)+
             '-'+
             InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+
             'Volt';
          aPDOGrid.Cells[aPDOGridColumn,3]:=s;
        end;
      end
      else
      begin
        aPDOGrid.Cells[aPDOGridColumn,2]:='No data';
        aPDOGrid.Cells[aPDOGridColumn,3]:='No data';
      end;
    end;
  end;
end;

procedure TPowerbankMainForm.SetGridSNKEPRPDO(PDONumber:integer);
var
  aPDO:TSINKPDO;
  aPDOType:TSUPPLY_TYPES;
  aPDOGrid:TStringGrid;
  aPDOGridColumn:integer;
  PDOIndex:integer;
  s:string;
begin
  aPDOGrid:=gridRemoteSinkEPRPDO;

  for PDOIndex:=8 to 11 do
  begin
    if (PDONumber>0) AND (PDOIndex<>PDONumber) then continue;

    aPDO:=DUT.SinkEPRPDOs[PDOIndex];
    aPDOGridColumn:=PDOIndex-7;

    if aPDO.Raw=0 then
    begin
      aPDOGrid.Cells[aPDOGridColumn,0]:='';
      aPDOGrid.Cells[aPDOGridColumn,1]:='';
      aPDOGrid.Cells[aPDOGridColumn,2]:='';
      aPDOGrid.Cells[aPDOGridColumn,3]:='';
    end
    else
    begin
      aPDOType:=TSUPPLY_TYPES(aPDO.GenericPdo.Supply);

      aPDOGrid.Cells[aPDOGridColumn,0]:='PDO '+InttoStr(aPDOGridColumn);
      aPDOGrid.Cells[aPDOGridColumn,1]:=SUPPLY_TYPES[aPDOType];


      if (aPDOType=TSUPPLY_TYPES.Fixed) then
      begin
        with aPDO.FixedSupplyPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(OperationalCurrentIn10mA*10)+ 'mA';
          aPDOGrid.Cells[aPDOGridColumn,3]:=InttoStr(VoltageIn50mV*50 DIV 1000)+'Volt';
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Battery) then
      begin
        with aPDO.BatterySupplyPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(MaximumAllowablePowerIn250mW*250)+ 'mW';
          s:=InttoStr(MinimumVoltageIn50mV*50 DIV 1000)+
             '-'+
             InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+
             'Volt';
          aPDOGrid.Cells[aPDOGridColumn,3]:=s;
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Variable) then
      begin
        with aPDO.VariableSupplyNonBatteryPdo do
        begin
          aPDOGrid.Cells[aPDOGridColumn,2]:=InttoStr(OperationalCurrentIn10mA*10)+ 'mA';
          s:=InttoStr(MinimumVoltageIn50mV*50 DIV 1000)+
             '-'+
             InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+
             'Volt';
          aPDOGrid.Cells[aPDOGridColumn,3]:=s;
        end;
      end
      else
      begin
        aPDOGrid.Cells[aPDOGridColumn,2]:='No data';
        aPDOGrid.Cells[aPDOGridColumn,3]:='No data';
      end;
    end;
  end;
end;


procedure TPowerbankMainForm.SetBatteryData;
var
  aBattCaps:TBATTCAPS;
  aBattStatus:TBATTSTATS;
begin
  aBattCaps:=DUT.BatteryCaps;
  aBattStatus:=DUT.BatteryStatus;
end;

procedure TPowerbankMainForm.DataTimerTimer(Sender: TObject);
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
  FillChar({%H-}newdata,4096,0);
  sensordata:=Default(TKM003CSensorData);
  result_code := InEndPoint.Recv(newdata ,4096 , 10);
  if (result_code>sizeof(TKM003CSensorData)) then result_code:=sizeof(TKM003CSensorData);
  move(newdata,sensordata,result_code);

  // Did we receive a packet from our command ?
  if ((sensordata.header.Data.typ=TKM003C_CMD_PUT_DATA) AND (sensordata.header_ext.Header.att=TKM003C_ATT_ADC)) then
  begin
    {$push}
    {$R-}
    normal:=(sensordata.V_bus);
    {$pop}
    signed:=@normal;
    avalue:=signed^/1000000;
    RealVoltageDisplay.Value:=avalue;

    {$push}
    {$R-}
    normal:=(sensordata.I_bus);
    {$pop}
    signed:=@normal;
    avalue:=signed^/1000000;
    RealCurrentDisplay.Value:=avalue;

    // INA228/9 datasheet LSB = 7.8125 m°C = 1000/128
    Temperature:=((sensordata.temp[1]*2000 + sensordata.temp[0]*1000/128)/1000);

    avalue:=(sensordata.V_dp)/10000;
    VdpVoltageDisplay.Value:=(avalue);
    avalue:=(sensordata.V_cc1)/10000;
    Vcc1VoltageDisplay.Value:=(avalue);
    avalue:=(sensordata.V_dm)/10000;
    VdmVoltageDisplay.Value:=(avalue);
    avalue:=(sensordata.V_cc2)/10000;
    Vcc2VoltageDisplay.Value:=(avalue);
  end;
end;

function TPowerbankMainForm.ProcessControlMessageGUI(MSGCTRL:TUSBPD_CONTROLMSG):boolean;
var
  enumname:string;
begin
  result:=true;
  Str(MSGCTRL,enumname);
  USBDebugLog.Lines.Append('PD control: '+enumname+'.');
end;

function TPowerbankMainForm.ProcessDataMessageGUI(aMSG:TUSBPD_DATAMSG):boolean;
var
  s:string;
begin
  result:=true;
  case aMSG of
    USBPD_DATAMSG_SRC_CAPABILITIES:
      begin
        SetGridSRCPDO(0);
      end;
    USBPD_DATAMSG_SNK_CAPABILITIES:
      begin
        SetGridSNKPDO(0);
      end;
    USBPD_DATAMSG_REQUEST:
    begin
      s:='Official Received RDO.'+#13#10;
      s:=s+DUT.GetRDOInfo;
      USBDebugLog.Lines.Append(s);
    end;
    USBPD_DATAMSG_BATTERY_STATUS:
    begin
      vleBatteryData.Values['SOC']:=InttoStr(DUT.BatteryStatus.Data.BatterySOC100mWh);
    end;
    USBPD_DATAMSG_VENDOR_DEFINED:
    begin
      if DUT.VDM_Header.GenericVDM.VDMType=1 then
      begin
        if (DUT.VDM_Header.StructuredVDM.Command=Ord(TVDMCommands.DiscoverIdentity)) then s:=s+'Discover Identity. ';
        if (DUT.VDM_Header.StructuredVDM.Command=Ord(TVDMCommands.DiscoverSVIDs)) then s:=s+'Discover SVIDs. ';
        if (DUT.VDM_Header.StructuredVDM.Command=Ord(TVDMCommands.DiscoverModes)) then s:=s+'Discover Modes. ';

        if (DUT.VDO_ID.SOPProductTypeUFP=3) then
        begin
          s:=s+'Passive cable.'+#13#10;
        end;
        if (DUT.VDO_ID.SOPProductTypeUFP=4) then
        begin
          s:=s+'Active cable.'+#13#10;
        end;
        s:=s+DUT.GetCableInfo;
        USBDebugLog.Lines.Append(s);
      end;
    end;
    USBPD_DATAMSG_EPR_MODE:
    begin
      s:=s+'EPR mode message.'+#13#10;
      s:=s+DUT.EPRModeInfo;
      USBDebugLog.Lines.Append(s);
    end
    else
    begin
      result:=false;
    end;
  end;
end;

function TPowerbankMainForm.ProcessExtendedMessageGUI(aMSG:TUSBPD_EXTENDEDMSG):boolean;
var
  j:integer;
begin
  result:=true;
  case aMSG of
    USBPD_EXTMSG_SOURCE_CAPABILITIES_EXTENDED:
    begin
      with DUT.SRCExtended.Data do
      begin
        j:=VID;
        vleSourceExtended.Values['VID']:=InttoStr(j)+' ('+DUT.GetVID(j)+')';
        vleSourceExtended.Values['PID']:=InttoStr(PID);
        vleSourceExtended.Values['XID']:=InttoStr(XID);
        vleSourceExtended.Values['FW_Version']:=InttoStr(FW_Version);
        vleSourceExtended.Values['HW_Version']:=InttoStr(HW_Version);
        vleSourceExtended.Values['Voltage_Regulation']:=InttoStr(Voltage_Regulation);
        vleSourceExtended.Values['Holdup_Time']:=InttoStr(Holdup_Time);
        vleSourceExtended.Values['Compliance']:=InttoStr(Compliance);
        vleSourceExtended.Values['Touch_Current']:=InttoStr(Touch_Current);
        vleSourceExtended.Values['Peak_Current1']:=InttoStr(Peak_Current1);
        vleSourceExtended.Values['Peak_Current2']:=InttoStr(Peak_Current2);
        vleSourceExtended.Values['Peak_Current3']:=InttoStr(Peak_Current3);
        vleSourceExtended.Values['Touch_Temp']:=InttoStr(Touch_Temp);
        vleSourceExtended.Values['Source_Inputs']:=InttoStr(Source_Inputs);
        vleSourceExtended.Values['Batteries']:=InttoStr(Batteries);
        vleSourceExtended.Values['Source_PDP']:=InttoStr(Source_PDP)+'W';
      end;
    end;
    USBPD_EXTMSG_SINK_CAPABILITIES_EXTENDED:
    begin
      with DUT.SNKExtended.Data do
      begin
        j:=VID;
        vleSinkExtended.Values['VID']:=InttoStr(j)+' ('+DUT.GetVID(j)+')';
        vleSinkExtended.Values['PID']:=InttoStr(PID);
        vleSinkExtended.Values['XID']:=InttoStr(XID);
        vleSinkExtended.Values['FW_Version']:=InttoStr(FW_Version);
        vleSinkExtended.Values['HW_Version']:=InttoStr(HW_Version);
        vleSinkExtended.Values['SKEDB_Version']:=InttoStr(SKEDB_Version);
        vleSinkExtended.Values['Load_Step']:=InttoStr(Load_Step);
        vleSinkExtended.Values['Sink_Load_Characteristics']:=InttoStr(Sink_Load_Characteristics);
        vleSinkExtended.Values['Compliance']:=InttoStr(Compliance);
        vleSinkExtended.Values['Touch_Temp']:=InttoStr(Touch_Temp);
        vleSinkExtended.Values['Battery_Info']:=InttoStr(Battery_Info);
        vleSinkExtended.Values['Sink_Modes']:=InttoStr(Sink_Modes);
        vleSinkExtended.Values['Sink_Minimum_PDP']:=InttoStr(Sink_Minimum_PDP);
        vleSinkExtended.Values['Sink_Operational_PDP']:=InttoStr(Sink_Operational_PDP);
        vleSinkExtended.Values['Sink_Maximum_PDP']:=InttoStr(Sink_Maximum_PDP);
        vleSinkExtended.Values['EPR_Sink_Minimum_PDP']:=InttoStr(EPR_Sink_Minimum_PDP);
        vleSinkExtended.Values['EPR_Sink_Operational_PDP']:=InttoStr(EPR_Sink_Operational_PDP);
        vleSinkExtended.Values['EPR_Sink_Maximum_PDP']:=InttoStr(EPR_Sink_Maximum_PDP);
      end;
    end;
    USBPD_EXTMSG_PPS_STATUS:
    begin
      vlePPS.Values['Output Voltage']:=InttoStr(DUT.PPSSDB.Data.OutputVoltage20mV*20)+'mV';
      vlePPS.Values['Output Current']:=InttoStr(DUT.PPSSDB.Data.OutputCurrent50mA*50)+'mA';
    end;
    USBPD_EXTMSG_BATTERY_CAPABILITIES:
    begin
      j:=DUT.BatteryCaps.Data.VID;
      vleBatteryData.Values['VID']:=InttoStr(j)+' ('+DUT.GetVID(j)+')';
      vleBatteryData.Values['Type']:=InttoStr(DUT.BatteryCaps.Data.BatteryType);
      vleBatteryData.Values['Capacity']:=FloattoStrF((DUT.BatteryCaps.Data.BatteryDesignCapacity/10),ffFixed, 5, 1)+'Wh';
    end;
    USBPD_EXTMSG_STATUS:
    begin
      if DUT.SDB.Data.InternalTemp>1 then
        vleStatus.Values['InternalTemp']:=InttoStr(DUT.SDB.Data.InternalTemp)+'°C'
      else
        vleStatus.Values['InternalTemp']:='-';
      vleStatus.Values['PresentInput']:=DUT.GetStatusPresentInputInfo;
      vleStatus.Values['PresentBatteryInput']:=InttoStr(DUT.SDB.Data.PresentBatteryInput);
      vleStatus.Values['EventFlags']:=InttoStr(DUT.SDB.Data.EventFlags);
      vleStatus.Values['TemperatureStatus']:=DUT.GetStatusTemperatureStatusInfo;
      vleStatus.Values['PowerStatus']:=InttoStr(DUT.SDB.Data.PowerStatus);
      vleStatus.Values['PowerStateChange']:=DUT.GetStatusPowerStateChangeInfo;
    end;
    USBPD_EXTMSG_EPR_SINK_CAPABILITIES:
    begin
      SetGridSNKPDO(0);
      SetGridSNKEPRPDO(0);
    end;
    USBPD_EXTMSG_EPR_SOURCE_CAPABILITIES:
    begin
      SetGridSRCPDO(0);
      SetGridSRCEPRPDO(0);
    end;
    else
    begin
      result:=false;
    end;
  end;
end;

procedure TPowerbankMainForm.CheckTimerTimer(Sender: TObject);
var
  header                : TKM003CMsgHeader;
  header_ext            : TKM003CMsgHeader;
  PacketHeader          : TKM003CPacketHeader;

  result_code           : LongInt;
  newdata               : packed array[0..4095] of byte;
  j                     : integer;
  dataindexer           : integer;
  datasize              : integer;
  SOPPacket             : boolean;

  aSOPDHEADER           : TPDHEADER;
  {%H-}aSOPExtendedHeader    : TPDHEADEREXTENDED;
  PDControlMessage      : TUSBPD_CONTROLMSG;
  PDDataMessage         : TUSBPD_DATAMSG;
  PDExtendedMessage     : TUSBPD_EXTENDEDMSG;

  enumname              : string;
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

        if PacketHeader.Data.Size.Raw=0 then
        begin
          USBDebugLog.Lines.Append('Very empty data !!');
          break;
        end;

        if (datasize=0) then
        begin
           USBDebugLog.Lines.Append('Empty data. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(Int64(PacketHeader.Data.Time.Raw)))));
          break;
        end;

        if (NOT SOPPacket) then
        begin
          //USBDebugLog.Lines.Append('Non SOP data ['+InttoStr(datasize)+']. Data size: '+InttoStr(PacketHeader.Data.Size.Raw)+'. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(PacketHeader.Data.Time.Raw))));
        end;

        if SOPPacket then
        begin
          PacketHeader.Data.Size.Raw:=PacketHeader.Data.Size.Raw-5;
          // PacketHeader.Data.Size.Raw is now SOP packet in bytes

          //USBDebugLog.Lines.Append('Data size: '+InttoStr(PacketHeader.Data.Size.Raw)+'. SOP: '+InttoStr(PacketHeader.Data.SOP)+'. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(PacketHeader.Data.Time.Raw))));

          // Now a 2 byte SOP header
          for j:=0 to 1 do aSOPDHEADER.Bytes[j]:=newdata[dataindexer+j];
          Inc(dataindexer,2);
          Dec(datasize,2);
          //USBDebugLog.Lines.Append('SOP: '+GetSOPInfo(aSOPDHEADER));

          PDControlMessage:=USBPD_CONTROLMSG_RESERVED0;
          PDDataMessage:=USBPD_DATAMSG_RESERVED0;
          PDExtendedMessage:=USBPD_EXTMSG_RESERVED0;
          if ((aSOPDHEADER.Data.Number_of_Data_Objects=0) AND (aSOPDHEADER.Data.Extended=0)) then
          begin
            PDControlMessage:=TUSBPD_CONTROLMSG(aSOPDHEADER.Data.Message_Type);
          end
          else
          begin
            if (aSOPDHEADER.Data.Extended=0) then PDDataMessage:=TUSBPD_DATAMSG(aSOPDHEADER.Data.Message_Type);
            if (aSOPDHEADER.Data.Extended=1) then
            begin
              PDExtendedMessage:=TUSBPD_EXTENDEDMSG(aSOPDHEADER.Data.Message_Type);
              // The outgoing packet has a strange extra empty word
              // Handle it !
              if (aSOPDHEADER.Data.Port_Power_Role_or_Plug=0) then
              begin
                Inc(dataindexer,2);
                Dec(datasize,2);
              end;
              for j:=0 to 1 do aSOPExtendedHeader.Bytes[j]:=newdata[dataindexer+j];
              Inc(dataindexer,2);
              Dec(datasize,2);
            end;
          end;

          if PDControlMessage<>USBPD_CONTROLMSG_RESERVED0 then
          begin
            ProcessControlMessageGUI(PDControlMessage);
          end;

          if PDDataMessage<>USBPD_DATAMSG_RESERVED0 then
          begin
            if DUT.ProcessDataMessage(PDDataMessage,aSOPDHEADER.Data.Number_of_Data_Objects,@newdata[dataindexer]) then
            begin
              ProcessDataMessageGUI(PDDataMessage);
            end
            else
            begin
              Str(PDDataMessage,enumname);
              MemoUnhandled.Lines.Append('UNHANDLED DATA MESSAGE: '+enumname);
            end;
          end;

          if PDExtendedMessage<>USBPD_EXTMSG_RESERVED0 then
          begin
            if DUT.ProcessExtendedMessage(PDExtendedMessage,@newdata[dataindexer]) then
            begin
              ProcessExtendedMessageGUI(PDExtendedMessage);
            end
            else
            begin
              Str(PDExtendedMessage,enumname);
              MemoUnhandled.Lines.Append('UNHANDLED EXTENDED MESSAGE: '+enumname);
            end;
            // The outgoing packet has a strange extra empty word
            // Handle it !
            if (aSOPDHEADER.Data.Port_Power_Role_or_Plug=0) then
            begin
              Dec(dataindexer,4);
              Inc(datasize,4);
            end
            else
            begin
              Dec(dataindexer,2);
              Inc(datasize,2);
            end;
          end;
          // Skip to  next data packet
          Inc(dataindexer,aSOPDHEADER.Data.Number_of_Data_Objects*4);
          Dec(datasize,aSOPDHEADER.Data.Number_of_Data_Objects*4);
        end;
      end;
    end;

  end;
end;

procedure TPowerbankMainForm.DisConnect(Sender: TObject);
begin
  PDTimer.Enabled:=False;
  DataTimer.Enabled:=False;

  KM003C.Active:=False;
  STM32.Active:=False;

  if Assigned(OutEndPoint) then OutEndPoint.Destroy;
  OutEndPoint:=nil;

  if Assigned(InEndPoint) then InEndPoint.Destroy;
  InEndPoint:=nil;

  if Assigned(DeviceInterface) then DeviceInterface.Destroy;
  DeviceInterface:=nil;

  if Assigned(Device) then Device.Destroy;
  Device:=nil;

  if Assigned(Context) then Context.Destroy;
  Context:=nil;
end;

procedure TPowerbankMainForm.Connect(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  DisConnect(Sender);

  if (Sender=btnConnectSTM32) then
  begin
    if (Length(STMComport)>0) then
    begin
      STM32.Active:=False;
      STM32.Device:=STMComport;
      STM32.BaudRate:=br921600;
      STM32.FlowControl:=fcNone;
      STM32.Parity:=pNone;
      STM32.DataBits:=db8bits;
      STM32.StopBits:=sbOne;
      STM32.OnRxData:=@OnRXUSBCData;
      STM32.Active:=True;

      FillChar({%H-}Buffer,SizeOf(Buffer),0);
      Buffer[0]:=Ord(GUI_INIT_HWBOARDVERSION);
      Buffer[1]:=0;
      Buffer[2]:=0;
      SendCommand(0,Ord(DPM_INIT_REQ),Buffer,3);
      SendCommand(1,Ord(DPM_INIT_REQ),Buffer,3);
      //SendCommand(2,Ord(DPM_INIT_REQ),Buffer,3);

      {$ifndef DEBUG}
      FillChar({%H-}Buffer,SizeOf(Buffer),0);
      Buffer[0]:=Ord(GUI_PARAM_MEASUREREPORTING);
      Buffer[1]:=0;
      Buffer[2]:=1;
      Buffer[3]:=((1000 DIV 40) AND $7f) + (1 shl 7);
      SendCommand(1,Ord(DPM_CONFIG_SET_REQ),Buffer,4);
      //SendCommand(2,Ord(DPM_CONFIG_SET_REQ),Buffer,4);
      {$endif}
    end;
  end;

  if (Sender=btnConnectKC003C) then
  begin
    if (Length(KM003CComport)>0) then
    begin
      KM003C.Active:=False;
      KM003C.Device:=KM003CComport;
      KM003C.BaudRate:=br921600;
      KM003C.FlowControl:=fcNone;
      KM003C.Parity:=pNone;
      KM003C.DataBits:=db8bits;
      KM003C.StopBits:=sbOne;
      KM003C.Active:=True;
    end;

    if (NOT Assigned(Context)) then Context := TLibUsbContext.Create;
    try
      if (NOT Assigned(Device)) then
      begin
        Device := TLibUsbDevice.Create(Context,DevVID,DevPID);
        if Assigned(Device) then
        begin
          Device.SetConfiguration(ConfigUSBConfiguration);
          DeviceInterface := TLibUsbInterface.Create(Device,Device.FindInterface(ConfigUSBInterface,ConfigUSBAltInterface));

          //Device.SetConfiguration(1);
          //DeviceInterface := TLibUsbInterface.Create(Device,Device.FindInterface(0,0));

          OutEndPoint := TLibUsbBulkOutEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_OUT));
          InEndPoint := TLibUsbBulkInEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_IN));
        end;
      end;
    except
      USBDebugLog.Lines.Append('Could not reach device');
      Device := nil;
    end;
  end;
end;

procedure TPowerbankMainForm.SetChartAxis(Sender:TObject);
begin
  Chart1LineSeries1.Clear;
  Chart1LineSeries2.Clear;
  Chart1LineSeries3.Clear;
  {$ifdef FPC}
  //Chart1LineSeries3.Active:=(Sender=btnCurve);
  {$else}
  //Chart1LineSeries3.Visible:=(Sender=btnCurve);
  {$endif}
  {$ifndef FPC}
  Chart1.UndoZoom;
  {$endif}
  {$ifdef FPC}
  Chart1.AxisList.GetAxisByAlign(calRight).Range.Min:=0;
  Chart1.AxisList.GetAxisByAlign(calRight).Range.UseMin:=True;
  Chart1.AxisList.GetAxisByAlign(calRight).Range.Max:=ActiveTestType.Current*1.2/1000;
  Chart1.AxisList.GetAxisByAlign(calRight).Range.UseMax:=True;
  Chart1.AxisList.GetAxisByAlign(calRight).Intervals.Count:=10;
  Chart1.AxisList.GetAxisByAlign(calLeft).Range.Min:=0;
  Chart1.AxisList.GetAxisByAlign(calLeft).Range.UseMin:=True;
  Chart1.AxisList.GetAxisByAlign(calLeft).Range.Max:=ActiveTestType.Voltage*1.2/1000;
  Chart1.AxisList.GetAxisByAlign(calLeft).Range.UseMax:=True;
  Chart1.AxisList.GetAxisByAlign(calLeft).Intervals.Count:=10;
  {$else}
  Chart1.RightAxis.SetMinMax(0,ActiveTestType.Current*1.2/1000);
  Chart1.RightAxis.Increment:=0.1;
  {$endif}
end;

procedure TPowerbankMainForm.CreateDataFile(Sender: TObject);
var
  BatteryDataFileBackup      : string;
  Oldfile,NewFile            : TFileStream;
  TimeString                 : string;
begin
  TimeString := Format('%.2d-%.2d-%.4d',[DayOfTheMonth(NowUTC), MonthOfTheYear(NowUTC), YearOf(NowUTC)])+'__';
  TimeString := TimeString + Format('%.2d-%.2d-%.2d',[HourOfTheDay(NowUTC), MinuteOfTheHour(NowUTC), SecondOfTheMinute(NowUTC)])+'__';

  BatteryDataFile:=Copy(TypesBox.Text,1,pos(' ',TypesBox.Text)-1)+'_'+SamplesBox.Text;
  if Assigned(Sender) then
  begin
    //if Sender=btnCurve then BatteryDataFile:=BatteryDataFile+'_Curve';
    if Sender=StartStopButton then BatteryDataFile:=BatteryDataFile+'_Normal';
  end
  else
  begin
    BatteryDataFile:=BatteryDataFile+'_Special';
  end;
  BatteryDataFile:=BatteryDataFile+'_'+ActiveTestType.Name+'__'+TimeString;
  BatteryDataFile:=BatteryDataFile+'data.csv';

  BatteryDataFile:=StringReplace(BatteryDataFile,' ','-',[rfReplaceAll]);
  BatteryDataFile:=StringReplace(BatteryDataFile,'@','-',[rfReplaceAll]);
  BatteryDataFile:=StringReplace(BatteryDataFile,'/','-',[rfReplaceAll]);

  BatteryDataFile:=ExtractFilePath(Application.ExeName)+BatteryDataFile;

  if FileExists(BatteryDataFile) then
  begin
    TestInfoMemo.Lines.Append('Making backup of batterydatafile.');

    //BatteryDataFileBackup := ExtractFileName(BatteryDataFile);
    //BatteryDataFileBackup := ExtractFilePath(Application.ExeName)+ChangeFileExt(BatteryDataFileBackup, '.bak');

    BatteryDataFileBackup := BatteryDataFile+'.bak';

    OldFile := TFileStream.Create(BatteryDataFile, fmOpenRead or fmShareDenyWrite);
    try
      NewFile := TFileStream.Create(BatteryDataFileBackup, fmCreate or fmShareDenyRead);
      try
        NewFile.CopyFrom(OldFile, OldFile.Size);
      finally
        FreeAndNil(NewFile);
      end;
    finally
      FreeAndNil(OldFile);
    end;
    DeleteFile(BatteryDataFile);
    TestInfoMemo.Lines.Append('Making backup of batterydatafile done.');
  end;
end;

procedure TPowerbankMainForm.AllStop(Sender: TObject);
begin
  StoreTimer.Enabled:=False;
  TestTimer.Enabled:=False;

  StartTime:=0;

  SystemActive:=False;
  if HPsource.Connected then
  begin
    HPsource.SetOutput(SystemActive);
  end;
  SetEnable(Sender,true);

  StartStopButton.Down:=false;
  StartStopButton.Caption:='Start';
  StartStopButton.Font.Color:=clLime;

  btnTestDischarge.Down:=false;

  ProgressBar1.Position:=0;
end;

procedure TPowerbankMainForm.Measure;
begin
  Voltage:=RealVoltageDisplay.Value;
  Current:=RealCurrentDisplay.Value;
  if (false) then
  begin
    //if HPsource.Connected then
    if true then
    begin
      HPsource.Measure;
      Current:=Abs(HPsource.Current);
      Voltage:=HPsource.Voltage;
    end
    else
    begin
      Sleep(100);
      Current:=1.456;
      Voltage:=5.123;
    end;
  end;
end;

procedure TPowerbankMainForm.SaveBatteryData(Elapsed:longword);
var
  F : textfile;
  PC,PB,PS:string;
  i:integer;
begin
  i:=pos(' ',TypesBox.Text);
  PC:=Copy(TypesBox.Text,1,i-1);
  PB:=Copy(TypesBox.Text,i+1,MaxInt);
  i:=pos(' ',PB);
  PS:=Copy(PB,i+1,MaxInt);
  PB:=Copy(PB,1,i-1);
  if (Length(BatteryDataFile)=0) then CreateDataFile(nil);

  AssignFile(F,BatteryDataFile );
  try

    if FileExists(BatteryDataFile)
       then Append(F)
       else
       begin
         Rewrite(F);
         writeln(F,'Code: ',PC);
         writeln(F,'Brand: ',PB);
         writeln(F,'Model: ',PS);
         writeln(F,'Sample: ',SamplesBox.Text);
         writeln(F,'Test: ',ActiveTestType.Name);
         writeln(F,'Current: ',ActiveTestType.Current,' mA');
         writeln(F,'Voltage: ',ActiveTestType.Voltage,' mV');
         //writeln(F,'Extra info:');
         //writeln(F,DataInfoMemo.Lines.Text);
         writeln(F);
         writeln(F,DateTimeToStr(NowUTC));
         writeln(F,'*************************************************');
         writeln(F);

         write(F,'B_code',LocalFS.ListSeparator);
         write(F,'Sample#',LocalFS.ListSeparator);
         write(F,'Battery mode',LocalFS.ListSeparator); // = CD
         write(F,'Battery mode value',LocalFS.ListSeparator);
         write(F,'Trigger moment UTC',LocalFS.ListSeparator);
         write(F,'Human time',LocalFS.ListSeparator);
         write(F,'Time(sec)',LocalFS.ListSeparator);
         write(F,'Voltage(V)',LocalFS.ListSeparator);
         write(F,'Current(mA)',LocalFS.ListSeparator);
         write(F,'Capacity(mAh)',LocalFS.ListSeparator);
         write(F,'Energy(mWh)',LocalFS.ListSeparator);
         writeln(F);
       end;

    if (ProgressBar1.Position=ProgressBar1.Max) then
      ProgressBar1.Position:=1
    else
      ProgressBar1.StepIt;

    write(F,PC,LocalFS.ListSeparator);
    write(F,SamplesBox.Text,LocalFS.ListSeparator);
    write(F,'CD',LocalFS.ListSeparator);
    write(F,ActiveTestType.Current,LocalFS.ListSeparator);
    write(F,FloattoStr(NowUTC,LocalFS),LocalFS.ListSeparator);
    write(F,FormatDateTime('dd-mm-yyyy hh:nn:ss',NowUTC),LocalFS.ListSeparator);
    write(F,FloattoStrF((Elapsed/1000),ffFixed,6,1,LocalFS),LocalFS.ListSeparator);
    write(F,FloattoStrF(Voltage,ffFixed,10,3,LocalFS),LocalFS.ListSeparator);
    write(F,FloattoStrF(Current,ffFixed,10,4,LocalFS),LocalFS.ListSeparator);
    write(F,FloattoStrF(Capacity,ffFixed,10,1,LocalFS),LocalFS.ListSeparator);
    write(F,FloattoStrF(Energy,ffFixed,10,1,LocalFS),LocalFS.ListSeparator);

    writeln(F);
  finally
    CloseFile(F);
  end;
end;

procedure TPowerbankMainForm.SetActive(value:boolean);
begin
  if value<>FSystemActive then
  begin
    FSystemActive:=value;
    if value
       then Led.Brush.Color := clRed
       else Led.Brush.Color := clLime;
  end;
end;

end.

