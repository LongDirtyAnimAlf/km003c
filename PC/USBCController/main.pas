unit main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  Windows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Buttons, ComCtrls, ValEdit, Menus, TAGraph, TASeries, Types,
  libusb,
  libusboop,
  fptimernew,
  dsLeds,
  usbcpd,
  pddevice,
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
    btnConnectKC003C: TButton;
    btnGetPPSStatus: TButton;
    btnGetStatus: TButton;
    btnKC003CRcvRemoteSink: TButton;
    btnKC003CRcvRemoteSource: TButton;
    btnKC003CReset: TButton;
    btnKC003CEnterCommandMode: TButton;
    btnRcvRemoteSinkExt: TButton;
    btnRcvRemoteSourceExt: TButton;
    btnHardReset: TButton;
    btnCleanLogs: TButton;
    btnGetSourceInfo: TButton;
    btnRcvRemoteEPRSource: TButton;
    chkgrpPDOFLags: TCheckGroup;
    cmboSerialPorts: TComboBox;
    GroupBattery: TGroupBox;
    grpEPRPDOs: TGroupBox;
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
    grpPowerStatus: TGroupBox;
    grpPDOs: TGroupBox;
    ListBox1: TListBox;
    ListView1: TListView;
    HidePDGoodCRC: TMenuItem;
    ClearPDMessages: TMenuItem;
    HidePDCtrl: TMenuItem;
    PopupPDMessages: TPopupMenu;
    SourceEPRPDODrawGrid: TDrawGrid;
    SourcePDODrawGrid: TDrawGrid;
    SinkPDODrawGrid: TDrawGrid;
    SinkEPRPDODrawGrid: TDrawGrid;
    TabSheet3: TTabSheet;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StoreTimer: TTimer;
    TestTimer: TTimer;
    UpdateTimer: TTimer;
    USBComLog: TMemo;
    USBDebugLog: TMemo;
    USBDetailsLog: TMemo;
    vleBatteryData: TValueListEditor;
    vlePPS: TValueListEditor;
    vleSinkExtended: TValueListEditor;
    vleSourceExtended: TValueListEditor;
    vleStatus: TValueListEditor;
    procedure btnBatteryCapabilitiesClick({%H-}Sender: TObject);
    procedure btnBatteryStatusClick({%H-}Sender: TObject);
    procedure btnCleanLogsClick({%H-}Sender: TObject);
    procedure btnConnectKC003CClick(Sender: TObject);
    procedure btnConnectSTM32Click(Sender: TObject);
    procedure btnGetStatusClick({%H-}Sender: TObject);
    procedure btnKC003CEnterCommandModeClick(Sender: TObject);
    procedure btnKC003CRcvRemoteSinkClick({%H-}Sender: TObject);
    procedure btnKC003CRcvRemoteSourceClick({%H-}Sender: TObject);
    procedure btnKC003CResetClick({%H-}Sender: TObject);
    procedure btnRcvRemoteEPRSourceClick(Sender: TObject);
    procedure btnRcvRemoteSourceExtClick({%H-}Sender: TObject);
    procedure btnRcvRemoteSinkExtClick({%H-}Sender: TObject);
    procedure btnGetSourceInfoClick({%H-}Sender: TObject);
    procedure btnGetPPSStatusClick({%H-}Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ClearPDMessagesClick(Sender: TObject);
    procedure DataEditKeyPress(Sender: TObject; var Key: char);
    procedure gridPDOResize(Sender: TObject);
    procedure grpVADataResize(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SourcePDODrawGridButtonClick(Sender: TObject; aCol, aRow: Integer
      );
    procedure SourcePDODrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);

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
    TemperatureDisplay  : TdsSevenSegmentMultiDisplay;

    FSystemActive       : boolean;

    FVoltage            : double;
    FCurrent            : double;
    FEnergy             : double;
    FPower              : double;
    FTemperature        : double;

    DUT                 : TUSBPDDevice;

    KM003CSerial        : TLazSerial;
    KM003CComport       : string;

    PDTimer             : TFPTimer;
    DataTimer           : TFPTimer;

    KM003CUSB           : TLibUsbDevice;
    Context             : TLibUsbContext;
    DeviceInterface     : TLibUsbInterface;
    OutEndPoint         : TLibUsbBulkOutEndpoint;
    InEndPoint          : TLibUsbBulkInEndpoint;

    TestTypes           : array of TTestType;
    ActiveTestType      : TTestType;
    BatteryDataFile     : string;

    procedure AllStop(Sender: TObject);
    procedure Measure;

    function  CorrectVoltage(value:double):double;
    function  CorrectCurrent(value:double):double;

    procedure SetActive(value:boolean);

    procedure SetVoltage(value:double);
    function  GetVoltage:double;
    procedure SetCurrent(value:double);
    function  GetCurrent:double;
    procedure SetTemperature(value:double);
    function  GetTemperature:double;

    procedure SetBatteryData;

    function ProcessControlMessageGUI(aMSG:TUSBPD_CONTROLMSG; aSOP:TUSBPD_SOPTYPE; PDData:rawbytestring):boolean;
    function ProcessDataMessageGUI(aMSG:TUSBPD_DATAMSG; aSOP:TUSBPD_SOPTYPE; PDData:rawbytestring):boolean;
    function ProcessExtendedMessageGUI(aMSG:TUSBPD_EXTENDEDMSG; aSOP:TUSBPD_SOPTYPE; PDData:rawbytestring):boolean;

    procedure Connect(Sender: TObject);
    procedure DisConnect({%H-}Sender: TObject);

    procedure DataTimerTimer({%H-}Sender: TObject);
    procedure CheckTimerTimer({%H-}Sender: TObject);

    procedure KM003CStartCommandMode(em:byte);
  public
    Capacity                     : double;

    property SystemActive        : boolean read FSystemActive write SetActive;

    property Voltage             : double read GetVoltage write SetVoltage;
    property Current             : double read GetCurrent write SetCurrent;
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
  Clipbrd,
  LCLType,
  km003c,
  Bits,Tools;

Const
  DevVID                       = $5FC9;
  DevPID                       = $0063;
  //DevVID                       = $2E3C;
  //DevPID                       = $5558;
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
  CList,CListDetails:TStringList;
begin
  LocalFS:=DefaultFormatSettings;

  LocalFS.ShortDateFormat:='dd-mm-yyyy';
  LocalFS.LongTimeFormat:='hh:nn:ss';
  LocalFS.DecimalSeparator:=',';
  LocalFS.ListSeparator:=';';

  DUT:=TUSBPDDevice.Create;
  DUT.Cleanup;
  DUT.SopData:=Default(TSopData);

  {$ifdef DEBUG}
  USBComLog.Visible:=false;
  USBDetailsLog.Visible:=false;
  {$endif}

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

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) );
  try
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

  KM003CComport:='';

  //if ((Length(STMComport)=0) OR (Length(HPComport)=0)) then
  begin
    CLIst:=TStringList.Create;
    try
      EnumerateCOMPorts(CLIst);
      CListDetails:=TStringList.Create;
      try
        for i:=0 to Pred(CLIst.Count) do
        begin
          CListDetails.Delimiter:=DefaultFormatSettings.ListSeparator;
          CListDetails.StrictDelimiter:=True;
          CListDetails.DelimitedText:=CLIst[i];
          (*
          for s in CListDetails do
          begin
            STMComport:=s;
          end;
          *)
          if (CListDetails.Count>=4) then
          begin
            s:=CListDetails[3];
            {$ifdef MSWINDOWS}
            s:=StringReplace(s,'COM','',[rfReplaceAll]);
            {$else}
            s:=StringReplace(s,'ttyUSB','',[rfReplaceAll]);
            {$endif MSWINDOWS}
            cmboSerialPorts.Items.Append(s);
            if (CListDetails.Count>=5) then
            begin
              s:=CListDetails[4];
              if (Pos('POWER-Z',s)=1) then KM003CComport:=CListDetails[3];
              if (Pos('APP',s)=1) then KM003CComport:=CListDetails[3];
            end;
          end;
        end;
      finally
        CListDetails.Free
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

  KM003CSerial:=TLazSerial.Create(Self);
  KM003CSerial.Async:=false;

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

  ListView1.DoubleBuffered := True;
end;

procedure TPowerbankMainForm.grpVADataResize(Sender: TObject);
begin
  RealVoltageDisplay.Top:=0;
  RealVoltageDisplay.Left:=5;

  RealVoltageDisplay.Height:=(TControl(Sender).Height)-24;
  RealVoltageDisplay.Width:=(TControl(Sender).Width DIV 2)-6;

  RealCurrentDisplay.Width:=RealVoltageDisplay.Width;
  RealCurrentDisplay.Height:=RealVoltageDisplay.Height;
  RealCurrentDisplay.Top:=RealVoltageDisplay.Top;
  RealCurrentDisplay.Left:=RealVoltageDisplay.Left+RealVoltageDisplay.Width+2;
end;

procedure TPowerbankMainForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox absolute Control;
  ts: TTextStyle;
begin
  lb.Canvas.Brush.Color := PtrInt(lb.Items.Objects[Index]);
  lb.Canvas.FillRect(ARect);

  ts := lb.Canvas.TextStyle;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlCenter;
  lb.Canvas.Pen.Color := clBlack;
  lb.Canvas.TextRect(ARect, ARect.Left+2, ARect.Top, lb.Items[Index], ts);
end;

procedure TPowerbankMainForm.ListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  TListView(Sender).Canvas.Brush.Color := {%H-}PtrUInt(Item.Data);
end;

procedure TPowerbankMainForm.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  s: string;
  i: Integer;
  Item: TListItem;
begin
  if (Key = VK_C) and (ssCtrl in Shift) then
  begin
    s := '';
    Item := TListView(Sender).Selected;
    while Assigned(Item) do
    begin
      //if (Item.Selected AND (Item.SubItems.Count>2)) then
      //  s := s + Item.SubItems[2] + LineEnding;
      if Item.Selected then
      begin
        if (Item.SubItems.Count>2) then
          s := s + Item.SubItems[2] + LineEnding
        else
         if (Item.SubItems.Count>0) then
           s := s + Item.SubItems[0] + LineEnding
      end;
      Item := TListView(Sender).GetNextItem(Item, sdAll, [lisSelected]);
    end;

    if s <> '' then
      Clipboard.AsText := s;
  end;
end;

procedure TPowerbankMainForm.SourcePDODrawGridButtonClick(Sender: TObject;
  aCol, aRow: Integer);
var
  PDOVoltage      : integer;
  PDOCurrent      : integer;
  PDONumber       : byte;
  index           : integer;
  aButton         : TButton;
  Buffer          : array[0..255] of byte;
  aSourcePDO      : TSOURCEPDO;
  aSinkPDO        : TSINKPDO;
  aPDOType        : TSUPPLY_TYPES;
begin
  PDOCurrent:=0;
  PDOVoltage:=0;

  aSourcePDO.Raw:=0;

  if Sender=SourcePDODrawGrid then aSourcePDO:=DUT.SourcePDOs[aRow];
  if Sender=SourceEPRPDODrawGrid then aSourcePDO:=DUT.SourceEPRPDOs[aRow];

  PDONumber:=aRow;
  // EPRs always start at 8 !!
  if Sender=SourceEPRPDODrawGrid then Inc(PDONumber,7);

  if ((Sender=SourcePDODrawGrid) OR (Sender=SourceEPRPDODrawGrid)) then
  begin
    if aSourcePDO.Raw>0 then
    begin
      aPDOType:=TSUPPLY_TYPES(aSourcePDO.GenericPdo.Supply);


      if (aPDOType=TSUPPLY_TYPES.Fixed) then
      begin
        with aSourcePDO.FixedSupplyPdo do
        begin
          PDOCurrent:=(MaximumCurrentIn10mA*10);
          PDOVoltage:=(VoltageIn50mV*50);
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.Variable) then
      begin
        with aSourcePDO.VariableSupplyNonBatteryPdo do
        begin
          PDOCurrent:=(MaximumCurrentIn10mA*10);
          PDOVoltage:=(MaximumVoltageIn50mV*50);
        end;
      end
      else
      if (aPDOType=TSUPPLY_TYPES.APDO) then
      begin
        with aSourcePDO.SPRPowerSupplyApdo do
        begin
          PDOCurrent:=(MaximumCurrentIn50mA*50);
          PDOVoltage:=(MaximumVoltageIn100mV*100);
        end;
      end;

      if ((PDOVoltage=0) OR (PDOCurrent=0)) then exit;

      USBDebugLog.Lines.Append('Requesting PDO #'+InttoStr(PDONumber)+' at '+InttoStr(PDOVoltage)+'mV and '+InttoStr(PDOCurrent)+'mA.');

      //KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDREQSIMPLE].Command,[PDONumber]));
      KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDREQEXT].Command,[PDONumber,PDOVoltage,PDOCurrent]));

    end;
  end;

end;

procedure TPowerbankMainForm.SourcePDODrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aDrawGrid      : TDrawGrid;
  s              : string;
  aSourcePDO     : TSOURCEPDO;
  aSinkPDO       : TSINKPDO;
  aPDOType       : TSUPPLY_TYPES;

begin
  if ((aRow>0) AND (aCol>0)) then
  begin
    aDrawGrid:=TDrawGrid(Sender);
    s:='';
    if (aCol=1) then
    begin
      s:='PDO #'+InttoStr(aRow);
    end
    else
    begin
      s:='-';
      // aRow == PDO index !!
      // Easy ... ;-)

      if ((Sender=SourcePDODrawGrid) OR (Sender=SourceEPRPDODrawGrid)) then
      begin
        if (Sender=SourcePDODrawGrid) then aSourcePDO:=DUT.SourcePDOs[aRow];
        if (Sender=SourceEPRPDODrawGrid) then aSourcePDO:=DUT.SourceEPRPDOs[aRow];
        if (aSourcePDO.Raw>0) then
        begin
          aPDOType:=TSUPPLY_TYPES(aSourcePDO.GenericPdo.Supply);
          if (aCol=2) then s:=SUPPLY_TYPES[aPDOType];
        end;
      end;

      if ((Sender=SinkPDODrawGrid) OR (Sender=SinkEPRPDODrawGrid)) then
      begin
        if (Sender=SinkPDODrawGrid) then aSinkPDO:=DUT.SinkPDOs[aRow];
        if (Sender=SinkEPRPDODrawGrid) then aSinkPDO:=DUT.SinkEPRPDOs[aRow];
        if (aSinkPDO.Raw>0) then
        begin
          aPDOType:=TSUPPLY_TYPES(aSinkPDO.GenericPdo.Supply);
          if (aCol=2) then s:=SUPPLY_TYPES[aPDOType];
        end;
      end;

      if Sender=SinkPDODrawGrid then
      begin
        if (aSinkPDO.Raw>0) then
        begin
          if (aPDOType=TSUPPLY_TYPES.Fixed) then
          begin
            with aSinkPDO.FixedSupplyPdo do
            begin
              if aCol=4 then s:=InttoStr(OperationalCurrentIn10mA*10)+ ' mA';
              if aCol=3 then s:=FloattoStrF(VoltageIn50mV*0.05,ffFixed,8,1)+' Volt';
            end;
          end
          else
          if (aPDOType=TSUPPLY_TYPES.Battery) then
          begin
            with aSinkPDO.BatterySupplyPdo do
            begin
              if aCol=5 then s:=UIntToStr(MaximumAllowablePowerIn250mW*250)+ 'mW';
              if aCol=3 then s:=
                FloattoStrF(MinimumVoltageIn50mV*0.05,ffFixed,8,2)+
                '-'+
                FloattoStrF(MaximumVoltageIn50mV*0.05,ffFixed,8,2)+
                ' Volt';
            end;
          end
          else
          if (aPDOType=TSUPPLY_TYPES.Variable) then
          begin
            with aSinkPDO.VariableSupplyNonBatteryPdo do
            begin
              if aCol=4 then s:=InttoStr(OperationalCurrentIn10mA*10)+ 'mA';
              if aCol=3 then s:=
                FloattoStrF(MinimumVoltageIn50mV*0.05,ffFixed,8,2)+
                '-'+
                FloattoStrF(MaximumVoltageIn50mV*0.05,ffFixed,8,2)+
                ' Volt';
            end;
          end;
        end;
      end;


      //if Sender=SourcePDODrawGrid then
      begin
        if (aSourcePDO.Raw>0) then
        begin
          if (aPDOType=TSUPPLY_TYPES.Fixed) then
          begin
            with aSourcePDO.FixedSupplyPdo do
            begin
              if aCol=4 then s:=InttoStr(MaximumCurrentIn10mA*10)+ ' mA';
              if aCol=3 then s:=InttoStr(VoltageIn50mV*50 DIV 1000)+' Volt';
            end;
          end
          else
          if (aPDOType=TSUPPLY_TYPES.Variable) then
          begin
            with aSourcePDO.VariableSupplyNonBatteryPdo do
            begin
              if aCol=4 then s:=InttoStr(MaximumCurrentIn10mA*10)+ ' mA';
              if aCol=3 then s:=InttoStr(MaximumVoltageIn50mV*50 DIV 1000)+' Volt';
            end;
          end
          else
          if (aPDOType=TSUPPLY_TYPES.APDO) then
          begin
            with aSourcePDO.SPRPowerSupplyApdo do
            begin
              if aCol=4 then s:=InttoStr(MaximumCurrentIn50mA*50)+ 'mA';
              if aCol=3 then s:=
                FloattoStrF(MinimumVoltageIn100mV*0.1,ffFixed,8,1)+
                '-'+
                FloattoStrF(MaximumVoltageIn100mV*0.1,ffFixed,8,1)+
                ' Volt';
            end;
          end;

        end;
      end;

      (*
      if Sender=SourceEPRPDODrawGrid then
      begin
        if (aSourcePDO.Raw>0) then
        begin
          if (aPDOType=TSUPPLY_TYPES.Fixed) then
          begin
            with aSourcePDO.FixedSupplyPdo do
            begin
              if aCol=4 then s:=InttoStr(MaximumCurrentIn10mA*10)+ ' mA';
              if aCol=3 then s:=InttoStr(VoltageIn50mV*50 DIV 1000)+' Volt';
            end;
          end

          if (aPDOType=TSUPPLY_TYPES.APDO) then
          begin
            if (TAPDO_TYPES(aSourcePDO.GenericAPdo.APO)=TAPDO_TYPES.SPR) then
            begin
              with aSourcePDO.SPRPowerSupplyApdo do
              begin
                if aCol=4 then s:=InttoStr(MaximumCurrentIn50mA*50)+ ' mA';
                if aCol=3 then s:=
                  FloattoStrF(MinimumVoltageIn100mV*0.1,ffFixed,8,1)+
                  '-'+
                  FloattoStrF(MaximumVoltageIn100mV*0.1,ffFixed,8,1)+
                  ' Volt';
              end;
            end;

            if (TAPDO_TYPES(aSourcePDO.GenericAPdo.APO)=TAPDO_TYPES.EPR) then
            begin
              with aSourcePDO.EPRPowerSupplyApdo do
              begin
                if aCol=4 then s:=
                   FloattoStrF(MinimumVoltageIn100mV*0.1,ffFixed,8,1)+
                   '-'+
                   FloattoStrF(MaximumVoltageIn100mV*0.1,ffFixed,8,1)+
                   ' Volt';
                if aCol=3 then s:=InttoStr(PDPInW)+' Watt';
              end;
            end;
          end;
        end;
      end;
      *)
    end;

    (*
    if DI then
    begin
      // We have invalid data
      // Show it by painting red !
      aDrawGrid.Canvas.Font.Color:=clWhite;
      aDrawGrid.Canvas.Font.Style:=[fsBold];
      aDrawGrid.Canvas.Brush.Color:=clRed;
      aDrawGrid.Canvas.FillRect(ARect);
    end;
    *)
    InflateRect(ARect, -constCellpadding, -constCellPadding);
    aDrawGrid.Canvas.TextRect(ARect, ARect.Left, ARect.Top, s);
  end;
end;

procedure TPowerbankMainForm.btnCleanLogsClick(Sender: TObject);
begin
  USBComLog.Lines.Clear;
  USBDetailsLog.Lines.Clear;
  USBDebugLog.Lines.Clear;
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
begin
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_STATUS]));
end;

procedure TPowerbankMainForm.btnKC003CEnterCommandModeClick(Sender: TObject);
begin
  KM003CStartCommandMode(2);
end;

procedure TPowerbankMainForm.btnKC003CRcvRemoteSinkClick(Sender: TObject);
var
  i:integer;
begin
  for i:=1 to MAXPDO do DUT.SinkPDOs[i].Raw:=0;
  for i:=1 to MAXEPRPDO do DUT.SinkEPRPDOs[i].Raw:=0;
  SinkPDODrawGrid.Invalidate;
  SinkEPRPDODrawGrid.Invalidate;

  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SNK_CAP]));
end;

procedure TPowerbankMainForm.btnKC003CRcvRemoteSourceClick(Sender: TObject);
var
  s,t:AnsiString;
  data:TStringList;
  i:integer;
begin
  for i:=1 to MAXPDO do DUT.SourcePDOs[i].Raw:=0;
  for i:=1 to MAXEPRPDO do DUT.SourceEPRPDOs[i].Raw:=0;
  SourcePDODrawGrid.Invalidate;
  SourceEPRPDODrawGrid.Invalidate;

  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SRC_CAP]));

  if ((NOT Assigned(KM003CUSB)) OR (NOT Assigned(KM003CUSB.Handle))) then
  begin
    s:=KM003CSerial.ReadString(250);
    data:=TStringList.Create;
    try
      data.Delimiter:=#$0A;
      data.StrictDelimiter:=True;
      data.DelimitedText:=s;
      i:=data.Count;
      for s in data do
      begin
        t:=s;
      end;
    finally
      data.Free;
    end;

    //'ok'#$0A'max power 0W'#$0A'Fixed:     5.00V 3.00A'#$0A'Fixed:     9.00V 3.00A'#$0A'Fixed:    12.00V 3.00A'#$0A'Fixed:    15.00V 3.00A'#$0A'Fixed:    20.00V 5.00A'#$0A'PPS: 3.30-21.00V 5.00A'#$0A'cc1 attach'#$0A'pdo:6'#$0B','#$91#$81#$08','#$D1#$02#$00','#$C1#$03#$00','#$B1#$04#$00',A'#$06#$00'<!'#$A4#$C1#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$0A'ready:5200mV,0mA'#$0A

    (*
    NumberSRCPDO:=NumberOfDataObjects;
    for i:=1 to NumberSRCPDO do
    begin
      for j:=0 to 3 do DUT.SourcePDOs[i].Bytes[j]:=data^[j+(i-1)*4];
    end;
    *)
  end;
end;

procedure TPowerbankMainForm.btnKC003CResetClick(Sender: TObject);
begin
  DUT.Cleanup;
  grpPDOs.Invalidate;
  grpEPRPDOs.Invalidate;
  grpPowerStatus.Invalidate;
  TabSheet1.Invalidate;
  KM003CSerial.WriteString(KC003CCommand[TKC003CCOMMAND.RESET].Command);
end;

procedure TPowerbankMainForm.btnRcvRemoteEPRSourceClick(Sender: TObject);
var
  SOPHeader:TPDHEADER;
  SOPHeaderExtended:TPDHEADEREXTENDED;
  GBDB:TGBDB;
  EPRMDO:TEPRMDO;
  DWordData:TDWordData;
  ByteData:TByteData;
  Data:string;
  j:integer;
begin
  (*
  // Create Header to send the EPR_Mode_Enter Message
  SOPHeader.Raw:=0;
  SOPHeader.Data.Message_Type:=Ord(USBPD_DATAMSG_EPR_MODE);
  SOPHeader.Data.Specification_Revision:=2; // Revision 3.0/3.1+
  SOPHeader.Data.Port_Power_Role:=0; // We are the sink
  SOPHeader.Data.Number_of_Data_Objects:=1;
  SOPHeader.Data.Extended:=0;
  SOPHeader.Data.MessageID:=001;

  // Create DataObject
  EPRMDO.Raw:=0;
  EPRMDO.Data.Action:=USBPD_EPRMDO_ACTION_ENTERSINK; // Action: 01h - Enter


  // Add SOP
  Data:=InttoHex(Ord(SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add DataObject
  for j:=0 to 3 do Data:=Data+InttoHex(EPRMDO.Bytes[j],2);

  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));
  *)

  // CSend the EPR_Get_Source_Cap Message
  // Create Header
  SOPHeader.Raw:=0;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_EXTENDED_CONTROL);
  SOPHeader.Data.Specification_Revision:=2; // Revision 3.0/3.1+
  SOPHeader.Data.Port_Power_Role:=0; // We are the sink
  SOPHeader.Data.Number_of_Data_Objects:=1;
  SOPHeader.Data.Extended:=1;
  SOPHeader.Data.MessageID:=002;
  // Create Extended Header
  SOPHeaderExtended.Raw:=0;
  SOPHeaderExtended.Data.Data_Size:=1;

  // Create DataObject
  ByteData.Raw:=1;

  // Add SOP
  Data:=InttoHex(Ord(SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  for j:=0 to 0 do Data:=Data+InttoHex(ByteData.Raw,2);

  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));


  (*

  // Create Header
  SOPHeader.Raw:=0;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_EPR_SOURCE_CAPABILITIES);
  SOPHeader.Data.Specification_Revision:=2;
  SOPHeader.Data.Port_Power_Role:=0;
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
  Data:=InttoHex(Ord(SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  //Data:=Data+InttoHex(GBDB.Raw,2);
  //for j:=0 to 3 do Data:=Data+InttoHex(DWordData.Bytes[j],2);

  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));

  //KM003CSerial.WriteString('pd data=0B8000800000');

  //KM003CSerial.WriteString('pd data=21800100060001');
  //sleep(2000);
  //KM003CSerial.WriteString('pd data=0F800100FF000191');
  //sleep(2000);
  //KM003CSerial.WriteString('pd data=0B840000');
  *)

end;

procedure TPowerbankMainForm.btnBatteryStatusClick(Sender: TObject);
const
  BATTNUMBER = 0;
var
  SOPHeader:TPDHEADER;
  SOPHeaderExtended:TPDHEADEREXTENDED;
  GBDB:TGBDB;
  DWordData:TDWordData;
  Data:string;
  j:integer;
begin
  vleBatteryData.Values['SOC']:='';

  // Create Header
  SOPHeader.Raw:=$0000;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_GET_BATTERY_STATUS);
  SOPHeader.Data.Specification_Revision:=2;
  SOPHeader.Data.Port_Power_Role:=0;
  SOPHeader.Data.Number_of_Data_Objects:=1;
  SOPHeader.Data.Extended:=1;
  // Create Extended Header
  SOPHeaderExtended.Raw:=$0000;
  SOPHeaderExtended.Data.Data_Size:=1;
  SOPHeaderExtended.Data.Chunked:=1;
  // Create DataObject
  DWordData.Raw:=0;
  GBDB.Raw:=0;
  GBDB.Data.FixedBatteries:=0;
  DWordData.Bytes[0]:=GBDB.Raw;
  // Add SOP
  Data:=InttoHex(Ord(SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  for j:=0 to 3 do Data:=Data+InttoHex(DWordData.Bytes[j],2);
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));
end;

procedure TPowerbankMainForm.btnBatteryCapabilitiesClick(Sender: TObject);
const
  BATTNUMBER = 0;
var
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

  // Create Header
  SOPHeader.Raw:=0;
  SOPHeader.Data.Message_Type:=Ord(USBPD_EXTMSG_GET_BATTERY_CAP);
  SOPHeader.Data.Specification_Revision:=2;
  SOPHeader.Data.Port_Power_Role:=0;
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
  Data:=InttoHex(Ord(SOP),2);
  // Add Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeader.Bytes[j],2);
  // Add padding
  //for j:=0 to 1 do Data:=Data+InttoHex(0,2);
  // Add Extended Header
  for j:=0 to 1 do Data:=Data+InttoHex(SOPHeaderExtended.Bytes[j],2);
  // Add DataObject
  Data:=Data+InttoHex(GBDB.Raw,2);
  for j:=0 to 3 do Data:=Data+InttoHex(DWordData.Bytes[j],2);

  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDDATA].Command,[Data]));
end;

procedure TPowerbankMainForm.btnRcvRemoteSourceExtClick(Sender: TObject);
begin
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SRC_CAPEXT]));
end;

procedure TPowerbankMainForm.btnRcvRemoteSinkExtClick(Sender: TObject);
begin
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SNK_CAPEXT]));
end;

procedure TPowerbankMainForm.btnGetSourceInfoClick(Sender: TObject);
begin
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_SOURCE_INFO]));
end;

procedure TPowerbankMainForm.btnGetPPSStatusClick(Sender: TObject);
begin
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDCMD].Command,[USBPD_CONTROLMSG_GET_PPS_STATUS]));
end;


procedure TPowerbankMainForm.Button1Click(Sender: TObject);
begin
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDMSETTYPE].Command,[0,2,1]));
end;

procedure TPowerbankMainForm.ClearPDMessagesClick(Sender: TObject);
begin
  ListBox1.Clear;
  ListView1.Clear;
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
         AllStop(Sender);

         Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) );
         try
           ini.WriteInteger(Self.Name,'Top',Self.Top);
           ini.WriteInteger(Self.Name,'Left',Self.Left);
           ini.WriteInteger(Self.Name,'Width',Self.Width);
           ini.WriteInteger(Self.Name,'Height',Self.Height);
         finally
           Ini.Free;
         end;
         CloseAction:=caFree;
       end;
end;

procedure TPowerbankMainForm.FormDestroy(Sender: TObject);
begin
  DisConnect(Sender);
  DUT.Free;
end;

procedure TPowerbankMainForm.gridPDOResize(Sender: TObject);
var
  aGrid:TStringGrid;
begin
  aGrid:=TStringGrid(Sender);
  aGrid.Height:=aGrid.DefaultRowHeight*aGrid.RowCount+4;
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
  signed                : ^int32;
  normal                : int32;
begin
  if (Assigned(KM003CUSB) AND Assigned(KM003CUSB.Handle)) then
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
end;

function TPowerbankMainForm.ProcessControlMessageGUI(aMSG:TUSBPD_CONTROLMSG; aSOP:TUSBPD_SOPTYPE; PDData:rawbytestring):boolean;
var
  enumname:string;
  ListItem : TListItem;
begin
  result:=true;

  Str(aMSG,enumname);
  if (NOT (HidePDGoodCRC.Checked AND (aMSG=TUSBPD_CONTROLMSG.USBPD_CONTROLMSG_GOODCRC))) then
  begin
    USBDebugLog.Lines.Append('PD control: '+enumname+'.');
    //ColorListBox1.AddItem('Ctrl: '+TUSBPD_CONTROLMSG_DATAS[aMSG].Name,TObject(TUSBPD_CONTROLMSG_DATAS[aMSG].Color));
    ListBox1.Items.AddObject('Ctrl: '+TUSBPD_CONTROLMSG_DATAS[aMSG].Name, TObject(PtrInt(TUSBPD_CONTROLMSG_DATAS[aMSG].Color)));

    //ListView1.BeginUpdate;
    //ListView1.Items.BeginUpdate;
    ListItem := ListView1.Items.Add;
    ListItem.Data:={%H-}Pointer(TUSBPD_CONTROLMSG_DATAS[aMSG].Color);
    ListItem.Caption := InttoStr(ListItem.Index);
    ListItem.SubItems.Add('Ctrl: '+TUSBPD_CONTROLMSG_DATAS[aMSG].Name);
    Str(aSOP,enumname);
    ListItem.SubItems.Add(enumname);
    ListItem.SubItems.Add(PDData);

    {$ifdef Windows}
    if ListView1.Items.Count > 0 then
      SendMessage(ListView1.Handle, LVM_ENSUREVISIBLE, ListView1.Items.Count - 1, 0);
    {$else}
    if ListView1.Items.Count > 0 then
      ListView1.Items[ListView1.Items.Count - 1].MakeVisible(False);
    {$endif}

    //ListView1.Items.EndUpdate;
    //ListView1.EndUpdate;
  end;
end;

function TPowerbankMainForm.ProcessDataMessageGUI(aMSG:TUSBPD_DATAMSG; aSOP:TUSBPD_SOPTYPE; PDData:rawbytestring):boolean;
var
  s:string;
  enumname:string;
  ListItem : TListItem;
begin
  result:=true;
  ListBox1.AddItem('Data: '+TUSBPD_DATAMSG_DATAS[aMSG].Name,TObject(TUSBPD_DATAMSG_DATAS[aMSG].Color));

  //ListView1.BeginUpdate;
  //ListView1.Items.BeginUpdate;
  ListItem := ListView1.Items.Add;

  ListItem.Data:={%H-}Pointer(TUSBPD_DATAMSG_DATAS[aMSG].Color);
  ListItem.Caption := InttoStr(ListItem.Index);
  ListItem.SubItems.Add('Data: '+TUSBPD_DATAMSG_DATAS[aMSG].Name);
  Str(aSOP,enumname);
  ListItem.SubItems.Add(enumname);
  ListItem.SubItems.Add(PDData);

  {$ifdef Windows}
  if ListView1.Items.Count > 0 then
    SendMessage(ListView1.Handle, LVM_ENSUREVISIBLE, ListView1.Items.Count - 1, 0);
  {$else}
  if ListView1.Items.Count > 0 then
    ListView1.Items[ListView1.Items.Count - 1].MakeVisible(False);
  {$endif}

  //ListView1.Items.EndUpdate;
  //ListView1.EndUpdate;

  case aMSG of
    USBPD_DATAMSG_SRC_CAPABILITIES:
      begin
        SourcePDODrawGrid.Invalidate;
      end;
    USBPD_DATAMSG_SNK_CAPABILITIES:
      begin
        SinkPDODrawGrid.Invalidate;
      end;
    USBPD_DATAMSG_REQUEST:
    begin
      s:='Official Received RDO.'+#13#10;
      s:=s+DUT.GetRDOInfo;
      USBDebugLog.Lines.Append(s);
    end;
    USBPD_DATAMSG_BATTERY_STATUS:
    begin
      if DUT.BatteryStatus.Data.BatterySOC100mWh=$FFFF then
        vleBatteryData.Values['SOC']:='SOC unknown'
      else
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
      s:=s+'EPR mode message: '+DUT.EPRModeInfo;
      USBDebugLog.Lines.Append(s);
    end
    else
    begin
      result:=false;
    end;
  end;
end;

function TPowerbankMainForm.ProcessExtendedMessageGUI(aMSG:TUSBPD_EXTENDEDMSG; aSOP:TUSBPD_SOPTYPE; PDData:rawbytestring):boolean;
var
  j:integer;
  aFlag:TByteData;
  enumname:string;
  ListItem : TListItem;
begin
  result:=true;

  if (NOT (HidePDCtrl.Checked AND (aMSG=TUSBPD_EXTENDEDMSG.USBPD_EXTMSG_EXTENDED_CONTROL))) then
  begin
    ListBox1.AddItem('Ext: '+TUSBPD_EXTENDEDMSG_DATAS[aMSG].Name,TObject(TUSBPD_EXTENDEDMSG_DATAS[aMSG].Color));

    //ListView1.BeginUpdate;
    //ListView1.Items.BeginUpdate;
    ListItem := ListView1.Items.Add;

    ListItem.Data:={%H-}Pointer(TUSBPD_EXTENDEDMSG_DATAS[aMSG].Color);
    ListItem.Caption := InttoStr(ListItem.Index);
    ListItem.SubItems.Add('Ext: '+TUSBPD_EXTENDEDMSG_DATAS[aMSG].Name);
    Str(aSOP,enumname);
    ListItem.SubItems.Add(enumname);
    ListItem.SubItems.Add(PDData);

    {$ifdef Windows}
    if ListView1.Items.Count > 0 then
      SendMessage(ListView1.Handle, LVM_ENSUREVISIBLE, ListView1.Items.Count - 1, 0);
    {$else}
    if ListView1.Items.Count > 0 then
      ListView1.Items[ListView1.Items.Count - 1].MakeVisible(False);
    {$endif}


    //ListView1.Items.EndUpdate;
    //ListView1.EndUpdate;
  end;

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
      if (DUT.PPSSDB.Data.OutputVoltage20mV=$FFFF) then
        vlePPS.Values['Output Voltage']:='Not supported'
      else
        vlePPS.Values['Output Voltage']:=InttoStr(DUT.PPSSDB.Data.OutputVoltage20mV*20)+'mV';

      if (DUT.PPSSDB.Data.OutputCurrent50mA=$FF) then
        vlePPS.Values['Output Current']:='Not supported'
      else
        vlePPS.Values['Output Current']:=InttoStr(DUT.PPSSDB.Data.OutputCurrent50mA*50)+'mA';

      aFlag.Raw:=DUT.PPSSDB.Data.RealTimeFlags;
    end;
    USBPD_EXTMSG_BATTERY_CAPABILITIES:
    begin
      j:=DUT.BatteryCaps.Data.VID;
      vleBatteryData.Values['VID']:=InttoStr(j)+' ('+DUT.GetVID(j)+')';
      vleBatteryData.Values['Type']:=InttoStr(DUT.BatteryCaps.Data.BatteryType);
      if DUT.BatteryCaps.Data.BatteryDesignCapacity=$FFFF then
        vleBatteryData.Values['Capacity']:='Unknown'
      else
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
      SinkPDODrawGrid.Invalidate;
      SinkEPRPDODrawGrid.Invalidate;
    end;
    USBPD_EXTMSG_EPR_SOURCE_CAPABILITIES:
    begin
      SourcePDODrawGrid.Invalidate;
      SourceEPRPDODrawGrid.Invalidate;
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
  EventHeader           : TKM003CEventHeader;

  HexResult             : ansistring;

  avalue                : double;
  signed                : ^int16;
  normal                : int16;

  result_code           : LongInt;
  newdata               : packed array[0..4095] of byte;
  j                     : integer;
  dataindexer           : integer;
  datasize              : integer;
  SOPPacket             : boolean;
  ChunkPayloadLen       : byte;

  PDControlMessage      : TUSBPD_CONTROLMSG;
  PDDataMessage         : TUSBPD_DATAMSG;
  PDExtendedMessage     : TUSBPD_EXTENDEDMSG;

  enumname              : ansistring;
  ListItem              : TListItem;
begin
  if (Assigned(KM003CUSB) AND Assigned(KM003CUSB.Handle)) then
  begin
    // Send command to get PD data

    header.Raw:=0;
    header.Ctrl.typ:=Ord(TKM003CHeaderCommand.CMD_GET_DATA);
    header.Ctrl.att:=TKM003C_ATT_PD_PACKET;
    result_code := OutEndPoint.Send(header.Bytes , 4 ,10);

    if (result_code>0) then
    begin
      // Receive PD data
      FillChar({%H-}newdata,4096,0);
      result_code := InEndPoint.Recv(newdata ,SizeOf(newdata) , 10);

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

          // Get packet header
          for j:=0 to 11 do PacketHeader.Bytes[j]:=newdata[dataindexer+j];
          Inc(dataindexer,12);
          Dec(datasize,12);

          if (datasize=0) then
          begin
            // We received the PD status header
          end
          else
          begin
            // We received the PD preamble
          end;

          avalue:=PacketHeader.Status.vbus_mV/1000;
          RealVoltageDisplay.Value:=avalue;

          {$push}
          {$R-}
          normal:=(PacketHeader.Status.ibus_mA);
          {$pop}
          signed:=@normal;
          avalue:=signed^/1000;
          RealCurrentDisplay.Value:=avalue;

          avalue:=(PacketHeader.Status.cc1_mV)/1000;
          Vcc1VoltageDisplay.Value:=(avalue);
          avalue:=(PacketHeader.Status.cc2_mV)/1000;
          Vcc2VoltageDisplay.Value:=(avalue);

          while (datasize>0) do
          begin
            // Now an event header of 6 bytes
            for j:=0 to 5 do EventHeader.Bytes[j]:=newdata[dataindexer+j];
            Inc(dataindexer,6);
            Dec(datasize,6);

            //EventHeader.
            //454f3c000012


            if (datasize=0) then
            begin
              // Only an event received !!
              // Give some info about it.
              enumname:='';

              if (EventHeader.EventData.EventCode=TKM003C_CMD_CRCERROR) then
              begin
                if EventHeader.EventData.Marker=$05 then enumname:='Receive Error Code: CRC Check';
              end
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_RETRIESERROR) then
              begin
                if EventHeader.EventData.Marker=$05 then enumname:='Receive Error Code: Retries';
              end
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_DATACOUNTERROR) then
              begin
                if EventHeader.EventData.Marker=$05 then enumname:='Receive Error Code: DataCount';
              end
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_SOPERROR) then
              begin
                if EventHeader.EventData.Marker=$05 then enumname:='Receive Error Code: SOP';
              end
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_EOPERROR) then
              begin
                if EventHeader.EventData.Marker=$05 then enumname:='Receive Error Code: EOP';
              end
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_HARDRESET) then
              begin
                if EventHeader.EventData.Marker=$45 then enumname:='Send Hard Reset';
                if EventHeader.EventData.Marker=$05 then enumname:='Receive Hard Reset';
              end
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_DETACHCC1) then
                enumname:='Detach: CC1'
              else
              if (EventHeader.EventData.EventCode=TKM003C_CMD_ATTACHCC1) then
                enumname:='Attach: CC1'
              else
                //USBDebugLog.Lines.Append('Empty data. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(Int64(EventHeader.PDData.TimeStamp.Raw)))));
                enumname:='Empty event data. Event:0x'+InttoHex(EventHeader.EventData.EventCode,2);

              if (Length(enumname)>0) then
              begin
                ListBox1.Items.AddObject('Event: '+enumname, TObject(PtrInt(clRed)));

                ListItem := ListView1.Items.Add;
                ListItem.Data:={%H-}Pointer(clRed);
                ListItem.Caption := InttoStr(ListItem.Index);
                ListItem.SubItems.Add('Event: '+enumname);
                {$ifdef Windows}
                if ListView1.Items.Count > 0 then
                  SendMessage(ListView1.Handle, LVM_ENSUREVISIBLE, ListView1.Items.Count - 1, 0);
                {$else}
                if ListView1.Items.Count > 0 then
                  ListView1.Items[ListView1.Items.Count - 1].MakeVisible(False);
                {$endif}
              end;

              break;
            end;

            if EventHeader.EventData.Marker=TKM003C_CMD_CONN_EVENT then
            begin
              // Might be for HID only
              //continue;
            end;

            // If bit8 is set, we seem to have a valid SOP packet.
            SOPPacket:=(EventHeader.PDData.Size.Bits[7]=1);
            // This might also be a valid marker
            //SOPPacket:=((EventHeader.PDData.Size.Bits[7]=1) AND (EventHeader.PDData.Size.Bits[6]=0));
            // This might also be a valid marker
            //SOPPacket:=(($80<EventHeader.PDData.Size.Raw) AND (EventHeader.PDData.Size.Raw<$9F));

            // Get/set packet size by resetting SOP-bits
            EventHeader.PDData.Size.Bits[7]:=0;
            EventHeader.PDData.Size.Bits[6]:=0;

            if EventHeader.PDData.Size.Raw=0 then
            begin
              USBDebugLog.Lines.Append('Very empty PD data !!');
              break;
            end;


            DUT.SopData.SOPTYPE:=TUSBPD_SOPTYPE(EventHeader.PDData.SOP);

            if (NOT SOPPacket) then
            begin
              //USBDebugLog.Lines.Append('Non SOP data ['+InttoStr(datasize)+']. Data size: '+InttoStr(EventHeader.PDData.Size.Raw)+'. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(EventHeader.PDData.TimeStamp.Raw))));
            end;

            if SOPPacket then
            begin
              EventHeader.PDData.Size.Raw:=(EventHeader.PDData.Size.Raw-TKM003C_CMD_SIZE_OFFSET);
              // EventHeader.PDData.Size.Raw is now SOP packet in bytes

              //USBDebugLog.Lines.Append('Data size: '+InttoStr(EventHeader.PDData.Size.Raw)+'. SOP: '+InttoStr(EventHeader.PDData.SOP)+'. Time: '+FormatDateTime('hh:nn:ss.zzz', TimeStampToDateTime(MSecsToTimeStamp(EventHeader.PDData.TimeStamp.Raw))));

              // Now a 2 byte SOP header
              for j:=0 to 1 do DUT.SopData.HEADER.Bytes[j]:=newdata[dataindexer+j];
              Inc(dataindexer,2);
              Dec(datasize,2);
              //USBDebugLog.Lines.Append('SOP: '+GetSOPInfo(aSOPDHEADER));

              PDControlMessage:=USBPD_CONTROLMSG_RESERVED0;
              PDDataMessage:=USBPD_DATAMSG_RESERVED0;
              PDExtendedMessage:=USBPD_EXTMSG_RESERVED0;

              if ((DUT.SopData.HEADER.Data.Number_of_Data_Objects=0) AND (DUT.SopData.HEADER.Data.Extended=0)) then
              begin
                PDControlMessage:=TUSBPD_CONTROLMSG(DUT.SopData.HEADER.Data.Message_Type);
                DUT.SopData.EXTHEADER.Raw:=0;
              end
              else
              begin
                if (DUT.SopData.HEADER.Data.Extended=0) then
                begin
                  PDDataMessage:=TUSBPD_DATAMSG(DUT.SopData.HEADER.Data.Message_Type);
                  DUT.SopData.EXTHEADER.Raw:=0;
                end
                else
                begin
                  PDExtendedMessage:=TUSBPD_EXTENDEDMSG(DUT.SopData.HEADER.Data.Message_Type);
                  for j:=0 to 1 do DUT.SopData.EXTHEADER.Bytes[j]:=newdata[dataindexer+j];
                  Inc(dataindexer,2);
                  Dec(datasize,2);
                end;

                if DUT.SopData.EXTHEADER.Data.Chunked=0 then // will handle both unchunked and non-extended data
                begin
                  // Single chunk message or not extended data - easy
                  if (DUT.SopData.HEADER.Data.Extended=1) then
                    DUT.SopData.DATA.TotalSize := DUT.SopData.EXTHEADER.Data.Data_Size
                  else
                    DUT.SopData.DATA.TotalSize := DUT.SopData.HEADER.Data.Number_of_Data_Objects*4;
                  Move(newdata[dataindexer], DUT.SopData.DATA.Data[0], DUT.SopData.DATA.TotalSize);
                  DUT.SopData.DATA.ReceivedBytes := DUT.SopData.DATA.TotalSize;
                  DUT.SopData.DATA.IsComplete := True;
                end
                else
                begin
                  // Handle chuncked data
                  if DUT.SopData.EXTHEADER.Data.Request_Chunk=0 then
                  begin
                    // Handle only chunk data after a chunk request

                    // First chunk sets the size
                    if (DUT.SopData.EXTHEADER.Data.Chunk_Number=0) then DUT.SopData.DATA.TotalSize:=DUT.SopData.EXTHEADER.Data.Data_Size;
                    // Check out of order or missing chunk → error handling
                    if (DUT.SopData.EXTHEADER.Data.Chunk_Number<>DUT.SopData.DATA.NextChunkNum) then
                    begin
                    end;

                    ChunkPayloadLen := DUT.SopData.HEADER.Data.Number_of_Data_Objects * 4 - 2;   // subtract 2 bytes of Extended Header

                    // Copy only what we still need
                    if DUT.SopData.DATA.ReceivedBytes + ChunkPayloadLen > DUT.SopData.DATA.TotalSize then
                      ChunkPayloadLen := DUT.SopData.DATA.TotalSize - DUT.SopData.DATA.ReceivedBytes;

                    Move(newdata[dataindexer],
                         DUT.SopData.DATA.Data[DUT.SopData.DATA.ReceivedBytes],
                         ChunkPayloadLen);

                    Inc(DUT.SopData.DATA.ReceivedBytes, ChunkPayloadLen);
                    Inc(DUT.SopData.DATA.NextChunkNum);

                    if DUT.SopData.DATA.ReceivedBytes >= DUT.SopData.DATA.TotalSize then
                    begin
                      DUT.SopData.DATA.IsComplete := True;
                    end;

                  end
                  else
                  begin
                    // Handle chuck requests
                    //DUT.SopData.DATA.TotalSize := 2; // Always 2 bytes ??!!
                  end;

                end;
              end;

              if PDControlMessage<>USBPD_CONTROLMSG_RESERVED0 then
              begin
                SetLength({%H-}HexResult, 2*2);
                BinToHex(@DUT.SopData.HEADER.Bytes, PAnsiChar(HexResult), 2);
                enumname:='0x'+HexResult;
                ProcessControlMessageGUI(PDControlMessage,DUT.SopData.SOPTYPE,enumname);
              end;

              if ((PDExtendedMessage<>USBPD_EXTMSG_RESERVED0) OR (PDDataMessage<>USBPD_DATAMSG_RESERVED0)) then
              begin
                if DUT.SopData.DATA.IsComplete then
                begin
                  try
                    SetLength({%H-}HexResult, 2*2);
                    BinToHex(@DUT.SopData.HEADER.Bytes, PAnsiChar(HexResult), 2);
                    enumname:='0x'+HexResult;
                    if DUT.SopData.HEADER.Data.Extended=1 then
                    begin
                      SetLength({%H-}HexResult, 2*2);
                      BinToHex(@DUT.SopData.EXTHEADER.Bytes, PAnsiChar(HexResult), 2);
                      enumname:=enumname+' 0x'+HexResult;
                    end;
                    SetLength({%H-}HexResult, DUT.SopData.DATA.TotalSize*2);
                    BinToHex(@DUT.SopData.DATA.Data, PAnsiChar(HexResult), DUT.SopData.DATA.TotalSize);
                    enumname:=Concat(enumname,' 0x',HexResult);

                    if PDDataMessage<>USBPD_DATAMSG_RESERVED0 then
                    begin
                      if DUT.ProcessDataMessage(PDDataMessage,(DUT.SopData.DATA.TotalSize DIV 4),@DUT.SopData.DATA.Data) then
                      begin
                        ProcessDataMessageGUI(PDDataMessage,DUT.SopData.SOPTYPE,enumname);
                      end
                      else
                      begin
                        Str(PDDataMessage,enumname);
                        //MemoUnhandled.Lines.Append('UNHANDLED DATA MESSAGE: '+enumname);
                      end;
                    end;

                    if (PDExtendedMessage<>USBPD_EXTMSG_RESERVED0) then
                    begin
                      if DUT.ProcessExtendedMessage(PDExtendedMessage,DUT.SopData.DATA.TotalSize,@DUT.SopData.DATA.Data) then
                      begin
                        ProcessExtendedMessageGUI(PDExtendedMessage,DUT.SopData.SOPTYPE,enumname);
                      end
                      else
                      if PDExtendedMessage=USBPD_EXTMSG_EXTENDED_CONTROL then
                      begin
                        Str(TUSBPD_ECMTYPE(DUT.SopData.DATA.Data[0]),enumname);
                        //USBDebugLog.Lines.Append('USBPD_EXTMSG_EXTENDED_CONTROL: '+enumname);
                      end
                      else
                      begin
                        Str(PDExtendedMessage,enumname);
                        //MemoUnhandled.Lines.Append('UNHANDLED EXTENDED MESSAGE: '+enumname);
                      end;
                    end;

                  finally
                    DUT.SopData.DATA:=Default(TDataBuffer);
                    DUT.SopData.DATA.IsComplete:=False;
                    DUT.SopData.DATA.NextChunkNum:=0;
                    DUT.SopData.DATA.ReceivedBytes:=0;
                    FillChar(DUT.SopData.DATA.Data,SizeOf(DUT.SopData.DATA.Data),0);
                  end;
                end;

              end;

              // Skip to  next data packet
              //USBDebugLog.Lines.Append('TotalSize: '+InttoStr(DUT.SopData.DATA.TotalSize));
              ChunkPayloadLen := DUT.SopData.HEADER.Data.Number_of_Data_Objects * 4;
              if (ChunkPayloadLen>0) then
              begin
                if DUT.SopData.HEADER.Data.Extended=1 then Dec(ChunkPayloadLen,2);
                Inc(dataindexer,ChunkPayloadLen);
                Dec(datasize,ChunkPayloadLen);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPowerbankMainForm.DisConnect(Sender: TObject);
begin
  PDTimer.Enabled:=False;
  DataTimer.Enabled:=False;

  KM003CSerial.Active:=False;

  if Assigned(OutEndPoint) then OutEndPoint.Destroy;
  OutEndPoint:=nil;

  if Assigned(InEndPoint) then InEndPoint.Destroy;
  InEndPoint:=nil;

  if Assigned(DeviceInterface) then DeviceInterface.Destroy;
  DeviceInterface:=nil;

  if Assigned(KM003CUSB) then KM003CUSB.Destroy;
  KM003CUSB:=nil;

  if Assigned(Context) then Context.Destroy;
  Context:=nil;
end;

procedure TPowerbankMainForm.Connect(Sender: TObject);
var
  settingsdata          : TKM003CSettingsData;
  header                : TKM003CMsgHeader;
  result_code           : LongInt;
  newdata               : packed array[0..4095] of byte;
begin
  DisConnect(Sender);

  if (Sender=btnConnectKC003C) then
  begin
    if (Length(KM003CComport)>0) then
    begin
      KM003CSerial.Active:=False;
      KM003CSerial.Device:=KM003CComport;
      KM003CSerial.BaudRate:=br921600;
      KM003CSerial.FlowControl:=fcNone;
      KM003CSerial.Parity:=pNone;
      KM003CSerial.DataBits:=db8bits;
      KM003CSerial.StopBits:=sbOne;
      KM003CSerial.Active:=True;
    end;

    if (NOT Assigned(Context)) then Context := TLibUsbContext.Create;
    try
      if (NOT Assigned(KM003CUSB)) then
      begin
        KM003CUSB := TLibUsbDevice.Create(Context,DevVID,DevPID);
        if (Assigned(KM003CUSB) AND Assigned(KM003CUSB.Handle)) then
        begin
          KM003CUSB.SetConfiguration(ConfigUSBConfiguration);
          DeviceInterface := TLibUsbInterface.Create(KM003CUSB,KM003CUSB.FindInterface(ConfigUSBInterface,ConfigUSBAltInterface));

          //KM003CUSB.SetConfiguration(1);
          //DeviceInterface := TLibUsbInterface.Create(KM003CUSB,KM003CUSB.FindInterface(0,0));

          OutEndPoint := TLibUsbBulkOutEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_OUT));
          InEndPoint := TLibUsbBulkInEndpoint.Create(DeviceInterface , DeviceInterface.FindEndpoint(EP_IN));

          // Get KM003CUSB info
          header.Raw:=0;
          header.Ctrl.typ:=Ord(TKM003CHeaderCommand.CMD_GET_DATA);
          header.Ctrl.att:=TKM003C_ATT_SETTINGS;
          result_code := OutEndPoint.Send(header.Bytes , 4 ,10);

          // Receive settings data
          FillChar({%H-}newdata,4096,0);
          settingsdata:=Default(TKM003CSettingsData);
          result_code := InEndPoint.Recv(newdata ,4096 , 10);
          if (result_code>sizeof(TKM003CSettingsData)) then result_code:=sizeof(TKM003CSettingsData);
          move(newdata,settingsdata,result_code);

          // Did we receive a valid packet from our command ?
          if ((settingsdata.header.Data.typ=TKM003C_CMD_PUT_DATA) AND (settingsdata.header_ext.Header.att=TKM003C_ATT_SETTINGS)) then
          begin
            result_code:=0;
          end;
        end;
      end;
    except
      USBDebugLog.Lines.Append('Could not reach device');
      KM003CUSB := nil;
    end;
  end;
end;

procedure TPowerbankMainForm.AllStop(Sender: TObject);
begin
  TestTimer.Enabled:=False;
  SystemActive:=False;
end;

procedure TPowerbankMainForm.Measure;
begin
  Voltage:=RealVoltageDisplay.Value;
  Current:=RealCurrentDisplay.Value;
end;

procedure TPowerbankMainForm.SetActive(value:boolean);
begin
  if value<>FSystemActive then
  begin
    FSystemActive:=value;
  end;
end;

procedure TPowerbankMainForm.KM003CStartCommandMode(em:byte);
var
  s:ansistring;
begin
  (*
  KM003CSerial.WriteString(KC003CCommand[TKC003CCOMMAND.PDMCLOSE].Command);
  if ((NOT Assigned(KM003CUSB)) OR (NOT Assigned(KM003CUSB.Handle))) then
  begin
    s:=KM003CSerial.ReadString(250);
  end;
  *)

  KM003CSerial.WriteString(KC003CCommand[TKC003CCOMMAND.PDMOPEN].Command);
  if ((NOT Assigned(KM003CUSB)) OR (NOT Assigned(KM003CUSB.Handle))) then
  begin
    s:=KM003CSerial.ReadString(250);
  end;

  (*
  KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDMSETTYPE].Command,[2,em]));
  if ((NOT Assigned(KM003CUSB)) OR (NOT Assigned(KM003CUSB.Handle))) then
  begin
    s:=KM003CSerial.ReadString(250);
  end;
  *)

  //KM003CSerial.WriteString(Format(KC003CCommand[TKC003CCOMMAND.PDMSETTYPE].Command,[{automatic=}0,{em=}0]));
  KM003CSerial.WriteString(KC003CCommand[TKC003CCOMMAND.ENTRYPD].Command);

  if ((NOT Assigned(KM003CUSB)) OR (NOT Assigned(KM003CUSB.Handle))) then
  begin
    // This command causes a hard reset, so give device some time to restart
    Sleep(10000);
    s:=KM003CSerial.ReadString(250);
  end;
end;


end.

