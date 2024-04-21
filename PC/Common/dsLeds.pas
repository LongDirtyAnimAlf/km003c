{
  Author: Grega Loboda
  E-mail: grega.loboda@email.si
  Web: http://delphistep.cis.si

  ver 1.2

  Last modified: 25.03.2004

  Copyright (c) 2000 Grega Loboda

  -------------------------------------------------------------------
  TdsLed
  TdsSevenSegmentDisplay
  TdsDotMatrixDisplay
}
unit dsLeds;

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StrUtils;

type

  TSegmentShape = (ssRectangle, ssEdge, ssDoubleEdge);

  TdsDisplaySegment = (tsTop, tsUpperLeft, tsUpperRight, tsMiddle, tsLowerLeft, tsLowerRight, tsBottom, tsDecimal);
  TdsDisplaySegments = set of TdsDisplaySegment;

  TPLSLED7Seg = class(TCustomControl)
  private
    { Private declarations }
    FBrightColor: TColor;
    FDimColor: TColor;
    FBackColor: TColor;
    FValue: byte;
    FDecimal: boolean;
    FSpacing: byte;
    FGap: byte;
    FSegWidth: byte;
    FSegShape: TSegmentShape;
    FSegPoints: array[0..7,0..5] of TPoint;
    FHorzSegLength: integer;
    FVertSegLength: integer;
    procedure SetBackColor(const Value: TColor);
    procedure SetBrightColor(const Value: TColor);
    procedure SetDimColor(const Value: TColor);
    procedure SetValue(const Value: byte);
    procedure SetDecimal(const Value: boolean);
    procedure SetSpacing(const Value: byte);
    procedure SetSegWidth(const Value: byte);
    procedure SetSegShape(const Value: TSegmentShape);
    procedure Geometry;
    procedure GeometryRectangle;
    procedure GeometryEdge;
    procedure GeometryDoubleEdge;
    procedure SetGap(const Value: byte);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property BrightColor: TColor read FBrightColor write SetBrightColor;
    property DimColor: TColor read FDimColor write SetDimColor;
    property OnColor: TColor read FBrightColor write SetBrightColor;
    property OffColor: TColor read FDimColor write SetDimColor;
    property BackColor: TColor read FBackColor write SetBackColor;
    property Value: byte read FValue write SetValue;
    property Spacing: byte read FSpacing write SetSpacing;
    property Gap: byte read FGap write SetGap;
    property SegWidth: byte read FSegWidth write SetSegWidth;
    property SegmentWidth: byte read FSegWidth write SetSegWidth;
    property SegShape: TSegmentShape read FSegShape write SetSegShape;
    property Decimal: boolean read FDecimal write SetDecimal;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Cursor;
    property Enabled;
    property Visible;
  end;


  TdsSevenSegmentDisplay = class(TCustomControl)
  private
    FSegments: TdsDisplaySegments;
    FSegWidth: Integer;
    FBackColor: TColor;
    FOnColor: TColor;
    FOffColor: TColor;
    FValue: Integer;
    FDecimal:boolean;

    FOnChange: TNotifyEvent;

    FOnDouble: TNotifyEvent;

    procedure DrawSegment(s: Byte);

    procedure SetSegments(Value: TdsDisplaySegments);
    procedure SetSegWidth(Value: Integer);
    procedure SetBackColor(Value: TColor);
    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);
    procedure SetValue(Value: Integer);
    procedure SetDecimal(Value: boolean);

  protected
    procedure Paint; override;
    procedure DoChange; virtual;
    procedure DblClick; virtual;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property Segments: TdsDisplaySegments read FSegments write SetSegments;
    property SegmentWidth: Integer read FSegWidth write SetSegWidth;
    property BackColor: TColor read FBackColor write SetBackColor;
    property OnColor: TColor read FOnColor write SetOnColor;
    property OffColor: TColor read FOffColor write SetOffColor;
    property Value: Integer read FValue write SetValue;
    property Decimal: boolean read FDecimal write SetDecimal;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Visible;
    property OnClick;
    //property OnDblClick;//: TNotifyEvent read FOnDouble write FOnDouble;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TdsSevenSegmentMultiDisplay = class(TPanel)
  private
    //FDisplays: array of TdsSevenSegmentDisplay;
    FDisplays: array of TPLSLED7Seg;

    FDisplayCount:integer;
    FSignDigit:boolean;
    FValue:double;

    FTemp:double;

    FOnColor: TColor;
    FOffColor: TColor;

    FBevel:integer;

    FOnDouble: TNotifyEvent;

    procedure SetDisplayCount(value:integer);
    procedure SetValue(value:double);
    procedure SetTemp(value:double);

    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);

    procedure SetBevel(Value: integer);

    procedure DblClick(Sender:TObject);

    {
    FSegWidth: Integer;
    FBackColor: TColor;
    FOnColor: TColor;
    FOffColor: TColor;
    FValue: Integer;

    FOnChange: TNotifyEvent;

    procedure DrawDisplay(s: Byte);

    procedure SetDisplay(index,value:integer);
    procedure SetSegWidth(Value: Integer);
    procedure SetBackColor(Value: TColor);
    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);
    procedure SetValue(Value: Integer);
    }

  protected
    procedure Paint; override;
    //procedure DoChange; virtual;

  public
    constructor Create(AOwner: TComponent);override;

  published
    //property SegmentWidth: Integer read FSegWidth write SetSegWidth;
    //property BackColor: TColor read FBackColor write SetBackColor;
    //property OnColor: TColor read FOnColor write SetOnColor;
    //property OffColor: TColor read FOffColor write SetOffColor;
    //property Value: Integer read FValue write SetValue;

    property DisplayCount:integer read FDisplayCount write SetDisplayCount;
    property SignDigit:boolean read FSignDigit write FSignDigit;
    property Value:double read FValue write SetValue;
    property Temp:double read FTemp write SetTemp;
    property OnColor: TColor read FOnColor write SetOnColor;
    property OffColor: TColor read FOffColor write SetOffColor;

    property BorderWidth: integer read FBevel write SetBevel;


    //property OnChange: TNotifyEvent read FOnChange write FOnChange;

    //property Visible;
    //property OnClick;
    //property OnDblClick: TNotifyEvent read FOnDouble write FOnDouble;
    //property OnMouseDown;
    //property OnMouseMove;
    //property OnMouseUp;

  end;


  TdsDisplayDots = String[35];

  TdsDotMatrixDisplay = class(TGraphicControl)
  private
    FDots: TdsDisplayDots;
    FBackColor: TColor;
    FOnColor: TColor;
    FOffColor: TColor;

    FOnChange: TNotifyEvent;

    procedure SeTdsDisplayDots(Value: TdsDisplayDots);
    procedure SetBackColor(Value: TColor);
    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);

  protected
    procedure Paint; override;
    procedure DoChange; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ExtSetDots(const Value: array of Byte);

  published
    property Dots: TdsDisplayDots read FDots write SeTdsDisplayDots;
    property BackColor: TColor read FBackColor write SetBackColor;
    property OnColor: TColor read FOnColor write SetOnColor;
    property OffColor: TColor read FOffColor write SetOffColor;
  end;


  const
  KValue2Segment: array[0..17, 0..6] of boolean = (
    (true,  true,  true,  true,  true,  true,  false),
    (false, true,  true,  false, false, false, false),
    (true,  true,  false, true,  true,  false, true ),
    (true,  true,  true,  true,  false, false, true ),
    (false, true,  true,  false, false, true,  true ),
    (true,  false, true,  true,  false, true,  true ),
    (true,  false, true,  true,  true,  true,  true ),
    (true,  true,  true,  false, false, false, false),
    (true,  true,  true,  true,  true,  true,  true ),
    (true,  true,  true,  true,  false,  true,  true ),
    (true,  true,  true,  false,  true,  true,  true ),
    (true,  true,  true,  true,  true,  true,  true ),
    (true,  false,  false,  true,  true,  true,  false ),
    (true,  true,  true,  true,  true,  true,  false ),
    (true,  false,  false,  true,  true,  true,  true ),
    (true,  false,  false,  false,  true,  true,  true ),
    (false,  false,  false,  false,  false, false,  true ),
    (false,  false,  false,  false,  false, false,  false )
    );

implementation

constructor TPLSLED7Seg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  // Default Value for all properties
  ControlStyle := ControlStyle + [csOpaque];
  FValue := 0;
  FDecimal := false;
  FSegWidth := 30;
  FSegShape := ssDoubleEdge;
  FSpacing := 8;
  FGap := 4;
  Width := 100;
  Height := 140;
  FBrightColor := clRed;
  FDimColor := clMaroon;
  FDimColor := clBlack;
  FBackColor := clBlack;
end;

destructor TPLSLED7Seg.Destroy;
begin
  inherited Destroy;
end;

procedure TPLSLED7Seg.Geometry;
begin
  case FSegShape of
    ssRectangle: GeometryRectangle;
    ssEdge: GeometryEdge;
    ssDoubleEdge: GeometryDoubleEdge;
  end;
end;

procedure TPLSLED7Seg.GeometryDoubleEdge;
var
  swb2, segLength, orgX, orgY: integer;
begin
  if (Width = 0) or (Height = 0) then Exit;
  // Horizontal Segment Length
  // Maintain sufficient segment length
  // increase width if necessary
  // room for space on left and right and
  // room for vertical segments on the left and right
  segLength := Width - FSpacing - FSpacing - FGap - FGap - FSegWidth;
  {
  if segLength < 5 + FSegWidth + FSegWidth then
  begin
    Width := Width + (5 + FSegWidth + FSegWidth - segLength);
    Exit;
  end;
  }
  FHorzSegLength := segLength;

  // Vertical Segment Length
  // Maintain sufficient segment length
  // increase height if necessary
  // room for space on top and bottom and
  // room for horizontal segments on the top, middle and bottom
  segLength := Height - FSpacing - FSpacing - FGap - FGap - FGap - FGap - FSegWidth;
  segLength := segLength div 2;
  {
  if segLength < 5 + FSegWidth + FSegWidth then
  begin
    Height := Height + (2 * (5 + FSegWidth + FSegWidth - segLength));
    Exit;
  end;
  }
  FVertSegLength := segLength;

  swb2 := FSegWidth div 2;
  // Points for segment a
  orgX := FSpacing + FGap + swb2;
  orgY := FSpacing + swb2;
  FSegPoints[0, 0] := Point(orgX, orgY);
  FSegPoints[0, 1] := Point(orgX + swb2, orgY + swb2);
  FSegPoints[0, 2] := Point(orgX + FHorzSegLength - swb2, orgY + swb2);
  FSegPoints[0, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[0, 4] := Point(orgX + FHorzSegLength - swb2, orgY - swb2);
  FSegPoints[0, 5] := Point(orgX + swb2, orgY - swb2);

  // Points for segment b
  orgX := FSpacing + FGap + swb2 + FHorzSegLength + FGap;
  orgY := FSpacing + FGap + swb2;
  FSegPoints[1, 0] := Point(orgX, orgY);
  FSegPoints[1, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[1, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[1, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[1, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[1, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment c
  orgX := FSpacing + FGap + swb2 + FHorzSegLength + FGap;
  orgY := FSpacing + FGap + swb2 + FGap + FGap + FVertSegLength + FGap;
  FSegPoints[2, 0] := Point(orgX, orgY);
  FSegPoints[2, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[2, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[2, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[2, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[2, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment d
  orgX := FSpacing + FGap + swb2;
  orgY := FSpacing + swb2 + FGap + FVertSegLength + FGap + FGap +
    FVertSegLength + FGap + 2;
  FSegPoints[3, 0] := Point(orgX, orgY);
  FSegPoints[3, 1] := Point(orgX + swb2, orgY + swb2);
  FSegPoints[3, 2] := Point(orgX + FHorzSegLength - swb2, orgY + swb2);
  FSegPoints[3, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[3, 4] := Point(orgX + FHorzSegLength - swb2, orgY - swb2);
  FSegPoints[3, 5] := Point(orgX + swb2, orgY - swb2);

  // Points for segment e
  orgX := FSpacing + swb2;
  orgY := FSpacing + FGap + swb2 + FGap + FGap + FVertSegLength + FGap;
  FSegPoints[4, 0] := Point(orgX, orgY);
  FSegPoints[4, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[4, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[4, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[4, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[4, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment f
  orgX := FSpacing + swb2;
  orgY := FSpacing + FGap + swb2;
  FSegPoints[5, 0] := Point(orgX, orgY);
  FSegPoints[5, 1] := Point(orgX - swb2, orgY + swb2);
  FSegPoints[5, 2] := Point(orgX - swb2, orgY + FVertSegLength - swb2);
  FSegPoints[5, 3] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[5, 4] := Point(orgX + swb2, orgY + FVertSegLength - swb2);
  FSegPoints[5, 5] := Point(orgX + swb2, orgY + swb2);

  // Points for segment g
  orgX := FSpacing + FGap + swb2;
  orgY := FSpacing + swb2 + FGap + FVertSegLength + FGap;
  FSegPoints[6, 0] := Point(orgX, orgY);
  FSegPoints[6, 1] := Point(orgX + swb2, orgY + swb2);
  FSegPoints[6, 2] := Point(orgX + FHorzSegLength - swb2, orgY + swb2);
  FSegPoints[6, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[6, 4] := Point(orgX + FHorzSegLength - swb2, orgY - swb2);
  FSegPoints[6, 5] := Point(orgX + swb2, orgY - swb2);
end;

procedure TPLSLED7Seg.GeometryEdge;
var
  segLength, orgX, orgY: integer;
begin
  if (Width = 0) or (Height = 0) then Exit;
  // Horizontal Segment Length
  // Maintain sufficient segment length
  // increase width if necessary
  // room for space on left and right and
  // room for vertical segments on the left and right
  segLength := Width - FSpacing - FSpacing - FGap - FGap;
  if segLength < 5 + FSegWidth + FSegWidth then
  begin
    Width := Width + (5 + FSegWidth + FSegWidth - segLength);
    Exit;
  end;
  FHorzSegLength := segLength;

  // Vertical Segment Length
  // Maintain sufficient segment length
  // increase height if necessary
  // room for space on top and bottom and
  // room for horizontal segments on the top, middle and bottom
  segLength := Height - FSpacing - FSpacing - FGap - FGap - FGap - FGap;
  segLength := segLength div 2;
  if segLength < 5 then
  begin
    Height := Height + (2 * (5 - segLength));
    Exit;
  end;
  FVertSegLength := segLength;

  // Points for segment a
  orgX := FSpacing + FGap;
  orgY := FSpacing;
  FSegPoints[0, 0] := Point(orgX, orgY);
  FSegPoints[0, 1] := Point(orgX + FSegWidth, orgY + FSegWidth);
  FSegPoints[0, 2] := Point(orgX + FHorzSegLength - FSegWidth, orgY + FSegWidth);
  FSegPoints[0, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[0, 4] := FSegPoints[0, 3];
  FSegPoints[0, 5] := FSegPoints[0, 3];

  // Points for segment b
  orgX := FSpacing + FGap + FHorzSegLength + FGap - FSegWidth;
  orgY := FSpacing + FGap + FSegWidth;
  FSegPoints[1, 0] := Point(orgX, orgY);
  FSegPoints[1, 1] := Point(orgX, orgY + FVertSegLength - FSegWidth - FSegWidth);
  FSegPoints[1, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[1, 3] := Point(orgX + FSegWidth, orgY - FSegWidth);
  FSegPoints[1, 4] := FSegPoints[1, 3];
  FSegPoints[1, 5] := FSegPoints[1, 3];

  // Points for segment c
  orgX := FSpacing + FGap + FHorzSegLength + FGap - FSegWidth;
  orgY := FSpacing + FGap + FSegWidth + FVertSegLength + FGap + FGap;
  FSegPoints[2, 0] := Point(orgX, orgY);
  FSegPoints[2, 1] := Point(orgX, orgY + FVertSegLength - FSegWidth - FSegWidth);
  FSegPoints[2, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[2, 3] := Point(orgX + FSegWidth, orgY - FSegWidth);
  FSegPoints[2, 4] := FSegPoints[2, 3];
  FSegPoints[2, 5] := FSegPoints[2, 3];

  // Points for segment d
  orgX := FSpacing + FGap + FSegWidth;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap + FVertSegLength +
    FGap - FSegWidth + 2;
  FSegPoints[3, 0] := Point(orgX, orgY);
  FSegPoints[3, 1] := Point(orgX - FSegWidth, orgY + FSegWidth);
  FSegPoints[3, 2] := Point(orgX + FHorzSegLength - FSegWidth, orgY + FSegWidth);
  FSegPoints[3, 3] := Point(orgX + FHorzSegLength - FSegWidth - FSegWidth, orgY);
  FSegPoints[3, 4] := FSegPoints[3, 3];
  FSegPoints[3, 5] := FSegPoints[3, 3];

  // Points for segment e
  orgX := FSpacing;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap;
  FSegPoints[4, 0] := Point(orgX, orgY);
  FSegPoints[4, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[4, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[4, 3] := Point(orgX + FSegWidth, orgY + FSegWidth);
  FSegPoints[4, 4] := FSegPoints[4, 3];
  FSegPoints[4, 5] := FSegPoints[4, 3];

  // Points for segment f
  orgX := FSpacing;
  orgY := FSpacing + FGap;
  FSegPoints[5, 0] := Point(orgX, orgY);
  FSegPoints[5, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[5, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength - FSegWidth);
  FSegPoints[5, 3] := Point(orgX + FSegWidth, orgY + FSegWidth);
  FSegPoints[5, 4] := FSegPoints[5, 3];
  FSegPoints[5, 5] := FSegPoints[5, 3];

  // Points for segment g
  orgX := FSpacing + FGap;
  orgY := FSpacing + FGap + FVertSegLength + FGap;
  FSegPoints[6, 0] := Point(orgX, orgY);
  FSegPoints[6, 1] := Point(orgX + FSegWidth, orgY + (FSegWidth div 2));
  FSegPoints[6, 2] := Point(orgX + FHorzSegLength - FSegWidth, orgY + (FSegWidth div 2));
  FSegPoints[6, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[6, 4] := Point(orgX + FHorzSegLength - FSegWidth, orgY - (FSegWidth div 2));
  FSegPoints[6, 5] := Point(orgX + FSegWidth, orgY - (FSegWidth div 2));
end;

procedure TPLSLED7Seg.GeometryRectangle;
var
  segLength, orgX, orgY: integer;
begin
  if (Width = 0) or (Height = 0) then Exit;
  // Horizontal Segment Length
  // Maintain sufficient segment length
  // increase width if necessary
  // room for space on left and right and
  // room for vertical segments on the left and right
  segLength := Width - FSpacing - FSpacing - FSegWidth - FSegWidth - FGap - FGap;
  if segLength < 5 then
  begin
    Width := Width + (5 - segLength);
    Exit;
  end;
  FHorzSegLength := segLength;

  // Vertical Segment Length
  // Maintain sufficient segment length
  // increase height if necessary
  // room for space on top and bottom and
  // room for horizontal segments on the top, middle and bottom
  segLength := Height - FSpacing - FSpacing - FGap - FGap - FGap - FGap;
  segLength := segLength div 2;
  if segLength < 5 then
  begin
    Height := Height + (2 * (5 - segLength));
    Exit;
  end;
  FVertSegLength := segLength;

  // Points for segment a
  orgX := FSpacing + FSegWidth + FGap;
  orgY := FSpacing;
  FSegPoints[0, 0] := Point(orgX, orgY);
  FSegPoints[0, 1] := Point(orgX, orgY + FSegWidth);
  FSegPoints[0, 2] := Point(orgX + FHorzSegLength, orgY + FSegWidth);
  FSegPoints[0, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[0, 4] := FSegPoints[0, 3];
  FSegPoints[0, 5] := FSegPoints[0, 3];

  // Points for segment b
  orgX := FSpacing + FSegWidth + FGap + FGap + FHorzSegLength;
  orgY := FSpacing + FGap;
  FSegPoints[1, 0] := Point(orgX, orgY);
  FSegPoints[1, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[1, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[1, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[1, 4] := FSegPoints[1, 3];
  FSegPoints[1, 5] := FSegPoints[1, 3];

  // Points for segment c
  orgX := FSpacing + FSegWidth + FGap + FGap + FHorzSegLength;
  orgY := FSpacing + FGap + FGap + FGap + FVertSegLength;
  FSegPoints[2, 0] := Point(orgX, orgY);
  FSegPoints[2, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[2, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[2, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[2, 4] := FSegPoints[2, 3];
  FSegPoints[2, 5] := FSegPoints[2, 3];

  // Points for segment d
  orgX := FSpacing + FSegWidth + FGap;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap + FVertSegLength +
    FGap - FSegWidth + 2;
  FSegPoints[3, 0] := Point(orgX, orgY);
  FSegPoints[3, 1] := Point(orgX, orgY + FSegWidth);
  FSegPoints[3, 2] := Point(orgX + FHorzSegLength, orgY + FSegWidth);
  FSegPoints[3, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[3, 4] := FSegPoints[3, 3];
  FSegPoints[3, 5] := FSegPoints[3, 3];

  // Points for segment e
  orgX := FSpacing;
  orgY := FSpacing + FGap + FVertSegLength + FGap + FGap;
  FSegPoints[4, 0] := Point(orgX, orgY);
  FSegPoints[4, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[4, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[4, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[4, 4] := FSegPoints[4, 3];
  FSegPoints[4, 5] := FSegPoints[4, 3];

  // Points for segment f
  orgX := FSpacing;
  orgY := FSpacing + FGap;
  FSegPoints[5, 0] := Point(orgX, orgY);
  FSegPoints[5, 1] := Point(orgX, orgY + FVertSegLength);
  FSegPoints[5, 2] := Point(orgX + FSegWidth, orgY + FVertSegLength);
  FSegPoints[5, 3] := Point(orgX + FSegWidth, orgY);
  FSegPoints[5, 4] := FSegPoints[5, 3];
  FSegPoints[5, 5] := FSegPoints[5, 3];

  // Points for segment g
  orgX := FSpacing + FSegWidth + FGap;
  orgY := FSpacing + FGap + FVertSegLength + FGap - (FSegWidth div 2);
  FSegPoints[6, 0] := Point(orgX, orgY);
  FSegPoints[6, 1] := Point(orgX, orgY + FSegWidth);
  FSegPoints[6, 2] := Point(orgX + FHorzSegLength, orgY + FSegWidth);
  FSegPoints[6, 3] := Point(orgX + FHorzSegLength, orgY);
  FSegPoints[6, 4] := FSegPoints[6, 3];
  FSegPoints[6, 5] := FSegPoints[6, 3];
end;

procedure TPLSLED7Seg.Paint;
var
  i: integer;
  localFValue:byte;
begin
  inherited;
  with Self.Canvas do
  begin
    // Fill the background
    Brush.Color := FBackColor;
    Pen.Color := FBackColor;
    Rectangle(0, 0, Width, Height);
    //FValue:=Ord('%');
    if FValue>31 then
    begin
      {$ifndef FPC}
      SetTextAlign( Handle, TA_CENTER );
      {$endif}
      i:=Height;
      Font.Height:=i;
      while TextWidth('%')>Width do
      begin
        Dec(i);
        Font.Height:=i;
      end;

      Font.Color:=FBrightColor;
      //Brush.Color := FBrightColor;
      TextOut(Width DIV 2,0,'%');
      {$ifndef FPC}
      SetTextAlign( Handle, TA_LEFT );
      {$endif}
      exit;
    end;

    // Paint the segments
    // Lookup the table and paint bright if segment is
    // supposed to be on
    for i := 0 to 6 do
    begin
      if Enabled then
      begin
        localFValue:=FValue;
        if FValue>17 then localFValue:=17;
        if KValue2Segment[localFValue, i] then
          Brush.Color := FBrightColor
        else
          Brush.Color := FDimColor;
      end
      else
        Brush.Color := FDimColor;

      Polygon(FSegPoints[i]);
    end;

    if FDecimal then
    begin
      Brush.Color := FBrightColor;
      Ellipse(Self.Width-FSegWidth,Self.Height-FSegWidth, (Self.Width), (Self.Height));
    end;

  end;
end;

procedure TPLSLED7Seg.Resize;
var
  temp:word;
begin
  inherited;
  //if Height>255 then Height:=255;

  temp := Height DIV 6;

  if Width DIV 6 < temp then temp:=Width DIV 6;

  //if temp<4 then temp:=4;

  FSegWidth:=temp;

  FGap := FSegWidth DIV 5;

  FSpacing := FSegWidth DIV 2;

  Geometry;
  Invalidate;
end;

procedure TPLSLED7Seg.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetBrightColor(const Value: TColor);
begin
  if FBrightColor <> Value then
  begin
    FBrightColor := Value;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetDimColor(const Value: TColor);
begin
  if FDimColor <> Value then
  begin
    FDimColor := Value;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetGap(const Value: byte);
begin
  if FGap <> Value then
  begin
    FGap := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetSegShape(const Value: TSegmentShape);
begin
  if FSegShape <> Value then
  begin
    FSegShape := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetSegWidth(const Value: byte);
begin
  if FSegWidth <> Value then
  begin
    FSegWidth := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetSpacing(const Value: byte);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Geometry;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetValue(const Value: byte);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    Invalidate;
  end;
end;

procedure TPLSLED7Seg.SetDecimal(const Value: boolean);
begin
  if Value <> FDecimal then
  begin
    FDecimal := Value;
    Invalidate;
  end;
end;



constructor TdsSevenSegmentDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 40;
  Height:= 76;

  FSegWidth := 4;

  FBackColor := clGray;
  FOnColor := clBlack;
  FOffColor := clSilver;

  FDecimal:=false;
  FValue:=0;
  Value :=-1;

end;

procedure TdsSevenSegmentDisplay.DblClick;
begin
  halt;
  if Assigned(fOnDouble) then fOnDouble(Self);
end;


procedure TdsSevenSegmentDisplay.SetSegments(Value: TdsDisplaySegments);
begin
  if Value <> FSegments then
  begin
    FSegments := Value;
    Refresh;
    DoChange;
  end;
end;

procedure TdsSevenSegmentDisplay.SetSegWidth(Value: Integer);
begin
  if Value <> FSegWidth then
  begin
    FSegWidth := Value;
    //Refresh;
  end;
end;

procedure TdsSevenSegmentDisplay.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Refresh;
  end;
end;

procedure TdsSevenSegmentDisplay.SetOnColor(Value: TColor);
begin
  if Value <> FOnColor then
  begin
    FOnColor := Value;
    Refresh;
  end;
end;

procedure TdsSevenSegmentDisplay.SetOffColor(Value: TColor);
begin
  if Value <> FOffColor then
  begin
    FOffColor := Value;
    Refresh;
  end;
end;

procedure TdsSevenSegmentDisplay.SetValue(Value: Integer);
begin
  if Value <> FValue then
  begin
    FValue := Value;

    case Value of
      0: Segments := [tsTop, tsUpperLeft, tsUpperRight, tsLowerLeft, tsLowerRight, tsBottom];
      1: Segments := [tsUpperRight, tsLowerRight];
      2: Segments := [tsTop, tsUpperRight, tsMiddle, tsLowerLeft, tsBottom];
      3: Segments := [tsTop, tsUpperRight, tsMiddle, tsLowerRight, tsBottom];
      4: Segments := [tsUpperLeft, tsUpperRight, tsMiddle, tsLowerRight];
      5: Segments := [tsTop, tsUpperLeft, tsMiddle, tsLowerRight, tsBottom];
      6: Segments := [tsTop, tsUpperLeft, tsMiddle, tsLowerLeft, tsLowerRight, tsBottom];
      7: Segments := [tsTop, tsUpperRight, tsLowerRight];
      8: Segments := [tsTop, tsUpperLeft, tsUpperRight, tsMiddle, tsLowerLeft, tsLowerRight, tsBottom];
      9: Segments := [tsTop, tsUpperLeft, tsUpperRight, tsMiddle, tsLowerRight, tsBottom];
 {A} 10: Segments := [tsTop, tsUpperLeft, tsUpperRight, tsMiddle, tsLowerRight, tsLowerLeft];
 {B} 11: Segments := [tsTop, tsUpperLeft, tsUpperRight, tsMiddle, tsLowerLeft, tsLowerRight, tsBottom];
 {C} 12: Segments := [tsTop, tsUpperLeft, tsLowerLeft, tsBottom];
 {D} 13: Segments := [tsTop, tsUpperLeft, tsUpperRight, tsLowerLeft, tsLowerRight, tsBottom];
 {E} 14: Segments := [tsTop, tsUpperLeft, tsMiddle, tsLowerLeft, tsBottom];
 {F} 15: Segments := [tsTop, tsUpperLeft, tsMiddle, tsLowerLeft];
 {-} 16: Segments := [tsMiddle];
 { } 17: Segments := [];
      else Segments := [tsMiddle];
    end;
    if Decimal then Segments:=Segments+[tsDecimal];
  end;
end;

procedure TdsSevenSegmentDisplay.SetDecimal(Value: boolean);
begin
  if Value <> FDecimal then
  begin
    FDecimal := Value;
    if Value
       then Segments:=Segments+[tsDecimal]
       else Segments:=Segments-[tsDecimal];

    //if (Value) AND (NOT ([tsDecimal] in Segments)) then Segments:=Segments+[tsDecimal];
    //if (NOT Value) AND ([tsDecimal] in Segments) then Segments:=Segments-[tsDecimal];

  end;
end;



procedure TdsSevenSegmentDisplay.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;
{
procedure TdsSevenSegmentDisplay.DrawSegment(s: Byte);
var
  W, H: Double;
begin
  W := (Width-4) / 20;
  H := Height / 38;

  case s of
    0://top
      Canvas.Polygon([Point(round(w*2), round(h*1)),
                      Point(round(w*18), round(h*1)),
                      Point(round(w*(18-FSegWidth)), round(h*(1+FSegWidth))),
                      Point(round(w*(2+FSegWidth)), round(h*(1+FSegWidth)))]);
    1://upper left
      Canvas.Polygon([Point(round(w*1), round(h*2)),
                      Point(round(w*1), round(h*18)),
                      Point(round(w*(1+FSegWidth)), round(h*(18-FSegWidth))),
                      Point(round(w*(1+FSegWidth)), round(h*(2+FSegWidth)))]);
    2://upper right
      Canvas.Polygon([Point(round(w*19), round(h*2)),
                      Point(round(w*19), round(h*18)),
                      Point(round(w*(19-FSegWidth)), round(h*(18-FSegWidth))),
                      Point(round(w*(19-FSegWidth)), round(h*(2+FSegWidth)))]);

    4://lower left
      Canvas.Polygon([Point(round(w*1), round(h*20)),
                      Point(round(w*1), round(h*36)),
                      Point(round(w*(1+FSegWidth)), round(h*(36-FSegWidth))),
                      Point(round(w*(1+FSegWidth)), round(h*(20+FSegWidth)))]);

    5://lower right
      Canvas.Polygon([Point(round(w*19), round(h*20)),
                      Point(round(w*19), round(h*36)),
                      Point(round(w*(19-FSegWidth)), round(h*(36-FSegWidth))),
                      Point(round(w*(19-FSegWidth)), round(h*(20+FSegWidth)))]);
    6://bottom
      Canvas.Polygon([Point(round(w*2), round(h*37)),
                      Point(round(w*18), round(h*37)),
                      Point(round(w*(18-FSegWidth)), round(h*(37-FSegWidth))),
                      Point(round(w*(2+FSegWidth)), round(h*(37-FSegWidth)))]);

    3://middle
      Canvas.Polygon([Point(round(w*2), round(h*19)),
                      Point(round(w*(2+FSegWidth/2)), round(h*(19-FSegWidth/2))),
                      Point(round(w*(18-FSegWidth/2)), round(h*(19-FSegWidth/2))),
                      Point(round(w*18), round(h*19)),
                      Point(round(w*(18-FSegWidth/2)), round(h*(19+FSegWidth/2))),
                      Point(round(w*(2+FSegWidth/2)), round(h*(19+FSegWidth/2)))]);
    7:
    begin
      MoveWindowOrg(Canvas.Handle, round(w*(19))+5, round(h*(37))+2);
      Canvas.Ellipse(-FSegWidth,-FSegWidth,FSegWidth,FSegWidth);
    end;

  end;
end;
}
procedure TdsSevenSegmentDisplay.DrawSegment(s: Byte);
var
  W, H: Double;
begin
  W := (Width-4) / 20;
  H := Height / 38;

  case s of
    0://top
      Canvas.Polygon([Point(round(w*2), round(h*1)),
                      Point(round(w*18), round(h*1)),
                      Point(round(w*(18-FSegWidth)), round(h*(1+FSegWidth))),
                      Point(round(w*(2+FSegWidth)), round(h*(1+FSegWidth)))]);
    1://upper left
      Canvas.Polygon([Point(round(w*1), round(h*2)),
                      Point(round(w*1), round(h*18)),
                      Point(round(w*(1+FSegWidth)), round(h*(18-FSegWidth))),
                      Point(round(w*(1+FSegWidth)), round(h*(2+FSegWidth)))]);
    2://upper right
      Canvas.Polygon([Point(round(w*19), round(h*2)),
                      Point(round(w*19), round(h*18)),
                      Point(round(w*(19-FSegWidth)), round(h*(18-FSegWidth))),
                      Point(round(w*(19-FSegWidth)), round(h*(2+FSegWidth)))]);

    4://lower left
      Canvas.Polygon([Point(round(w*1), round(h*20)),
                      Point(round(w*1), round(h*36)),
                      Point(round(w*(1+FSegWidth)), round(h*(36-FSegWidth))),
                      Point(round(w*(1+FSegWidth)), round(h*(20+FSegWidth)))]);

    5://lower right
      Canvas.Polygon([Point(round(w*19), round(h*20)),
                      Point(round(w*19), round(h*36)),
                      Point(round(w*(19-FSegWidth)), round(h*(36-FSegWidth))),
                      Point(round(w*(19-FSegWidth)), round(h*(20+FSegWidth)))]);
    6://bottom
      Canvas.Polygon([Point(round(w*2), round(h*37)),
                      Point(round(w*18), round(h*37)),
                      Point(round(w*(18-FSegWidth)), round(h*(37-FSegWidth))),
                      Point(round(w*(2+FSegWidth)), round(h*(37-FSegWidth)))]);

    3://middle
      Canvas.Polygon([Point(round(w*2), round(h*19)),
                      Point(round(w*(2+FSegWidth/2)), round(h*(19-FSegWidth/2))),
                      Point(round(w*(18-FSegWidth/2)), round(h*(19-FSegWidth/2))),
                      Point(round(w*18), round(h*19)),
                      Point(round(w*(18-FSegWidth/2)), round(h*(19+FSegWidth/2))),
                      Point(round(w*(2+FSegWidth/2)), round(h*(19+FSegWidth/2)))]);
    7:
    begin
      MoveWindowOrg(Canvas.Handle, round(w*(19))+2, round(h*(37))+2);
      Canvas.Ellipse(-FSegWidth,-FSegWidth,FSegWidth,FSegWidth);
    end;

  end;
end;


procedure TdsSevenSegmentDisplay.Paint;
var
  i: Integer;
begin
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := FBackColor;
  Canvas.Rectangle(0, 0, Width, Height);

  for i := 0 to 7 do
  begin
    if TdsDisplaySegment(i) in FSegments
       then Canvas.Brush.Color := FOnColor
       else Canvas.Brush.Color := FOffColor;
    if i<7 then DrawSegment(i) else if Canvas.Brush.Color = FOnColor then DrawSegment(i);
  end;
end;

constructor TdsSevenSegmentMultiDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvLowered;
  FBevel:=5;
  BevelWidth:=FBevel;
  Color:=clBlack;
  Width:=400;
  Height:=300;
  FOnColor:=clRed;
  FOffColor:=RGB(50,0,0);
  FSignDigit:=false;
  DisplayCount:=4;
end;

procedure TdsSevenSegmentMultiDisplay.SetDisplayCount(value:integer);
var
  x:integer;
begin
  if (value>0) AND (value<>FDisplayCount) then
  begin
    FDisplayCount:=value;
    for x:=Low(FDisplays) to High(FDisplays) do FDisplays[x].Destroy;
    Setlength(FDisplays,DisplayCount);
    for x:=0 to (DisplayCount-1) do
    begin
      //FDisplays[x]:=TdsSevenSegmentDisplay.Create(Self);
      FDisplays[x]:=TPLSLED7Seg.Create(Self);
      FDisplays[x].Parent:=Self;
      {$ifndef FPC}
      FDisplays[x].ParentDoubleBuffered:=True;
      {$endif}
      //FDisplays[x].OnDblClick:=Self.OnDblClick;
      FDisplays[x].OnDblClick:=@DblClick;
      //FDisplays[x].Value:=0;
    end;
  end;
end;

procedure TdsSevenSegmentMultiDisplay.SetValue(value:double);
var
  temp2:string;
  temp1,x:integer;
  numberchar:char;
begin
  FValue:=value;
  temp2:=FloattoStrF(value,fffixed,12,7);
  temp1:=1;
  x:=0;
  repeat
    numberchar:=temp2[x+temp1];
    if x=0 then
    begin
      if (numberchar='-') OR (FSignDigit) then
      begin
        if (numberchar='-') then
          FDisplays[x].Value:=16
        else
        begin
          FDisplays[x].Value:=17;
          Inc(x);
          Dec(temp1);
        end;
      end;
    end;
    if CharInSet(numberchar,['0'..'9']) then FDisplays[x].Value:=StrtoInt(numberchar);
    FDisplays[x].Decimal:=False;
    if numberchar=FormatSettings.DecimalSeparator then
    begin
      FDisplays[x-1].Decimal:=True;
      Inc(temp1);
    end else Inc(x);
  until x>High(FDisplays);
end;

procedure TdsSevenSegmentMultiDisplay.SetTemp(value:double);
var
  temp2:string;
  temp1,x:integer;
  numberchar:char;
begin
  FValue:=value;
  temp2:=FloattoStrF(value,fffixed,12,5);
  temp1:=0;
  x:=0;
  repeat
    numberchar:=temp2[x+1+temp1];
    if CharInSet(numberchar,['0'..'9']) then FDisplays[x].Value:=StrtoInt(numberchar);
    if numberchar='-' then FDisplays[x].Value:=20;
    FDisplays[x].Decimal:=False;
    if numberchar=FormatSettings.DecimalSeparator then
    begin
      FDisplays[x-1].Decimal:=True;
      temp1:=1;
    end else Inc(x);
  until x>(High(FDisplays)-1);
  FDisplays[x].Value:=12;
end;


procedure TdsSevenSegmentMultiDisplay.Paint;
var
  x:integer;
begin
  inherited;
  for x:=0 to (DisplayCount-1) do
  begin
    FDisplays[x].OnColor:=FOnColor;
    //FDisplays[x].OffColor:=clMaroon;
    //FDisplays[x].OffColor:=RGB(GetRValue(Self.Color),GetGValue(Self.Color),GetBValue(Self.Color));
    //FDisplays[x].OffColor:=clSilver;
    //FDisplays[x].OffColor:=Self.Color;
    FDisplays[x].OffColor:=FOffColor;
    //FDisplays[x].SegmentWidth:=5;
    //FDisplays[x].BackColor:=FDisplays[x].OffColor;
    FDisplays[x].Top:=10;
    FDisplays[x].Height:=Self.Height-20;
    FDisplays[x].Width:=((Self.Width-20) DIV DisplayCount)-1;
    FDisplays[x].Left:=(FDisplays[x].Width+1)*x+10;
  end;
end;

procedure TdsSevenSegmentMultiDisplay.SetOnColor(Value: TColor);
begin
  if Value <> FOnColor then
  begin
    FOnColor := Value;
    Refresh;
  end;
end;

procedure TdsSevenSegmentMultiDisplay.SetOffColor(Value: TColor);
begin
  if Value <> FOffColor then
  begin
    FOffColor := Value;
    Refresh;
  end;
end;

procedure TdsSevenSegmentMultiDisplay.SetBevel(Value: integer);
begin
  if Value <> FBevel then
  begin
    FBevel := Value;
    BevelWidth := FBevel;
    Refresh;
  end;
end;

procedure TdsSevenSegmentMultiDisplay.DblClick(Sender:TObject);
begin
  Self.OnDblClick(Sender);
end;

constructor TdsDotMatrixDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 50;
  Height:= 70;

  FBackColor := clSilver;
  FOnColor := clBlack;
  FOffColor := clYellow;

  FDots := StringOfChar('0', 35);
end;

procedure TdsDotMatrixDisplay.SeTdsDisplayDots(Value: TdsDisplayDots);
begin
  if Value <> FDots then
  begin
    FDots := Value;
    Refresh;

    DoChange;
  end;
end;

procedure TdsDotMatrixDisplay.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Refresh;
  end;
end;

procedure TdsDotMatrixDisplay.SetOnColor(Value: TColor);
begin
  if Value <> FOnColor then
  begin
    FOnColor := Value;
    Refresh;
  end;
end;

procedure TdsDotMatrixDisplay.SetOffColor(Value: TColor);
begin
  if Value <> FOffColor then
  begin
    FOffColor := Value;
    Refresh;
  end;
end;

procedure TdsDotMatrixDisplay.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TdsDotMatrixDisplay.Paint;
var
  i, j: Integer;
  w, h: Double;
begin
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := FBackColor;
  Canvas.Rectangle(0, 0, Width, Height);

  w := Width / 5;
  h := Height / 7;

  for i := 0 to 6 do
    for j := 0 to 4 do
    begin
      if FDots[i * 5 + j + 1] = '1' then
        Canvas.Brush.Color := FOnColor
      else
        Canvas.Brush.Color := FOffColor;

      Canvas.Rectangle(Round(j*w), Round(i*h), Round((j+1)*w), Round((i+1)*h));
    end;
end;

procedure TdsDotMatrixDisplay.ExtSetDots(const Value: array of Byte);
var
  i: Integer;
  tmpstr: TdsDisplayDots;
begin
  tmpstr := StringOfChar('0', 35);
  for i := 0 to Length(Value) - 1 do tmpstr[Value[i]] := '1';
  Dots := tmpstr;
end;




end.
