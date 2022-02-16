// ***************************************************************************
// ************************** VECTORS UNIT ***********************************
// **********************   Juan José Montero  *******************************
// ******************* juanjo.montero@telefonica.net *************************
// *********************** Release 19/11/2003 ********************************
// ***************************************************************************

unit Vectors;

interface

uses Windows;

type TQuaternion = record
  x,y,z,a: Single;
end;

type
  TVector2D = record
    X, Y:Single;
end;


type
  TVector3D = record
    X, Y, Z:Single;
end;

type
  TVector4D = record
    X, Y, Z, Angle:Single;
end;

type
  TByteColor = record
    Red,
    Green,
    Blue: Byte;
end;

type
  PVector4f = ^TVector4f;
  TVector4f = record
    Red:Single;
    Green:Single;
    Blue:Single;
    Alpha:Single;
end;

function MakeQuaternion ( x,y,z,a : single): TQuaternion;
function VectorSetValue(Value:Single):TVector3D;
procedure VectorClear(var Vector:TVector3D);
procedure VectorInvert(var Vector:TVector3D);
function VectorSize(const Vector:TVector3D) : Single;
function VectorNormalize(var Vector:TVector3D) : Single;
function VectorSquareNorm(const Vector:TVector3D) : Single;
function VectorAdd(const Vector1, Vector2:TVector3D):TVector3D; overload;
procedure VectorAdd(var Vector:TVector3D; Value : Single); overload;
function VectorSub(const Vector1, Vector2:TVector3D) : TVector3D; overload;
procedure VectorSub(var Vector:TVector3D; Value : Single); overload;
function VectorDivide(const Vector1, Vector2:TVector3D):TVector3D;overload;
function VectorDivide(const Vector:TVector3D; Value:Single):TVector3D;overload;
function VectorMultiply(const Vector1, Vector2:TVector3D):TVector3D; overload;
procedure VectorScale(var Vector:TVector3D; Value:Single);
function VectorCrossProduct(const Vector1, Vector2:TVector3D):TVector3D;
function VectorDotProduct(const Vector1, Vector2:TVector3D):Single;
procedure VectorRotateX(Angle:Single; var Vector:TVector3D);
procedure VectorRotateY(Angle:Single; var Vector:TVector3D);
procedure VectorRotateZ(Angle:Single; var Vector:TVector3D);
function VectorIsEqual(const Vector1, Vector2:TVector3D):Boolean;
function VectorIsGreater(const Vector1, Vector2:TVector3D):Boolean;
function VectorIsGreaterEqual(const Vector1, Vector2:TVector3D):Boolean;
function VectorIsLess(const Vector1, Vector2:TVector3D):Boolean;
function VectorIsLessEqual(const Vector1, Vector2:TVector3D):Boolean;
function ByteColorTo4f(const BytesToWrap:TByteColor):TVector4f;
function Color4fToByte(const BytesToWrap:TVector4f):TByteColor;

implementation

uses Math;


function VectorSetValue(Value:Single):TVector3D;
begin
  Result.X:=Value;
  Result.Y:=Value;
  Result.Z:=Value;
end;

procedure VectorClear(var Vector:TVector3D);
begin
  ZeroMemory(@Vector, SizeOf(TVector3D));
end;


procedure VectorInvert(var Vector:TVector3D);
begin
  Vector.X := -Vector.X;
  Vector.Y := -Vector.Y;
  Vector.Z := -Vector.Z;
end;


function VectorSize(const Vector:TVector3D):Single;
begin
  Result:=Sqrt((Vector.X * Vector.X) + (Vector.Y * Vector.Y) + (Vector.Z * Vector.Z));
end;


{function VectorNormalize(var Vector:TVector3D):Single;
var ScaleFactor:Single;
begin
  Result:=VectorSize(Vector);
  if (Result=0.0) then
   Exit;
  ScaleFactor:=1.0/Result;

  Vector.X := Vector.X * ScaleFactor;
  Vector.Y := Vector.Y * ScaleFactor;
  Vector.Z := Vector.Z * ScaleFactor;
end;}


function VectorNormalize(var Vector:TVector3D):Single;
begin
  Result:=VectorSize(Vector);
  if (Result=0.0) then
   Exit;

  Vector.X := Vector.X / Result;
  Vector.Y := Vector.Y / Result;
  Vector.Z := Vector.Z / Result;
end;
function MakeQuaternion ( x,y,z,a : single): TQuaternion;
begin
  result.x := x * sin(a / 2);
  result.y := y * sin(a / 2);
  result.z := z * sin(a / 2);
  result.a := cos(a / 2);
end;



function VectorSquareNorm(const Vector:TVector3D):Single;
begin
  Result := Vector.X * Vector.X;
  Result := Result + (Vector.Y * Vector.Y);
  Result := Result + (Vector.Z * Vector.Z);
end;

function VectorAdd(const Vector1, Vector2:TVector3D):TVector3D; overload;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

procedure VectorAdd(var Vector:TVector3D; Value : Single);overload;
begin
  Vector.X := Vector.X + Value;
  Vector.Y := Vector.Y + Value;
  Vector.Z := Vector.Z + Value;
end;

function VectorSub(const Vector1, Vector2:TVector3D) : TVector3D; overload;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

procedure VectorSub(var Vector:TVector3D; Value : Single); overload;
begin
  Vector.X := Vector.X - Value;
  Vector.Y := Vector.Y - Value;
  Vector.Z := Vector.Z - Value;
end;

function VectorDivide(const Vector1, Vector2:TVector3D):TVector3D;overload
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
end;

function VectorDivide(const Vector:TVector3D; Value:Single):TVector3D;overload;
begin
  Result.X := Vector.X / Value;
  Result.Y := Vector.Y / Value;
  Result.Z := Vector.Z / Value;
end;


function VectorMultiply(const Vector1, Vector2:TVector3D):TVector3D; overload;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

procedure VectorScale(var Vector:TVector3D; Value:Single);
begin
  Vector.X := Vector.X * Value;
  Vector.Y := Vector.Y * Value;
  Vector.Z := Vector.Z * Value;
end;

function VectorCrossProduct(const Vector1, Vector2:TVector3D):TVector3D;
begin
  Result.X := (Vector1.Y * Vector2.Z) - (Vector1.Z * Vector2.Y);
  Result.Y := (Vector1.Z * Vector2.X) - (Vector1.X * Vector2.Z);
  Result.Z := (Vector1.X * Vector2.Y) - (Vector1.Y * Vector2.X);
end;


function VectorDotProduct(const Vector1, Vector2:TVector3D):Single;
begin
  Result := (Vector1.X * Vector2.X) + (Vector1.Y * Vector2.Y) + (Vector1.Z * Vector2.Z);
end;


procedure VectorRotateX(Angle:Single; var Vector:TVector3D);
var Y0, Z0   : Single;
    Radians  : Single;
begin
  Y0 := Vector.Y;
  Z0 := Vector.Z;
  Radians := DegToRad(Angle);
  Vector.Y := (Y0 * Cos(Radians)) - (Z0 * Sin(Radians));
  Vector.Z := (Y0 * Sin(Radians)) + (Z0 * Cos(Radians));
end;

procedure VectorRotateY(Angle:Single; var Vector:TVector3D);
var X0, Z0   : Single;
    Radians  : Single;
begin
  X0 := Vector.X;
  Z0 := Vector.Z;
  Radians := DegToRad(Angle);
  Vector.X := (X0 * Cos(Radians)) - (Z0 * Sin(Radians));
  Vector.Z := (X0 * Sin(Radians)) + (Z0 * Cos(Radians));
end;

procedure VectorRotateZ(Angle:Single; var Vector:TVector3D);
var X0, Y0   : Single;
    Radians  : Single;
begin
  X0 := Vector.X;
  Y0 := Vector.Y;
  Radians := DegToRad(Angle);
  Vector.X := (X0 * Cos(Radians)) - (Y0 * Sin(Radians));
  Vector.Y := (X0 * Sin(Radians)) + (Y0 * Cos(Radians));
end;

function VectorIsEqual(const Vector1, Vector2:TVector3D):Boolean;
begin
  Result:=(Vector1.X=Vector2.X) and (Vector1.Y=Vector2.Y) and (Vector1.Z=Vector2.Z);
end;

function VectorIsGreater(const Vector1, Vector2:TVector3D):Boolean;
begin
  Result:=(Vector1.X>Vector2.X) and (Vector1.Y>Vector2.Y) and (Vector1.Z>Vector2.Z);
end;

function VectorIsGreaterEqual(const Vector1, Vector2:TVector3D):Boolean;
begin
  Result:=(Vector1.X>=Vector2.X) and (Vector1.Y>=Vector2.Y) and (Vector1.Z>=Vector2.Z);
end;

function VectorIsLess(const Vector1, Vector2:TVector3D):Boolean;
begin
  Result:=(Vector1.X<Vector2.X) and (Vector1.Y<Vector2.Y) and (Vector1.Z<Vector2.Z);
end;

function VectorIsLessEqual(const Vector1, Vector2:TVector3D):Boolean;
begin
  Result:=(Vector1.X<=Vector2.X) and (Vector1.Y<=Vector2.Y) and (Vector1.Z<=Vector2.Z);
end;



function ByteColorTo4f(const BytesToWrap:TByteColor):TVector4f;
const Scaler:Single = 1.0/255.0;
begin
  Result.Red:=BytesToWrap.Red * Scaler;
  Result.Green:=BytesToWrap.Green * Scaler;
  Result.Blue:=BytesToWrap.Blue * Scaler;
  Result.Alpha:=1.0;
end;


function Color4fToByte(const BytesToWrap:TVector4f):TByteColor;
begin
  Result.Red:=Trunc(BytesToWrap.Red * 255);
  Result.Green:=Trunc(BytesToWrap.Green * 255);
  Result.Blue:=Trunc(BytesToWrap.Blue * 255);
end;


end.
