// ***************************************************************************
// ****************** Portions for file loading by DIGIBEN *******************
// ********************** T3DModel Class for 3DS Files ***********************
// **********************   Juan José Montero  *******************************
// ******************* juanjo.montero@telefonica.net *************************
// *********************** Release 19/11/2003 ********************************
// ***************************************************************************
// + MDL Neverwinter Night Gabriele Canazza
     { TODO : a_ba.mdl animations supermodel + a_ba non combat , spells ecc...}
     { TODO : a_ba.mdl viene caricato prima, dopo il model fa un override, ma i shoulder ci saranno}
     { TODO : Array Texture e fix texture head/left
   { TODO : ZeroMem32 }
unit Unit3DS;


interface


uses Windows, Classes, Graphics, Vectors,StrUtils, System.Generics.Collections;


const
// Root Node
  M3DMAGIC     = $4D4D;

// Primary Blocks
  M3D_VERSION  = $0002; // File version
  MDATA        = $3D3D; // All the Object information
  KFDATA       = $B000; // Animation frames

// Definitions for MDATA
  MAT_ENTRY    = $AFFF; // Material information
  NAMED_OBJECT = $4000; // Object data (vertices, faces ...)

// Definitions for MAT_ENTRY
  MAT_NAME         = $A000; // Material name
  MAT_AMBIENT      = $A010; // Ambient
  MAT_DIFFUSE      = $A020; // Diffuse
  MAT_SPECULAR     = $A030; // Specular
  MAT_SHININESS    = $A040; // Shininess
  MAT_TRANSPARENCY = $A050; // Transparency
  MAT_TEXMAP       = $A200; // Texture information
  MAT_MAPNAME      = $A300; // Texture file name
  N_TRI_OBJECT     = $4100; // For each object  ...

// Definitions for N_TRI_OBJECT
  POINT_ARRAY  = $4110; // Vertices
  FACE_ARRAY   = $4120; // Faces
  MSH_MAT_GROUP= $4130; // Material for the object
  TEX_VERTS    = $4140; // Texture coordinates

  SUPPORTEDVERSION = $03; // Vserión máxima de 3DS soportada


// -----------------------------------------------------------------------------

type
  TMaterialType = (mtAmbient, mtDiffuse, mtSpecular);

type
  TVertexIndex = array [0..2] of Integer;


type
  TFace = record
    VertIndex :TVertexIndex;
    CoordIndex:TVertexIndex;
end;


type
   TRenderMode = (rmTriangles, rmLines, rmPoints);

type
   TTransformType = (ttRotate, ttTranslate, ttScale);



// This Class is used to apply one transformation to 3ds object. The instance
// is stored in TTransformList class
type
  TTransformation = class(TObject)
  private
    FX: Single;
    FY: Single;
    FAngle: Single;
    FZ: Single;
    FTransformType: TTransformType;
    FEnabled: Boolean;

  public
    constructor Create;
    procedure Apply;
    procedure Restore;
    property Angle:Single read FAngle write FAngle;
    property TransformType:TTransformType read FTransformType write FTransformType;
    property X:Single read FX write FX;
    property Y:Single read FY write FY;
    property Z:Single read FZ write FZ;
    property Enabled:Boolean read FEnabled write FEnabled;
end;



// Class to store the material properties. Ambient, diffuse, specular
// and emission are classes of TMaterialProperties
type
  TMaterialProperties = class(TObject)
  private
    FVector:TVector4f;
    FglDefFace, FglDefType:Cardinal;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);

  public
    constructor Create(glDefFace, glDefType:Cardinal);
    destructor Destroy;override;
    function ToString:string;
    procedure SetRGBA(const R, G, B, Alpha:Single);
    procedure Apply;
    property Vector:TVector4f read FVector write FVector;
    property Color:TColor read GetColor write SetColor;
    property Alpha:Single read FVector.Alpha write FVector.Alpha;
end;


// List of TTransformation class
type
  TTransformList = class(TList)
  private
    FEnabled: Boolean;

  public
    constructor Create;
    destructor Destroy;override;
    procedure Push;
    procedure Pop;
    property Enabled:Boolean read FEnabled write FEnabled;
    function AddTransform:TTransformation;
    function AddTransformEx(const _TransformType:TTransformType; const _Angle, _X, _Y, _Z:Single):TTransformation;
end;



type

  T3DModel = class;

// TChunk only used for loading the 3ds file. This class is destroyed after
// loading
  TChunk = class(TObject)
  private
    FId:Word;
    FLength:Integer;
    FBytesReaded:Integer;
    FModel:T3DModel;
    FFileHandle:Integer;
    function GetBytesRemaining: Integer;
  public
    constructor Create(Model:T3DModel);
    destructor Destroy;override;
    function Read:Integer;
    function ReadBuffer(var Buffer; Count:Integer):Integer;
    function ReadRemainingBuffer(var Buffer):Integer;
    procedure ProcessNextChunk(PreviousChunk:TChunk);
    procedure NexFilePosition;
    function ReadObjectString:string;
    function WaitForLength:Boolean;
    procedure UpdateByteCounter(Value:Integer);
    property Id:Word read FId;
    property Length:Integer read FLength;
    property BytesReaded:Integer read FBytesReaded;
    property BytesRemaining:Integer read GetBytesRemaining;
    property FileHandle:Integer read FFileHandle write FFileHandle;
end;


// Store the complete material properties for each 3ds object
  TMaterial = class(TObject)
  private
    FMaterialName:string;
    FMaterialFile:string;
    FHasTexture:Boolean;
    FGenTexture:Cardinal;
    FDisableTexture:Boolean;
    FEnviromentMap: Boolean;
    FSphericalMap: Boolean;
    FAmbient: TMaterialProperties;
    FEmission: TMaterialProperties;
    FSpecular: TMaterialProperties;
    FDiffuse: TMaterialProperties;
    // uTile: Single;
    // vTile: Single;
    // uOffset: Single;
    // vOffset: Single;
    procedure SetMaterial;
    procedure EnviromentApply;
    procedure SphericalApply;
    function TextureActive:Boolean;
  public
    Shininess, Transparency:Single;
    constructor Create;
    destructor Destroy;override;
    procedure ProcessNextMaterialChunk(PreviousChunk:TChunk);
    procedure ReadColorChunk(PreviousChunk:TChunk; var Buffer);
    procedure CopyFrom(Source:TMaterial);
    procedure SetMaterialColor(const MaterialType: TMaterialType; const R, G, B, Alpha:Single);
    property HasTexture:Boolean read FHasTexture;
    property DisableTexture:Boolean read FDisableTexture write FDisableTexture;
    property EnviromentMap:Boolean read  FEnviromentMap write FEnviromentMap;
    property SphericalMap:Boolean read  FSphericalMap write FSphericalMap;
    property GenTexture:Cardinal read FGenTexture write FGenTexture;
    property MaterialName:string read FMaterialName;
    property MaterialFile:string read FMaterialFile;
    property Ambient:TMaterialProperties read FAmbient;
    property Diffuse:TMaterialProperties read FDiffuse;
    property Specular:TMaterialProperties read FSpecular;
    property Emission:TMaterialProperties read FEmission;
end;


  TAnimatedFrame = record   // same for Position and orientation
    KeyTime : Single;
    KeyValue : TVector3D;
    Angle : Single; // only orientation
  end;

  TAnimatedObject = class(TObject)
  private
    FChildrenCount : Integer;
    function GetChildrenCount: Integer;
  public
    ObjectName : string;
    ParentAnimatedObjectName : string;
    ParentAnimatedObject : TAnimatedObject;
    PositionKeys: array of TAnimatedFrame;
    OrientationKeys: array of TAnimatedFrame;
    Children : TObjectlist <TAnimatedObject>;
    function GetPositionCount: Integer;
    function GetOrientationCount: Integer;
    procedure AddChildren ( Animatedobject3d :TAnimatedObject);
    procedure Clear;
    constructor Create;
    destructor Destroy;override;
    property PositionKeyCount : Integer read GetPositionCount;
    property OrientationKeyCount : Integer read GetOrientationCount;
  end;
  p3DObject = ^T3DObject;

// This class stores the complete definition for each 3ds object. Is used to
// draw it in the scene also.
  T3DObject = class(TObject)
  private
    FModel: T3DModel;
    FVisible:Boolean;
    FObjectName:string;
    FObjectType:string;
    FParentObjectName:string;
    FParentObject:T3DObject;
    FPosition,FOrientation : TVector3D;
    FMaterial:TMaterial;
    FVertexCount, FTexVertexCount,
    FFaceCount, FNormalCount:Integer;
    FChildrenCount:Integer;
    FObjectIndex:Integer;
    FMaxVector, FMinVector, FMidVector:TVector3D;
    FSelected:Boolean;
    FRMode:Cardinal;
    FRenderMode: TRenderMode;
    FTransformList: TTransformList;
    FElapsedTime,FoElapsedTime: Single;
    FLastCursorAnim : Integer;
    function GetChildrenCount: Integer;
    procedure AddChildren ( object3d :T3DObject);
    procedure SetRenderMode(const Value: TRenderMode);
    procedure DrawBox;
  public
    Rot  : Single;
    Verts:array of TVector3D;
    Normals:array of TVector3D;
    TexVerts:array of TVector2D;
    Faces:array of TFace;
    Children : TObjectlist <T3DObject>;
    CurrentAnimation : TAnimatedObject;
    constructor Create;
    destructor Destroy;override;
    procedure Draw;
    procedure Anim ( ms: Single);
    procedure ProcessNextObjectChunk(PreviousChunk:TChunk);
    procedure ReadVertices(PreviousChunk:TChunk);
    procedure ReadVertexIndices(PreviousChunk:TChunk);
    procedure ReadObjectMaterial(PreviousChunk:TChunk);
    procedure ReadUVCoordinates(PreviousChunk:TChunk);
    procedure AdjustNormals;
    // procedure Transform(const Transformation:TTransformObject; const T, X, Y, Z:Single);
    property Visible:Boolean read FVisible write FVisible;
    property ObjectName:string read FObjectName write FObjectName;
    property ObjectType:string read FObjectType write FObjectType;
    property ParentObjectName:string read FParentObjectName write FParentObjectName;
    property ParentObject:T3DObject read FParentObject write FParentObject;
    property Position : TVector3D read fPosition write fPosition;
    property Orientation : TVector3D read FOrientation write FOrientation;
    property Material:TMaterial read FMaterial;
    property VertexCount:Integer read FVertexCount write FVertexCount;
    property TexVertexCount:Integer read FTexVertexCount write FTexVertexCount;
    property FaceCount:Integer read FFaceCount write FFaceCount;
    property NormalCount:Integer read FNormalCount write FNormalCount;
    property ChildrenCount:Integer read GetChildrenCount;
    property ObjectIndex:Integer read FObjectIndex;
    property RenderMode:TRenderMode read FRenderMode write SetRenderMode;
    property TransformList:TTransformList read FTransformList;
    property Selected:Boolean read FSelected write FSelected;
end;

  TAnimation = class(TObject)
  private
    FAnimationName:string;
    FAnimationModelName: string;
    FAnimationIndex:Integer;
    FCurrentKeyTime: Single;
    FLoop : Boolean;
    FLength: Single;
    FTranstime: Single;  // 0.25  Time in seconds where the animation overlaps with other animation to ensure a smooth transitions.
    AnimationRoot: string;// <node_name> //The animations entry point. This is useful for only animating part of the model and having the rest play a different animation. This used in the torch an shield holding animations to only influence the movement of a single arm.
    function GetAnimatedObjectCount: integer;
  public
    AnimatedObjects : array of TAnimatedObject;
    function FindAnimatedObject(const aName:string):TAnimatedObject;
    function AddAnimatedObject: TAnimatedObject;
    constructor Create;
    destructor Destroy;override;
    Property AnimationName : string read FAnimationName Write FAnimationName;
    Property AnimatedObjectCount : Integer read GetAnimatedObjectCount;
  end;

// This class is the core of this unit. It is used to load, draw all and more
  T3DModel = class(TObject)
  private
    FActiveAnimationName: string;    // if <> '' then process animation by name in model.draw event
    FActiveAnimationIndex: Integer;
    FMaterials:array of TMaterial;
    FFileHandle:Integer;
    FRootChunk:TChunk;
    function GetMaterialCount: Integer;
    function GetObjectCount: Integer;
    function GetAnimationCount: Integer;
    procedure CleanUp;
    procedure CleanUpMdl;
    procedure ComputeNormals;
    procedure SwitchToAnimation ( const value:string);
  public
    Name : string;
    Objects:array of T3DObject;
    Animations: array of TAnimation;
    constructor Create;
    destructor Destroy;override;
    function AddMaterial:TMaterial;
    function AddObject:T3DObject;
    function AddAnimation:TAnimation;
    procedure Clear;
    procedure VisibleAll;
    function LoadFromFile(const FileName:string):Boolean;
    function LoadFromFileMDL(const FileName, MdlPath, TexturePath, SuperModelPath:string ): Boolean;
      function LoadGeometry(const fmodel: TextFile;  MdlPath, TexturePath:string): string;
      function LoadAnimations(const fmodel: TextFile;  MdlPath,  SuperModelPath, supermodel :string): Boolean;
        procedure ProcessNewAnim  ( const fmodel: TextFile; FirstString: string );
    function MakeVector3D (aString: string): TVector3D;
    function MakeVector3Dp (aString: string): TVector3D;
    function MakeFace (aString: string): TFace;
    function MakeVector2D (aString: string): TVector2D;
    function FindObject(const aName:string):T3DObject;
    function FindAnimation(const aName:string):TAnimation;
    function Select(const Index:Integer):T3DObject;
    procedure Anim (ms: Single) ;
    procedure Draw;
    property ObjectCount:Integer read GetObjectCount;
    property MaterialCount:Integer read GetMaterialCount;
    property AnimationCount:Integer read GetAnimationCount;
    property ActiveAnimationName: string read FActiveAnimationName write SwitchToAnimation;
end;
function IsOpen(const txt:TextFile):Boolean;
procedure ZeroMem32(P:Pointer;Size:integer);
function HasExtensionL(const Name : String; var DotPos : Cardinal) : Boolean;
function JustFilenameL(const PathName : String) : String;
function JustPathL(const PathName : String) : String;
function JustNameL(const PathName : String) : String;
function CharExistsL(const S : String; C : Char) : Boolean;
function WordPositionL(N : Cardinal; const S, WordDelims : String;
                      var Pos : Cardinal) : Boolean;
function ExtractWordL(N : Cardinal; const S, WordDelims : String) : String;
function WordCountL(const S, WordDelims : String) : Cardinal;

function getToken( var sString: String; const sDelim: String ): String;
function GetNextToken  (Const S: string;   Separator: char;   var StartPos: integer): String;
procedure Split  (const S: String;   Separator: Char;   MyStringList: TStringList) ;
function AddToken    (const aToken, S: String;   Separator: Char;   StringLimit: integer): String;




implementation

uses SysUtils, OpenGL, Textures, Math;
Const DosDelimSet  : set of AnsiChar = ['\', ':', #0];
Const stMaxFileLen  = 260;

// utils
function IsOpen(const txt:TextFile):Boolean;
const
  fmTextOpenRead = 55217;
  fmTextOpenWrite = 55218;
begin
  Result := (TTextRec(txt).Mode = fmTextOpenRead) or (TTextRec(txt).Mode = fmTextOpenWrite)
end;
procedure ZeroMem32(P:Pointer;Size:integer);
// Size=number of dword elements to fill
// assumes that Size>4
asm
  push edi
  mov ecx,edx
  xor edx,edx
  mov dword ptr [eax],edx
  mov dword ptr [eax+4],edx
  mov dword ptr [eax+8],edx
  mov dword ptr [eax+12],edx
  mov edx,eax
  add edx,15
  and edx,-16
  mov edi,edx
  sub edx,eax
  shr edx,2
  sub ecx,edx
  xor eax,eax
  rep stosd
  pop edi
end;

function GetNextToken  (Const S: string;   Separator: char;   var StartPos: integer): String;
var Index: integer;
begin
   Result := '';

   While (S[StartPos] = Separator)
   and (StartPos <= length(S))do
    StartPos := StartPos + 1;

   if StartPos > length(S) then Exit;

   Index := StartPos;

   While (S[Index] <> Separator)
   and (Index <= length(S))do
    Index := Index + 1;

   Result := Copy(S, StartPos, Index - StartPos) ;

   StartPos := Index + 1;
end;

procedure Split    (const S: String;   Separator: Char;   MyStringList: TStringList) ;
var Start: integer;
begin
   Start := 1;
   While Start <= Length(S) do
     MyStringList.Add
       (GetNextToken(S, Separator, Start)) ;
end;

function AddToken (const aToken, S: String; Separator: Char; StringLimit: integer): String;
begin
   if Length(aToken) + Length(S) < StringLimit then
     begin
       if S = '' then
         Result := ''
       else Result := S + Separator;

       Result := Result + aToken;
     end
   else
     Raise Exception.Create('Cannot add token') ;
end;
function getToken( var sString: String; const sDelim: String ): String;
var
  nPos: integer;
begin
  nPos := Pos( sDelim, sString );
  if nPos > 0 then  begin
    GetToken := Copy( sString, 1, nPos - 1 );
    sString := Copy( sString, nPos + 1, Length( sString ) - nPos );
  end
  else  begin
    GetToken := sString;
    sString := '';
  end;
end;

function ExtractWordL(N : Cardinal; const S, WordDelims : String) : String;
var
  C : Cardinal;
  I, J   : Longint;
begin
  Result := '';
  if WordPositionL(N, S, WordDelims, C) then begin
    I := C;
    J := I;
    while (I <= Length(S)) and not
           CharExistsL(WordDelims, S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], (I-J) * SizeOf(Char));
  end;
end;
function WordCountL(const S, WordDelims : String) : Cardinal;
var
  I    : Cardinal;
  SLen : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    if I <= SLen then
      Inc(Result);

    while (I <= SLen) and not CharExistsL(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionL(N : Cardinal; const S, WordDelims : String;
                      var Pos : Cardinal) : Boolean;
var
  Count : Longint;
  I     : Longint;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    if I <= Length(S) then
      Inc(Count);

    if Count <> LongInt(N) then
      while (I <= Length(S)) and not CharExistsL(WordDelims, S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;
function CharExistsL(const S : String; C : Char) : Boolean; register;
  {-Count the number of a given character in a string. }
{$IFDEF UNICODE}
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do begin
    if S[I] = C then begin
      Result := True;
      Break;
    end;
  end;
end;
{$ELSE}
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;
{$ENDIF}
function JustNameL(const PathName : String) : String;
var
  DotPos : Cardinal;
  S      : AnsiString;
begin
  S := JustFileNameL(PathName);
  if HasExtensionL(S, DotPos) then
    S := System.Copy(S, 1, DotPos-1);
  Result := S;
end;
function HasExtensionL(const Name : String; var DotPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    and not CharExistsL(System.Copy(Name, Succ(DotPos), StMaxFileLen), '\');
end;
function JustFilenameL(const PathName : String) : String;
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);
  Result := System.Copy(PathName, Succ(I), StMaxFileLen);
end;
function JustPathL(const PathName : String) : String;
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);
  Result := System.Copy(PathName, 0, Succ(I-1));
end;

// Convert TColor to RGBA
function ColorToVector4f(const aColor: TColor; Alpha:Single): TVector4f;
begin
  Result.Red:=(aColor and $FF)/255;
  Result.Green:=((aColor shr 8) and $FF)/255;
  Result.Blue:=((aColor shr 16) and $FF)/255;
  Result.Alpha:=Alpha;
end;

// Convert RGBA to TColor
function Vector4fToColor(const aVector: TVector4f): TColor;
var C:TByteColor;
begin
  C:=Color4fToByte(aVector);
  Result:=Rgb(C.Red, C.Green, C.Blue);
end;

function RGBToColor(const R, G, B:Byte):TColor;
begin
  Result:=R;
  Result:=Result or (G shl 8);
  Result:=Result or (B shl 16);
end;


{ TChunk }

constructor TChunk.Create(Model:T3DModel);
begin
  inherited Create;
  FId:=0;
  FLength:=0;
  FBytesReaded:=0;
  FModel:=Model;
  FFileHandle:=FModel.FFileHandle;
end;

destructor TChunk.Destroy;
begin
  inherited;
end;

function TChunk.GetBytesRemaining: Integer;
begin
  Result:=FLength - BytesReaded;
end;

function TChunk.WaitForLength: Boolean;
begin
  Result:=FBytesReaded < FLength;
end;

procedure TChunk.NexFilePosition;
var NewPosition:Integer;
begin
  NewPosition:=BytesRemaining;
  FileSeek(FFileHandle, NewPosition, 1);
  UpdateByteCounter(NewPosition);
end;

procedure TChunk.ProcessNextChunk(PreviousChunk: TChunk);
var NewChunk, TempChunk:TChunk;
    FileVersion, MeshVersion:Cardinal;
begin
  NewChunk:=TChunk.Create(PreviousChunk.FModel);
  while PreviousChunk.WaitForLength do
   begin
     NewChunk.Read;
     case NewChunk.Id of

      M3D_VERSION:
       begin
         NewChunk.ReadRemainingBuffer(FileVersion);
          if (FileVersion > SUPPORTEDVERSION) then
           MessageBox(0, 'Unsupported file version.', 'Warning', MB_OK);
       end;


       MDATA:
         begin
           TempChunk:=TChunk.Create(PreviousChunk.FModel);
           TempChunk.Read;
           TempChunk.ReadRemainingBuffer(MeshVersion);
           NewChunk.UpdateByteCounter(TempChunk.BytesReaded);
           TempChunk.Free;
	   ProcessNextChunk(NewChunk);
	 end;

       MAT_ENTRY:
         with FModel.AddMaterial do
	  ProcessNextMaterialChunk(NewChunk);


       // This holds the name of the object being read
       NAMED_OBJECT:
         with FModel.AddObject do
          begin
            ObjectName:=NewChunk.ReadObjectString;
            ProcessNextObjectChunk(NewChunk);
          end;

       KFDATA: NewChunk.NexFilePosition;

     else
       NewChunk.NexFilePosition;
     end;
     PreviousChunk.UpdateByteCounter(NewChunk.BytesReaded);
   end;
  NewChunk.Free;
end;

function TChunk.Read: Integer;
begin
  FBytesReaded:=FileRead(FFileHandle, FId, 2);
  FBytesReaded:=FBytesReaded + FileRead(FFileHandle, FLength, 4);
  Result:=FBytesReaded;
end;

function TChunk.ReadBuffer(var Buffer; Count: Integer): Integer;
begin
  Result:=FileRead(FFileHandle, Buffer, Count);
  FBytesReaded:=FBytesReaded + Result;
end;


function TChunk.ReadObjectString:string;
var I:Integer;
    C:AnsiChar;
    S:ShortString;
begin
  I:=0;
  C:=#255; // Initialization for "while"
  while C<>#0 do
   begin
     FileRead(FFileHandle, C, 1);
     S[I+1]:=C;
     Inc(I);
   end;

  SetLength(Result, I-1);
  Move(S[1], Result[1], I-1);
  UpdateByteCounter(I);
end;


function TChunk.ReadRemainingBuffer(var Buffer): Integer;
var I:Integer;
begin
  I:=BytesRemaining;
  Result:=FileRead(FFileHandle, Buffer, I);
  FBytesReaded:=FBytesReaded + Result;
end;


procedure TChunk.UpdateByteCounter(Value: Integer);
begin
  FBytesReaded:=FBytesReaded + Value;
end;


// ************************** END TCHUNK **************************************



// ************************** TMATERIAL **************************************


{ TMaterial }

constructor TMaterial.Create;
begin
  inherited;
  FAmbient:=TMaterialProperties.Create(GL_FRONT, GL_AMBIENT);
  FDiffuse:=TMaterialProperties.Create(GL_FRONT, GL_DIFFUSE);
  FSpecular:=TMaterialProperties.Create(GL_FRONT, GL_SPECULAR);
  FEmission:=TMaterialProperties.Create(GL_FRONT, GL_EMISSION);
end;



destructor TMaterial.Destroy;
begin
  FEmission.Free;
  FSpecular.Free;
  FDiffuse.Free;
  FAmbient.Free;
  inherited;
end;


procedure TMaterial.ReadColorChunk(PreviousChunk:TChunk; var Buffer);
var TempChunk:TChunk;
begin
  TempChunk:=TChunk.Create(PreviousChunk.FModel);
  TempChunk.Read;
  TempChunk.ReadRemainingBuffer(Buffer);
  PreviousChunk.UpdateByteCounter(TempChunk.BytesReaded);
  TempChunk.Free;
end;

procedure TMaterial.ProcessNextMaterialChunk(PreviousChunk: TChunk);
var NewChunk:TChunk;
    I:Integer;
    Buffer:TByteColor;
    __Shininess, __Transparency:Word;
begin
  NewChunk:=TChunk.Create(PreviousChunk.FModel);
  while PreviousChunk.WaitForLength do
   begin
     NewChunk.Read;

     case NewChunk.Id of

       MAT_NAME:
         begin
           I:=NewChunk.BytesRemaining;
       	   SetLength(FMaterialName, I);
           NewChunk.ReadBuffer(FMaterialName[1], I);
           SetLength(FMaterialName, Length(FMaterialName) - 1);
         end;


       MAT_AMBIENT:
         begin
           ReadColorChunk(NewChunk, Buffer);
           Ambient.Vector:=ByteColorTo4f(Buffer);
         end;

       MAT_DIFFUSE:
         begin
           ReadColorChunk(NewChunk, Buffer);
           Diffuse.Vector:=ByteColorTo4f(Buffer);
         end;

       MAT_SPECULAR:
         begin
           ReadColorChunk(NewChunk, Buffer);
           Specular.Vector:=ByteColorTo4f(Buffer);
         end;


       MAT_SHININESS:
         begin
           ReadColorChunk(NewChunk, __Shininess);
           Shininess:=128 - (__Shininess * 1.28);
         end;

       MAT_TRANSPARENCY:
         begin
           ReadColorChunk(NewChunk, __Transparency);
           Transparency:=1 - (__Transparency / 100);
         end;

       MAT_TEXMAP:ProcessNextMaterialChunk(NewChunk);

       MAT_MAPNAME:
         begin
           I:=NewChunk.BytesRemaining;
           SetLength(FMaterialFile, I);
           NewChunk.ReadBuffer(FMaterialFile[1], I);
           SetLength(FMaterialFile, Length(FMaterialFile) - 1);
           //FMaterialFile := 'c_a_deer.tga'; //GAB
           FHasTexture:=LoadTexture(FMaterialFile, FGenTexture, False);
         end;

     else
       NewChunk.NexFilePosition;
     end;
     PreviousChunk.UpdateByteCounter(NewChunk.BytesReaded);
   end;
  NewChunk.Free;
end;


procedure TMaterial.CopyFrom(Source: TMaterial);
begin
  FMaterialName:=Source.MaterialName;
  FMaterialFile:=Source.MaterialFile;
  Ambient.Vector:=Source.Ambient.Vector;
  Diffuse.Vector:=Source.Diffuse.Vector;
  Specular.Vector:=Source.Specular.Vector;
  Shininess:=Source.Shininess;
  FHasTexture:=Source.HasTexture;
  FGenTexture:=Source.GenTexture;
end;


procedure TMaterial.SetMaterialColor(const MaterialType: TMaterialType; const R, G,
  B, Alpha: Single);
begin
  case MaterialType of
    mtAmbient : Ambient.SetRGBA(R, G, B, Alpha);
    mtDiffuse : Diffuse.SetRGBA(R, G, B, Alpha);
    mtSpecular: Specular.SetRGBA(R, G, B, Alpha);
  end;
end;

procedure TMaterial.EnviromentApply;
begin
  if FEnviromentMap then
   begin
     glTexGenf(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
     glEnable(GL_TEXTURE_GEN_T);
   end
    else
     glDisable(GL_TEXTURE_GEN_T);

end;

procedure TMaterial.SphericalApply;
begin
  if FSphericalMap then
   begin
     glTexGenf(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
     glEnable(GL_TEXTURE_GEN_S);
   end
    else
     glDisable(GL_TEXTURE_GEN_S);
end;


function TMaterial.TextureActive: Boolean;
begin
  Result:=FHasTexture and (not FDisableTexture);
end;


procedure TMaterial.SetMaterial;
begin
  Ambient.Apply;
  Diffuse.Apply;
  Specular.Apply;
  Emission.Apply;
  glMaterialfv(GL_FRONT, GL_SHININESS, @Shininess);
  EnviromentApply;
  SphericalApply;
  if TextureActive then
   begin
     glEnable(GL_TEXTURE_2D);
     glBindTexture(GL_TEXTURE_2D, FGenTexture);
   end
    else
     glDisable(GL_TEXTURE_2D);
end;



// ************************** END TMATERIAL ***********************************

// ************************** TANIMATION **************************************

constructor TAnimation.Create;
begin
  inherited;
  FLoop := False;
  AnimatedObjects:=nil;
end;

destructor TAnimation.Destroy;
begin
  Finalize(AnimatedObjects);
  inherited;
end;
function TAnimation.AddAnimatedObject: TAnimatedObject;
var C, I:Integer;
begin
  Result:=TAnimatedObject.Create;
  C:=AnimatedObjectCount;
  SetLength(AnimatedObjects, C+1);
  AnimatedObjects[C]:=Result;
end;
function TAnimation.GetAnimatedObjectCount: integer;
begin
  result := length ( AnimatedObjects);
end;

// ************************** END TANIMATION **************************************
// ************************** TANIMATEDOBJECT**************************************

constructor TAnimatedObject.Create;
begin
  inherited;
  PositionKeys:=nil;
  OrientationKeys:=nil;
  Children := TObjectlist<TAnimatedObject>.Create ( False );
end;
destructor TAnimatedObject.Destroy;
begin
  Finalize(PositionKeys);
  Finalize(OrientationKeys);
  Children.Free;
  inherited;
end;
function TAnimatedObject.GetPositionCount: Integer;
begin
  Result:=Length(PositionKeys);
end;
function TAnimatedObject.GetOrientationCount: Integer;
begin
  Result:=Length(OrientationKeys);
end;
procedure TAnimatedObject.Clear;
var i: Integer;
begin
  SetLength(PositionKeys,0);
  SetLength(OrientationKeys,0);
end;


// ************************** END TANIMATEDOBJECT **************************************


// ************************** T3DOBJECT **************************************




{ T3DObject }


constructor T3DObject.Create;
begin
  inherited;
  Verts:=nil;
  Normals:=nil;
  TexVerts:=nil;
  Faces:=nil;
  Children:=nil;
  CurrentAnimation := TAnimatedObject.Create;
  RenderMode:=rmTriangles;
  FMaterial:=TMaterial.Create;
  FTransformList:=TTransformList.Create;
  FVisible:=True;
  Children := TObjectlist<T3dObject>.Create ( False );
end;

destructor T3DObject.Destroy;
begin
  Finalize(Verts);
  Finalize(Normals);
  Finalize(TexVerts);
  Finalize(Faces);
  Finalize(Children);
  glDeleteTextures(1, FMaterial.FGenTexture);
  FMaterial.Free;
  FTransformList.Free;
  CurrentAnimation.Free;
  Children.Free;
  inherited;
end;

procedure T3DObject.ProcessNextObjectChunk(PreviousChunk:TChunk);
var NewChunk:TChunk;
begin
  NewChunk:=TChunk.Create(PreviousChunk.FModel);
  while PreviousChunk.WaitForLength do
   begin
     NewChunk.Read;
     case NewChunk.Id of
       N_TRI_OBJECT:ProcessNextObjectChunk(NewChunk);
       POINT_ARRAY:ReadVertices(NewChunk);
       FACE_ARRAY:ReadVertexIndices(NewChunk);
       MSH_MAT_GROUP:ReadObjectMaterial(NewChunk);
       TEX_VERTS:ReadUVCoordinates(NewChunk);
     else
       NewChunk.NexFilePosition;
     end;
     PreviousChunk.UpdateByteCounter(NewChunk.BytesReaded);
   end;
  NewChunk.Free;
end;



procedure T3DObject.ReadObjectMaterial(PreviousChunk: TChunk);
var I:Integer;
    SourceMaterial:TMaterial;
begin
  FMaterial.FMaterialName:=PreviousChunk.ReadObjectString;
  for I:=0 to PreviousChunk.FModel.MaterialCount-1 do
   begin
     SourceMaterial:=PreviousChunk.FModel.FMaterials[I];
     if CompareStr(FMaterial.MaterialName, SourceMaterial.MaterialName) = 0 then
      begin
        FMaterial.CopyFrom(SourceMaterial);
        Break;
      end;  
   end;
  PreviousChunk.NexFilePosition;
end;


procedure T3DObject.ReadUVCoordinates(PreviousChunk: TChunk);
begin
  PreviousChunk.ReadBuffer(FTexVertexCount, 2);
  SetLength(TexVerts, FTexVertexCount);
  PreviousChunk.ReadRemainingBuffer(TexVerts[0]);
end;


procedure T3DObject.ReadVertexIndices(PreviousChunk: TChunk);
var I, J:Integer;
    Index:Word;
begin
  PreviousChunk.ReadBuffer(FFaceCount, 2);
  SetLength(Faces, FFaceCount);

  for I:=0 to FFaceCount-1 do
   for J:=0 to 3 do
    begin
      PreviousChunk.ReadBuffer(Index, SizeOf(Index));
      if J < 3 then
       Faces[I].VertIndex[J] := Index;
    end;
end;


procedure T3DObject.ReadVertices(PreviousChunk: TChunk);
var I:Integer;
    TempY:Single;
    FirstVertice:Boolean;
begin
  FirstVertice:=False;
  PreviousChunk.ReadBuffer(FVertexCount, 2);
  SetLength(Verts, FVertexCount);
  PreviousChunk.ReadRemainingBuffer(Verts[0]);
  for I:=0 to FVertexCount-1 do
   begin
     TempY:=Verts[I].Y;
     Verts[I].Y:=Verts[I].Z;
     Verts[I].Z:=-TempY;
     if not FirstVertice then
      begin
        FMaxVector:=Verts[I];
        FMinVector:=FMaxVector;
        FirstVertice:=True;
      end;
     FMaxVector.X:=Max(FMaxVector.X, Verts[I].X);
     FMaxVector.Y:=Max(FMaxVector.Y, Verts[I].Y);
     FMaxVector.Z:=Max(FMaxVector.Z, Verts[I].Z);
     FMinVector.X:=Min(FMinVector.X, Verts[I].X);
     FMinVector.Y:=Min(FMinVector.Y, Verts[I].Y);
     FMinVector.Z:=Min(FMinVector.Z, Verts[I].Z);
   end;
  FMidVector:=VectorSub(FMaxVector, FMinVector);
  //VectorAdd(FMaxVector, 0.0);
  //VectorAdd(FMinVector, -0.0);
end;


procedure T3DObject.AdjustNormals;
var V1, V2, Normal :TVector3D;
    TempNormals:array of TVector3D;
    VPoly:array[0..2] of TVector3D;
    I, Shared, J:Integer;
    VSum, VZero:TVector3D;
begin
  SetLength(TempNormals, FaceCount);
  SetLength(Normals, VertexCount);

  for I:=0 to FaceCount-1 do
   begin
     vPoly[0]:=Verts[Faces[I].VertIndex[0]];
     vPoly[1]:=Verts[Faces[I].VertIndex[1]];
     vPoly[2]:=Verts[Faces[I].VertIndex[2]];

     V1 := VectorSub(vPoly[0], vPoly[2]);
     V2 := VectorSub(vPoly[2], vPoly[1]);

     Normal:=VectorCrossProduct(V1, V2);
     TempNormals[I]:=Normal;
     // VectorNormalize(Normal);
   end;

  VectorClear(vSum);
  VectorClear(VZero);
  Shared:=0;

  for I:=0 to VertexCount-1 do begin
     for J:=0 to FaceCount-1 do
      if (Faces[J].VertIndex[0] = I) or (Faces[J].VertIndex[1] = I) or
         (Faces[J].VertIndex[2] = I) then
       begin
         VSum:=VectorAdd(VSum, TempNormals[J]);
         Inc(Shared);
       end;

     Normals[I]:=VectorDivide(vSum, -Shared);
     VectorNormalize(Normals[I]);
     vSum:=vZero;
     Shared:=0;
   end;

  Finalize(TempNormals);
end;

procedure T3DObject.AddChildren ( object3d :T3DObject);
var C:Integer;
begin
  Children.add ( object3d );
end;
function T3DObject.GetChildrenCount: Integer;
begin
  Result := Children.count;
end;
procedure TAnimatedObject.AddChildren ( Animatedobject3d :TAnimatedObject);
var C:Integer;
begin
  Children.add ( Animatedobject3d );
end;
function TAnimatedObject.GetChildrenCount: Integer;
begin
  Result := Children.count;
end;

procedure T3DObject.Draw;
var F, iVertex, PointIndex:Integer;
begin

  //Anim (20);

  FTransformList.Push;
  if FSelected then
   DrawBox;
  Material.SetMaterial;
  glPushName(FObjectIndex);
  glBegin(FRMode);
  glEnable( GL_TEXTURE_2D );
  //glEnable( GL_TEXTURE_GEN_T );

    for F:=0 to FaceCount-1 do
     for iVertex:=0 to 2 do
      begin
        PointIndex:=Faces[F].VertIndex[iVertex];
        glNormal3f(Normals[PointIndex].X, Normals[PointIndex].Y, Normals[PointIndex].Z);
        if Material.HasTexture then
         glTexCoord2f(TexVerts[PointIndex].X, TexVerts[PointIndex].Y)
          else
           glColor(Material.Diffuse.Vector.Red, Material.Diffuse.Vector.Green, Material.Diffuse.Vector.Blue);
        glVertex3f(Verts[PointIndex].X, Verts[PointIndex].Y, Verts[PointIndex].Z);
      end;
  glEnd;
  glPopName;
  FTransformList.Pop;

//  Rot:=Rot + 1;
//  if Rot>=360 then
//   Rot:=0.0;

end;
procedure T3DObject.Anim( ms: Single);
var ao, pk,i,ok:Integer; TmpVector: TVector3D; flog: TextFile; Delta: Single;
begin
//    AssignFile(flog, 'log.txt'); Append(flog);
 // if objectname ='Deer_neckend' then asm Int 3; end;

  FElapsedTime := FElapsedTime + ms;
  if FModel.AnimationCount <= 0 then Exit;
    //  if 'Deer_Rfrontlowleg' = FObjectName then asm int 3; end;
//      if FElapsedTime > FModel.Animations[FModel.FActiveAnimationIndex].FLength then
//        FElapsedTime :=0;

    for pk := CurrentAnimation.PositionKeyCount downto 1 do begin // 1 non 0
        if (FElapsedTime >= CurrentAnimation.PositionKeys [pk-1].KeyTime) and
        (FElapsedTime < CurrentAnimation.PositionKeys [pk].KeyTime) then begin
          {if (pk-1) = FLastCursorAnim then begin  // troppo veloce, sono ancora nel pk di prima
float DeltaTime = pNodeAnim->mRotationKeys[NextRotationIndex].mTime - pNodeAnim->mRotationKeys[RotationIndex].mTime;
    float Factor = (AnimationTime - (float)pNodeAnim->mRotationKeys[RotationIndex].mTime) / DeltaTime;
    assert(Factor >= 0.0f && Factor <= 1.0f);
    const aiQuaternion& StartRotationQ = pNodeAnim->mRotationKeys[RotationIndex].mValue;
    const aiQuaternion& EndRotationQ = pNodeAnim->mRotationKeys[NextRotationIndex].mValue;
    aiQuaternion::Interpolate(Out, StartRotationQ, EndRotationQ, Factor);

            Delta := CurrentAnimation.PositionKeys [pk].KeyValue.X - CurrentAnimation.PositionKeys [pk-1].KeyValue.X;
            TTransformation(TransformList.Items[0]).X := Delta+position.x;
            Delta := CurrentAnimation.PositionKeys [pk].KeyValue.Y- CurrentAnimation.PositionKeys [pk-1].KeyValue.Y;
            TTransformation(TransformList.Items[0]).Y := Delta+position.Y;
            Delta := CurrentAnimation.PositionKeys [pk].KeyValue.z - CurrentAnimation.PositionKeys [pk-1].KeyValue.z;
            TTransformation(TransformList.Items[0]).z := Delta+position.z;
            FLastCursorAnim := pk-1;
            Break;
          end;    }
          FLastCursorAnim := pk-1;
          TTransformation(TransformList.Items[0]).X := CurrentAnimation.PositionKeys[pk].KeyValue.X+ position.x;
          TTransformation(TransformList.Items[0]).Y := CurrentAnimation.PositionKeys[pk].KeyValue.Y+ position.y;
          //TTransformation(TransformList.Items[0]).Z := CurrentAnimation.PositionKeys[pk].KeyValue.z;
     //     writeln ( flog, 'time: ' + FloatToStr(FElapsedTime)+ ' object: '+ FObjectName + ' positionkeyIndex: '+IntToStr(pk-1)  );

          Break;
        end
        else if (FElapsedTime >= CurrentAnimation.PositionKeys [CurrentAnimation.PositionKeyCount-1].KeyTime) then begin
          TTransformation(TransformList.Items[0]).X := CurrentAnimation.PositionKeys[CurrentAnimation.PositionKeyCount-1].KeyValue.X+position.x;
          TTransformation(TransformList.Items[0]).Y := CurrentAnimation.PositionKeys[CurrentAnimation.PositionKeyCount-1].KeyValue.Y+position.y;
          FElapsedTime :=0;
          Break;
        end;
    end;

    for ok := CurrentAnimation.orientationKeyCount -1 downto 1 do begin // 1 non 0
      if (FElapsedTime >= CurrentAnimation.orientationKeys [ok-1].KeyTime) and
      (FElapsedTime < CurrentAnimation.orientationKeys [ok].KeyTime) then begin
      //  writeln ( flog, 'time: ' + FloatToStr(FElapsedTime)+ ' object: '+ FObjectName + ' orientationkeyIndex: '+IntToStr(ok-1)  );

          //  TmpVector := CurrentAnimation.orientationKeys[ok].KeyValue;
           // TmpVector := VectorAdd ( TmpVector , ParentObject.orientation);
            TTransformation(TransformList.Items[1]).Angle := CurrentAnimation.orientationKeys[ok].Angle* (180/3.14) ;
            TTransformation(TransformList.Items[1]).X := TTransformation(TransformList.Items[1]).Angle*CurrentAnimation.orientationKeys[ok].KeyValue.X;//+ Orientation.x;
            TTransformation(TransformList.Items[1]).Y := TTransformation(TransformList.Items[1]).Angle*CurrentAnimation.orientationKeys[ok].KeyValue.Y;//+ Orientation.y;
            TTransformation(TransformList.Items[1]).Z := TTransformation(TransformList.Items[1]).Angle*CurrentAnimation.orientationKeys[ok].KeyValue.Z;//+ Orientation.z ;
        // end;
        Break;
          //end;

      end
      else if (FElapsedTime > CurrentAnimation.orientationKeys [CurrentAnimation.orientationKeyCount-1].KeyTime) then begin
            TTransformation(TransformList.Items[1]).Angle := CurrentAnimation.orientationKeys[ok-1].Angle* (180/3.14) ;
            TTransformation(TransformList.Items[1]).X := CurrentAnimation.orientationKeys[ok-1].KeyValue.X;//+ Orientation.x;
            TTransformation(TransformList.Items[1]).Y := CurrentAnimation.orientationKeys[ok-1].KeyValue.Y;//+ Orientation.y;
            TTransformation(TransformList.Items[1]).Z := CurrentAnimation.orientationKeys[ok-1].KeyValue.Z;//+ Orientation.z ;
        FElapsedTime :=0;
        Break;
      end;

    end;

 // CloseFile (flog);
end;

procedure T3DObject.DrawBox;
//var OldLineWidth:Single;
begin
  //glGetFloatv(GL_LINE_WIDTH, @OldLineWidth);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor3ub(0, 255, 0);
  //glLineWidth(1);

  glBegin(GL_LINE_STRIP);
    glVertex3f(FMaxVector.X, FMaxVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X, FMaxVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X, FMaxVector.Y, FMaxVector.Z);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(FMaxVector.X, FMaxVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X, FMaxVector.Y-FMidVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X, FMaxVector.Y-FMidVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X, FMaxVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X, FMaxVector.Y, FMaxVector.Z);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(FMaxVector.X, FMinVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMinVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMinVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X, FMinVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X, FMinVector.Y, FMaxVector.Z);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y-FMidVector.Y, FMaxVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y-FMidVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y, FMaxVector.Z-FMidVector.Z);
    glVertex3f(FMaxVector.X-FMidVector.X, FMaxVector.Y, FMaxVector.Z);
  glEnd;

  glEnable(GL_LIGHTING);
  //glLineWidth(OldLineWidth);
end;



procedure T3DObject.SetRenderMode(const Value: TRenderMode);
begin
  FRenderMode := Value;
  case FRenderMode of
    rmTriangles:FRMode:=GL_TRIANGLES;
    rmLines:FRMode:=GL_LINES;
    rmPoints:FRMode:=GL_POINTS;
  end;
end;



// ************************** END T3DOBJECT **********************************

// ************************** T3DMODEL ***************************************



{ T3DModel }


constructor T3DModel.Create;
begin
  inherited;
end;


destructor T3DModel.Destroy;
begin
  Clear;
  inherited;
end;


procedure T3DModel.Clear;
var I:Integer;
begin
  for I:=0 to ObjectCount-1 do begin
   Objects[I].Free;
  end;
  for I:=0 to AnimationCount-1 do begin
   Animations[I].Free;
  end;
  Finalize(Objects);
  Finalize(FMaterials);
  Finalize(Animations);
end;

function T3DModel.AddMaterial: TMaterial;
var C:Integer;
begin
  Result:=TMaterial.Create;
  C:=MaterialCount;
  SetLength(FMaterials, C + 1);
  FMaterials[C]:=Result;
end;

function T3DModel.AddObject: T3DObject;
var C, I:Integer;
begin
  Result:=T3DObject.Create;
  C:=ObjectCount;
  I:=C + 1;
  Result.FObjectIndex:=I;
  SetLength(Objects, I);
  Objects[C]:=Result;
end;

function T3DModel.AddAnimation: TAnimation;
var C, I:Integer;
begin
  Result:=TAnimation.Create;
  C:=AnimationCount;
  I:=C + 1;
  Result.FAnimationIndex:=I;
  SetLength(Animations, I);
  Animations[C]:=Result;
end;

function T3DModel.GetMaterialCount: Integer;
begin
  Result:=Length(FMaterials);
end;

function T3DModel.GetObjectCount: Integer;
begin
  Result:=Length(Objects);
end;
function T3DModel.GetAnimationCount: Integer;
begin
  Result:=Length(Animations);
end;

function T3DModel.LoadFromFile(const FileName:string): Boolean;
begin
  Clear;
  FRootChunk:=TChunk.Create(Self);
  FFileHandle:=FileOpen(FileName, fmOpenRead);
  if FFileHandle < 0 then
   begin
     Result:=False;
     CleanUp;
     Exit;
   end;

  FRootChunk.FileHandle:=FFileHandle;
  FRootChunk.Read;
  if FRootChunk.Id <> M3DMAGIC then
   begin
     Result:=False;
     CleanUp;
     Exit;
   end;

  FRootChunk.ProcessNextChunk(FRootChunk);

  ComputeNormals;
  CleanUp;
  Result:=True;
end;

procedure T3DModel.CleanUp;
var I:Integer;
begin
  for I:=0 to MaterialCount-1 do
   FMaterials[I].Free;
  Finalize(FMaterials);
  FRootChunk.Free;
  FRootChunk:=nil;
  FileClose(FFileHandle);
end;
procedure T3DModel.CleanUpMdl;
var I:Integer;
begin
  for I:=0 to MaterialCount-1 do
   FMaterials[I].Free;
  Finalize(FMaterials);
end;

procedure T3DModel.ComputeNormals;
var I:Integer;
begin
  for I:=0 to ObjectCount-1 do
   Objects[I].AdjustNormals;
end;
procedure T3DModel.SwitchToAnimation ( const value:string);
var
  i,ao,pk,ok: Integer;Animation:TAnimation;
begin
  FActiveAnimationName := value;
  for I := 0 to AnimationCount -1 do begin
    if Animations[i].FAnimationName = FActiveAnimationName then begin
      FActiveAnimationIndex := I;
      Break;
    end;
  end;
  Animation :=  Animations[FActiveAnimationIndex];
  for i := 0 to ObjectCount -1 do begin
    Objects[i].CurrentAnimation.Clear;

    For ao := 0 to Animation.AnimatedObjectCount -1 do begin
      if Animation.AnimatedObjects [ao].ObjectName = Objects[i].ObjectName then begin  // this Object
         SetLength( Objects[i].CurrentAnimation.PositionKeys, Animation.AnimatedObjects [ao].PositionKeyCount);
         for pk := 0 to Animation.AnimatedObjects [ao].PositionKeyCount -1 do begin
          Objects[i].CurrentAnimation.PositionKeys[pk].KeyTime := Animation.AnimatedObjects [ao].PositionKeys[pk].KeyTime;
          Objects[i].CurrentAnimation.PositionKeys[pk].KeyValue := Animation.AnimatedObjects [ao].PositionKeys[pk].KeyValue;
          Objects[i].CurrentAnimation.PositionKeys[pk].Angle := Animation.AnimatedObjects [ao].PositionKeys[pk].Angle;
         end;
         SetLength( Objects[i].CurrentAnimation.orientationKeys, Animation.AnimatedObjects [ao].orientationKeyCount);
         for ok := 0 to Animation.AnimatedObjects [ao].orientationKeyCount -1 do begin
          Objects[i].CurrentAnimation.orientationKeys[ok].KeyTime := Animation.AnimatedObjects [ao].orientationKeys[ok].KeyTime;
          Objects[i].CurrentAnimation.orientationKeys[ok].KeyValue := Animation.AnimatedObjects [ao].orientationKeys[ok].KeyValue;
          Objects[i].CurrentAnimation.orientationKeys[ok].Angle := Animation.AnimatedObjects [ao].orientationKeys[ok].Angle;
         end;
         break;
      end;
    end;
  end;
end;

procedure T3DModel.Draw;
var I:Integer;
begin
  for I:=0 to ObjectCount-1 do
   if Objects[I].Visible then
    Objects[I].Draw;
end;

procedure T3DModel.Anim (ms: Single) ;
var I:Integer;
begin
// Animroot mi dice da quale index iniziare
  for I:=0 to ObjectCount-1 do begin
   if Objects[I].Visible then begin
   // if objects[i].FObjectName = 'Deer_tail' then

    Objects[I].Anim (ms) ;
   end;

  end;
end;

function T3DModel.FindObject(const aName: string): T3DObject;
var I:Integer;
begin
  Result:=nil;
  for I:=0 to ObjectCount-1 do
   if SameText(aName, Objects[I].ObjectName) then
    begin
      Result:=Objects[I];
      Break;
    end;
end;
function TAnimation.FindAnimatedObject(const aName: string): TAnimatedObject;
var I:Integer;
begin
  Result:=nil;
  for I:=0 to AnimatedObjectCount-1 do
   if SameText(aName, AnimatedObjects[I].ObjectName) then
    begin
      Result:=AnimatedObjects[I];
      Break;
    end;
end;
function T3DModel.FindAnimation(const aName:string):TAnimation;
var I:Integer;
begin
  Result:=nil;
  for I:=0 to ObjectCount-1 do
   if SameText(aName, Animations[I].AnimationName) then
    begin
      Result:=Animations[I];
      Break;
    end;

end;



function T3DModel.Select(const Index: Integer): T3DObject;
var I:Integer;
begin
  for I:=0 to ObjectCount-1 do
   Objects[I].Selected:=False;
  if Index > 0 then
   begin
     Result:=Objects[Index - 1];
     Result.Selected:=True;
   end
    else
     Result:=nil;
end;


procedure T3DModel.VisibleAll;
var I:Integer;
begin
  for I:=0 to ObjectCount-1 do
   Objects[I].Visible:=True;
end;

{ TTransformation }

constructor TTransformation.Create;
begin
  inherited;
  FEnabled:=True;
end;


procedure TTransformation.Apply;
begin
  if FEnabled then
   case FTransformType of
     ttRotate    : glRotate(FAngle, FX, FY, FZ);
     ttTranslate : glTranslate(FX, FY, FZ);
     ttScale     : glScale(FX, FY, FZ);
   end;
end;


procedure TTransformation.Restore;
begin
  if FEnabled then
   glPopMatrix;
end;

{ TTransformList }


constructor TTransformList.Create;
begin
  inherited;
  FEnabled:=True;
end;


destructor TTransformList.Destroy;
var I:Integer;
begin
  for I:=0 to Count-1 do
   TTransformation(Items[I]).Free;
  Clear;
  inherited;
end;



function TTransformList.AddTransform: TTransformation;
begin
  Result:=TTransformation.Create;
  Add(Result);
end;

function TTransformList.AddTransformEx(const _TransformType: TTransformType;
  const _Angle, _X, _Y, _Z: Single): TTransformation;
begin
  Result:=TTransformation.Create;
  with Result do
   begin
     TransformType:=_TransformType;
     Angle:=_Angle;
     X:=_X;
     Y:=_Y;
     Z:=_Z;
   end;
  Add(Result);
end;


procedure TTransformList.Push;
var I:Integer;
begin
  if FEnabled then
   for I:=0 to Count-1 do
    begin
      if I=0 then
       glPushMatrix;
      TTransformation(Items[I]).Apply;
    end;
end;


procedure TTransformList.Pop;
var I:Integer;
begin
  if FEnabled then
   for I:=Count-1 downto 0 do
    begin
      TTransformation(Items[I]).Restore;
      if I=0 then
       glPopMatrix;
    end;
end;




{ TMaterialProperties }

constructor TMaterialProperties.Create(glDefFace, glDefType:Cardinal);
begin
  inherited Create;
  ZeroMemory(@FVector, SizeOf(TVector4f));
  FglDefFace:=glDefFace;
  FglDefType:=glDefType;
end;

destructor TMaterialProperties.Destroy;
begin
  inherited;
end;


procedure TMaterialProperties.SetRGBA(const R, G, B, Alpha: Single);
begin
  FVector.Red:=R;
  FVector.Green:=G;
  FVector.Blue:=B;
  FVector.Alpha:=Alpha;
end;


function TMaterialProperties.ToString: string;
begin
  with FVector do
   Result:=Format('R%.1f - G%.1f - B%.1f - A%.1f', [Red, Green, Blue, Alpha]);
end;


procedure TMaterialProperties.Apply;
begin
  glMaterialfv(FglDefFace, FglDefType, @FVector);
end;


function TMaterialProperties.GetColor: TColor;
begin
  Result:=Vector4fToColor(FVector);
end;


procedure TMaterialProperties.SetColor(const Value: TColor);
var CurAlpha:Single;
begin
  CurAlpha:=FVector.Alpha;
  FVector:=ColorToVector4f(Value, CurAlpha);
end;
function T3DModel.LoadFromFileMDL( const FileName, MdlPath, TexturePath, SuperModelPath:string ): Boolean;
var fModel :  TextFile; SuperModel: string;
begin
  Clear;

  AssignFile(fModel,filename);
  Reset(fModel);
  SuperModel:= LoadGeometry ( fModel, MdlPath, TexturePath ) ;
  LoadAnimations ( fModel, MdlPath, SuperModelPath, SuperModel);



//  if isOpen(fmodel) then
     CloseFile(fModel);
  ComputeNormals;
  CleanUpMdl;



  Result:=True;

end;
function T3DModel.LoadGeometry(const fmodel: TextFile;  MdlPath, TexturePath:string): string;
var  aString, newmodel : string; Object3d,ParentObject3d,Child : T3DObject; TmpVector : TVector3D;i: Integer;
begin
  while LeftStr( aString ,12 ) <> 'endmodelgeom' do begin
    Readln ( fModel, aString);
    aString := TrimLeft(aString);

    if Leftstr(  aString , 8) = 'newmodel' then newmodel:= ExtractWordL (2,aString,' ')
    else if Leftstr(  aString , 13) = 'setsupermodel' then result:= ExtractWordL (3,aString,' ') + '.mdl'

    { A Dummy is a single point in space without any data - no geometry, no surface, no volume, nothing. Therefore it is never rendered. Dummies are
    used to group objects or indicate special locations to the engine like target coordinates for spells and projectiles.}
    else if (Leftstr(  aString , 14) = 'beginmodelgeom') then begin
      while  Leftstr(  aString , 12) <> 'endmodelgeom' do begin
        Readln ( fModel, aString);
        aString := TrimLeft(aString);
        if (Leftstr(  aString , 12) = 'node trimesh') or ( Leftstr(  aString , 10) = 'node dummy')   or ( Leftstr(  aString , 15) = 'node danglymesh') then begin
              Object3d :=  AddObject;
              Object3d.FModel:= Self;

              Object3d.ObjectName := ExtractWordL (3,aString,' ');
              Object3d.ObjectType :=  ExtractWordL (2,aString,' ');
              Object3d.TransformList.AddTransformEx (  ttTranslate , 0, Object3d.position.X , Object3d.position.Y,Object3d.position.Z);
              Object3d.TransformList.AddTransformEx (  ttRotate , 0, Object3d.orientation.X , Object3d.orientation.Y,Object3d.orientation.Z);
            while Leftstr(  aString , 7) <> 'endnode' do begin
              Readln ( fModel, aString);
              aString := TrimLeft(aString);
              if  leftstr ( aString, 6) = 'parent' then begin
                Object3d.ParentObjectName := ExtractWordL (2,aString,' ');
                if Object3d.ParentObjectName <> 'NULL' then begin
                  Object3d.ParentObject := FindObject(Object3d.ParentObjectName);
                  Object3d.ParentObject.AddChildren ( Object3d ) ;
                end;
              end
              else if leftstr ( aString, 8) = 'position'  then begin
                TmpVector := MakeVector3Dp ( aString );

                ParentObject3d := FindObject( Object3d.ParentObjectName );
                TmpVector := VectorAdd ( TmpVector , ParentObject3d.Position);
                Object3d.position := TmpVector;
                TTransformation(Object3d.TransformList.items[0]).Angle := 0;
                TTransformation(Object3d.TransformList.items[0]).X := Object3d.position.X;
                TTransformation(Object3d.TransformList.items[0]).Y := Object3d.position.Y;
                TTransformation(Object3d.TransformList.items[0]).Z := Object3d.position.Z;
                  //Object3d.TransformList.AddTransformEx (  ttTranslate , 0, Object3d.position.X , Object3d.position.Y,Object3d.position.Z);
              end
              else if  leftstr ( aString, 11) = 'orientation' then begin
                ParentObject3d := FindObject( Object3d.ParentObjectName );
                TmpVector := MakeVector3Dp ( aString );
                TmpVector := VectorAdd ( TmpVector , ParentObject3d.orientation);
                Object3d.orientation := TmpVector;
                TTransformation(Object3d.TransformList.items[1]).Angle := 0;
                TTransformation(Object3d.TransformList.items[1]).X := Object3d.orientation.X;
                TTransformation(Object3d.TransformList.items[1]).Y := Object3d.orientation.Y;
                TTransformation(Object3d.TransformList.items[1]).Z := Object3d.orientation.Z;

                //                Object3d.TransformList.AddTransformEx (  ttRotate , 0, Object3d.orientation.X , Object3d.orientation.Y,Object3d.orientation.Z);
              end


              else if  leftstr ( aString, 6) = 'bitmap' then begin
                 if ExtractWordL (2,aString,' ') <> 'NULL' then begin
                  Object3d.Material.FMaterialFile :=  TexturePath + ExtractWordL (2,aString,' ') + '.tga';
                  Object3d.Material.FHasTexture:=LoadTexture(Object3d.Material.FMaterialFile, Object3d.Material.FGenTexture, False);
                 end;

              end
              else if  leftstr (  aString , 5) = 'verts' then begin
                Object3d.VertexCount := StrToInt( ExtractWordL (2,aString,' '));
                SetLength(Object3d.Verts,Object3d.VertexCount);

                for I := 0 to Object3d.VertexCount -1 do begin
                  Readln ( fModel, aString);
                  Object3d.Verts[i] := MakeVector3D (aString );
                end;
              end
              else if  leftstr (  aString , 5) = 'faces' then begin
                Object3d.FaceCount := StrToInt( ExtractWordL (2,aString,' '));
                SetLength(Object3d.Faces,Object3d.FaceCount);
                for I := 0 to Object3d.FaceCount -1 do begin
                  Readln ( fModel, aString);
                  Object3d.Faces[i] := MakeFace (aString );
                end;
              end
              else if  leftstr (  aString , 6) = 'tverts' then begin
                Object3d.TexVertexCount := StrToInt( ExtractWordL (2,aString,' '));
                SetLength(Object3d.TexVerts,Object3d.TexVertexCount);
                for I := 0 to Object3d.TexVertexCount -1 do begin
                  Readln ( fModel, aString);
                  Object3d.TexVerts[i] := MakeVector2D (aString );
                end;
              end;
            end;

          end;

    //        else   { TODO : node lights  }
    {    if (Leftstr(  aString , 10) = 'node light') then begin
      parent TCN01_A20_02
      ambientonly 0
      shadow 0
      isdynamic 0
      affectdynamic 1
      lightpriority 5
      fadingLight 1
      flareradius 0
      position 0 0 5
      orientation 0 0 0 0
      radius 14
      multiplier 1
      color 0 0 0
        // sul draw creo le luci
        end
    }
      end;
    end;
  end;
end;
function T3DModel.LoadAnimations(const fmodel: TextFile; MdlPath, SuperModelPath, SuperModel:string): Boolean;
var AnimFile: TextFile; aString: string;
begin

  while not Eof(fmodel) do begin
    Readln ( fmodel, aString);
    aString := TrimLeft(aString);
    if (Leftstr(  aString , 7) = 'newanim') then begin
      ProcessNewAnim  ( fmodel , aString );  // some model have  NULL supermodels, animation are inside here
    end;
  end;

  if supermodel <> 'NULL.mdl' then begin
    if FileExists(  MdlPath + supermodel ) then
      AssignFile(AnimFile,MdlPath + supermodel)
    else AssignFile(AnimFile,SuperModelPath + supermodel);
    Reset(AnimFile);
    while not Eof(AnimFile) do begin
      Readln ( AnimFile, aString);
      aString := TrimLeft(aString);

      if (Leftstr(  aString , 7) = 'newanim') then begin
        ProcessNewAnim  ( AnimFile , aString );  // some model have  NULL supermodels, animation are inside here
      end;
    end;
    CloseFile(AnimFile);
  end;



end;

procedure T3DModel.ProcessNewAnim  ( const fmodel: TextFile; FirstString: string );
var
  Anim:TAnimation;
  aString ,tmp: string;
  AnimatedObject : TAnimatedObject;
  object3d: T3DObject;
  TmpVector :TVector3D;
  PositionKey, OrientationKey : TAnimatedFrame;
  I,C,P: Integer;
  IndexAllpos: Integer;
  newPK: TAnimatedFrame;
  flog : TextFile;
begin
  AssignFile(flog, 'log.txt'); rewrite(flog);

  Anim :=  AddAnimation;
  Anim.FAnimationName := ExtractWordL (2,FirstString,' ');
  Anim.FAnimationModelName := ExtractWordL (3,FirstString,' ');

  while Leftstr(  aString , 8) <> 'doneanim' do begin
 //         if Anim.FAnimationName = 'ca1slashl' then asm int 3 ; end;
    Readln ( fModel, aString);  aString := TrimLeft(aString);
    if Leftstr(  aString , 6) ='length' then Anim.FLength := StrToFloat(ExtractWordL( 2,aString,' ' )) // Milliseconds
    else if Leftstr(  aString , 9) ='transtime' then Anim.FTranstime:= StrToFloat(ExtractWordL( 2,aString,' ' ))
    else if Leftstr(  aString , 8) ='animroot' then Anim.AnimationRoot:= ExtractWordL( 2,aString,' ' )
   // else if Leftstr(  aString , 8) ='event' then Anim.AnimationRoot:= ExtractWordL( 2,aString,' ' ) { TODO : event 0.5 hit }

    else if  (leftstr ( aString, 10) = 'node dummy' )or ( Leftstr(  aString , 12) = 'node trimesh')   or ( Leftstr(  aString , 15) = 'node danglymesh') then begin
      AnimatedObject:=Anim.AddAnimatedObject;
      AnimatedObject.ObjectName :=  ExtractWordL( 3,aString,' ' ) ;
     // if AnimatedObject.ObjectName ='a_ba' then
     //   AnimatedObject.ObjectName := objects[0].ObjectName;
    //      if AnimatedObject.ObjectName = 'Deer_Rfrontlowleg' then asm int 3 ; end;
      while Leftstr(  aString , 7) <> 'endnode' do begin
        Readln ( fModel, aString); aString := TrimLeft(aString);
        if Leftstr(  aString , 6) ='parent' then begin
          AnimatedObject.ParentAnimatedObjectName := ExtractWordL (2,aString,' ');
          if AnimatedObject.ParentAnimatedObjectName <> 'NULL' then begin
            AnimatedObject.ParentAnimatedObject := Anim.FindAnimatedObject(AnimatedObject.ParentAnimatedObjectName);
//            if AnimatedObject.ParentAnimatedObject = nil then asm int 3; end;
         //   OutputDebugString(PChar( 'anim= ' +Anim.FAnimationName + AnimatedObject.ObjectName  + ' parent=' + AnimatedObject.ParentAnimatedObjectName  ));
         //   if (Anim.FAnimationName='cdisappearlp') and  (AnimatedObject.ObjectName ='Deer_body') then  asm int 3 ; end;

            AnimatedObject.ParentAnimatedObject.AddChildren ( AnimatedObject ) ;
          end;

  //    if AnimatedObject.ParentAnimatedObjectName ='a_ba' then
   //     AnimatedObject.ParentAnimatedObjectName := objects[0].ObjectName;
        end
        else if Leftstr(  aString , 11) ='positionkey' then begin
          SetLength( AnimatedObject.PositionKeys, StrToInt( ExtractWordL( 2,aString,' ' )) );

          for I:= 0 to AnimatedObject.PositionKeyCount -1 do begin
            Readln ( fModel, aString); aString := TrimLeft(aString);
            AnimatedObject.PositionKeys[i].KeyTime := StrToFloat( ExtractWordL( 1,aString,' '));
            tmp := ExtractWordL( 2,aString,' ') + ' ' +ExtractWordL( 3,aString,' ') +' ' + ExtractWordL( 4,aString,' ');
            AnimatedObject.PositionKeys[i].KeyValue := MakeVector3D ( tmp );
          end;
        end
        else if Leftstr(  aString , 14) ='orientationkey' then begin
          SetLength( AnimatedObject.OrientationKeys, StrToInt( ExtractWordL( 2,aString,' ' )) );
          for I:= 0 to AnimatedObject.OrientationKeyCount -1 do begin
            Readln ( fModel, aString); aString := TrimLeft(aString);

            AnimatedObject.orientationKeys[i].KeyTime := StrToFloat( ExtractWordL( 1,aString,' '));
            tmp := ExtractWordL( 2,aString,' ') + ' ' +ExtractWordL( 3,aString,' ') +' ' + ExtractWordL( 4,aString,' ');
            AnimatedObject.orientationKeys[i].KeyValue := MakeVector3D ( tmp );
            AnimatedObject.orientationKeys[i].Angle := StrToFloat( ExtractWordL( 5,aString,' '));

          end;
        end;
      end
    end;
  end;

  for I := 0 to Anim.AnimatedObjectCount -1 do begin   // individuo chi ha i positionKeys
    if Anim.AnimatedObjects[i].PositionKeyCount > 0 then begin
      IndexAllpos :=I;
      Break;
    end;
  end;
{  for C := 0 to Anim.AnimatedObjects[IndexAllpos].Children.Count -1 do begin
    for P := 0 to Anim.AnimatedObjects[IndexAllpos].PositionKeyCount -1 do begin
      newPK := Anim.AnimatedObjects[C].AddPositionKey;
      newPK.KeyTime := Anim.AnimatedObjects[IndexAllpos].PositionKeys[P].KeyTime;
      newPK.KeyValue := Anim.AnimatedObjects[IndexAllpos].PositionKeys[P].KeyValue;
    end;
  end;    }
  for I := 0 to Anim.AnimatedObjectCount -1 do begin
    if I <> IndexAllpos then begin
      SetLength( Anim.AnimatedObjects[I].PositionKeys, Anim.AnimatedObjects[IndexAllpos].PositionKeyCount );
      for P := 0 to Anim.AnimatedObjects[IndexAllpos].PositionKeyCount -1 do begin
        Anim.AnimatedObjects[I].PositionKeys[P].KeyTime := Anim.AnimatedObjects[IndexAllpos].PositionKeys[P].KeyTime;
        Anim.AnimatedObjects[I].PositionKeys[P].KeyValue := Anim.AnimatedObjects[IndexAllpos].PositionKeys[P].KeyValue;
//        writeln ( flog,  ' object: ' + Anim.AnimatedObjects[I].ObjectName );
//        writeln ( flog,  'time: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyTime));
//        writeln ( flog,  'positionkeyX: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyValue.X));
//        writeln ( flog,  'positionkeyY: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyValue.Y));
//        writeln ( flog,  'positionkeyZ: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyValue.Z));
      end;
    end;
  end;

  for I := 0 to Anim.AnimatedObjectCount -1 do begin
      for P := 0 to Anim.AnimatedObjects[I].OrientationKeyCount -1 do begin
   //     writeln ( flog,  ' object: ' + Anim.AnimatedObjects[I].ObjectName );
    //    writeln ( flog,  'time: ' +  FloatToStr(Anim.AnimatedObjects[I].OrientationKeys[P].KeyTime));
    //    writeln ( flog,  'orientationkeyX: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].KeyValue.X));
    //    writeln ( flog,  'orientationkeyY: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].KeyValue.Y));
     //   writeln ( flog,  'orientationkeyZ: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].KeyValue.Z));
      end;
  end;


  CloseFile (flog);

end;
function T3DModel.MakeVector3D (aString: string ): TVector3D;
begin

  Result.x := StrToFloat( ExtractWordL (1,aString,' '));
  Result.y := StrToFloat( ExtractWordL (2,aString,' '));
  Result.z := StrToFloat( ExtractWordL (3,aString,' '));

end;
function T3DModel.MakeVector3Dp (aString: string ): TVector3D;
begin

  Result.x := StrToFloat( ExtractWordL (2,aString,' '));
  Result.y := StrToFloat( ExtractWordL (3,aString,' '));
  Result.z := StrToFloat( ExtractWordL (4,aString,' '));

end;
function T3DModel.MakeFace (aString: string): TFace;
begin
  Result.VertIndex[0] :=StrToInt( ExtractWordL (1,aString,' '));
  Result.VertIndex[1] :=StrToInt( ExtractWordL (2,aString,' '));
  Result.VertIndex[2] :=StrToInt( ExtractWordL (3,aString,' '));
end;
function T3DModel.MakeVector2D (aString: string): TVector2D;
begin
  Result.x := StrToFloat( ExtractWordL (1,aString,' '));
  Result.y := StrToFloat( ExtractWordL (2,aString,' '));

end;
initialization
  Randomize;


end.

