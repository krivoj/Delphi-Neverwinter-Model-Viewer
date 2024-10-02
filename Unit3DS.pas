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
  T3dObject = class;
  TAnimatedObject = class;
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
    ParentAnimatedObject3d : T3DObject;
    PositionKeys: array of TAnimatedFrame;
    OrientationKeys: array of TAnimatedFrame;
    CurrentTime : single;
    AnimLength: Single;
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
    FOldPosition,FPosition,FOrientation : TVector3D;
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
    FDebug : Boolean;
    Verts:array of TVector3D;
    Normals:array of TVector3D;
    TexVerts:array of TVector2D;
    Faces:array of TFace;
    Children : TObjectlist <T3DObject>;
    CurrentAnimatedObject : TAnimatedObject;
    AnimationIndex : Integer; // uso futuro  { TODO : fare questa versione eliminare switchanimation }
    //    Animation : Tlist of TObject3dAnimationInfo  -->  PositionKeys: array of TAnimatedFrame; OrientationKeys: array of TAnimatedFrame;
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
    property OldPosition : TVector3D read fOldPosition write fOldPosition;
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
    FActiveAnimation: TAnimation;
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
    procedure SwitchAnimation ( const value:string);
  public
    Root : T3DObject;
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
    function FindObject(const aName:string):T3DObject;
    function FindAnimation(const aName:string):TAnimation;
    function Select(const Index:Integer):T3DObject;
    procedure Anim (ms: Single) ;
    procedure Draw;
    property ObjectCount:Integer read GetObjectCount;
    property MaterialCount:Integer read GetMaterialCount;
    property AnimationCount:Integer read GetAnimationCount;
    property ActiveAnimationName: string read FActiveAnimationName write SwitchAnimation;

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
function MakeVector3Df ( x , y , z : single  ): TVector3D;
function MakeVector3D (aString: string ): TVector3D;
function MakeVector3Dp (aString: string ): TVector3D;
function MakeFace (aString: string): TFace;
function MakeVector2D (aString: string): TVector2D;




implementation

uses SysUtils, OpenGL, Textures, Math{unit1};
Const DosDelimSet  : set of AnsiChar = ['\', ':', #0];
Const stMaxFileLen  = 260;

function MakeVector3Df ( x , y , z : single  ): TVector3D;
begin

  Result.x := x;
  Result.y := y;
  Result.z := z;

end;

function MakeVector3D (aString: string ): TVector3D;
begin

  Result.x := StrToFloat( ExtractWordL (1,aString,' '));
  Result.y := StrToFloat( ExtractWordL (2,aString,' '));
  Result.z := StrToFloat( ExtractWordL (3,aString,' '));

end;
function MakeVector3Dp (aString: string ): TVector3D;
begin

  Result.x := StrToFloat( ExtractWordL (2,aString,' '));
  Result.y := StrToFloat( ExtractWordL (3,aString,' '));
  Result.z := StrToFloat( ExtractWordL (4,aString,' '));

end;
function MakeFace (aString: string): TFace;
begin
  Result.VertIndex[0] :=StrToInt( ExtractWordL (1,aString,' '));
  Result.VertIndex[1] :=StrToInt( ExtractWordL (2,aString,' '));
  Result.VertIndex[2] :=StrToInt( ExtractWordL (3,aString,' '));
end;
function MakeVector2D (aString: string): TVector2D;
begin
  Result.x := StrToFloat( ExtractWordL (1,aString,' '));
  Result.y := StrToFloat( ExtractWordL (2,aString,' '));

end;

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
var i: integer;
begin
  for i := low(AnimatedObjects) to high(AnimatedObjects) do begin
    AnimatedObjects[i].free;
  end;
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
  CurrentAnimatedObject := TAnimatedObject.Create;
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
  CurrentAnimatedObject.Free;
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

  for I:=0 to FaceCount-1 do begin
     vPoly[0]:=Verts[Faces[I].VertIndex[0]];
     vPoly[1]:=Verts[Faces[I].VertIndex[1]];
     vPoly[2]:=Verts[Faces[I].VertIndex[2]];

     V1 := VectorSub(vPoly[0], vPoly[2]);
     V2 := VectorSub(vPoly[2], vPoly[1]);

     Normal:=VectorCrossProduct(V1, V2);
     TempNormals[I]:=Normal;
   //VectorNormalize(Normal);
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
z : string;
flog : TextFile;
matrixMV: array of Single;
xCenter,yCenter,zCenter: Single;

begin

  //Anim (20);
//  if ( FObjectName ='Deer_body') and (fdebug) then asm int 3 ; end;

 { if ( FObjectName ='Deer_body') and (fdebug) then begin
    AssignFile(flog, 'logDeer_body.txt');
    Append ( flog);
    writeln ( flog, 'X: ' + FloatToStr(TTransformation(FTransformList[2]).Fx) + ' Y: ' +  FloatToStr(TTransformation(FTransformList[2]).Fy) + ' Z: ' + FloatToStr(TTransformation(FTransformList[2]).Fz) + ' A: ' + FloatToStr(TTransformation(FTransformList[2]).Angle)   );
    CloseFile(flog);
  end; }

  FTransformList.Push;
  //if FSelected then DrawBox;
  AdjustNormals;
  Material.SetMaterial;
  glPushName(FObjectIndex);
  glBegin(FRMode);
  glEnable( GL_TEXTURE_2D );
  //glEnable( GL_TEXTURE_GEN_T );

    for F:=0 to FaceCount-1 do begin

      for iVertex:=0 to 2 do  begin
        PointIndex:=Faces[F].VertIndex[iVertex];
        glNormal3f(Normals[PointIndex].X, Normals[PointIndex].Y, Normals[PointIndex].Z);
        if Material.HasTexture then
         glTexCoord2f(TexVerts[PointIndex].X, TexVerts[PointIndex].Y)
          else
           glColor(Material.Diffuse.Vector.Red, Material.Diffuse.Vector.Green, Material.Diffuse.Vector.Blue);
        glVertex3f(Verts[PointIndex].X, Verts[PointIndex].Y, Verts[PointIndex].Z);

      end;
    end;
  glEnd;
  glPopName;
  FTransformList.Pop;

//        Position := makevector3df ( Verts[PointIndex].X , Verts[PointIndex].Y,Verts[PointIndex].Z);
 //       Position := makevector3df ( Normals[PointIndex].X, Normals[PointIndex].Y, Normals[PointIndex].Z);
//float modelview[16];
 SetLength(matrixMV,0);
SetLength(matrixMV,16);

  glGetFloatv(GL_MODELVIEW_MATRIX, @matrixMV[0]);
 xCenter := matrixMV[12];
 yCenter := matrixMV[13];
 zCenter := matrixMV[14];
 Position := makevector3df (xCenter , yCenter, zCenter);

end;
procedure T3DObject.Anim( ms: Single);
var
  ao, pk,i,c,ok:Integer; TmpVector: TVector3D; flog: TextFile; DeltaTime, fraction: Single;
//  m_rel,	m_frame : clsMatrix;
    	pPosition : array [0..2] of single;
  	rRotation : array [0..3] of single;
    startRotation : array [0..2] of single;
	//	m_relative : clsMatrix;				// fixed transformation matrix relative to parent
	//	m_final : clsMatrix;				  // absolute in accordance to animation
		startPosition : array [0..2] of single;
    tempm : array [0..15] of single;
    aRoot : T3DObject;
    aQuaternion: Tquaternion;
    p,o:Integer;
    CurrentPosition: TVector3D;
    CurrentOrientation: TVector4D;
    label rotation, normalo, normalp;
begin
//    AssignFile(flog, 'log.txt'); Append(flog);
 // if objectname ='Deer_neckend' then asm Int 3; end;

      //    if (ObjectName = 'Deer_body') and (CurrentAnimatedObject.OrientationKeyCount > 0 )   then asm int 3 ; end;
//  CurrentAnimatedObject := Animations[AnimationIndex] uso futuro
  FElapsedTime := FElapsedTime + ms;
  if FModel.AnimationCount <= 0 then Exit;
    //  if 'Deer_Rfrontlowleg' = FObjectName then asm int 3; end;
        //  if ObjectName = 'Deer_body' then asm int 3 ; end;

  //  pk := Trunc(Ms);
  //  ok:= Trunc(MS);

  PPosition[0]:=0;
  PPosition[1]:=0;
  PPosition[2]:=0;

  rRotation[0]:=0;
  rRotation[1]:=0;
  rRotation[2]:=0;
  rRotation[3]:=0;

 // ms := ms / 4;
  CurrentAnimatedObject.CurrentTime := CurrentAnimatedObject.CurrentTime + ms;
  oldPosition := Position;

  if CurrentAnimatedObject.PositionKeyCount = 0 then begin
    goto rotation;
  end;
	i := 0;
	while( (i < CurrentAnimatedObject.PositionKeyCount-1) and (CurrentAnimatedObject.PositionKeys[i].KeyTime < CurrentAnimatedObject.CurrentTime) ) do
		i := i + 1;

	if i > CurrentAnimatedObject.PositionKeyCount then begin
    goto rotation;
  end;

  if CurrentAnimatedObject.CurrentTime > CurrentAnimatedObject.Animlength then begin
    goto rotation;
  end;

  if( i > 0 ) then 	begin

    CurrentPosition.X :=0;
    CurrentPosition.Y :=0;
    CurrentPosition.Z :=0;
    for p := 0 to CurrentAnimatedObject.PositionKeyCount -1 do begin
      CurrentPosition.X := CurrentPosition.X + CurrentAnimatedObject.PositionKeys[p].KeyValue.X;
      CurrentPosition.Y := CurrentPosition.Y + CurrentAnimatedObject.PositionKeys[p].KeyValue.Y;
      CurrentPosition.Z := CurrentPosition.Z + CurrentAnimatedObject.PositionKeys[p].KeyValue.Z;
    end;

		// Interpolate between 2 key frames

		// time between the 2 key frames
		deltaTime := CurrentAnimatedObject.PositionKeys[i].KeyTime - CurrentAnimatedObject.PositionKeys[i-1].KeyTime;
    if deltaTime <= 0 then Exit;

		// relative position of interpolation point to the keyframes [0..1]
		fraction := (CurrentAnimatedObject.CurrentTime - CurrentAnimatedObject.PositionKeys[i-1].KeyTime) / deltaTime;
    if (fraction <=0) or (fraction >=1) then goto normalp;

		//PPosition[0] := CurrentAnimatedObject.PositionKeys[i-1].KeyValue.X + fraction * (CurrentAnimatedObject.PositionKeys[i].KeyValue.X - CurrentAnimatedObject.PositionKeys[i-1].KeyValue.X);
		//PPosition[1] := CurrentAnimatedObject.PositionKeys[i-1].KeyValue.Y + fraction * (CurrentAnimatedObject.PositionKeys[i].KeyValue.Y - CurrentAnimatedObject.PositionKeys[i-1].KeyValue.Y);
		//PPosition[2] := CurrentAnimatedObject.PositionKeys[i-1].KeyValue.Z + fraction * (CurrentAnimatedObject.PositionKeys[i].KeyValue.Z - CurrentAnimatedObject.PositionKeys[i-1].KeyValue.Z);

		PPosition[0] := CurrentPosition.X  + fraction * (CurrentAnimatedObject.PositionKeys[i].KeyValue.X - CurrentAnimatedObject.PositionKeys[i-1].KeyValue.X);
		PPosition[1] := CurrentPosition.Y  + fraction * (CurrentAnimatedObject.PositionKeys[i].KeyValue.Y - CurrentAnimatedObject.PositionKeys[i-1].KeyValue.Y);
		PPosition[2] := CurrentPosition.Z  + fraction * (CurrentAnimatedObject.PositionKeys[i].KeyValue.Z - CurrentAnimatedObject.PositionKeys[i-1].KeyValue.Z);
  end
   else begin
normalp:
    PPosition[0] :=  fmodel.root.Position.X + CurrentAnimatedObject.PositionKeys[i].KeyValue.X;
    PPosition[1] :=  fmodel.root.Position.Y + CurrentAnimatedObject.PositionKeys[i].KeyValue.Y;
    PPosition[2] :=  CurrentAnimatedObject.PositionKeys[i].KeyValue.Z;
  end;
  Rotation:

  //if ParentObject <> nil then begin

  //  PPosition[0] := PPosition[0] + ParentObject.Position.X;
 //   PPosition[1] := PPosition[1] + ParentObject.Position.Y;
  //  PPosition[2] := ParentObject.Position.Z;
 // end;

  if (PPosition[0] <> 0) or (PPosition[1] <> 0) or (PPosition[2] <> 0 ) then begin
    TTransformation(TransformList.Items[0]).Angle := 0  ; //
    TTransformation(TransformList.Items[0]).X := PPosition[0];
    TTransformation(TransformList.Items[0]).Y := PPosition[1];
    TTransformation(TransformList.Items[0]).Z := PPosition[2];
  end;

//  Position := VectorAdd( OldPosition , MakeVector3Df ( PPosition[0],PPosition[1],PPosition[2]));
//  if (Position.X <> oldposition.x)  or (Position.y <> oldposition.y) or (Position.z <> oldposition.z) then asm int 3 ; end;



//    Position := makevector3df ( Verts[PointIndex].X , Verts[PointIndex].Y,Verts[PointIndex].Z);
//        Position := makevector3df ( Normals[PointIndex].X, Normals[PointIndex].Y, Normals[PointIndex].Z);

     outputdebugstring (pchar(  FloatToStr(oldPosition.X)  + FloatToStr(oldPosition.Y) + FloatToStr(oldPosition.Z)) );
         //  for C := 0 to ChildrenCount -1 do begin
         //     Children[c].Position := VectorAdd ( Children[c].ParentObject.Position, Children[c].Position );
         //  end;

  { for C := 0 to ChildrenCount -1 do begin
      TTransformation(Children[c].TransformList[0]).X := PPosition[0];
      TTransformation(Children[c].TransformList[0]).Y := PPosition[1];
      TTransformation(Children[c].TransformList[0]).Z := PPosition[2];
   end;  }

        outputdebugstring (pchar(  'time: ' + FloatToStr(FElapsedTime)+ ' object: '+ FObjectName + ' orientationkeyIndex: '+IntToStr(ok-1)  ));

       //   if ObjectName = 'Deer_body' then asm int 3 ; end;
	// Find appropriate rotation key frame
  //CurrentAnimatedObject.CurrentTime := CurrentAnimatedObject.CurrentTime + ms;
  if CurrentAnimatedObject.OrientationKeyCount = 0 then begin
    CurrentAnimatedObject.CurrentTime := 0;
    Exit;
  end;
	i := 0;
	while( (i < CurrentAnimatedObject.OrientationKeyCount-1) and (CurrentAnimatedObject.orientationKeys[i].KeyTime < CurrentAnimatedObject.CurrentTime) ) do
		i := i + 1;

	if i > CurrentAnimatedObject.OrientationKeyCount then begin
    CurrentAnimatedObject.CurrentTime := 0;
    Exit;
  end;

  if CurrentAnimatedObject.CurrentTime > CurrentAnimatedObject.Animlength then begin
    CurrentAnimatedObject.CurrentTime := 0;
    Exit;
  end;

	if( i > 0 ) then 	begin
    CurrentOrientation.X :=0;
    CurrentOrientation.Y :=0;
    CurrentOrientation.Z :=0;
    CurrentOrientation.Angle :=0;
    for o := 0 to CurrentAnimatedObject.OrientationKeyCount -1 do begin
      CurrentOrientation.X := CurrentOrientation.X + CurrentAnimatedObject.OrientationKeys[o].KeyValue.X;
      CurrentOrientation.Y := CurrentOrientation.Y + CurrentAnimatedObject.OrientationKeys[o].KeyValue.Y;
      CurrentOrientation.Z := CurrentOrientation.Z + CurrentAnimatedObject.OrientationKeys[o].KeyValue.Z;
      CurrentOrientation.Angle := CurrentOrientation.Z + CurrentAnimatedObject.OrientationKeys[o].Angle;
    end;

		// Interpolate between 2 key frames

		// time between the 2 key frames
		deltaTime := CurrentAnimatedObject.orientationKeys[i].KeyTime - CurrentAnimatedObject.orientationKeys[i-1].KeyTime;
    if deltaTime <= 0 then Exit;

		// relative position of interpolation point to the keyframes [0..1]
		fraction := (CurrentAnimatedObject.CurrentTime - CurrentAnimatedObject.orientationKeys[i-1].KeyTime) / deltaTime;
    if (fraction <=0) or (fraction >=1) then goto normalo;

//		RRotation[0] := CurrentAnimatedObject.orientationKeys[i-1].KeyValue.X + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.X - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.X);
//		RRotation[1] := CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Y + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Y - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Y);
//		RRotation[2] := CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Z + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Z - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Z);
//		RRotation[3] := (CurrentAnimatedObject.orientationKeys[i].angle);

		RRotation[0] := CurrentOrientation.X + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.X - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.X);
		RRotation[1] := CurrentOrientation.Y + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Y - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Y);
		RRotation[2] := CurrentOrientation.Z + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Z - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Z);
		RRotation[3] := CurrentOrientation.Angle + (CurrentAnimatedObject.orientationKeys[i].angle);
   end
   else begin
normalo:
    RRotation[0] :=  CurrentAnimatedObject.orientationKeys[i].KeyValue.X;
    RRotation[1] :=  CurrentAnimatedObject.orientationKeys[i].KeyValue.Y;
    RRotation[2] :=  CurrentAnimatedObject.orientationKeys[i].KeyValue.Z;
    RRotation[3] :=  CurrentAnimatedObject.orientationKeys[i].angle;
  end;


    // TransformList.AddTransformEx( )   { TODO : transformlist dinamiche.clear allinizo }
  //  if (ObjectName = 'Deer_body') and (fdebug) then asm int 3 ; end;

  //  for I := 0 to 2 do begin       { TODO : 2! }
  //  TTransformation(TransformList.Items[i]).Angle := 0;
  //  TTransformation(TransformList.Items[i]).X := 0;
  //  TTransformation(TransformList.Items[i]).Y := 0;
  //  TTransformation(TransformList.Items[i]).Z := 0;

  //  end;

    if (RRotation[0] <> 0) or (RRotation[1] <> 0) or (RRotation[2] <> 0 ) then begin
    aQuaternion := MakeQuaternion  ( RRotation[0],RRotation[1],RRotation[2],RRotation[3]  );
    TTransformation(TransformList.Items[1]).Angle := RadToDeg(aQuaternion.A)  ; //
    TTransformation(TransformList.Items[1]).X := aQuaternion.x;
    TTransformation(TransformList.Items[1]).Y := aQuaternion.y;
    TTransformation(TransformList.Items[1]).Z := aQuaternion.z;
   // if Trunc(TTransformation(TransformList.Items[2]).Angle) = 57 then asm int 3; end;

     outputdebugstring (pchar(  FloatToStr(Position.X)  + FloatToStr(Position.Y) + FloatToStr(Position.Z)) );
    end;


 //   if (RRotation[0] <> 0) or (RRotation[1] <> 0) or (RRotation[2] <> 0 ) then begin
  //  aQuaternion := MakeQuaternion  ( CurrentAnimatedObject.ParentAnimatedObject3d.OldPosition.x ,CurrentAnimatedObject.ParentAnimatedObject3d.OldPosition.y,CurrentAnimatedObject.ParentAnimatedObject3d.OldPosition.z,RRotation[3]  );
  //  TTransformation(TransformList.Items[1]).Angle := RadToDeg(aQuaternion.A)  ; //
  //  TTransformation(TransformList.Items[1]).X := aQuaternion.x;
  //  TTransformation(TransformList.Items[1]).Y := aQuaternion.y;
  //  TTransformation(TransformList.Items[1]).Z := aQuaternion.z;
    // outputdebugstring (pchar(  FloatToStr(Position.X)  + FloatToStr(Position.Y) + FloatToStr(Position.Z)) );
  //  end;
      //  AdjustNormals;

    if FElapsedTime > FModel.Animations[FModel.FActiveAnimationIndex].FLength then
      FElapsedTime :=0;

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
procedure T3DModel.SwitchAnimation ( const value:string);
var
  i,ao,pk,ok,c: Integer;Animation:TAnimation;
  Anroot : TAnimatedObject;
  tmpVector : TVector3D;
  object3d,ParentObject3d : T3DObject;
  AnimatedObject,ParentAnimatedObject3d,aChild: TAnimatedObject;

begin
  FActiveAnimationName := value;
  for I := 0 to AnimationCount -1 do begin
    if Animations[i].FAnimationName = FActiveAnimationName then begin
      FActiveAnimation := Animations[i];
      FActiveAnimationIndex := I;
      Break;
    end;
  end;
  Animation :=  Animations[FActiveAnimationIndex];


   For ao := 0 to Animation.AnimatedObjectCount -1 do begin
     if Animation.AnimatedObjects [ao].ObjectName  = Animation.AnimationRoot then begin
      anroot := Animation.AnimatedObjects [ao];
      Root := FindObject( anroot.ObjectName );

     // Root := root.Children[0]; // FIX
      Break;
     end;
   end;

   // sommo i valori del rootdummy a tutti i children creando forzatamente le positionKey
   For ao := 0 to Animation.AnimatedObjectCount -1 do begin
     AnimatedObject:= Animation.AnimatedObjects [ao];
     if AnimatedObject <> AnRoot then begin
       SetLength( AnimatedObject.PositionKeys, AnRoot.PositionKeyCount); // forzo i positionCount
       for pk := 0 to AnimatedObject.PositionKeyCount-1 do begin
        // AnimatedObject.PositionKeys[pk] := Root.PositionKeys[pk];
       end;

     end;
   end;

   For ao := 0 to Animation.AnimatedObjectCount -1 do begin
     AnimatedObject:= Animation.AnimatedObjects [ao];

    // if ParentAnimatedObject3d <> nil then begin // prendo la position del parent
     if AnimatedObject <> AnRoot then begin
       ParentAnimatedObject3d := AnimatedObject.ParentAnimatedObject;
       if ParentAnimatedObject3d <> nil then begin
         for pk := 0 to AnimatedObject.PositionKeyCount -1 do begin    // sposto tutto insieme gltranslate
          AnimatedObject.PositionKeys[pk].KeyTime := ParentAnimatedObject3d.PositionKeys[pk].KeyTime   ;  // setto keytime e x,y,z
          object3d := FindObject( AnimatedObject.ObjectName );
          tmpVector := object3d.Position;

       //   AnimatedObject.PositionKeys[pk].KeyValue.X := AnimatedObject.PositionKeys[pk].KeyValue.X+Root.PositionKeys[pk].KeyValue.X + tmpVector.X;
        //  AnimatedObject.PositionKeys[pk].KeyValue.Y := AnimatedObject.PositionKeys[pk].KeyValue.Y+Root.PositionKeys[pk].KeyValue.Y + tmpVector.Y;
        //  AnimatedObject.PositionKeys[pk].KeyValue.Z := tmpVector.Z;   // per non farlo volare

//          AnimatedObject.PositionKeys[pk].KeyValue.X := ParentAnimatedObject3d.PositionKeys[pk].KeyValue.X;// + tmpVector.X;
//          AnimatedObject.PositionKeys[pk].KeyValue.Y := ParentAnimatedObject3d.PositionKeys[pk].KeyValue.Y;// + tmpVector.Y;
//          AnimatedObject.PositionKeys[pk].KeyValue.Z := tmpVector.Z;   // per non farlo volare

        //  AnimatedObject.PositionKeys[pk].KeyValue.X := AnimatedObject.PositionKeys[pk].KeyValue.X + tmpVector.X;
        //  AnimatedObject.PositionKeys[pk].KeyValue.Y := AnimatedObject.PositionKeys[pk].KeyValue.Y + tmpVector.Y;
        //  AnimatedObject.PositionKeys[pk].KeyValue.Z := tmpVector.Z;   // per non farlo volare
            if ParentObject3d <> nil then begin

           // AnimatedObject.PositionKeys[pk].KeyValue.X := AnimatedObject.PositionKeys[pk].KeyValue.X + ParentObject3d.Position.X;
           // AnimatedObject.PositionKeys[pk].KeyValue.Y := AnimatedObject.PositionKeys[pk].KeyValue.Y + ParentObject3d.Position.Y;
            end;
         end;
       end;
     end;

       for ok := 0 to AnimatedObject.OrientationKeyCount -1 do begin
       {  for c := 0 to AnimatedObject.Children.Count -1 do begin
          aChild := AnimatedObject.Children[c];
          aChild.OrientationKeys[ok].KeyValue.X := AnimatedObject.OrientationKeys[ok].KeyValue.X + aChild.OrientationKeys[ok].KeyValue.X ;
          aChild.OrientationKeys[ok].KeyValue.Y := AnimatedObject.OrientationKeys[ok].KeyValue.Y + aChild.OrientationKeys[ok].KeyValue.Y ;
          aChild.OrientationKeys[ok].KeyValue.Z := AnimatedObject.OrientationKeys[ok].KeyValue.Z + aChild.OrientationKeys[ok].KeyValue.Z ;
          aChild.OrientationKeys[ok].Angle := AnimatedObject.OrientationKeys[ok].KeyValue.Z + aChild.OrientationKeys[ok].Angle ;

         end; }


         // se ho un parent gli sommo le orientation
       {  if ParentAnimatedObject3d.OrientationKeyCount > ok then begin   // se esiste quel orientationKey (il parent può avere una animazione più corta)
          object3d := FindObject( AnimatedObject.ObjectName );
          tmpVector := object3d.Orientation;
          AnimatedObject.OrientationKeys[ok].KeyValue.X := AnimatedObject.OrientationKeys[ok].KeyValue.X + ParentAnimatedObject3d.OrientationKeys[ok].KeyValue.X;// + tmpVector.X;
          AnimatedObject.OrientationKeys[ok].KeyValue.Y := AnimatedObject.OrientationKeys[ok].KeyValue.Y + ParentAnimatedObject3d.OrientationKeys[ok].KeyValue.Y;// + tmpVector.Y;
          AnimatedObject.OrientationKeys[ok].KeyValue.Z := AnimatedObject.OrientationKeys[ok].KeyValue.Z + ParentAnimatedObject3d.OrientationKeys[ok].KeyValue.Z;// + tmpVector.Z;
          AnimatedObject.OrientationKeys[ok].Angle := ParentAnimatedObject3d.OrientationKeys[ok].Angle;
         end;  }

         { object3d := FindObject( AnimatedObject.ObjectName );
          tmpVector := object3d.Orientation;
          AnimatedObject.OrientationKeys[ok].KeyValue.X := AnimatedObject.OrientationKeys[ok].KeyValue.X + tmpVector.X;
          AnimatedObject.OrientationKeys[ok].KeyValue.Y := AnimatedObject.OrientationKeys[ok].KeyValue.Y + tmpVector.Y;
          AnimatedObject.OrientationKeys[ok].KeyValue.Z := AnimatedObject.OrientationKeys[ok].KeyValue.Z + tmpVector.Z;
          AnimatedObject.OrientationKeys[ok].Angle := AnimatedObject.OrientationKeys[ok].Angle;}
        {  object3d := FindObject( AnimatedObject.ObjectName );
          tmpVector := object3d.Orientation;
          AnimatedObject.OrientationKeys[ok].KeyValue.X := AnimatedObject.OrientationKeys[ok].KeyValue.X + root.OrientationKeys[ok].KeyValue.x;
          AnimatedObject.OrientationKeys[ok].KeyValue.Y := AnimatedObject.OrientationKeys[ok].KeyValue.Y + root.OrientationKeys[ok].KeyValue.y;
          AnimatedObject.OrientationKeys[ok].KeyValue.Z := AnimatedObject.OrientationKeys[ok].KeyValue.Z + root.OrientationKeys[ok].KeyValue.z;
          AnimatedObject.OrientationKeys[ok].Angle := AnimatedObject.OrientationKeys[ok].Angle + root.OrientationKeys[ok].Angle; }


       end;
    end;



  // copio dalla Animation.AnimatedObjects le informazioni scritte sopra nella objects reale.
  // in pratica Object3d.CurrentAnimatedObject sono le informazione che il render tratta.
  // le modifiche all'animazione le faccio sopra, questo è solo il risultato finale

    Object3d.CurrentAnimatedObject.Clear;

    For ao := 0 to Animation.AnimatedObjectCount -1 do begin
      AnimatedObject:= Animation.AnimatedObjects [ao];
      object3d := FindObject( AnimatedObject.ObjectName );

      if object3d <> nil then begin

  //        if Objects[i].ObjectName = 'Deer_body' then asm int 3 ; end;
         Object3d.CurrentAnimatedObject.CurrentTime := 0;
         Object3d.CurrentAnimatedObject.AnimLength := Animation.FLength;
         Object3d.CurrentAnimatedObject.ParentAnimatedObject3d:= FindObject( AnimatedObject.ObjectName );

         SetLength( Object3d.CurrentAnimatedObject.PositionKeys, AnimatedObject.PositionKeyCount);
         for pk := 0 to AnimatedObject.PositionKeyCount -1 do begin
          Object3d.CurrentAnimatedObject.PositionKeys[pk].KeyTime := AnimatedObject.PositionKeys[pk].KeyTime;
          Object3d.CurrentAnimatedObject.PositionKeys[pk].KeyValue := AnimatedObject.PositionKeys[pk].KeyValue;
          Object3d.CurrentAnimatedObject.PositionKeys[pk].Angle := AnimatedObject.PositionKeys[pk].Angle;
         end;
         SetLength( Object3d.CurrentAnimatedObject.orientationKeys, AnimatedObject.orientationKeyCount);
         for ok := 0 to AnimatedObject.orientationKeyCount -1 do begin
          Object3d.CurrentAnimatedObject.orientationKeys[ok].KeyTime := AnimatedObject.orientationKeys[ok].KeyTime;
          Object3d.CurrentAnimatedObject.orientationKeys[ok].KeyValue := AnimatedObject.orientationKeys[ok].KeyValue;
          Object3d.CurrentAnimatedObject.orientationKeys[ok].Angle := AnimatedObject.orientationKeys[ok].Angle;
         end;


        // Continue;
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
var
  ao, i,c,ok:Integer; TmpVector: TVector3D; flog: TextFile; DeltaTime, fraction: Single;
//  m_rel,	m_frame : clsMatrix;
    	pPosition : array [0..2] of single;
  	rRotation : array [0..3] of single;
    startRotation : array [0..2] of single;
	//	m_relative : clsMatrix;				// fixed transformation matrix relative to parent
	//	m_final : clsMatrix;				  // absolute in accordance to animation
		startPosition : array [0..2] of single;
    tempm : array [0..15] of single;
    aRoot : T3DObject;
    aQuaternion: Tquaternion;
    frame,pp,o:Integer;
    CurrentPosition: TVector3D;
    CurrentOrientation: TVector4D;
    label rotation, normalo;
begin
 { TODO : MDl PASSA i MS non pk, pk lo devo calcolare io }
  for I := Low(Objects) to High(Objects) do  begin

    if Objects[i].CurrentAnimatedObject.PositionKeyCount = 0 then
      goto rotation;

    frame := 0;
//    while( (p < Objects[i].CurrentAnimatedObject.PositionKeyCount-1) and (Objects[i].CurrentAnimatedObject.PositionKeys[p].KeyTime < Objects[i].CurrentAnimatedObject.CurrentTime) ) do
//      p := p + 1;
    frame:= 4;
    if frame > Objects[i].CurrentAnimatedObject.PositionKeyCount then goto rotation;

    if Objects[i].CurrentAnimatedObject.CurrentTime > Objects[i].CurrentAnimatedObject.Animlength then goto rotation;


    if( frame > 0 ) then 	begin
		// Interpolate between 2 key frames

		// time between the 2 key frames
//		deltaTime := Objects[i].CurrentAnimatedObject.PositionKeys[p].KeyTime - Objects[i].CurrentAnimatedObject.PositionKeys[p-1].KeyTime;
//    if deltaTime <= 0 then Exit;

		// relative position of interpolation point to the keyframes [0..1]
		fraction := (Objects[i].CurrentAnimatedObject.CurrentTime - Objects[i].CurrentAnimatedObject.PositionKeys[frame-1].KeyTime) / deltaTime;
   // if (fraction <=0) or (fraction >=1) then goto normalp;

        CurrentPosition.X := CurrentPosition.X +  Objects[i].CurrentAnimatedObject.PositionKeys[frame].KeyValue.X;
        CurrentPosition.Y := CurrentPosition.Y +  Objects[i].CurrentAnimatedObject.PositionKeys[frame].KeyValue.Y;
        CurrentPosition.Z := CurrentPosition.Z +  Objects[i].CurrentAnimatedObject.PositionKeys[frame].KeyValue.Z;
        for c := 0 to Objects[i].ChildrenCount -1 do begin
          Objects[i].Children[c].CurrentAnimatedObject.PositionKeys[frame].KeyValue.X := Objects[i].Children[c].CurrentAnimatedObject.PositionKeys[frame].KeyValue.X + CurrentPosition.X;
          Objects[i].Children[c].CurrentAnimatedObject.PositionKeys[frame].KeyValue.y := Objects[i].Children[c].CurrentAnimatedObject.PositionKeys[frame].KeyValue.X + CurrentPosition.y;
          Objects[i].Children[c].CurrentAnimatedObject.PositionKeys[frame].KeyValue.z := Objects[i].Children[c].CurrentAnimatedObject.PositionKeys[frame].KeyValue.X + CurrentPosition.z;

        end;

  Rotation:

  //if (PPosition[0] <> 0) or (PPosition[1] <> 0) or (PPosition[2] <> 0 ) then begin
    TTransformation(Objects[i].TransformList.Items[0]).Angle := 0  ; //
    TTransformation(Objects[i].TransformList.Items[0]).X := CurrentPosition.X;
    TTransformation(Objects[i].TransformList.Items[0]).Y := CurrentPosition.Y;
    TTransformation(Objects[i].TransformList.Items[0]).Z := CurrentPosition.Z;
 // end;

//  Position := VectorAdd( OldPosition , MakeVector3Df ( PPosition[0],PPosition[1],PPosition[2]));
//  if (Position.X <> oldposition.x)  or (Position.y <> oldposition.y) or (Position.z <> oldposition.z) then asm int 3 ; end;



//    Position := makevector3df ( Verts[PointIndex].X , Verts[PointIndex].Y,Verts[PointIndex].Z);
//        Position := makevector3df ( Normals[PointIndex].X, Normals[PointIndex].Y, Normals[PointIndex].Z);

   //  outputdebugstring (pchar(  FloatToStr(oldPosition.X)  + FloatToStr(oldPosition.Y) + FloatToStr(oldPosition.Z)) );
         //  for C := 0 to ChildrenCount -1 do begin
         //     Children[c].Position := VectorAdd ( Children[c].ParentObject.Position, Children[c].Position );
         //  end;

  { for C := 0 to ChildrenCount -1 do begin
      TTransformation(Children[c].TransformList[0]).X := PPosition[0];
      TTransformation(Children[c].TransformList[0]).Y := PPosition[1];
      TTransformation(Children[c].TransformList[0]).Z := PPosition[2];
   end;  }

     //   outputdebugstring (pchar(  'time: ' + FloatToStr(FElapsedTime)+ ' object: '+ FObjectName + ' orientationkeyIndex: '+IntToStr(ok-1)  ));

       //   if ObjectName = 'Deer_body' then asm int 3 ; end;
	// Find appropriate rotation key frame
  //CurrentAnimatedObject.CurrentTime := CurrentAnimatedObject.CurrentTime + ms;
  if Objects[i].CurrentAnimatedObject.OrientationKeyCount = 0 then begin
    Objects[i].CurrentAnimatedObject.CurrentTime := 0;
    Exit;
  end;

	frame := 0;
	while( (i < Objects[i].CurrentAnimatedObject.OrientationKeyCount-1) and (Objects[i].CurrentAnimatedObject.orientationKeys[frame].KeyTime < Objects[i].CurrentAnimatedObject.CurrentTime) ) do
		frame := frame + 1;

  frame:=4;
	if frame > Objects[i].CurrentAnimatedObject.OrientationKeyCount then begin
    Objects[i].CurrentAnimatedObject.CurrentTime := 0;
    Exit;
  end;

  if Objects[i].CurrentAnimatedObject.CurrentTime > Objects[i].CurrentAnimatedObject.Animlength then begin
    Objects[i].CurrentAnimatedObject.CurrentTime := 0;
    Exit;
  end;

	if( frame > 0 ) then 	begin
    CurrentOrientation.X :=0;
    CurrentOrientation.Y :=0;
    CurrentOrientation.Z :=0;
    CurrentOrientation.Angle :=0;
		// Interpolate between 2 key frames

		// time between the 2 key frames
	 //	deltaTime := Objects[i].CurrentAnimatedObject.orientationKeys[p].KeyTime - Objects[i].CurrentAnimatedObject.orientationKeys[p-1].KeyTime;
   // if deltaTime <= 0 then Exit;

		// relative position of interpolation point to the keyframes [0..1]
	 //	fraction := (Objects[i].CurrentAnimatedObject.CurrentTime - Objects[i].CurrentAnimatedObject.orientationKeys[p-1].KeyTime) / deltaTime;
   // if (fraction <=0) or (fraction >=1) then goto normalo;

  //  for o := 0 to Objects[i].CurrentAnimatedObject.OrientationKeyCount -1 do begin
      CurrentOrientation.X := CurrentOrientation.X + Objects[i].CurrentAnimatedObject.OrientationKeys[frame].KeyValue.X;
      CurrentOrientation.Y := CurrentOrientation.Y + Objects[i].CurrentAnimatedObject.OrientationKeys[frame].KeyValue.Y;
      CurrentOrientation.Z := CurrentOrientation.Z + Objects[i].CurrentAnimatedObject.OrientationKeys[frame].KeyValue.Z;
      CurrentOrientation.Angle := CurrentOrientation.Z + Objects[i].CurrentAnimatedObject.OrientationKeys[o].Angle;
  //  end;


//		RRotation[0] := CurrentAnimatedObject.orientationKeys[i-1].KeyValue.X + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.X - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.X);
//		RRotation[1] := CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Y + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Y - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Y);
//		RRotation[2] := CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Z + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Z - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Z);
//		RRotation[3] := (CurrentAnimatedObject.orientationKeys[i].angle);

//		RRotation[0] := CurrentOrientation.X + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.X - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.X);
//		RRotation[1] := CurrentOrientation.Y + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Y - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Y);
//		RRotation[2] := CurrentOrientation.Z + fraction * (CurrentAnimatedObject.orientationKeys[i].KeyValue.Z - CurrentAnimatedObject.orientationKeys[i-1].KeyValue.Z);
//		RRotation[3] := CurrentOrientation.Angle + (CurrentAnimatedObject.orientationKeys[i].angle);
 //  end
  // else begin
normalo:
   // RRotation[0] :=  CurrentAnimatedObject.orientationKeys[i].KeyValue.X;
   // RRotation[1] :=  CurrentAnimatedObject.orientationKeys[i].KeyValue.Y;
   // RRotation[2] :=  CurrentAnimatedObject.orientationKeys[i].KeyValue.Z;
   // RRotation[3] :=  CurrentAnimatedObject.orientationKeys[i].angle;
  //end;


    // TransformList.AddTransformEx( )   { TODO : transformlist dinamiche.clear allinizo }
  //  if (ObjectName = 'Deer_body') and (fdebug) then asm int 3 ; end;

  //  for I := 0 to 2 do begin       { TODO : 2! }
  //  TTransformation(TransformList.Items[i]).Angle := 0;
  //  TTransformation(TransformList.Items[i]).X := 0;
  //  TTransformation(TransformList.Items[i]).Y := 0;
  //  TTransformation(TransformList.Items[i]).Z := 0;

  //  end;

    //if (RRotation[0] <> 0) or (RRotation[1] <> 0) or (RRotation[2] <> 0 ) then begin
    //FORSE QUI VA BENE   !!!!!!   --->   aQuaternion := MakeQuaternion  ( RRotation[0],RRotation[1],RRotation[2],RRotation[3]  );
    TTransformation(Objects[i].TransformList.Items[1]).Angle := RadToDeg(aQuaternion.A)  ; //
    TTransformation(Objects[i].TransformList.Items[1]).X := CurrentOrientation.x;
    TTransformation(Objects[i].TransformList.Items[1]).Y := CurrentOrientation.y;
    TTransformation(Objects[i].TransformList.Items[1]).Z := CurrentOrientation.z;

  end;
 end;
end;



    //    TTransformation(Objects[i].TransformList.Items[1]).X := aQuaternion.x;
//    TTransformation(Objects[i].TransformList.Items[1]).Y := aQuaternion.y;
//    TTransformation(Objects[i].TransformList.Items[1]).Z := aQuaternion.z;
   // if Trunc(TTransformation(TransformList.Items[2]).Angle) = 57 then asm int 3; end;

   //  outputdebugstring (pchar(  FloatToStr(Position.X)  + FloatToStr(Position.Y) + FloatToStr(Position.Z)) );
   // end;


 //   if (RRotation[0] <> 0) or (RRotation[1] <> 0) or (RRotation[2] <> 0 ) then begin
  //  aQuaternion := MakeQuaternion  ( CurrentAnimatedObject.ParentAnimatedObject3d.OldPosition.x ,CurrentAnimatedObject.ParentAnimatedObject3d.OldPosition.y,CurrentAnimatedObject.ParentAnimatedObject3d.OldPosition.z,RRotation[3]  );
  //  TTransformation(TransformList.Items[1]).Angle := RadToDeg(aQuaternion.A)  ; //
  //  TTransformation(TransformList.Items[1]).X := aQuaternion.x;
  //  TTransformation(TransformList.Items[1]).Y := aQuaternion.y;
  //  TTransformation(TransformList.Items[1]).Z := aQuaternion.z;
    // outputdebugstring (pchar(  FloatToStr(Position.X)  + FloatToStr(Position.Y) + FloatToStr(Position.Z)) );
  //  end;
      //  AdjustNormals;

    //if FElapsedTime > FModel.Animations[FModel.FActiveAnimationIndex].FLength then
    //  FElapsedTime :=0;

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
//     ttRotate    : begin if (fx <> 0) or (FY <> 0) then asm int 3; end; glRotatef(FAngle, FX, FY, FZ); end;
     ttRotate    : glRotatef(FAngle, FX, FY, FZ);
     ttTranslate : glTranslatef(FX, FY, FZ);
     ttScale     : glScalef(FX, FY, FZ);
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
   for I:=0 to Count-1 do begin
      if I=0 then
       glPushMatrix;
      TTransformation(Items[I]).Apply;
    end;
end;


procedure TTransformList.Pop;
var I:Integer;
begin
  if FEnabled then
   for I:=Count-1 downto 0 do begin
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
    aQuaternion: Tquaternion;
    label retry;

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
RETRY:
        Readln ( fModel, aString);
        aString := TrimLeft(aString);
        if (Leftstr(  aString , 12) = 'node trimesh') or ( Leftstr(  aString , 10) = 'node dummy')   or ( Leftstr(  aString , 15) = 'node danglymesh') then begin

              Object3d :=  AddObject;
              Object3d.FModel:= Self;
              Object3d.ObjectName := ExtractWordL (3,aString,' ');
             // if not ((Object3d.ObjectName = 'Deer_body') or (Object3d.ObjectName = 'rootdummy')) then goto retry;

              Object3d.ObjectType :=  ExtractWordL (2,aString,' ');
              Object3d.TransformList.AddTransformEx (  ttTranslate , 0, Object3d.position.X , Object3d.position.Y,Object3d.position.Z);
              Object3d.TransformList.AddTransformEx (  ttRotate , 0, Object3d.orientation.X , Object3d.orientation.Y,Object3d.orientation.Z);
             // Object3d.TransformList.AddTransformEx (  ttRotate , 0, Object3d.orientation.X , Object3d.orientation.Y,Object3d.orientation.Z);


            while Leftstr(  aString , 7) <> 'endnode' do begin
              Readln ( fModel, aString);
              aString := TrimLeft(aString);
              if  leftstr ( aString, 6) = 'parent' then begin
                Object3d.ParentObjectName := ExtractWordL (2,aString,' ');
                if uppercase(Object3d.ParentObjectName) <> uppercase('NULL') then begin
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
               // TmpVector := VectorAdd ( TmpVector , ParentObject3d.orientation);
//                if object3d.ObjectName='Deer_body' then asm int 3; end;
                Object3d.orientation := TmpVector;
                aQuaternion := MakeQuaternion  ( Object3d.orientation.X,Object3d.orientation.Y,Object3d.orientation.Z,RadToDeg(StrToFloat(ExtractWordL(4,aString,' ')))  );

                if (aQuaternion.x <> 0) or (aQuaternion.y <> 0) or (aQuaternion.z <> 0) then begin
                  TTransformation(Object3d.TransformList.items[1]).Angle := aQuaternion.a;
                  TTransformation(Object3d.TransformList.items[1]).X :=  aQuaternion.x;
                  TTransformation(Object3d.TransformList.items[1]).Y :=  aQuaternion.y;
                  TTransformation(Object3d.TransformList.items[1]).Z :=  aQuaternion.z;
                end;

              {  TTransformation(Object3d.TransformList.items[2]).Angle := aQuaternion.a;
                TTransformation(Object3d.TransformList.items[2]).X :=  aQuaternion.x;
                TTransformation(Object3d.TransformList.items[2]).Y :=  aQuaternion.y;
                TTransformation(Object3d.TransformList.items[2]).Z :=  aQuaternion.z;

                TTransformation(Object3d.TransformList.items[3]).Angle := aQuaternion.a;
                TTransformation(Object3d.TransformList.items[3]).X := aQuaternion.x;
                TTransformation(Object3d.TransformList.items[3]).Y := aQuaternion.y;
                TTransformation(Object3d.TransformList.items[3]).Z := aQuaternion.z;

                TTransformation(Object3d.TransformList.items[4]).Angle := aQuaternion.a;
                TTransformation(Object3d.TransformList.items[4]).X := aQuaternion.x;
                TTransformation(Object3d.TransformList.items[4]).Y := aQuaternion.y;
                TTransformation(Object3d.TransformList.items[4]).Z := aQuaternion.z;    }

                //                Object3d.TransformList.AddTransformEx (  ttRotate , 0, Object3d.orientation.X , Object3d.orientation.Y,Object3d.orientation.Z);
              end


              else if  leftstr ( aString, 6) = 'bitmap' then begin
                 if ExtractWordL (2,aString,' ') <> 'NULL' then begin
                  Object3d.Material.FMaterialFile :=  TexturePath + ExtractWordL (2,aString,' ') ;
                  Object3d.Material.FHasTexture:=LoadTexture(Object3d.Material.FMaterialFile, Object3d.Material.FGenTexture, False);
              { TODO : qui material }  //  Object3d.Material.Ambient.SetRGBA(  );
//                   Object3d.Material.Shininess;
                 end;

              end
              else if  leftstr ( aString, 7) = 'ambient' then begin
                Object3d.Material.Ambient.SetRGBA( StrToFloat( ExtractWordL (2,aString,' ')),StrToFloat( ExtractWordL (3,aString,' ')),StrToFloat( ExtractWordL (4,aString,' ')),0   );
              end
              else if  leftstr ( aString, 7) = 'diffuse' then begin
                Object3d.Material.Diffuse.SetRGBA( StrToFloat( ExtractWordL (2,aString,' ')),StrToFloat( ExtractWordL (3,aString,' ')),StrToFloat( ExtractWordL (4,aString,' ')),0   );
              end
              else if  leftstr ( aString, 7) = 'specular' then begin
                Object3d.Material.Specular.SetRGBA( StrToFloat( ExtractWordL (2,aString,' ')),StrToFloat( ExtractWordL (3,aString,' ')),StrToFloat( ExtractWordL (4,aString,' ')),0   );
              end
              else if  leftstr ( aString, 8) = 'shininess' then begin
                Object3d.Material.Shininess := StrToFloat( ExtractWordL (2,aString,' '));
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

  if UpperCase(supermodel) <> uppercase('NULL.mdl') then begin
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
  label retry;
begin

  Anim :=  AddAnimation;
  Anim.FAnimationName := ExtractWordL (2,FirstString,' ');
  Anim.FAnimationModelName := ExtractWordL (3,FirstString,' ');

  while Leftstr(  aString , 8) <> 'doneanim' do begin
 //         if Anim.FAnimationName = 'ca1slashl' then asm int 3 ; end;
retry:
    Readln ( fModel, aString);  aString := TrimLeft(aString);
    if Leftstr(  aString , 6) ='length' then Anim.FLength := StrToFloat(ExtractWordL( 2,aString,' ' )) // Milliseconds
    else if Leftstr(  aString , 9) ='transtime' then Anim.FTranstime:= StrToFloat(ExtractWordL( 2,aString,' ' ))
    else if Leftstr(  aString , 8) ='animroot' then Anim.AnimationRoot:= ExtractWordL( 2,aString,' ' )

   // else if Leftstr(  aString , 8) ='event' then Anim.AnimationRoot:= ExtractWordL( 2,aString,' ' ) { TODO : event 0.5 hit }

    else if  (leftstr ( aString, 10) = 'node dummy' )or ( Leftstr(  aString , 12) = 'node trimesh')   or ( Leftstr(  aString , 15) = 'node danglymesh') then begin
      AnimatedObject:=Anim.AddAnimatedObject;
      AnimatedObject.ObjectName :=  ExtractWordL( 3,aString,' ' ) ;
//      if not ((AnimatedObject.ObjectName = 'Deer_body') or (AnimatedObject.ObjectName = 'rootdummy')) then goto retry;

     // if AnimatedObject.ObjectName ='a_ba' then
     //   AnimatedObject.ObjectName := objects[0].ObjectName;
       //   if AnimatedObject.ObjectName = 'Deer_body' then asm int 3 ; end;
      while Leftstr(  aString , 7) <> 'endnode' do begin
        Readln ( fModel, aString); aString := TrimLeft(aString);
        if Leftstr(  aString , 6) ='parent' then begin
          AnimatedObject.ParentAnimatedObjectName := ExtractWordL (2,aString,' ');
          if AnimatedObject.ParentAnimatedObjectName <> 'NULL' then begin
            AnimatedObject.ParentAnimatedObject := Anim.FindAnimatedObject(AnimatedObject.ParentAnimatedObjectName);
            AnimatedObject.ParentAnimatedObject.AddChildren ( AnimatedObject ) ;
            AnimatedObject.ParentAnimatedObject3d := FindObject(AnimatedObject.ParentAnimatedObjectName);
          end;

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
      Anim.AnimationRoot:= Anim.AnimatedObjects[i].ObjectName;
      Break;
    end;
  end;
  {  for C := 0 to Anim.AnimatedObjects[IndexAllpos].Children.Count -1 do begin
      for P := 0 to Anim.AnimatedObjects[IndexAllpos].PositionKeyCount -1 do begin
        newPK := Anim.AnimatedObjects[C].AddPositionKey;
        newPK.KeyTime := Anim.AnimatedObjects[IndexAllpos].PositionKeys[P].KeyTime;
        newPK.KeyValue := Anim.AnimatedObjects[IndexAllpos].PositionKeys[P].KeyValue;
      end;
    end; }
{$IFDEF  DEBUG}
  if  Anim.FAnimationName = 'ca1slashl' then begin
  AssignFile(flog, 'log.txt'); rewrite(flog);
    writeln ( flog,  'ANIM: ' + Anim.FAnimationName );
        writeln ( flog,  ' ');
        writeln ( flog,  ' ');

  for I := 0 to Anim.AnimatedObjectCount -1 do begin
    for P := 0 to Anim.AnimatedObjects[i].PositionKeyCount -1 do begin
        writeln ( flog,  'object: ' + Anim.AnimatedObjects[I].ObjectName );
        writeln ( flog,  ' ');
        writeln ( flog,  'time: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyTime));
        writeln ( flog,  'positionkeyX: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyValue.X));
        writeln ( flog,  'positionkeyY: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyValue.Y));
        writeln ( flog,  'positionkeyZ: ' +  FloatToStr(Anim.AnimatedObjects[I].PositionKeys[P].KeyValue.Z));
        writeln ( flog,  ' ' );
        writeln ( flog,  ' ' );
    end;
    for P := 0 to Anim.AnimatedObjects[I].OrientationKeyCount -1 do begin
      writeln ( flog,  'object: ' + Anim.AnimatedObjects[I].ObjectName );
      writeln ( flog,  ' '  );
      writeln ( flog,  'time: ' +  FloatToStr(Anim.AnimatedObjects[I].OrientationKeys[P].KeyTime));
      writeln ( flog,  'orientationkeyX: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].KeyValue.X));
      writeln ( flog,  'orientationkeyY: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].KeyValue.Y));
      writeln ( flog,  'orientationkeyZ: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].KeyValue.Z));
      writeln ( flog,  'orientationkeyA: ' +  FloatToStr(Anim.AnimatedObjects[I].orientationKeys[P].Angle));
      writeln ( flog,  ' ');
      writeln ( flog,  ' ');
    end;

  end;

  CloseFile (flog);

  end;
{$ENDIF}

end;
initialization
  Randomize;


end.
	//
	// Position
  //
 {
	// Find appropriate position key frame
	i := 0;
	while ( (i < NumPositionKeys-1) and (PositionKeyFrames[i].Time < CurrentTime) ) do
		i := i + 1;

	assert(i < NumPositionKeys);


	if( i > 0 ) then
	begin
		// Interpolate between 2 key frames

		// time between the 2 key frames
		deltaTime := PositionKeyFrames[i].Time - PositionKeyFrames[i-1].Time;

		assert( deltaTime > 0 );

		// relative position of interpolation point to the keyframes [0..1]
		fraction := (CurrentTime - PositionKeyFrames[i-1].Time) / deltaTime;

		assert( fraction > 0 );
		assert( fraction < 1.0 );

		Position[0] := PositionKeyFrames[i-1].Value[0] + fraction * (PositionKeyFrames[i].Value[0] - PositionKeyFrames[i-1].Value[0]);
		Position[1] := PositionKeyFrames[i-1].Value[1] + fraction * (PositionKeyFrames[i].Value[1] - PositionKeyFrames[i-1].Value[1]);
		Position[2] := PositionKeyFrames[i-1].Value[2] + fraction * (PositionKeyFrames[i].Value[2] - PositionKeyFrames[i-1].Value[2]);
	end
	else
  begin
		Position[0] := PositionKeyFrames[i].Value[0];
    Position[1] := PositionKeyFrames[i].Value[1];
    Position[2] := PositionKeyFrames[i].Value[2];
  end;


	//
	// Rotation
	//

	// Find appropriate rotation key frame
	i := 0;
	while( (i < NumRotationKeys-1) and (RotationKeyFrames[i].Time < CurrentTime) ) do
		i := i + 1;

	assert(i < NumRotationKeys);

	if( i > 0 ) then
	begin
		// Interpolate between 2 key frames

		// time between the 2 key frames
		deltaTime := RotationKeyFrames[i].Time - RotationKeyFrames[i-1].Time;
		assert( deltaTime > 0 );

		// relative position of interpolation point to the keyframes [0..1]
		fraction := (CurrentTime - RotationKeyFrames[i-1].Time) / deltaTime;
		assert( fraction > 0 );
		assert( fraction < 1.0 );

		Rotation[0] := RotationKeyFrames[i-1].Value[0] + fraction * (RotationKeyFrames[i].Value[0] - RotationKeyFrames[i-1].Value[0]);
		Rotation[1] := RotationKeyFrames[i-1].Value[1] + fraction * (RotationKeyFrames[i].Value[1] - RotationKeyFrames[i-1].Value[1]);
		Rotation[2] := RotationKeyFrames[i-1].Value[2] + fraction * (RotationKeyFrames[i].Value[2] - RotationKeyFrames[i-1].Value[2]);
	end
	else
	begin
    Rotation[0] := RotationKeyFrames[i].Value[0];
    Rotation[1] := RotationKeyFrames[i].Value[1];
    Rotation[2] := RotationKeyFrames[i].Value[2];
	end;
}

