// ***************************************************************************
// ********************** Loading a MDL NeverWinter Night 1 object *******************************
// **********************   Gabriele Canazza  *******************************
// ******************* gabriele.canazza@gmail.com *************************

// This sample shows how to load a MDL Aurora file
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Winapi.ShellAPI, system.Math,
  Dialogs, AppEvnts, Unit3DS, OpenGL,DSE_SearchFiles, Vcl.StdCtrls, Vcl.ExtCtrls;
const
  HITBUFFERCOUNT = 64;


type
  PHit = ^THit;
  THit = packed record
    NCount, DNear, DFar:GLuint;
    Names:array[1..32] of GLuint;
end;

type
  TViewPort = packed record
    Left, Bottom, Width, Height:GLint;
end;

type
  THitBuffer = array[1..HITBUFFERCOUNT] of GLuint;

type
  TRef = array[0..0] of T3DModel;
  PRef = ^TRef;

type
  TForm1 = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    Panel1: TPanel;
    ListBox1: TListBox;
    sf: SE_SearchFiles;
    Button6: TButton;
    Button7: TButton;
    Button1: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button2: TButton;
    ComboBox2: TComboBox;
    Edit4: TEdit;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure ComboBox2CloseUp(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    procedure Selection(const X, Y: GLdouble);
    function SelectionDone:Integer;
    function Selected(const X, Y:Integer): Integer;
  public
    { Public declarations }

// ********************************** NEW NEW **********************************
    Model:T3DModel;  // Variable for instancing T3DModel
    Tcn:T3DModel;  // Variable for instancing T3DModel
// ********************************** END NEW **********************************

    HitBuffer:THitBuffer;
    procedure Render;
  end;

var
  Form1: TForm1;
  Ax, Mx:Single;
  Ay, My:Single;
  Zoom,Ratio: Single;
  nFrames : Integer;
  lastTickCount : Integer;
  ElapsedTime: Single;
  MdlPath,TexturePath,SuperModelPath: string;
  ModelLoaded: Boolean;
  SelectedObject: T3DObject;
implementation

uses UglContext;

{$R *.dfm}
procedure TForm1.FormShow(Sender: TObject);
var
  i:Integer;
begin
  CreateGLContext(Handle); // Pass the window handle to create opengl context
// ********************************** NEW NEW **********************************
  // ********************************** END NEW **********************************
  Model:=T3DModel.Create;            // Instance of T3DModel
  Tcn:=T3DModel.Create;            // Instance of T3DModel
  glEnable(GL_LIGHT0);    // Enable Light0
  glEnable(GL_LIGHTING);  // Enable Lighting
  glenable(GL_COLOR_MATERIAL);
  LastTickCount := GetTickCount;

end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  i:Integer;
begin
//  Model.LoadFromFile ('sulaco.3ds');  // Load the 3DS file
  Model.LoadFromFileMDL ( MdlPath + ListBox1.Items[ListBox1.ItemIndex], MdlPath, TexturePath,SuperModelPath );  // Load the MDL file
  Model.Name:='skeleton';
  ComboBox1.Clear;
  ComboBox2.Clear;
  for i :=0 to Model.AnimationCount -1 do begin
    ComboBox1.AddItem( Model.Animations[i].AnimationName,nil );
  end;
  for i :=0 to Model.ObjectCount -1 do begin
    ComboBox2.AddItem( Model.Objects[i].ObjectName,nil );
  end;


  tcn.LoadFromFileMDL ( MdlPath +'tcn01_a20_02.mdl',MdlPath,TexturePath,SuperModelPath );  // Load the MDL file
  ModelLoaded:= True;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Model.Anim(0.1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TTransformation(SelectedObject.TransformList.Items[0]).Angle := StrToFloat(Edit4.text) *180/3.14;
  TTransformation(SelectedObject.TransformList.Items[0]).X := StrToFloat(Edit1.text);
  TTransformation(SelectedObject.TransformList.Items[0]).Y := StrToFloat(Edit2.text);
  TTransformation(SelectedObject.TransformList.Items[0]).Z := StrToFloat(Edit3.text);
end;

procedure TForm1.Button3Click(Sender: TObject);
var o : T3DObject;i:Integer;
begin
  o := model.FindObject( ComboBox2.Items[ComboBox2.ItemIndex]);
  if o <> nil  then begin
  memo1.Clear;
  for I := 0 to o.Children.Count -1 do begin
    Memo1.Lines.Add( o.Children[i].ObjectName );
  end;
  end;

end;

procedure TForm1.Button4Click(Sender: TObject);
var o : T3DObject;i:Integer;oa:TAnimatedObject; Anim : TAnimation;
begin
  o := model.FindObject( ComboBox2.Items[ComboBox2.ItemIndex]);
  memo1.Clear;
  if o <> nil  then begin
{      Memo1.Lines.Add( 'p:.X ' + FloatToStr (o.Position.X) );
      Memo1.Lines.Add( 'p:.Y ' + FloatToStr (o.Position.Y) );
      Memo1.Lines.Add( 'p:.Z ' + FloatToStr (o.Position.Z) );
      Memo1.Lines.Add( 'o:.X ' + FloatToStr (o.Orientation.X ) );
      Memo1.Lines.Add( 'o:.Y ' + FloatToStr (o.Orientation.Y) );
      Memo1.Lines.Add( 'o:.Z ' + FloatToStr (o.Orientation.Z) );    }
      Anim := Model.FindAnimation(ComboBox1.Items[ComboBox1.ItemIndex]);
      oa :=Anim.FindAnimatedObject(o.ObjectName) ;
{      Memo1.Lines.Add( 'oa positionkeys' );
      for I := 0 to oa.PositionKeyCount -1 do begin
        Memo1.Lines.Add( 'o:FRAMEpk ' + IntToStr (I ) );
        Memo1.Lines.Add( 'o:pk.x ' + FloatToStr (oa.PositionKeys[i].KeyValue.x ) );
        Memo1.Lines.Add( 'o:pk.y ' + FloatToStr (oa.PositionKeys[i].KeyValue.y ) );
        Memo1.Lines.Add( 'o:pk.z ' + FloatToStr (oa.PositionKeys[i].KeyValue.z ) );

      end;        }
      Memo1.Lines.Add( 'oa orientationkeys' );
      for I := 0 to oa.orientationKeyCount -1 do begin
        Memo1.Lines.Add( 'o:FRAMEok ' + IntToStr (I ) );
        Memo1.Lines.Add( 'o:ok.angle rad' + FloatToStr ( oa.orientationKeys[i].Angle )  + ' deg ' +FloatToStr (RadToDeg( oa.orientationKeys[i].Angle ) ));
        Memo1.Lines.Add( 'o:ok.x ' + FloatToStr (oa.orientationKeys[i].KeyValue.x ) );
        Memo1.Lines.Add( 'o:ok.y ' + FloatToStr (oa.orientationKeys[i].KeyValue.y ) );
        Memo1.Lines.Add( 'o:ok.z ' + FloatToStr (oa.orientationKeys[i].KeyValue.z ) );
      end;
  end;

end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ResetModelView;
  Zoom := Zoom +0.1;
  ratio := Form1.Width / Form1.Height;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum(-ratio, ratio, -1, 1, zoom, 25*zoom);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  ResetModelView;
 Zoom := Zoom - 0.1;
  ratio := Form1.Width / Form1.Height;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum(-ratio, ratio, -1, 1, zoom, 25*zoom);
end;

procedure TForm1.ComboBox1CloseUp(Sender: TObject);
begin
  if ComboBox1.ItemIndex > -1 then
  Model.ActiveAnimationName := ComboBox1.Items[ComboBox1.ItemIndex];

end;

procedure TForm1.ComboBox2CloseUp(Sender: TObject);
begin
  SelectedObject := Model.FindObject( ComboBox2.Items[ComboBox2.ItemIndex]  );
  if SelectedObject = nil then exit;

  Edit1.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).x  );
  Edit2.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).y  );
  Edit3.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).z  );
  Edit4.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).angle  );

  Memo1.Lines.Add( IntToStr( SelectedObject.CurrentAnimation.PositionKeyCount));
 { Edit1.Text := FloatToStr( SelectedObject.Position.x  );
  Edit2.Text := FloatToStr( SelectedObject.Position.y  );
  Edit3.Text := FloatToStr( SelectedObject.Position.z  );   }
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// ********************************** NEW NEW **********************************
  Model.Free;        // Free instance (and memory)
  Tcn.Free;
// ********************************** END NEW **********************************
  DestroyGLContext; // Free device context created for opengl
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FormatSettings.DecimalSeparator :='.';
  MdlPath := ExtractFilePath(Application.ExeName) + 'mdl\';
  TexturePath := ExtractFilePath(Application.ExeName) + 'Textures\';
  SuperModelPath := ExtractFilePath(Application.ExeName) + 'SuperModels\';
  {This command references other model files. A model may have a (super)model from which it inherits animations. If there is no supermodel the
  second parameter has to be NULL. The model may overwrite any animation from its supermodel. The structure of the two models must to match, i.e.
  the order of the objects in both files must be the same.}
  sf.FromPath := MdlPath;
  sf.MaskInclude.Add('*.mdl');
  sf.SubDirectories := False;
  sf.Execute;

  while sf.SearchState <> ssIdle do begin
    Application.ProcessMessages;
  end;

  for I := 0 to sf.ListFiles.Count -1 do begin
    ListBox1.AddItem(sf.ListFiles[i],nil );
  end;

end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Mx:=X;   // captures new positions ...
  My:=Y;   // ... for next call to MouseMove event
// ********************************** NEW NEW **********************************
// Selected(X, Y) returns the index of the object selected. Passing this index
// to the Select method of T3DModel class, the object is "selected". As you will
// see in later samples, the Select method of T3DModel class also returns
// an object pointer to the selected object. You can use this pointer for
// changing color for example.
 // SelectedObject:= Model.Select(Selected(X, Y));
// ********************************** END NEW **********************************
//  Edit1.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).x  );
//  Edit2.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).y  );
 // Edit3.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).z  );
 // Edit4.Text := FloatToStr( TTransformation(SelectedObject.TransformList.Items[0]).angle  );

 // Caption:= SelectedObject.ObjectName;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then // when mouse moves with left button ....
  begin
    Ax := Ax + (Y-My)/2;  // ... calculate rotation for X ....
    Ay := Ay + (X-Mx)/2;  // ... and Y
    Mx:=X;                // and captures new positions ...
    My:=Y;                // ... for next call to MouseMove event
  end;

end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ResizeGL(Width, Height);  // Pass dimensions for Viewport and aspect ratio
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  Done:=False;
  if ModelLoaded then Render;      // Render the scene
end;

procedure TForm1.Render;
var
  ms,i :Integer;
  Text : string;
  List : GLuint;
//  Ref: PRef;

begin

  Inc(nFrames);
  ms := GetTickCount;
  ElapsedTime := (ms - lastTickCount) / 1000;
//  Model.Anim (ElapsedTime);  // anim the complete MDL file
  Caption := floattostr(elapsedtime);
  LastTickCount := ms;

  ClearGL;     // Clear frame buffer
  ResetModelView;

  glRotatef(Ax, 1, 0, 0);
  glRotatef(-Ay, 0, 1, 0);
//  Model.Anim;

//if Odd(Models.Count) then begin
//  Models[0].Draw;
//  Models[0].Anim;

//  Count := (Models.Count)-1;
//end;


//  Ref := @Models[Count];
//  i := -Models.Count;  // Assign NEGATIVE count here
//  while i < 0 do begin // and count UP to zero
//  T3DModel(Ref[i]).Draw;
//  T3DModel(Ref[i]).Anim(msTotal);     msTotal ...
//  T3DModel(Ref[i+1]).Draw;
//  T3DModel(Ref[i+1]).Anim;
//    Inc(i,2);
//  end;

  Model.Draw;  // Draws the complete MDL file
  Tcn.Draw;


  SwapGL;     // Swap buffers
end;
function TForm1.Selected(const X, Y:Integer): Integer;
begin
  Selection(X, Y);
  Render;
  result:=SelectionDone;
end;
procedure TForm1.Selection(const X, Y: GLdouble);
var ViewPort:TViewPort;
    Py:GLdouble;
begin
  glSelectBuffer(HITBUFFERCOUNT, @HitBuffer);
  glRenderMode(GL_SELECT);
  glInitNames;
  glPushName(0);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glGetIntegerv(GL_VIEWPORT, @ViewPort);
  Py:=ViewPort.Height - Y;
  gluPickMatrix(X, Py, 30.0, 30.0, @ViewPort);
  gluPerspective(45, Width/Height, 1, 300);
end;

function TForm1.SelectionDone:Integer;
var Hits:GLint;
    Hit:PHit;
begin
  Result:=-1;
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  Hits:=glRenderMode(GL_RENDER);
  if Hits=0 then
   Exit;
  Hit:=@HitBuffer[1];
  Result:=Hit^.Names[Hit^.NCount];
end;



end.
