// ***************************************************************************
// ********************** Loading a MDL NeverWinter Night 1 object *******************************
// **********************   Gabriele Canazza  *******************************
// ******************* gabriele.canazza@gmail.com *************************

// This sample shows how to load a MDL Aurora file
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Winapi.ShellAPI,
  Dialogs, AppEvnts, Unit3DS, OpenGL,DSE_SearchFiles, Vcl.StdCtrls, Vcl.ExtCtrls;
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
  private
    { Private declarations }
  public
    { Public declarations }

// ********************************** NEW NEW **********************************
    Model:T3DModel;  // Variable for instancing T3DModel
    Tcn:T3DModel;  // Variable for instancing T3DModel
// ********************************** END NEW **********************************

    procedure Render;
  end;

var
  Form1: TForm1;
  Ax, Mx:Single;
  Ay, My:Single;
  CX,CY,CZ: Single;
  nFrames : Integer;
  lastTickCount, msTotal : Integer;
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
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  i:Integer;
begin
//  Model.LoadFromFile ('sulaco.3ds');  // Load the 3DS file
  Model.LoadFromFileMDL ( ExtractFilePath(Application.ExeName) + ListBox1.Items[ListBox1.ItemIndex] );  // Load the MDL file
  tcn.LoadFromFileMDL ( ExtractFilePath(Application.ExeName)  + 'tcn01_a20_02.mdl' );  // Load the MDL file
  { TODO : node lights in unit3ds }


end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  CZ := CZ +1;
  ResetModelView;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  gluLookAt( 2 , -5, CZ, 0, 0, 0, 0, 1, 0); // Set position and orientation

end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  CZ := CZ -1;
  ResetModelView;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  gluLookAt( 2 , -5, CZ, 0, 0, 0, 0, 1, 0); // Set position and orientation

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
  CZ := 4;
  sf.FromPath := ExtractFilePath(Application.ExeName);
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
  Render;      // Render the scene
end;

procedure TForm1.Render;
var
  ms,i :Integer;
  Text : string;
  List : GLuint;
//  Ref: PRef;

begin
  Inc(nFrames);

  ClearGL;     // Clear frame buffer
 // ResetModelView;
  SetModelView ( 2, -5, CZ );

  glRotatef(Ax, 1, 0, 0);
  glRotatef(-Ay, 0, 1, 0);
//  Model.Anim;

//if Odd(Models.Count) then begin
//  Models[0].Anim;
//  Models[0].Draw;

//  Count := (Models.Count)-1;
//end;


//  Ref := @Models[Count];
//  i := -Models.Count;  // Assign NEGATIVE count here
//  while i < 0 do begin // and count UP to zero
//  T3DModel(Ref[i]).Anim;
//  T3DModel(Ref[i]).Draw;
//  T3DModel(Ref[i+1]).Anim;
//  T3DModel(Ref[i+1]).Draw;
//    Inc(i,2);
//  end;
  Model.Draw;  // Draws the complete MDL file
  Tcn.Draw;
 {  ms := GetTickCount;
  msTotal := msTotal + (ms - lastTickCount);
  LastTickCount := ms;

 if msTotal > 1000 then begin
    msTotal := 0;
    Caption := 'Framerate :' + IntToStr(nFrames);
    nFrames := 0;
//   glColor4fv(@Font.FColorVector);
   glColor4fv(@LastTickCount);   // random color

   glPushMatrix;
   glLoadIdentity;
   glRasterPos2i(100, 10);
   text := Caption;
   for i := 1 to Length(text) do  begin
      wglUseFontBitmapsW(dc, Ord(text[i]), 1, list);
      glCallList(list);
   end;
   glDeleteLists(list, 1);
  // glColor4fv(@FPenColor);
   glPopMatrix;

  end;   }

  SwapGL;     // Swap buffers
end;



end.
