// ***************************************************************************
// ************************* glContext Unit **********************************
// **********************   Juan José Montero  *******************************
// ******************* juanjo.montero@telefonica.net *************************
// *********************** Release 19/11/2003 ********************************
// ***************************************************************************

unit UglContext;

interface


uses Windows, OpenGL;


function CreateGLContext(const Wnd:THandle):Boolean;

procedure DestroyGLContext;

procedure ResizeGL(const W, H:Integer);

procedure SwapGL;

procedure ClearGL;

procedure ResetModelView;

var Dc:hDc;
    glContext:hglrc;



implementation



// This procedure is used for initialization some opengl parameters
procedure Initialize;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glShadeModel(GL_SMOOTH);
  glClearDepth(1);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
end;


// Change to ModelView matrix
procedure ResetModelView;
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  gluLookAt(2, -5, 4, 0, 0, 0, 0, 1, 0); // Set position and orientation
end;

// Set Viewport and Aspect Ratio for Projection Matrix
procedure ResizeGL(const W, H:Integer);
var Aspect:Single;
begin
  Aspect:=1;
  glViewPort(0, 0, W, H);
  if (W > 0) and (H > 0) then
   Aspect:=W / H;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45, Aspect, 1, 300);
  ResetModelView;
end;


// Create opengl context
function CreateGlContext(const Wnd:THandle):Boolean;
var Pfd: TPixelFormatDescriptor;
    iFormat: Integer;
begin
  ZeroMemory(@Pfd, SizeOf(TPixelFormatDescriptor));
  Dc:=GetDc(Wnd);
  Pfd.nSize := SizeOf( TPixelFormatDescriptor );
  Pfd.nVersion := 1;
  Pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  Pfd.iPixelType := PFD_TYPE_RGBA;
  Pfd.cColorBits := 32;
  Pfd.cDepthBits := 32;
  Pfd.iLayerType := PFD_MAIN_PLANE;
  iFormat := ChoosePixelFormat(Dc, @Pfd);
  if not SetPixelFormat(Dc, iFormat, @Pfd) then
   begin
     MessageBox(0, 'Error setting pixel format.', 'SetPixelFormat', MB_OK );
     Result:=False;
     Exit;
   end;
  glContext := wglCreateContext(Dc);
  wglMakeCurrent(Dc, glContext);
  Initialize;
  Result:=True;
end;

// Destroy opengl context and free device context
procedure DestroyGlContext;
begin
  wglMakeCurrent(Dc, 0);
  wglDeleteContext(glContext);
  DeleteDc(Dc);
end;

// Swaps opengl buffers
procedure SwapGL;
begin
  SwapBuffers(Dc);
end;

// Clear opengl buffers
procedure ClearGL;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;  

end.
