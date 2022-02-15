unit Vector;

interface
uses
  Matrix;

type psingle = ^single;
type
  clsVector = class
	public

    //	Vector data
		m_vector : array [0..3] of single;

		{	Constructor. (0, 0, 0, 1) }
		constructor create();overload;

		{	Constructor. 3 float values. }
		constructor create( vector : array of single );overload;

		{	Retrieve vector. }
		procedure getVector(var vector : array of single);

		{	Transform the vector by a matrix. }
		procedure transform( m : clsMatrix );

		{	Transform the vector by a matrix, not including the scaling or transformation elements (use only top-left 3x3 of matrix). }
		procedure transform3( m : clsMatrix );

		{	Set the values of the vector. Takes 3 float values. }
		procedure setVector( vector : array of single );

		{	Translate by another vector. }
		procedure add( v : clsVector );

		{	Reset to (0, 0, 0, 1). }
		procedure reset();

		{	Get the length of the vector. }
		function length() : double;

		{	Normalize (make it a unit vector). }
		procedure normalize();


end;
implementation

{ clsVector }

procedure clsVector.add(v: clsVector);
begin
  m_vector[0] := m_vector[0] + v.m_vector[0];
	m_vector[1] := m_vector[1] + v.m_vector[1];
	m_vector[2] := m_vector[2] + v.m_vector[2];
	m_vector[3] := m_vector[3] + v.m_vector[3];
end;

constructor clsVector.create;
begin
  reset();
end;

constructor clsVector.create(vector: array of single);
begin
  setVector( vector );
	m_vector[3] := 1;
end;

procedure clsVector.getVector(var vector : array of single);
var i : integer;
begin
  for i := 0 to 3 do
    vector[i] := m_vector[i];
end;

function clsVector.length: double;
begin
  result := sqrt( m_vector[0]*m_vector[0]+m_vector[1]*m_vector[1]+m_vector[2]*m_vector[2] );
end;

procedure clsVector.normalize;
var len : double;
begin
  len := length();

	m_vector[0] := m_vector[0]/len;
	m_vector[1] := m_vector[1]/len;
	m_vector[2] := m_vector[2]/len;
end;

procedure clsVector.reset;
begin
  m_vector[0] := 0;
  m_vector[1] := 0;
  m_vector[2] := 0;
	m_vector[3] := 1;
end;

procedure clsVector.setVector(vector: array of single);
begin
  m_vector[0] := vector[0];
	m_vector[1] := vector[1];
	m_vector[2] := vector[2];
end;

procedure clsVector.transform(m: clsMatrix);
var matrix : array [0..15] of single;
begin
	m.getMatrix(matrix);

	m_vector[0] := m_vector[0]*matrix[0]+m_vector[1]*matrix[4]+m_vector[2]*matrix[8]+matrix[12];
	m_vector[1] := m_vector[0]*matrix[1]+m_vector[1]*matrix[5]+m_vector[2]*matrix[9]+matrix[13];
	m_vector[2] := m_vector[0]*matrix[2]+m_vector[1]*matrix[6]+m_vector[2]*matrix[10]+matrix[14];
	m_vector[3] := m_vector[0]*matrix[3]+m_vector[1]*matrix[7]+m_vector[2]*matrix[11]+matrix[15];

end;

procedure clsVector.transform3(m: clsMatrix);
var matrix : array [0..15] of single;
begin
	m.getMatrix(matrix);

	m_vector[0] := m_vector[0]*matrix[0]+m_vector[1]*matrix[4]+m_vector[2]*matrix[8];
	m_vector[1] := m_vector[0]*matrix[1]+m_vector[1]*matrix[5]+m_vector[2]*matrix[9];
	m_vector[2] := m_vector[0]*matrix[2]+m_vector[1]*matrix[6]+m_vector[2]*matrix[10];
	m_vector[3] := 1;
end;

end.
 