#include "filebuffer.h"

using namespace std;
using namespace ShipCADGeometry;

union convert_type_t {
  unsigned char d[8];
  int ival;
  float fval;
};

FileBuffer::FileBuffer()
  : _fb(0), _pos(0)
{
  // does nothing
}

FileBuffer::~FileBuffer()
{
  delete _fb;
}

void FileBuffer::load(bool& val)
{
  convert_type_t ct;
  ct.d[0] = _data[_pos++];
  val = (ct.d[0] == 0) ? false : true;
}

void FileBuffer::add(bool val)
{
  _data.push_back((val ? 0x1 : 0x0));
}

void FileBuffer::load(float& val)
{
  convert_type_t ct;
  for (int i=0; _pos<_data.size() && i<4; ++i,++_pos)
    ct.d[i] = _data[_pos];
  val = ct.fval;
}

void FileBuffer::add(float val)
{
  convert_type_t ct;
  ct.fval = val;
  for (int i=0; i<4; ++i)
    _data.push_back(ct.d[i]);
}

void FileBuffer::load(int& val)
{
  convert_type_t ct;
  for (int i=0; _pos<_data.size() && i<4; ++i,++_pos)
    ct.d[i] = _data[_pos];
  val = ct.ival;
}

void FileBuffer::add(int val)
{
  convert_type_t ct;
  ct.ival = val;
  for (int i=0; i<4; ++i)
    _data.push_back(ct.d[i]);
}

void FileBuffer::load(QVector3D& val)
{
  convert_type_t ct;
  for (int i=0; _pos<_data.size() && i<4; ++i,++_pos)
    ct.d[i] = _data[_pos];
  val.setX(ct.fval);
  for (int i=0; _pos<_data.size() && i<4; ++i,++_pos)
    ct.d[i] = _data[_pos];
  val.setY(ct.fval);
  for (int i=0; _pos<_data.size() && i<4; ++i,++_pos)
    ct.d[i] = _data[_pos];
  val.setZ(ct.fval);
}

void FileBuffer::add(const QVector3D& val)
{
  convert_type_t ct;
  ct.fval = val.x();
  for (int i=0; i<4; ++i)
    _data.push_back(ct.d[i]);
  ct.fval = val.y();
  for (int i=0; i<4; ++i)
    _data.push_back(ct.d[i]);
  ct.fval = val.z();
  for (int i=0; i<4; ++i)
    _data.push_back(ct.d[i]);
}
