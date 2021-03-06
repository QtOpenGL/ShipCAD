/*##############################################################################################
 *    ShipCAD
 *    Copyright 2015, by Greg Green <ggreen@bit-builder.com>
 *    Original Copyright header below
 *
 *    This code is distributed as part of the FREE!ship project. FREE!ship is an               *
 *    open source surface-modelling program based on subdivision surfaces and intended for     *
 *    designing ships.                                                                         *
 *                                                                                             *
 *    Copyright © 2005, by Martijn van Engeland                                                *
 *    e-mail                  : Info@FREEship.org                                              *
 *    FREE!ship project page  : https://sourceforge.net/projects/freeship                      *
 *    FREE!ship homepage      : www.FREEship.org                                               *
 *                                                                                             *
 *    This program is free software; you can redistribute it and/or modify it under            *
 *    the terms of the GNU General Public License as published by the                          *
 *    Free Software Foundation; either version 2 of the License, or (at your option)           *
 *    any later version.                                                                       *
 *                                                                                             *
 *    This program is distributed in the hope that it will be useful, but WITHOUT ANY          *
 *    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          *
 *    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 *
 *                                                                                             *
 *    You should have received a copy of the GNU General Public License along with             *
 *    this program; if not, write to the Free Software Foundation, Inc.,                       *
 *    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    *
 *                                                                                             *
 *#############################################################################################*/

#ifndef NURBSURFACE_H_
#define NURBSURFACE_H_

#include <vector>
#include <iosfwd>
#include <QObject>
#include <QVector3D>
#include "entity.h"

namespace ShipCAD {

//////////////////////////////////////////////////////////////////////////////////////

class NURBSurface : public Entity
{
    Q_OBJECT

public:

    explicit NURBSurface();
    virtual ~NURBSurface() {}
    
    // altering
    virtual void clear();
    virtual void rebuild();

    // geometry ops

    // persistence

    // drawing
    //int distance_to_cursor(int x, int y, Viewport& vp) const;
    virtual void draw(Viewport& vp);

    // getters/setters
    QVector3D getPoint(size_t row, size_t col);
    void setColDegree(size_t val);
    void setRowDegree(size_t val);
    void setPoint(size_t row, size_t col, const QVector3D& val);
    virtual void setBuild(bool val);

    // output
    void dump(std::ostream& os) const;

protected:

protected:

    int _col_count;
    int _row_count;
    int _col_degree;
    int _row_degree;
    int _col_knots;
    int _row_knots;

    std::vector<QVector3D> _points;
    std::vector<bool> _knuckles;
    std::vector<float> _parameters;
    std::vector<QVector3D> _derivatives;
};

//////////////////////////////////////////////////////////////////////////////////////

};				/* end namespace */

std::ostream& operator << (std::ostream& os, const ShipCAD::NURBSurface& surface);

#endif

