/*###############################################################################################
 *    ShipCAD																					*
 *    Copyright 2016, by Greg Green <ggreen@bit-builder.com>									*
 *    Original Copyright header below															*
 *																								*
 *    This code is distributed as part of the FREE!ship project. FREE!ship is an                *
 *    open source surface-modelling program based on subdivision surfaces and intended for      *
 *    designing ships.                                                                          *
 *                                                                                              *
 *    Copyright © 2005, by Martijn van Engeland                                                 *
 *    e-mail                  : Info@FREEship.org                                               *
 *    FREE!ship project page  : https://sourceforge.net/projects/freeship                       *
 *    FREE!ship homepage      : www.FREEship.org                                                *
 *                                                                                              *
 *    This program is free software; you can redistribute it and/or modify it under             *
 *    the terms of the GNU General Public License as published by the                           *
 *    Free Software Foundation; either version 2 of the License, or (at your option)            *
 *    any later version.                                                                        *
 *                                                                                              *
 *    This program is distributed in the hope that it will be useful, but WITHOUT ANY           *
 *    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A           *
 *    PARTICULAR PURPOSE. See the GNU General Public License for more details.                  *
 *                                                                                              *
 *    You should have received a copy of the GNU General Public License along with              *
 *    this program; if not, write to the Free Software Foundation, Inc.,                        *
 *    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                     *
 *                                                                                              *
 *##############################################################################################*/

#ifndef CONTROLFACEGRID_H_
#define CONTROLFACEGRID_H_

#include <vector>
#include <iosfwd>

namespace ShipCAD {

//////////////////////////////////////////////////////////////////////////////////////

class SubdivisionControlFace;

struct ControlFaceGrid 
{
    std::vector<std::vector<SubdivisionControlFace*> > faces;
    size_t cols() {return faces[0].size();}
    size_t rows() {return faces.size();}

    void setRows(size_t rows) 
        {
            faces.resize(rows);
        }

    void setCols(size_t cols)
        {
            for (size_t i=0; i<faces.size(); i++)
                faces[i].resize(cols);
        }
    
    void setFace(size_t row, size_t col, SubdivisionControlFace* face) {
        faces[row][col] = face;
    }
    
    SubdivisionControlFace* getFace(size_t row, size_t col) {
        return faces[row][col];
    }
};

//////////////////////////////////////////////////////////////////////////////////////

};				/* end namespace */

//////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator << (std::ostream& os, const ShipCAD::ControlFaceGrid& grid);

//////////////////////////////////////////////////////////////////////////////////////

#endif

