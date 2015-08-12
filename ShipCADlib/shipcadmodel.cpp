/*##############################################################################################
 *    ShipCAD																				   *
 *    Copyright 2015, by Greg Green <ggreen@bit-builder.com>								   *
 *    Original Copyright header below														   *
 *																							   *
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

#include "shipcadmodel.h"
#include "filebuffer.h"
#include "subdivsurface.h"

using namespace ShipCAD;

ShipCADModel::ShipCADModel()
	: _prefs(this), _control_curves(true), // control curves are memory managed here
	  _vis(this), _settings(this)
{
	clear();
}

ShipCADModel::~ShipCADModel()
{
	clear();
}

void ShipCADModel::clear()
{
}

void ShipCADModel::setFileChanged(bool set)
{
	if (set != _file_changed) {
		_file_changed = set;
		emit onFileChanged();
	}
}

void ShipCADModel::loadBinary(FileBuffer& source)
{
	// remember the filename because it is erased by the clear method
	QString tmpstr = _filename;
	clear();
	_filename = tmpstr;
	source.reset();
	QString hdr;
	source.load(hdr);
	if (hdr == "FREE!ship") {
		int ver, cver;
		source.load(ver);
		_file_version = versionFromInt(ver);
		cver = versionToInt(k_current_version);
		source.setVersion(_file_version);
		if (_file_version <= cver) {
			int n;
			source.load(n);
			_precision = static_cast<precision_t>(n);
			_vis.loadBinary(source);
			_settings.loadBinary(source, 0);
			// load the subdivision surface
			_surface->loadBinary(source);
			// stations
			source.load(n);
			for (int i=0; i<n; i++) {
                Intersection* intersection = Intersection::construct(this);
				intersection->loadBinary(source);
				_stations.add(intersection);
			}
			// buttocks
			source.load(n);
			for (int i=0; i<n; i++) {
                Intersection* intersection = Intersection::construct(this);
				intersection->loadBinary(source);
				_buttocks.add(intersection);
			}
			// waterlines
			source.load(n);
			for (int i=0; i<n; i++) {
                Intersection* intersection = Intersection::construct(this);
				intersection->loadBinary(source);
				_waterlines.add(intersection);
			}
			if (_file_version >= fv180) {
				// diagonals
				source.load(n);
				for (int i=0; i<n; i++) {
                    Intersection* intersection = Intersection::construct(this);
					intersection->loadBinary(source);
					_diagonals.add(intersection);
				}
				if (_file_version >= fv191) {
					// markers
                    Marker* marker = new Marker(this);
                    marker->loadBinary(source);
                    _markers.add(marker);
                    if (_file_version >= fv210) {
                        // resistance and Kaper
                    }
                }
			}
		}
		else {
            // TODO version is later than this can handle
		}
	}
	else {
        // TODO this is not a free ship binary file
	}
	_file_changed = false;
    _surface->setDesiredSubdivisionLevel(static_cast<int>(_precision)+1);
	_surface->rebuild();
	emit onUpdateGeometryInfo();
}
