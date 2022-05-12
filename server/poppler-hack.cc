// Copyright (C) 2013, 2014  Andreas Politz

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <config.h>
#include <glib.h>
#include <glib-object.h>

extern "C"
{

GType poppler_annot_get_type (void) G_GNUC_CONST;
GType poppler_annot_markup_get_type (void) G_GNUC_CONST;

#define POPPLER_TYPE_ANNOT (poppler_annot_get_type ())
#define POPPLER_ANNOT(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), POPPLER_TYPE_ANNOT, PopplerAnnot))
#define POPPLER_IS_ANNOT_MARKUP(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), POPPLER_TYPE_ANNOT_MARKUP))
#define POPPLER_TYPE_ANNOT_MARKUP (poppler_annot_markup_get_type ())

}
