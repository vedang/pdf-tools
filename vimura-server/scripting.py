"""
Copyright (C) 2022  Daniel Nicolai

Author: Daniel Nicolai <dalanicolai@gmail.com>
Keywords: files, multimedia

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from vimura import *

doc
script_p = doc[4]
a = script_p.add_line_annot((0,0), (100, 100))
print(list(script_p.annots()))
print(x)
a.set_border(width=0.3, dashes=[2])
a.set_colors(stroke=(1,0,0), fill=(1,0,0))
a.update()
ans = script_p.annots()
print(next(ans))
