from vimura import *

script_p = doc[4]
a = script_p.add_line_annot((0,0), (100, 100))
print(list(script_p.annots()))
print(x)
a.set_border(width=0.3, dashes=[2])
a.set_colors(stroke=(1,0,0), fill=(1,0,0))
a.update()
ans = script_p.annots()
print(next(ans))
