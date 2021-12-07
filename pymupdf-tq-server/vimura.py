#!/usr/bin/env python

import sys

import fitz
from PIL import Image, ImageDraw

doc = None

def stringify(list):
    return ["'" + i + "'" for i in list]

def open(*args):
    global doc
    doc = fitz.open(args[0])
    print("OK\n.")

def quit():
    sys.exit()

def number_of_pages(*args):
    doc = fitz.open(args[0])
    print("OK\n{}\n.".format(len(doc)))

def pagesize(*args):
    p = int(args[1]) - 1
    size = doc[p].mediabox_size
    print("OK\n{}:{}\n.".format(size[0], size[1]))

def renderpage(*args, **kwargs):
    doc = fitz.open(args[0])
    p = doc[int(args[1]) - 1]
    width = int(args[2])
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    pix = p.get_pixmap(matrix=mat)
    tmpfile = "/tmp/tmpimg"

    pix.save(tmpfile)

    if len(args) > 3:
        with Image.open(tmpfile) as im:
            draw = ImageDraw.Draw(im)
            draw.line((0, 0) + im.size, fill=128)
            draw.line((0, im.size[1], im.size[0], 0), fill=128)

            im.save(tmpfile, "PNG")

    print("OK\n{}\n.".format(tmpfile))

def getselection(*args):
    p = doc[int(args[1]) - 1]
    size = p.mediabox_size
    selections = [[str(j[i]/size[0]) if i in [0, 2]
                   else str(j[i]/size[1])
                   for i in range(0, 4)]
                  for j in p.get_text("words")]
    selections_formatted = "\n".join([" ".join(j) for j in selections])
    print("OK\n{}\n.".format(selections_formatted))

def getannots(*args):
    print("OK\n.")

def features(*args):
    print("OK\n.")

# if len(sys.argv) > 2:
#     print("usage: epdfinfo [ERROR-LOGFILE]", file=sys.stderr)
#     exit(1)
# elif len(sys.argv) == 2:
#     error_log = sys.argv[1]
# else:
# def test(query):
while query := sys.stdin.readline():
    query = query.replace("-", "_")
    commands = [[list(filter(len, i.split(":")))
                 for i in j.split("\\")]
                for j in list(filter(len, query.split("\n")))]
    for c in commands:
        arglist = stringify(c[0][1:])
        # if len(c) > 1:
        kwargs = ['='.join([k[0], "'" + k[1] + "'"]) for k in c[1:]]
        arglist = arglist + kwargs
        eval(c[0][0] + "(" + ", ".join(["{}"] * len(arglist)).format(*arglist) + ")")
