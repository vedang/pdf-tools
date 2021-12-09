#!/usr/bin/env python

import sys

import fitz
from PIL import Image, ImageDraw

doc = None

def stringify(list):
    return ["'" + i + "'" for i in list]


def normalize_edges(page_obj, edges):
    "Transform vimura edges to (normalized) pdf-tools edges."
    size = page_obj.mediabox_size
    return [edges[i]/size[0] if i in [0, 2] else edges[i]/size[1]
            for i in range(0, 4)]

def denormalize_edges(page_obj, edges):
    "Transform (normalized) pdf-tools edges to vimura edges."
    size = page_obj.mediabox_size
    return [edges[i]*size[0] if i in [0, 2] else edges[i]*size[1]
            for i in range(0, 4)]

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

def renderpage(*args,
               foreground=None,
               background=None,
               highlight_text=None):
    doc = fitz.open(args[0])
    p = doc[int(args[1]) - 1]
    width = int(args[2])
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    pix = p.get_pixmap(matrix=mat)
    tmpfile = "/tmp/tmpimg"

    pix.save(tmpfile)

    if highlight_text:
        with Image.open(tmpfile) as im:
            draw = ImageDraw.Draw(im, 'RGBA')
            edges = denormalize_edges(p,
                                      [float(i)*zoom
                                       for i in highlight_text.split()])
            draw.rectangle(edges, fill=(128,128,128,128))  # foreground is lighter

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
