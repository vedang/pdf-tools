#!/usr/bin/env python

import sys
import datetime as dt
import time
import pytz


import fitz
from PIL import Image, ImageDraw

doc = None
selection = None

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
    doc = fitz.open(args[0].replace("_", "-"))  # replace back (see while loop)
    print("OK\n.")

def quit():
    sys.exit()

def save(*args):
    doc.save("/tmp/test.pdf")
    print("OK\n/tmp/test.pdf\n.")

def close(*args):
    doc.save("/tmp/test.pdf")
    print("OK\n1\n.")

def number_of_pages(*args):
    doc = fitz.open(args[0].replace("_", "-"))  # replace back (see while loop)
    print("OK\n{}\n.".format(len(doc)))

def pagesize(*args):
    p = int(args[1]) - 1
    size = doc[p].mediabox_size
    print("OK\n{}:{}\n.".format(size[0], size[1]))

# def renderpage(*args,
#                foreground=None,
#                background=None,
#                highlight_text=None):
#     doc = fitz.open(args[0])
#     p = doc[int(args[1]) - 1]
#     width = int(args[2])
#     zoom = width/p.mediabox_size[0]
#     mat = fitz.Matrix(zoom, zoom)
#     pix = p.get_pixmap(matrix=mat)
#     tmpfile = "/tmp/tmpimg"

#     pix.save(tmpfile)

#     if highlight_text:
#         with Image.open(tmpfile) as im:
#             draw = ImageDraw.Draw(im, 'RGBA')
#             edges = denormalize_edges(p,
#                                       [float(i)*zoom
#                                        for i in highlight_text.split()])
#             draw.rectangle(edges, fill=(128,128,128,128))  # foreground is lighter

#             im.save(tmpfile, "PNG")

#     print("OK\n{}\n.".format(tmpfile))

def renderpage(*args,
               foreground=None,
               background=None,
               alpha=None,
               highlight_text=None,
               highlight_region=None):
    doc = fitz.open(args[0].replace("_", "-"))  # replace back (see while loop)
    p = doc[int(args[1]) - 1]
    width = int(args[2])
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    pix = p.get_pixmap(matrix=mat)
    tmpfile = "/tmp/tmpimg"

    pix.save(tmpfile)

    if highlight_text:

        global selection

        with Image.open(tmpfile) as im:
            draw = ImageDraw.Draw(im, 'RGBA')
            drag_edges = highlight_text.split()
            edges = denormalize_edges(p, [float(e) for e in [0,
                                                             drag_edges[1],
                                                             1,
                                                             drag_edges[3]]])
            words = p.get_text('words', sort=True, clip=edges)
            selection = [[i*zoom for i in w[0:4]] for w in words]
            for w in selection:
                draw.rectangle(w, fill=(128, 128, 128, 128))

            im.save(tmpfile, "PNG")

    print("OK\n{}\n.".format(tmpfile))

def getselection(*args):
    p = doc[int(args[1]) - 1]
    # size = p.mediabox_size
    # selections = [[str(j[i]/size[0]) if i in [0, 2]
    #                else str(j[i]/size[1])
    #                for i in range(0, 4)]
    #               for j in p.get_text("words")]
    # selections_formatted = "\n".join([" ".join(j) for j in selections])
    print("OK\n{}\n.".format(args[2]))
    # print("OK\n{}\n.".format(selections_formatted))

def pdf_date ():
    now = dt.datetime.now(pytz.timezone(time.tzname[0]))
    ans_date  = now.strftime('%Y%m%d%H%M%S%z')
    # insert dingle quotes,
    # see https://pymupdf.readthedocs.io/en/latest/document.html#Document.metadata
    return ans_date[:-2] + "'" + ans_date[-2:] + "'"

def print_links(page):
    p = doc[page]
    l = p.first_link
    i = 0
    while l:
        r, g, b = [int(i*255) for i in l.colors['stroke']]
        print("{}:{}:link:annot-{}-{}:0:{}::".format(page + 1,
                                                     " ".join([str(e) for e in l.rect]),
                                                     page + 1,
                                                     len(list(p.annots())) + i,
                                                     "#{:02x}{:02x}{:02x}".format(r, g, b)))
        l = l.next
        i += 1

def getannots(*args):
    start_page = int(args[1]) - 1
    end_page = int(args[2])
    if end_page <= 0:
        end_page = len(doc)
    print("OK")
    for p in range(start_page, end_page):
        for i, a in  enumerate(doc[p].annots()):
            r, g, b = [int(c*255) for c in a.colors['stroke']]
            print("{}:{}:{}:annot-{}-{}:0:::D\\:{}:::1.0::::{}".format(p + 1,
                                                                       " ".join([str(e) for e in a.rect]),
                                                                       a.type[1].lower(),
                                                                       p + 1,
                                                                       i,
                                                                       pdf_date(),
                                                                       "#{:02x}{:02x}{:02x}".format(r, g, b)))
        print_links(p)
    print(".")

def addannot(*args):
    page = int(args[1]) - 1
    p = doc[page]
    edges = fitz.Rect(denormalize_edges(p, [float(e) for e in args[3].split()]))
    match args[2]:
        case 'highlight':
            p.add_highlight_annot(start=fitz.Point(edges[0:2]),
                                  stop=fitz.Point(edges[2:4]))
        case 'line':
            p.add_line_annot(fitz.Point(edges[0:2]),
                             fitz.Point(edges[2:4]))
    print("OK\n{}:{}:{}:annot-{}-{}:0:::D\\:{}:::1.0::::{}\
    \n.".format(args[1],
                args[3],
                args[2],
                args[1],
                len(list(p.annots())),
                pdf_date(),
                args[3]))

# def editannot(*args):
#     print("OK\n.")

def editannot(*args):
    key = args[1].replace("_", "-")
    key_parts = key.split("-")
    page, n = [int(i) for i in key_parts[1:]]
    p = doc[page - 1]
    annot = list(p.annots())[n - 1]
    r, g, b = [int(c*255) for c in annot.colors['stroke']]
    edges = " ".join([str(e) for e in normalize_edges(p, annot.rect)])
    print("OK\n{}:{}:{}:{}:color:{}:{}:Daniel Nicolai::1.0::::{}\n.".format(page,
                                                                      edges,
                                                                      annot.type[1].lower(),
                                                                      key,
                                                                      "#{:02x}{:02x}{:02x}".format(r, g, b),
                                                                      pdf_date(),
                                                                      edges))

def delannot(*args):
    print("OK\n.")

def features(*args):
    print("OK\ncase-sensitive-search:writable-annotations:markup-annotations\n.")

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
