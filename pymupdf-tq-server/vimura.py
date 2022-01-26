#!/usr/bin/env python

'''
Authors: Daniel Nicolai <dalanicolai@hotmail.com>
'''

# TODO Create separate annotation_print functions
# see also xpoppler_annot_type_string in epdfinfo.c

import sys, io
import traceback
import getpass
import logging
import datetime as dt
import time

import pytz

import fitz
from PIL import Image, ImageDraw

sys.stderr = open('vimura_err.log', 'w')

logging.basicConfig(filename='vimura.log', filemode='w', level=logging.DEBUG)
# logging.basicConfig(filename='vimura.log', encoding='utf-8', level=logging.DEBUG)


doc = None
# zoom = None
current_page = None
# current_page_text = None
# selection = None


### Helper functions

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

def point_to_word(point):
    i = 0
    while point[1] > current_page_text[i][3]:
        i += 1
    while point[0] > current_page_text[i][2]:
        i += 1
    return current_page_text[i]


### API functions

def test(*args):
    print("OK\n{}\n.".format(zoom))


def open(*args):
    global doc
    doc = fitz.open(args[0])  # .replace("_", "-"))  # replace back (see while loop)
    print("OK\n.")

def quit():
    sys.exit()

def save(*args):
    doc.save("/tmp/test.pdf")
    print("OK\n/tmp/test.pdf\n.")

def close(*args):
    doc = None
    print("OK\n1\n.")

def number_of_pages(*args):
    global doc
    doc = fitz.open(args[0])  # .replace("_", "-"))  # replace back (see while loop)
    print("OK\n{}\n.".format(len(doc)))

def pagesize(*args):
    global doc  # this function sometimes needs to load the doc
    if not doc:
        doc = fitz.open(args[0])  #.replace("_", "-"))  # replace back (see while loop)
    p = int(args[1]) - 1
    size = doc[p].mediabox_size
    print("OK\n{}:{}\n.".format(size[0], size[1]))

def renderpage(*args,
               foreground=None,
               background=None,
               alpha=None,
               highlight_text=None,
               highlight_region=None,
               draw_line=None):
    global doc, current_page, current_page_text, zoom
    logging.debug("And these are the args: %s", args)
    logging.debug("And the keyword args: %s\n\n", kwargs)
    if not doc:
        doc = fitz.open(args[0])  #.replace("_", "-"))  # replace back (see while loop)
    page = int(args[1]) - 1
    p = doc[page]
    if page != current_page:
        current_page_text = p.get_text("words")
        current_page = page
    width = int(args[2])
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    pix = p.get_pixmap(matrix=mat)
    tmpfile = "/tmp/tmpimg"

    if highlight_text or highlight_region or draw_line:
        image_data = pix.tobytes()
        with Image.open(io.BytesIO(image_data)) as im:
            draw = ImageDraw.Draw(im, 'RGBA')
            edges_from_type = highlight_text or highlight_region or draw_line
            drag_edges = edges_from_type.split()
            logging.debug("These are the edges_from_type and drag_edges: %s, %s", edges_from_type, drag_edges)
            # edges = denormalize_edges(p, [float(e) for e in [0, drag_edges[1], 1, drag_edges[3]]])
            edges = denormalize_edges(p, [float(e) for e in drag_edges])
            if highlight_text:
                selection = fitz.get_highlight_selection(p,
                                                        start=fitz.Point(point_to_word(edges[0:2])[0:2]),
                                                        stop=fitz.Point(point_to_word(edges[2:4])[2:4]))
                s = [[i*zoom for i in r] for r in selection]
            else:
                s = [[i*zoom for i in edges]]
            # words = p.get_text('words', sort=True, clip=edges)
            # word_rects = [[i*zoom for i in w[0:4]] for w in words]
            # for w in word_rects:
            #     draw.rectangle(w, fill=(128, 128, 128, 128))
            for r in s:
                if draw_line:
                    logging.debug("These are line r: %s", [tuple(r[0:2]), tuple(r[2:4])])
                    draw.line([tuple(r[:2]), tuple(r[2:4])], fill="red", width=2)
                else:
                    draw.rectangle(list(r), fill=(128, 128, 128, 128))

            im.save(tmpfile, "PNG")
    else:
        pix.save(tmpfile)


    print("OK\n{}\n.".format(tmpfile))

def getselection(*args):
    p = doc[int(args[1]) - 1]
    if args[2] == "0 0 1 1":
        size = p.mediabox_size
        selections = [[str(j[i]/size[0])
                       if i in [0, 2]
                       else str(j[i]/size[1])
                       for i in range(0, 4)]
                      for j in p.get_text("words")]
        selections_formatted = "\n".join([" ".join(j) for j in selections])
        print("OK\n{}\n.".format(selections_formatted))
    else:
        print("OK\n{}\n.".format(args[2]))

def get_text_line(text, word):
    line_text = ""
    i = 0
    while (t := text[i][3]) <= word[3]:
        if t == word[3]:
            line_text += text[i][4] + " "
        i += 1
    return line_text

def regexp_flags(*args):
    print("OK\n.")


def search_regexp(*args):
    start_page = int(args[1]) - 1
    end_page = int(args[2])
    print("OK")
    for i in range(start_page, end_page):
        p = doc[i]
        hits = p.search_for(args[3])
        if hits:
            page_text = p.get_text("words")
        for h in hits:
            nh = [i for i in normalize_edges(p, h)]
            print('{}:{}:{}:{}'.format(i+1,
                                       args[3],
                                       get_text_line(page_text, h),
                                       " ".join([str(n) for n in nh])))
    print(".")


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
        logging.debug("p and stroke are %s, %s", p, l.colors['stroke'])
        if s := l.colors['stroke']:
            r, g, b = [int(i*255) for i in s]
            print("{}:{}:link:annot-{}-{}:0:{}::".format(page + 1,
                                                         " ".join([str(e) for e in l.rect]),
                                                         page + 1,
                                                         len(list(p.annots())) + i,
                                                         "#{:02x}{:02x}{:02x}".format(r, g, b)))
        l = l.next
        i += 1

# TODO create links for other types
# (see xpoppler_action_type_string in epdfinfo.c)
def pagelinks(filepath, real_pn):
    p = doc[int(real_pn) - 1]
    size = p.mediabox_size
    print("OK")
    for l in p.links(kinds=[fitz.LINK_GOTO]):
        edges = l['from']
        dest_height = l['to'][1]/size[1]
        print("{}:goto-dest::{}:{}".format(" ".join(str(i) for i in normalize_edges(p, edges)),
                                           l['page'] + 1,
                                           dest_height))
    print(".")


def getannots(*args):
    start_page = int(args[1]) - 1
    end_page = int(args[2])
    if end_page <= 0:
        end_page = len(doc)
    print("OK")
    for p in range(start_page, end_page):
        logging.debug("p and doc are %s, %s", p, doc)
        for i, a in  enumerate(doc[p].annots()):
            annot_info = a.info
            logging.debug("i and a are %s, %s", i, a)
            annot_rect = " ".join([str(e) for e in normalize_edges(doc[p], a.rect)])
            annot_popup_rect = " ".join([str(e) for e in normalize_edges(doc[p], a.popup_rect)])
            r, g, b = [int(c*255) for c in a.colors['stroke']]
            # TODO find out how pdf sets user name to replace 'getpass'
            print(
                "{}:{}:{}:annot-{}-{}:0:{}:{}:D\\:{}:{}::1.0:{}:{}::{}".format(p + 1,
                                                                                 annot_rect,
                                                                                 a.type[1].lower(),
                                                                                 p + 1,
                                                                                 i,
                                                                                 "#{:02x}{:02x}{:02x}".format(r, g, b),
                                                                                 annot_info['content'],
                                                                                 annot_info['creationDate'],
                                                                                 getpass.getuser(),
                                                                                 annot_popup_rect,
                                                                                 annot_info['creationDate'],
                                                                                 annot_rect))
        print_links(p)
    print(".")

def addannot(*args):
    page = int(args[1]) - 1
    p = doc[page]
    edges = fitz.Rect(denormalize_edges(p, [float(e) for e in args[3].split()]))
    if args[2] == "line":
        annot = p.add_line_annot(fitz.Point(edges.tl),
                                 fitz.Point(edges.br))
        # logging.debug("annot is %s", annot,)
        blue = (0, 0, 1)
        annot.set_colors(stroke=blue, fill=blue)
        # see https://pymupdf.readthedocs.io/en/latest/vars.html#annotation-line-ending-styles
        annot.set_line_ends(0, fitz.PDF_ANNOT_LE_CLOSED_ARROW)
        annot.update()  # (fill_color=blue)
    else:
        b = fitz.Point(point_to_word(edges.tl)[0:2])
        e = fitz.Point(point_to_word(edges.br)[2:4])
        match args[2]:
            case 'highlight':
                p.add_highlight_annot(start=b, stop=e)
            case 'squiggly':
                p.add_squiggly_annot(start=b, stop=e)
            case 'underline':
                p.add_underline_annot(start=b, stop=e)
            case 'strike_out':
                p.add_strikeout_annot(start=b, stop=e)
    print("OK\n{}:{}:{}:annot-{}-{}:0:::D\\:{}:::1.0::::{}\
    \n.".format(args[1],
                args[3],
                args[2],
                args[1],
                len(list(p.annots())),
                pdf_date(),
                args[3]))

def editannot(*args):
    logging.debug("Run editannot with args: %s", args)
    key = args[1].replace("_", "-")
    key_parts = key.split("-")
    page, n = [int(i) for i in key_parts[1:]]
    p = doc[page - 1]
    annot = list(p.annots())[n - 1]
    r, g, b = [int(c*255) for c in annot.colors['stroke']]
    # edges = " ".join([str(e) for e in normalize_edges(p, annot.rect)])
    edges = " ".join([str(e) for e in annot.rect])
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

# server loop reading single lines
try:
    while query := sys.stdin.readline():
        logging.debug("This is the query: %s", query)
        raw_commands = [[list(filter(len, i.split(":")))
                    for i in j.split("\\")]
                    for j in list(filter(len, query.split("\n")))]
        commands =  [[[item.replace("-", "_")  if i != 1 else item
                    for i, item in enumerate(j)] for j in raw_commands[0]]]
        if len(raw_commands) > 1:
            commands += [raw_commands[1:]]
        for c in commands:
            arglist = stringify(c[0][1:])
            # if len(c) > 1:
            kwargs = ['='.join([k[0], "'" + k[1] + "'"]) for k in c[1:]]
            arglist = arglist + kwargs
            eval_string = c[0][0] + "(" + ", ".join(["{}"] * len(arglist)).format(*arglist) + ")"
            logging.debug("This is the eval string: %s", eval_string)
            eval(c[0][0] + "(" + ", ".join(["{}"] * len(arglist)).format(*arglist) + ")")
except:
    print("ERR")  # see ‘pdf-info-query--parse-response'
    print(traceback.format_exc())
    print(".")
