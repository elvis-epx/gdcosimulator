import Tkinter as tk
import math
from h_common import *
from functools import partial

try:
    import psyco
    psyco.full()
except ImportError:
    pass

vectorlist = []
data = []
ponto_ativo = []
header = []
ranges = []
filter = {}
xaxis = "K%"
menus = {}
menulabels = {}
menulists = {}
zlist = []

root = tk.Tk()
f = tk.Frame(root)
c = tk.Canvas(f, width=1100, height=500, bg='white')

dialog = None

class MyDialog:
    def __init__(self):
        top = self.top = tk.Toplevel(root)
        top.transient(root)
        self.tv = tk.StringVar()
        tk.Label(top, textvariable=self.tv).pack(side=tk.TOP)
        top.protocol("WM_DELETE_WINDOW", self.fechar)

    def text(self, txt):
        self.tv.set(txt)

    def fechar(self):
        global dialog
        dialog = None
        self.top.destroy()

def modal(txt):
    global dialog
    if not dialog:
        dialog = MyDialog()
    dialog.text(txt)

def v(s):
    return vectorlist.index(s)

def vectors(items):
    for ivar in header:
        vectorlist.append(ivar[0])
    vectorlist.extend(items)

def calc_ranges(seq=data):
    global ranges
    ranges.extend([ [99999999999.9, -999999999999.9, 0] \
               for i in range(0, len(vectorlist)) ])
    for record in seq:
        for vector in range(0, len(vectorlist)):
            ranges[vector][0] = min(ranges[vector][0], record[vector])
            ranges[vector][1] = max(ranges[vector][1], record[vector])
    for item in ranges:
        item[2] = max(item[1] - item[0], 0.000000001)

def fix_ranges():
    gr = {"Rend": [99999999999.0, -99999999999.0], \
          "Volat": [9999999999.9, -99999999999.0]}
    # find more general ranges
    for vs in vectorlist:
        vi = v(vs)
        for k in gr.keys():
            l = len(k)
            if vs[0:l] == k:
                gr[k][0] = min(gr[k][0], ranges[vi][0])
                gr[k][1] = max(gr[k][1], ranges[vi][1])

    # fix individual ranges 
    for vs in vectorlist:
        vi = v(vs)
        for k in gr.keys():
            l = len(k)
            if vs[0:l] == k:
                ranges[vi][0] = gr[k][0]
                ranges[vi][1] = gr[k][1]
                ranges[vi][2] = max(gr[k][1] - gr[k][0], 0.000001)

def steps(s):
    i = v(s)
    min, max, faixa = ranges[i]

    l = []
    for item in header:
        if item[0] == s:
            l = []
            step = item[2]
            while lte(step, max):
                l.append(step)
                step += item[4]
            return l

    step = faixa / 10.0

    while lte(min, max):
        l.append(min)
        min += step

    return l

def make_label(name, n):
    if name[-1] == "%":
        return "%.1f%%" % (n * 100.0)
    return "%.1f" % n


def read_data(f):
    global header, data
    rawdata = open(f).readlines()
    for l in rawdata:
        l = l[:-1]
        if l[0] == "@":
            header.extend(eval(l[1:]))
        elif l[0] == "#":
            data.append([ float(x) for x in l[1:].split() ])

def graph_item(virgin, offset, c, seq, xvar, yvar, cor, cor2):
    vx = v(xvar)
    vy = v(yvar)
    width = int(c.cget("width"))
    height = int(c.cget("height"))
    w = width - 250
    h = height - 100
    a = 3

    for ativo_status in [False, True]:
        if (ativo_status is False) and (virgin is False):
            continue
        tag = "ativo"
        cor_ponto = cor
        if not ativo_status:
            tag = "inativ"
            cor_ponto = cor2
        for i in range(0, len(seq)):
            ativo = ponto_ativo[i]
            if ativo is not ativo_status:
                if virgin:
                    ativo = False
                else:
                    continue
            record = seq[i]
            x = w  * (record[vx] - ranges[vx][0]) / ranges[vx][2]
            y = h * (record[vy] - ranges[vy][0]) / ranges[vy][2]
            y = 50 + h - y
            x = 100 + x
            x += offset * (a + 1) * 2
            # tag = yvar+str(i)
            id = c.create_rectangle(x-a, y-a, x+a, y+a, fill=cor_ponto, \
                                    outline=cor_ponto, tag=tag)
            c.tag_bind(id, '<Button-1>', partial(clicked, yvar, i, ativo))

def clicked(yvar, i, ativo, evento):
    record = data[i]
    txt = "Ponto #%d %s%s  " % (i, ativo and " " or "(inativo) ", yvar)
    txt += "\n\n"
    for ivar in vectorlist:
        txt += ivar + ": " + make_label(ivar, record[v(ivar)]) + "  \n"
    modal(txt)

virgin = True

def graph_points(c, seq, xvar):
    global virgin

    if virgin:
        vx = v(xvar)
        vy = v("RendCor")
        width = int(c.cget("width"))
        height = int(c.cget("height"))
        w = width - 250
        h = height - 100

        for step in steps(xvar):
            x = w * (step - ranges[vx][0]) / ranges[vx][2]
            x = 100 + x
            y = height - 10
            s = make_label(xvar, step)
            c.create_text(x, y, anchor=tk.SW, text=s)
            c.create_rectangle(x - 7, 0, x - 7, height, outline="#aaa")
        root.update_idletasks()
    
        vy = v("RendCor")
        for step in steps("RendCor"):
            x = 10
            y = h * (step - ranges[vy][0]) / ranges[vy][2]
            y = 50 + h - y
            s = make_label("RendCor", step)
            c.create_text(x, y, anchor=tk.SW, text=s)
            c.create_rectangle(x, y, width - x, y, outline="#aaa")
        root.update_idletasks()
            
        vy = v("Volat")
        for step in steps("Volat"):
            x = width - 40
            y = h * (step - ranges[vy][0]) / ranges[vy][2]
            y = 50 + h - y
            s = make_label("RendCor", step)
            c.create_text(x, y, anchor=tk.SW, text=s)
        root.update_idletasks()
    
    c.delete("ativo")

    pos = 0
    for zitem in zlist:
        if zitem:
            yvar, color, color2 = zitem
            graph_item(virgin, pos, c, seq, xvar, yvar, color, color2)
            root.update_idletasks()
        pos += 1

    virgin = False

def update_filters_and_plot(*dummy):
    update()

def update_filters(*dummy):
    global xaxis
    for ivar in menulists.keys():
        selected = menulists[ivar].curselection()
        if selected:
            selected = int(selected[0])
            if ivar == "X":
                xaxis = menus[ivar][selected]
            elif selected > 0:
                filter[ivar] = menus[ivar][selected]
            elif ivar in filter:
                del filter[ivar]
    update_filter_labels()

def update_filter_labels():
    for ivar in menulabels.keys():
        if ivar == "X":
            menulabels["X"].set(xaxis)
        elif ivar in filter:
            menulabels[ivar].set(make_label(ivar, filter[ivar]))
        else:
            menulabels[ivar].set("All")

def do_filter():
    del ponto_ativo[:]
    for record in data:
        ok = True
        for ivar in filter.keys():
            iv = v(ivar)
            ok = ok and eq(filter[ivar], record[iv])
        ponto_ativo.append(ok)

def update(*dummy):
    root.config(cursor="wait")
    root.update_idletasks()
    update_filters()
    root.update_idletasks()
    do_filter()
    root.update_idletasks()
    graph_points(c, data, xaxis)
    root.update_idletasks()
    root.config(cursor="")
    root.update_idletasks()

def add_listbox(dash, ivar, name, roll, roll_menu, numerico):
    menus[ivar] = {}
    fb = tk.Frame(dash)
    menulists[ivar] = b = tk.Listbox(fb, width=15)
    tb = tk.Label(fb, text=name)
    menulabels[ivar] = sv = tk.StringVar()
    tb2 = tk.Label(fb, textvariable=sv)
    pos = 0
    if numerico:
        b.insert(tk.END, "All")
        menus[ivar][pos] = None
        pos = 1
    for i in range(0, len(roll)):
        b.insert(tk.END, roll_menu[i])
        menus[ivar][pos] = roll[i]
        pos += 1
    tb.pack(side=tk.TOP)
    tb2.pack(side=tk.TOP)
    b.pack(side=tk.BOTTOM)
    fb.pack(side=tk.LEFT)
    b.select_set(0)
    b.bind("<Double-Button-1>", update_filters_and_plot)


def make_screen():
    dash = tk.Frame(f)
    c.pack(side=tk.TOP)
    dash.pack(side=tk.BOTTOM)
    f.pack()

    l = [ hrecord[0] for hrecord in header ]
    add_listbox(dash, "X", "X axis", l, l, False)

    for ivar in header:
        ivar = ivar[0]
        roll = steps(ivar)
        if len(roll) > 1:
            roll_labels = [ make_label(ivar, val) for val in roll ]
            add_listbox(dash, ivar, ivar, roll, roll_labels, True)

    # butt = tk.Button(dash, text="update", command=update)
    # butt.pack(side=tk.LEFT)
    root.after(300, update)
    root.mainloop()

def run_graph(pzlist):
    calc_ranges()
    fix_ranges()
    zlist.extend(pzlist)
    make_screen()
