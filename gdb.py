unit = gdb.lookup_global_symbol("Unit").value()
deadbeef = gdb.lookup_global_symbol("deadbeef").value()

abc_any = gdb.lookup_type("union any")

def color(s, c):
    return "\x1b[" + str(c) + "m" + s + "\x1b[0m"

def gray(s):
    return color(s, 90)

def red(s):
    return color(s, "1;31")

def p(indent, tag, value):
    print(" " * indent + tag + ": " + str(value))

def print_abc(i, v):
    v = v.cast(abc_any)
    vt = v['as_tagged']
    if v == unit:
        p(i, "Unit", "Unit")
    elif v == deadbeef:
        p(i, "Dead", "Beef")
    elif vt == 0:
        p(i, red("!!!NULL POINTER!!!"), "This should never happen")
    elif (vt & 0xfff0000000000000) != 0:
        p(i, "Number", (~vt).cast(abc_any)['as_num'])
    elif vt < 0x00007f0000000000: # FIXME should get actual mappings -- don't know how to.
        block = gdb.block_for_pc(int(vt))
        if block == None:
            name = str(v['as_indirect'])
        else:
            name = str(block.function)
        p(i, "Block", name)
    else:
        tag = vt &  0x3
        ptr = vt & ~0x3
        hexptr = gray(hex(int(ptr)))
        v = ptr.cast(abc_any)
        try:
            if tag == 0:
                pair = v['as_pair'].dereference()
                if pair['snd'] == deadbeef:
                    p(i, "Left", hexptr)
                    print_abc(i+4, pair['fst'])
                else:
                    p(i, "Pair", hexptr)
                    print_abc(i+4, pair['fst'])
                    print_abc(i+4, pair['snd'])
            elif tag == 1:
                pair = v['as_comp_block'].dereference()
                if pair['yz'] == deadbeef:
                    p(i, "Right", hexptr)
                    print_abc(i+4, pair['xy'])
                else:
                    p(i, "Composed", hexptr)
                    print_abc(i+4, pair['xy'])
                    print_abc(i+4, pair['yz'])
            elif tag == 2:
                p(i, "Quoted", hexptr)
                print_abc(i+4, v['as_indirect'].dereference())
            elif (tag & 0x4) != 0:
                p(i, "Forwarding pointer", hexptr)
                print_abc(i+4, v['as_indirect'].dereference())
            else:
                p(i, "INVALID TAG", hexptr)
        except gdb.MemoryError:
            p(i, red("!!!INVALID POINTER!!!"), hexptr)

class PrintABCValue(gdb.Command):
    def __init__(self):
        super(PrintABCValue, self).__init__('print-abc-value', gdb.COMMAND_DATA, gdb.COMPLETE_SYMBOL)

    def invoke(self, arg, tty):
        print_abc(0, gdb.parse_and_eval(arg))

PrintABCValue()
