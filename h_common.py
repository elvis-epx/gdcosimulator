from subprocess import *

def eq(a, b):
    return abs(a - b) < 0.000001

def lt(a, b):
    return (not eq(a, b)) and (a < b)

def lte(a, b):
    return eq(a, b) or a < b

def gt(a, b):
    return not eq(a, b) and not lt(a, b)

def explode(m):
    combinacoes = 1
    for i in m:
        min, max, step = i[2:]
        del i[2:]
        steps = []
        i.append(steps)
        x = min
        while lte(x, max):
            steps.append(x)
            x += step
        combinacoes *= len(steps)
    return combinacoes

def find_float_tuple(needle, haystack):
    for i in range(0, len(haystack)):
        hay = haystack[i]
        equal = True
        for j in range(0, len(needle)):
            equal = equal and eq(needle[j], hay[j])
        if equal:
            del haystack[i]
            return True
    return False

def run_simulation_grid(fname, v, script, condition=lambda x: True):
    valid_data_file = False
    ready = []

    try:
        previous_records = open(fname, "r").readlines()
        for line in previous_records:
            if line[0] == "@" and len(line) > 10:
                valid_data_file = True
            if line[0] == "#" and len(line) > 10:
                ivars = [ float(x) for x in line[1:].split() ]
                ivars = tuple(ivars[0:len(v)])
                ready.append(ivars)

    except IOError:
        print "Erro ao abrir arquivo de dados, criara um novo"
        pass

    if valid_data_file:
        f = open(fname, "a")
    else:
        ready = []
        f = open(fname, "w")
        f.write("@ " + str(v) + "\n")

    combinacoes = explode(v)
    print combinacoes, "combinacoes"

    cpu = 2
    params_list = []

    for rodada in range(0, combinacoes):
        r = rodada
        params = []
        for param in v:
            steps = param[2]
            item = rodada % len(steps)
            params.append(steps[item])
            rodada /= len(steps)

        params = tuple(params)
        if not condition(params):
            params = 1
        elif find_float_tuple(params, ready):
            params = 2

        params_list.append((r, params))

    while params_list:
        tasks = []

        for i in range(0, cpu):
            if params_list:
                r, params = params_list[0]
                del params_list[0]
                print "#%d:" % r, params
                if type(params) is int:
                    if params == 1:
                        print "\t\tPulado"
                    else:
                        print "\t\tJa calculado"
                    continue

                cmd = ("./%s " % script) + (("%.4f " * len(params)) % params)
                p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True,
                        shell=True)
                o, i, e = (p.stdout, p.stdin, p.stderr)
                tasks.append((r, p, o, i, e))

        error = None

        for task in tasks:
            r, p, o, i, e = task
            p.wait()
            data = o.readlines()
            error = e.readlines()
            i.close()
            o.close()
            e.close()

            if error:
                print error
                break

            data = data[0]
            print data
            f.write(data)
            f.flush()

        if error:
            break
