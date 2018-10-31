import csv
import sys

# open the tern op lists and output the avg add, avg reg, avg PAG
def get_perf_mat( fname ):
    f = open( fname )
    rdr = csv.reader( f )
    mat = [ [ int(x) for x in y ] for y in rdr ]
    outputs = set(mat[0])
    logic_depths = {}
    add_count = 0
    reg_count = 0
    for op in mat[1:]:
        if op[2] > -1:
            add_count += 1
        else:
            reg_count += 1
        if op[1] in logic_depths:
            logic_depths[op[0]] = logic_depths[op[1]] + 1
        else:
            logic_depths[op[0]] = 1
    max_depth = max([ logic_depths[x] for x in logic_depths ])
    for o in outputs:
        if o > -1:
            reg_count += ( max_depth - logic_depths[o] )
    return add_count, reg_count, add_count + reg_count

if __name__ == "__main__":
    fnames = sys.argv[1:]
    res = [ get_perf_mat(f) for f in fnames ]
    for f, r in zip( fnames, res ):
        print( f )
        print( "adds", r[0] )
        print( "regs", r[1] )
        print( "pag", r[2] )
    print( "avg add", sum( [ r[0] for r in res ] )/len(res) )
    print( "avg reg", sum( [ r[1] for r in res ] )/len(res) )
    print( "avg pag", sum( [ r[0]+r[1] for r in res ] )/len(res) )
