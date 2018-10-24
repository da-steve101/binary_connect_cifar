import csv

# open the tern op lists and output the avg add, avg reg, avg PAG
def get_perf_mat( idx ):
    f = open( "../resources/mat" + str(idx) + "_tern_op_list.csv" )
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
    return add_count, reg_count

if __name__ == "__main__":
    res = [ get_perf_mat(i+1) for i in range(10) ]
    # res = [ get_perf_mat(2) ]
    adds = sum([ x[0] for x in res ])/len(res)
    regs = sum([ x[1] for x in res ])/len(res)
    print( "adds", adds )
    print( "regs", regs )
    print( "pag", adds + regs )
    
