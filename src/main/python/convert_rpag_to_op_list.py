#! /usr/bin/python3

import csv

"""
Input Format:
[ opcode, out_ary, out_stage, a_ary, a_stage, a_shift, b_ary, b_stage, b_shift, { c_ary, c_stage, c_shift, } ]
Output Format:
[ outidx, a_idx, b_idx, c_idx, opcode, a_shift, b_shift, c_shift ]
opcode:
0 => - a - b - c
1 => - a - b + c
2 => - a + b - c
3 => - a + b + c
4 => + a - b - c
5 => + a - b + c
6 => + a + b - c
7 => + a + b + c
"""

def op_code( input_list, stage ):
    return "".join( [ str(x) for x in input_list ] ) + "_" + str(stage)

def find_idx( input_list, stage, op_codes ):
    if sum( [ x != 0 for x in input_list ] ) == 0:
        return "".join( [ str(x) for x in input_list ] ), False
    pos_code = op_code( input_list, stage )
    if pos_code not in op_codes:
        neg_code = op_code( [ -x for x in input_list ], stage )
        assert neg_code in op_codes, neg_code + " must be in op_codes: " + str(op_codes)
        return neg_code, True
    return pos_code, False

def get_adder_op_codes( op, op_codes ):
    out_code = op_code( op[1], op[2] )
    a_code, a_neg = find_idx( op[3], op[4], op_codes )
    b_code, b_neg = find_idx( op[6], op[7], op_codes )
    add_op = ( not a_neg ) * 4 + ( not b_neg ) * 2
    if len( op ) > 9:
        c_code, c_neg = find_idx( op[9], op[10], op_codes )
        c_idx = op_codes[c_code]
        add_op += ( not c_neg ) * 1
    else:
        c_idx = -1
    a_idx = op_codes[a_code]
    b_idx = op_codes[b_code]
    return out_code, a_idx, b_idx, c_idx, add_op

def get_reg_op_codes( op, op_codes ):
    out_code = op_code( op[1], op[2] )
    in_code, is_neg = find_idx( op[3], op[4], op_codes )
    in_idx = op_codes[in_code]
    return out_code, in_idx, ( not is_neg ) * 4

def get_output_op_code( op, op_codes ):
    return find_idx( op[1], op[2], op_codes )

def transform_to_op_list( vals, no_inputs ):
    op_codes = {}
    zero_code = "".join([ str(x) for x in [0]*no_inputs ])
    op_codes[zero_code] = -1
    for i in range( no_inputs ):
        input_ary = [0]*no_inputs
        input_ary[i] = 1
        input_code = op_code( input_ary, 0 )
        op_codes[input_code] = i
    op_list = []
    output_idxs = {}
    for op in vals:
        if op[0] == 'A':
            out_code, a_idx, b_idx, c_idx, add_op = get_adder_op_codes( op, op_codes )
            curr_idx = len(op_codes) - 1
            op_new = [ curr_idx, a_idx, b_idx, c_idx, add_op, op[5], op[8], 0 ]
            if len( op ) > 9:
                op_new[-1] = op[11]
            op_list += [ op_new ]
            op_codes[out_code] = curr_idx
        elif op[0] == 'R':
            out_code, in_idx, is_negate = get_reg_op_codes( op, op_codes )
            curr_idx = len(op_codes) - 1
            op_new = [ curr_idx, in_idx, -1, -1, is_negate, 0, 0, 0 ]
            op_list += [ op_new ]
            op_codes[out_code] = curr_idx
        elif op[0] == 'O':
            out_code, is_neg = get_output_op_code( op, op_codes )
            assert not is_neg, "Cant be neg on output"
            out_idx = op_codes[ out_code ]
            out_code = out_code.split( "_" )[0]
            output_idxs[out_code] = out_idx
        else:
            print( "ERROR: unknown option" )
    return op_list, output_idxs

def parse_line( line ):
    line = "[" + line + "]"
    return eval( line )

def parse_graph( lines ):
    lines = lines[0].split( "},{" )
    lines[0] = lines[0].split( "={{" )[1]
    lines[-1] = lines[-1].split( "}}" )[0]
    return [ parse_line( line ) for line in lines ]

def move_neg_shifts( op_list, output_idx ):
    # move all neg shifts to the last layer
    op_list_new = []
    shifted_inputs = {}
    for op in op_list:
        cnt = 0
        for idx, shift in [ ( op[1], 5 ), ( op[2], 6 ), ( op[3], 7 ) ]:
            if idx in shifted_inputs:
                op[shift] += shifted_inputs[idx]
        if op[0] not in output_idx:
            while op[5] < 0 or op[6] < 0 or op[7] < 0:
                op = op[:5] + [ op[5] + 1, op[6] + 1, op[7] + 1 ]
                cnt += 1
        op_list_new += [ op ]
        if cnt > 0:
            shifted_inputs[op[0]] = -cnt
    return op_list_new

if __name__ == "__main__":
    fname = "../resources/cifar_layer1.rpag"
    matname = "../resources/conv1_weights.csv"
    f = open( fname )
    lines = f.readlines()
    vals = parse_graph( lines )
    no_inputs = len(vals[0][1])
    op_list, output_idxs = transform_to_op_list( vals, no_inputs )
    op_list = move_neg_shifts( op_list, set(output_idxs) )
    f = open( matname )
    rdr = csv.reader( f )
    mat = [ [ int(x) for x in y ] for y in rdr ]
    output_codes = [ "".join( [ str(mat[i][j]) for i in range( no_inputs ) ] ) for j in range( len( mat[0] ) ) ]
    output_codes_neg = [ "".join( [ str(-mat[i][j]) for i in range( no_inputs ) ] ) for j in range( len( mat[0] ) ) ]
    f_out = open( fname[:-5] + "_op_list.csv", "w" )
    wrt = csv.writer( f_out )
    output_ordered = [ ( output_idxs[code], False ) if code in output_idxs else ( output_idxs[neg_code], True )
                       for code, neg_code in zip( output_codes, output_codes_neg ) ]
    # change op_list to negate the outputs
    for idx, neg in output_ordered:
        if idx >= no_inputs and neg:
            op_list[ idx - no_inputs ][4] = 7 - op_list[ idx - no_inputs ][4]
    tmp = wrt.writerow( [ x[0] for x in output_ordered ] )
    for op in op_list:
        tmp = wrt.writerow( op )
    f_out.close()
