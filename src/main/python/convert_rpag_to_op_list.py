#! /usr/bin/python3

import csv

def op_code( input_list, stage ):
    return "".join( [ str(x) for x in input_list ] ) + "_" + str(stage)

def find_idx( input_list, stage, op_codes ):
    if sum( [ x != 0 for x in input_list ] ) == 0:
        return "".join( [ str(x) for x in input_list ] ), False
    pos_code = op_code( input_list, stage )
    if pos_code not in op_codes:
        neg_code = op_code( [ -x for x in input_list ], stage )
        assert neg_code in op_codes, neg_code +" must be in op_codes: " + str(op_codes)
        return neg_code, True
    return pos_code, False

def get_adder_op_codes( op, op_codes ):
    out_code = op_code( op[1], op[2] )
    a_code, a_neg = find_idx( op[3], op[4], op_codes )
    b_code, b_neg = find_idx( op[6], op[7], op_codes )
    assert not ( a_neg and b_neg ), "Can't both be neg"
    is_add = ( not ( a_neg or b_neg ) ) * 1
    if a_neg:
        return out_code, b_code, a_code, is_add
    return out_code, a_code, b_code, is_add

def get_reg_op_codes( op, op_codes ):
    out_code = op_code( op[1], op[2] )
    in_code, is_neg = find_idx( op[3], op[4], op_codes )
    return out_code, in_code, ( not is_neg )*1

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
            out_code, a_code, b_code, is_add = get_adder_op_codes( op, op_codes )
            a_idx = op_codes[a_code]
            b_idx = op_codes[b_code]
            curr_idx = len(op_codes) - 1
            op_new = [ curr_idx, a_idx, b_idx, is_add, op[5], op[8] ]
            op_list += [ op_new ]
            op_codes[out_code] = curr_idx
        elif op[0] == 'R':
            out_code, in_code, is_negate = get_reg_op_codes( op, op_codes )
            in_idx = op_codes[in_code]
            curr_idx = len(op_codes) - 1
            op_new = [ curr_idx, in_idx, -1, is_negate, 0, 0 ]
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

if __name__ == "__main__":
    fname = "../resources/cifar_layer1.rpag"
    matname = "../resources/conv1_weights.csv"
    f = open( fname )
    lines = f.readlines()
    vals = parse_graph( lines )
    no_inputs = len(vals[0][1])
    op_list, output_idxs = transform_to_op_list( vals, no_inputs )
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
            if op_list[ idx - no_inputs ][3] == 1:
                op_list[ idx - no_inputs ][3] = -1
            else:
                # need to swap order
                tmp = op_list[ idx - no_inputs ][1]
                op_list[ idx - no_inputs ][1] = op_list[ idx - no_inputs ][2]
                op_list[ idx - no_inputs ][2] = tmp
                tmp = op_list[ idx - no_inputs ][4]
                op_list[ idx - no_inputs ][4] = op_list[ idx - no_inputs ][5]
                op_list[ idx - no_inputs ][5] = tmp
    # output_ordered.reverse()
    tmp = wrt.writerow( [ x[0] for x in output_ordered ] )
    for op in op_list:
        tmp = wrt.writerow( op )
    f_out.close()
