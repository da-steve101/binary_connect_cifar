#! /usr/bin/python3

import csv
import numpy as np
import sys
import math

def find_most_common( matrix ):
    patterns = []
    most_common_count = -1
    remaining = 0
    for sign in [ 1, -1 ]:
        for i in range( len(matrix) - 1 ):
            res_mat = np.absolute( matrix[:-(i+1),:] + sign*matrix[(i+1):,:] ) > 1
            res_mat_sum = np.sum( res_mat, 1 )
            res_mat_idx = np.argmax( res_mat_sum )
            remaining += sum( res_mat_sum > 1 )
            if res_mat_sum[res_mat_idx] > most_common_count:
                most_common_count = res_mat_sum[res_mat_idx]
                patterns = [ res_mat[res_mat_idx]*matrix[res_mat_idx,:] ]
            elif res_mat_sum[res_mat_idx] == most_common_count:
                patterns += [ res_mat[res_mat_idx]*matrix[res_mat_idx,:] ]
    print( str( remaining ) + " common subexpressions detected" )
    # find most common
    patterns = [ list(x) for x in patterns ]
    patterns = sorted( patterns )
    # group and find largest group ...
    best_idx = 0
    best_size = 0
    curr_start = 0
    curr_size = 0
    for idx, pattern in enumerate( patterns ):
        if patterns[curr_start] == pattern:
            curr_size += 1
        else:
            curr_size = 1
            curr_start = idx
        if curr_size > best_size:
            best_idx = curr_start
            best_size = curr_size
    pattern = patterns[best_idx]
    return most_common_count, pattern

def get_all_common( matrix, pattern ):
    count = len( [ x for x in pattern if x != 0 ] )
    res_pos = [ i for i, z in enumerate( np.dot( matrix, pattern ) ) if z == count ]
    res_neg = [ i for i, z in enumerate( np.dot( -matrix, pattern ) ) if z == count ]
    return res_pos, res_neg

def update_matrix( matrix, idxs_pos, idxs_neg, pattern ):
    # first eliminate common expr
    for i in idxs_pos:
        matrix[i,:] = matrix[i,:] - pattern
    for i in idxs_neg:
        matrix[i,:] = matrix[i,:] + pattern
    # add new row to matrix
    matrix = np.vstack( [ matrix, pattern ] )
    # add new col to matrix
    matrix = np.hstack( [ matrix, np.zeros( ( matrix.shape[0], 1 ), dtype = np.int16 ) ] )
    for i in idxs_pos:
        matrix[i,-1] = 1
    for i in idxs_neg:
        matrix[i,-1] = -1
    return matrix

def subexpression_elimination( matrix ):
    most_common_count, pattern = find_most_common( matrix )
    while most_common_count > 1:
        #if True:
        pattern_pos, pattern_neg = get_all_common( matrix, pattern )
        matrix = update_matrix( matrix, pattern_pos, pattern_neg, pattern )
        most_common_count, pattern = find_most_common( matrix )
        print( str(len(pattern_pos) + len(pattern_neg)) +
               " expressions have a common subexpression of size "
               + str(most_common_count) + " to be eliminated"  )
    return matrix

def size_of_tree( matrix ):
    return np.sum( np.absolute( matrix ) ) - matrix.shape[1]

def create_stage( curr_idx, idxs ):
    # group by 2 and create ops
    op_list = []
    for i in range( int( math.ceil( len(idxs) / 2 ) ) ):
        a = idxs[2*i]
        if len(idxs) < 2*(i+1):
            b = [ -1, 0, False ]
        else:
            b = idxs[2*i + 1]
        add_op = 4*a[2] + 2*b[2]
        op_new = [ curr_idx, a[0], b[0], -1, add_op, 0, 0, 0 ]
        curr_idx += 1
        op_list += [ op_new ]
    return op_list, curr_idx

def create_ops_for_tree( curr_idx, idxs_in ):
    # idxs_in = [ ( idx, depth_avail, is_pos ) ]
    curr_d = 0
    op_list = []
    max_add_d = max( [ x[1] for x in idxs_in ] )
    curr_idxs = idxs_in
    reserves = [1]
    while len(reserves) > 0 or len(curr_idxs) > 1 or len(op_list) < 1:
        reserves = [ x for x in curr_idxs if x[1] > curr_d ]
        to_reduce = [ x for x in curr_idxs if x[1] <= curr_d ]
        reduced_ops, curr_idx = create_stage( curr_idx, to_reduce )
        curr_idxs = [ ( x[0], curr_d + 1, True )  for x in reduced_ops ] + reserves
        op_list += reduced_ops
        curr_d += 1
    output_idx = op_list[-1][0]
    return op_list, curr_idx, output_idx, curr_d

def make_tree( matrix, no_in, no_out ):
    dependancies = {}
    for j, row in enumerate([ abs( matrix[no_in:,i] ) for i in range( matrix.shape[1] ) ]):
        dependancies[j] = list(np.nonzero( row )[0] + no_out)
    output_depths = {}
    outputs = {}
    op_list = []
    op_idx = no_in
    while len( outputs ) < matrix.shape[1]:
        for x in dependancies:
            if x not in outputs:
                if len(dependancies[x]) == 0:
                    mat_cnt = sum( abs( matrix[:,x] ) )
                    if mat_cnt == 0:
                        outputs[x] = -1
                        output_depths[x] = 0
                    else:
                        # make the ops
                        idxs = [ i for i, y in enumerate( matrix[:,x] ) if y != 0 ]
                        idxs_in = []
                        for i in idxs:
                            j = i
                            d = 0
                            is_pos = True
                            assert i < no_in, "For no dependancies, should have i < no_in"
                            if matrix[i,x] == -1:
                                is_pos = False
                            idxs_in += [ (j, d, is_pos) ]
                        new_ops, op_idx, output_idx, curr_d = create_ops_for_tree( op_idx, idxs_in )
                        outputs[ x ] = output_idx
                        output_depths[ x ] = curr_d
                        op_list += new_ops
                else:
                    dep_depths = [ output_depths[ y ] for y in dependancies[x] if y in output_depths ]
                    dep_depths.sort()
                    if len( dep_depths ) == len(dependancies[x]):
                        idxs = [ i for i, y in enumerate( matrix[:,x] ) if y != 0 ]
                        idxs_in = []
                        for i in idxs:
                            j = i
                            d = 0
                            is_pos = True
                            if i >= no_in:
                                j = outputs[ i + no_out - no_in ]
                                d = output_depths[ i + no_out - no_in ]
                            if matrix[i,x] == -1:
                                is_pos = False
                            idxs_in += [ (j, d, is_pos) ]
                        new_ops, op_idx, output_idx, curr_d = create_ops_for_tree( op_idx, idxs_in )
                        outputs[ x ] = output_idx
                        output_depths[ x ] = curr_d
                        op_list += new_ops
    return op_list, [ outputs[i] for i in range( no_out ) ]
    

if __name__ == "__main__":
    conv_idx = int( sys.argv[1] )
    f = open( "../resources/conv" + str(conv_idx) + "_weights.csv" )
    rdr = csv.reader( f )
    data = [ [ int(y) for y in x ] for x in rdr ]
    matrix = np.transpose( np.array( data, dtype = np.int16 ) )
    no_in = matrix.shape[1]
    no_out = matrix.shape[0]
    initial_no_adds = size_of_tree( matrix )
    print( "initial matrix is " + str( initial_no_adds  ) )
    matrix = subexpression_elimination( matrix )
    final_no_adds = size_of_tree( matrix )
    print( "improvement is from " + str( initial_no_adds ) + " to " +
           str( final_no_adds ) + " or " + str( final_no_adds*100/initial_no_adds ) + "%" )
    f_out = open( "../resources/conv" + str(conv_idx) + "_weights_tree.csv", "w" )
    wrt = csv.writer( f_out )
    for x in np.transpose( matrix ):
        tmp = wrt.writerow( x )
    f_out.close()
    f_out = open( "../resources/conv" + str(conv_idx) + "_tern_op_list.csv", "w" )
    wrt = csv.writer( f_out )
    tree_ops, outputs = make_tree( np.transpose( matrix ), no_in, no_out )
    tmp = wrt.writerow( outputs )
    for x in tree_ops:
        tmp = wrt.writerow( x )
    f_out.close()
