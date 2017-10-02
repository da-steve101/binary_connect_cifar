#! /usr/bin/python3

import numpy as np
from scipy import signal
from skimage.measure import block_reduce
from PIL import Image
import sys
import pickle
import os
import csv

def get_variables( model_name ):
    import tensorflow as tf
    config = tf.ConfigProto(allow_soft_placement=True)
    sess = tf.Session( config = config )
    new_saver = tf.train.import_meta_graph("train_out/" + model_name + ".meta")
    new_saver.restore( sess, "train_out/" + model_name )
    all_tri = tf.get_collection( tf.GraphKeys.WEIGHTS )
    var_dict = {}
    for x in all_tri:
        if "tower_0" in x.name and "add:0" in x.name:
            lyr_name = x.name.split("/")[1]
            var_dict[lyr_name] = sess.run( x )
    all_bias = tf.get_collection( tf.GraphKeys.BIASES )
    for x in all_bias:
        lyr_name = x.name.split("/")[0] + "/bias"
        var_dict[lyr_name] = sess.run( x )
    all_var = tf.get_collection( tf.GraphKeys.GLOBAL_VARIABLES )
    for x in all_var:
        bn_name = ""
        if "mean:0" in x.name:
            bn_name = x.name.split("/")[0] + "/mean"
        if "variance:0" in x.name:
            bn_name = x.name.split("/")[0] + "/variance"
        if "beta:0" in x.name:
            bn_name = x.name.split("/")[0] + "/beta"
        if "gamma:0" in x.name:
            bn_name = x.name.split("/")[0] + "/gamma"
        if not bn_name == "":
            var_dict[bn_name] = sess.run( x )
    return var_dict

def round_to( x, bits_prec = 5 ):
    factor = 1 << bits_prec
    return np.round( x * factor )/factor

def floor_to( x, bits_prec = 5 ):
    factor = 1 << bits_prec
    return np.floor( x * factor )/factor

def get_ternary( x ):
    x_flat = x.flatten()
    scaling_factor = np.mean(abs(x_flat[abs(x_flat) > 0]))
    tri_weights = np.round(x/scaling_factor).astype( int )
    return tri_weights, scaling_factor

def write_to_file( inputs, fname, no_dims = 4, additional_info = None ):
  f_out = open( fname, "w" )
  wrt = csv.writer( f_out )
  if additional_info is not None:
    wrt.writerow( additional_info )
  for a in inputs:
    if no_dims >= 3:
        for b in a:
            if no_dims == 4:
                for c in b:
                    tmp = wrt.writerow( c )
            else:
                tmp = wrt.writerow( b )
    else:
        tmp = wrt.writerow( a )
  f_out.close()

def compute_conv( img, conv_weights ):
    tri_weights, scaling_factor = get_ternary( conv_weights )
    filter_out = []
    for fno in range( conv_weights.shape[-1] ):
        ch_sum = 0
        for chno in range( conv_weights.shape[-2] ):
            conv_window = np.flip( np.flip( tri_weights[:,:,chno,fno], 0 ), 1 )
            ch_sum += signal.convolve2d( img[:,:,chno], conv_window, mode = "same" )
        filter_out += [ ch_sum ]
    filter_out = np.array( filter_out )
    return ( np.transpose( filter_out, [ 1, 2, 0 ] ), scaling_factor )

def print_info( msg, x ):
    print( msg, np.max( x ), np.min( x ), np.mean( x ), np.median( x ) )
    # pass

def get_AB( lyr, var_dict, scaling_factor, bias, bn_prec ):
    mean = var_dict[lyr + '/mean'] - bias
    stddev = np.sqrt( var_dict[lyr + '/variance'] )
    gamma = var_dict[lyr +'/gamma']
    beta = var_dict[lyr + '/beta']
    a = gamma / stddev
    a = a * ( a < 10**15 ) # if very big then prob is ignored anyway
    b = beta - a * mean
    a = a * scaling_factor
    a = round_to( a, bn_prec )
    b = round_to( b, bn_prec )
    return [ a, b ]

def compute_relu( img ):
    return img * ( img >= 0 )

def compute_max_pool( img ):
    return block_reduce( img, (2, 2, 1), np.max )

def linear_shift( img, a, b, prec ):
    return floor_to( a * img + b, prec )

def compute_conv_lyr( img, var_dict, idx, conv_prec = 5 ):
    lyr = "conv" + str(idx)
    conv_weights = var_dict[lyr]
    img = round_to( img, conv_prec )
    conv_res, scaling_factor = compute_conv( img, conv_weights )
    print_info( "conv" + str(idx), conv_res )
    a, b = get_AB(
        lyr,
        var_dict,
        scaling_factor,
        0,
        5
    )
    bn_res = linear_shift( conv_res, a, b, conv_prec )
    #print_info( "conv_bn" + str(idx), bn_res )
    relu_res = compute_relu( bn_res )
    #print_info( "conv_relu" + str(idx), relu_res )
    relu_res = floor_to( relu_res, conv_prec )
    return relu_res, a, b

def compute_dense_lyr( img, var_dict, lyr_name, dense_prec = 3 ):
    img = round_to( img, dense_prec )
    img_flat = img.flatten()
    mat, scaling_factor = get_ternary( var_dict[lyr_name] )
    bias = 0
    bias_name = lyr_name + "/bias"
    if bias_name in var_dict:
        bias = round_to( var_dict[bias_name], dense_prec )
    if lyr_name + '/mean'in var_dict: # apply batch norm
        a, b = get_AB( lyr_name, var_dict, scaling_factor, bias, 5 )
    else:
        a = round_to( scaling_factor, 5 )
        b = bias
    b = round_to( b, dense_prec )
    matmul_res = round_to( img_flat.dot( mat ), dense_prec )
    return linear_shift( matmul_res, a, b, dense_prec ), a, b

def get_image( fname ):
    img_handle = Image.open( fname )
    img = np.array( img_handle ).reshape( 32, 32, 3 ).astype( float )
    nu = np.mean( img )
    stddev = np.std( img )
    adj_stddev = max( stddev, np.sqrt( 1 / ( 32 * 32 * 3 ) ) )
    return (img - nu)/adj_stddev

def inference( img, var_dict ):
    for i in range( 2 ):
        img, a, b = compute_conv_lyr( img, var_dict, i + 1, 3 )
    img = compute_max_pool( img )
    for i in range( 2 ):
        img, a, b = compute_conv_lyr( img, var_dict, i + 3, 3 )
    img = compute_max_pool( img )
    for i in range( 2 ):
        img, a, b = compute_conv_lyr( img, var_dict, i + 5, 3 )
    img = compute_max_pool( img )
    img, a, b = compute_dense_lyr( img, var_dict, "fc_1024", 3 )
    img = compute_relu( img )
    pred, a, b = compute_dense_lyr( img, var_dict, "softmax", 3 )
    return pred

def max_pred( pred, labels ):
    return labels[ np.argmax( pred ) ]

if __name__ == "__main__":
    img_names = sys.argv[1:]
    model_name = "model.ckpt-0"
    output_file = open( "image_predictions.cnn", "w" )
    wrt = csv.writer( output_file )
    if os.path.exists( model_name + "_dict.pkl" ):
        f = open( model_name + "_dict.pkl", "rb" )
        var_dict = pickle.load( f )
    else:
        var_dict = get_variables( model_name )
        f = open( model_name + "_dict.pkl", "wb" )
        pickle.dump( var_dict, f )
    f.close()
    labels = [ "airplane",
               "automobile",
               "bird",
               "cat",
               "deer",
               "dog",
               "frog",
               "horse",
               "ship",
               "truck" ]
    for img_name in img_names:
        img = get_image( img_name )
        wrt.writerow( img )
        pred = inference( img, var_dict )
        print( img_name + " is " + max_pred( pred, labels ) )
    output_file.close()

    '''
    for i in range( 6 ):
        conv_str = "conv" + str( i + 1 )
        conv, conv_scaling = get_ternary( var_dict[conv_str] )
        write_to_file( conv.tolist(), "../resources/" + conv_str + "_weights.csv" )
        ab = get_AB_lyr( conv_str, conv_scaling, 5 )
        f_out = open( "../resources/" + conv_str + "_bn.csv", "w" )
        wrt = csv.writer( f_out )
        for x in ab:
            wrt.writerow( x )
        f_out.close()
    '''
