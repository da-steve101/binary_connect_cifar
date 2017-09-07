#! /usr/bin/python3

import tensorflow as tf
import numpy as np
from scipy import signal
from skimage.measure import block_reduce
from PIL import Image
import sys
import pickle
import os

def get_variables( model_name ):
    config = tf.ConfigProto(allow_soft_placement=True)
    sess = tf.Session( config = config )
    new_saver = tf.train.import_meta_graph("train_out/" + model_name + ".meta")
    new_saver.restore( sess, "train_out/" + model_name )
    all_tri = tf.get_collection( tf.GraphKeys.WEIGHTS )
    var_dict = {}
    for x in all_tri:
        if "tower_0" in x.name:
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

def compute_conv( img, conv_weights ):
    conv_flat = conv_weights.flatten()
    scaling_factor = np.mean(abs(conv_flat[abs(conv_flat) > 0]))
    tri_weights = np.round(conv_weights/scaling_factor).astype( int )
    filter_out = []
    for fno in range( conv_weights.shape[-1] ):
        ch_sum = 0
        for chno in range( conv_weights.shape[-2] ):
            conv_window = np.flip( np.flip( tri_weights[:,:,chno,fno], 0 ), 1 )
            ch_sum += signal.convolve2d( img[:,:,chno], conv_window, mode = "same" )
        filter_out += [ ch_sum ]
    filter_out = np.array( filter_out ) * scaling_factor
    return np.transpose( filter_out, [ 1, 2, 0 ] )

def compute_BN( img, mean, var, gamma, beta ):
    a = gamma / var
    b = beta - a * mean
    return a * img + b

def compute_relu( img ):
    return img * ( img > 0 )

def compute_max_pool( img ):
    return block_reduce( img, (2, 2, 1), np.max )

def compute_conv_lyr( img, var_dict, idx ):
    lyr = "conv" + str(idx)
    conv_weights = var_dict[lyr]
    conv_res = compute_conv( img, conv_weights )
    bn_res = compute_BN(
        conv_res,
        var_dict[lyr + '/mean'],
        var_dict[lyr + '/variance'],
        var_dict[lyr +'/gamma'],
        var_dict[lyr + '/beta'] )
    relu_res = compute_relu( bn_res )
    return relu_res

def compute_dense_lyr( img, var_dict, lyr_name ):
    img_flat = img.flatten()
    mat = var_dict[lyr_name]
    bias = 0
    bias_name = lyr_name + "/bias"
    if bias_name in var_dict:
        bias = var_dict[bias_name]
    matmul_res = img_flat.dot( mat ) + bias
    return matmul_res

def get_image( fname ):
    img_handle = Image.open( fname )
    img = np.array( img_handle ).reshape( 32, 32, 3 ).astype( float )
    nu = np.mean( img )
    stddev = np.std( img )
    adj_stddev = max( stddev, np.sqrt( 1 / ( 32 * 32 * 3 ) ) )
    return (img - nu)/adj_stddev

def inference( img, var_dict ):
    for i in range( 2 ):
        img = compute_conv_lyr( img, var_dict, i + 1 )
    img = compute_max_pool( img )
    for i in range( 2 ):
        img = compute_conv_lyr( img, var_dict, i + 3 )
    img = compute_max_pool( img )
    for i in range( 2 ):
        img = compute_conv_lyr( img, var_dict, i + 5 )
    img = compute_max_pool( img )
    lyr = "fc_1024"
    img = compute_dense_lyr( img, var_dict, lyr )
    img = compute_BN(
        img,
        var_dict[lyr + "/mean"],
        var_dict[lyr + "/variance"],
        var_dict[lyr + "/gamma"],
        var_dict[lyr + "/beta"] )
    img = compute_relu( img )
    return compute_dense_lyr( img, var_dict, "softmax" )

def max_pred( pred, labels ):
    return labels[ np.argmax( pred ) ]

if __name__ == "__main__":
    img_names = sys.argv[1:]
    model_name = "model.ckpt-0"
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
        pred = inference( img, var_dict )
        print( img_name + " is " + max_pred( pred, labels ) )
