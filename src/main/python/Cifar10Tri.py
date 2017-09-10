
import tensorflow as tf
import pickle
import numpy as np
import os

def trinarize( x, block_mask, nu = 0.7 ):
    clip_val = tf.clip_by_value( x, -1, 1 )
    x_shape = x.get_shape()
    thres = nu * tf.reduce_mean(tf.abs(x))
    clip_val = tf.multiply( clip_val, block_mask )
    unmasked = tf.where(
        tf.logical_and(
            tf.greater( clip_val, -thres ),
            tf.less( clip_val, thres )
        ),
        tf.constant( 0.0, shape = x_shape ),
        clip_val )
    eta = tf.reduce_mean( tf.abs( unmasked ) )
    t_x = tf.where( tf.less_equal( unmasked, -thres ),
                    tf.multiply( tf.constant( -1.0, shape = x_shape ), eta ),
                    unmasked )
    t_x = tf.where( tf.greater_equal( unmasked, thres ),
                    tf.multiply( tf.constant( 1.0, shape = x_shape ), eta ),
                    t_x )
    return t_x

def trin_stop_grad( x ):
    fname = "block_mask_dicts.pkl"
    block_mask = tf.constant( 1, shape = x.shape, dtype = tf.float32 )
    if os.path.exists( fname ):
        f = open( fname, "rb" )
        block_dict = pickle.load( f )
        f.close()
        if x.name in block_dict:
            print( x.name + " was found in block_dict" )
            block_mask = tf.constant( block_dict[x.name], dtype = tf.float32 )
    return x + tf.stop_gradient( trinarize( x, block_mask ) - x )

def create_block_dict( model_name = "train_out/model.ckpt-0" ):
    config = tf.ConfigProto(allow_soft_placement=True)
    sess = tf.Session( config = config )
    new_saver = tf.train.import_meta_graph( model_name + ".meta")
    new_saver.restore( sess, model_name )
    all_tri = tf.get_collection( tf.GraphKeys.WEIGHTS )
    var_dict = {}
    for x in all_tri:
        if "weights:0" in x.name:
            tmp = sess.run( x )
            thres = 1.1 * np.mean( abs( tmp ) )
            var_dict[x.name] = ( abs(tmp) > thres ) * 1
    f = open( "block_mask_dicts.pkl", "wb" )
    pickle.dump( var_dict, f )
    f.close()

def get_conv( x, filter_out, kernel_size, strides ):
    conv_shape = [ kernel_size, kernel_size, x.get_shape()[-1], filter_out ]
    conv_weights = tf.get_variable(
        "conv_weights",
        shape = conv_shape,
        initializer=tf.contrib.layers.xavier_initializer()
    )
    tf.add_to_collection( tf.GraphKeys.WEIGHTS, conv_weights )
    weight_decay = tf.multiply(tf.nn.l2_loss(conv_weights), 0.0001, name='weight_loss')
    tf.add_to_collection('losses', weight_decay)
    conv_tri_weights = trin_stop_grad( conv_weights )
    tf.add_to_collection( tf.GraphKeys.WEIGHTS, conv_tri_weights )
    conv_out = tf.nn.conv2d(
        x,
        conv_tri_weights,
        strides=strides,
        padding='SAME'
    )
    activ_summary( conv_out )
    activ_summary( conv_weights )
    activ_summary( conv_tri_weights, "weights_tri" )
    return conv_out

def get_dense( x, outputs ):
    w_shape = [ x.get_shape()[-1], outputs ]
    dense_weights = tf.get_variable(
        "dense_weights",
        shape = w_shape,
        initializer=tf.contrib.layers.xavier_initializer()
    )
    tf.add_to_collection( tf.GraphKeys.WEIGHTS, dense_weights )
    weight_decay = tf.multiply(tf.nn.l2_loss(dense_weights), 0.0001, name='weight_loss')
    tf.add_to_collection('losses', weight_decay)
    dense_tri_weights = trin_stop_grad( dense_weights )
    tf.add_to_collection( tf.GraphKeys.WEIGHTS, dense_tri_weights )
    activ_summary( dense_weights )
    activ_summary( dense_tri_weights, "dense_tri" )
    bias = tf.get_variable( "dense_bias", shape = [ outputs ] )
    tf.add_to_collection( tf.GraphKeys.BIASES, bias )
    return tf.add( tf.matmul( x, dense_tri_weights ), bias )

def conv_bn_relu( x, filter_out, kernel_size = 3, strides=(1,1,1,1), training = True ):
    conv_out = get_conv( x, filter_out, kernel_size, strides )
    tf.add_to_collection( tf.GraphKeys.ACTIVATIONS, conv_out )
    activ_summary( conv_out )
    batch_norm_out = tf.layers.batch_normalization(
        conv_out,
        training = training
    )
    tf.add_to_collection( tf.GraphKeys.ACTIVATIONS, batch_norm_out )
    return tf.nn.relu( batch_norm_out )

def max_pool( x ):
    return tf.layers.max_pooling2d( x, 2, 2 )

def activ_summary( x, override_name = None ):
    tensor_name = x.name if override_name is None else override_name
    tf.summary.histogram(tensor_name + '/activations', x)
    tf.summary.scalar(tensor_name + '/sparsity',
                      tf.nn.zero_fraction(x))

def inference( images, use_dropout = False, training = True ):
    filt_out = 64
    tf.add_to_collection( tf.GraphKeys.ACTIVATIONS, images )
    with tf.variable_scope('conv1') as scope:
        cnn = conv_bn_relu( images, filt_out, training = training )
        tf.add_to_collection( tf.GraphKeys.ACTIVATIONS, cnn )
    with tf.variable_scope('conv2') as scope:
        cnn = conv_bn_relu( cnn, filt_out, training = training )
        cnn = max_pool( cnn )
    with tf.variable_scope('conv3') as scope:
        cnn = conv_bn_relu( cnn, filt_out * 2, training = training )
    with tf.variable_scope('conv4') as scope:
        cnn = conv_bn_relu( cnn, filt_out * 2, training = training )
        cnn = max_pool( cnn )
    with tf.variable_scope('conv5') as scope:
        cnn = conv_bn_relu( cnn, filt_out * 4, training = training )
    with tf.variable_scope('conv6') as scope:
        cnn = conv_bn_relu( cnn, filt_out * 4, training = training )
        cnn = max_pool( cnn )
    with tf.variable_scope('fc_1024')  as scope:
        dims = 1
        for x in cnn.get_shape().as_list()[1:]:
            dims *= x
        cnn = tf.reshape( cnn, [ -1, dims ] )
        cnn = get_dense( cnn, 1024 )
        cnn = tf.layers.batch_normalization(
            cnn,
            training = training
        )
        cnn = tf.nn.relu( cnn )
        tf.add_to_collection( tf.GraphKeys.ACTIVATIONS, cnn )
    with tf.variable_scope('softmax')  as scope:
        dims = 1
        for x in cnn.get_shape().as_list()[1:]:
            dims *= x
        cnn = tf.reshape( cnn, [ -1, dims ] )
        cnn = get_dense( cnn, 10 )
        tf.add_to_collection( tf.GraphKeys.ACTIVATIONS, cnn )
    return cnn
