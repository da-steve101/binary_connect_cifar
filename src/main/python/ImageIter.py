#! /usr/bin/python3

import pickle
import numpy as np

class ImageIter:

  def __init__( self ):
    self.fname = "/opt/datasets/cifar10/cifar-10-batches-py/test_batch"
    self.f = open( self.fname, 'rb' )
    self.data = pickle.load( self.f, encoding='bytes' )
    self.size = len(self.data[b'labels'])

  def __iter__( self ):
    self.idx = 0
    return self

  def __next__( self ):
    if self.idx >= self.size:
      raise StopIteration
    img = self.data[b'data'][self.idx].reshape( 3, 32, 32 ).transpose( [ 1, 2, 0 ] )
    nu = np.mean( img )
    stddev = np.std( img )
    adj_stddev = max( stddev, np.sqrt( 1 / ( 32 * 32 * 3 ) ) )
    img = (img - nu)/adj_stddev
    label = self.data[b'labels'][self.idx]
    self.idx += 1
    return ( label, img )

  def __len__( self ):
    return self.size
