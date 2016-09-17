#!/usr/bin/python

import sys
import optparse
import numpy as np
import matplotlib as mpl
mpl.use( 'Agg' )
import matplotlib.pyplot as plt

parser = optparse.OptionParser( )

parser.add_option( '--input_file', dest = 'input_file', default = '' )
parser.add_option( '--output_file', dest = 'output_file', default = 'output.png' )
parser.add_option( '--title', dest = 'title', default = 'Fortran usage' )

options, remainder = parser.parse_args( )

data = np.genfromtxt( options.input_file, dtype = [ ( 'fortran_standard', 'a4' ), \
                                                    ( 'fortran_usage', 'i8' ), ], \
                                          comments = '#' )

fortran_standard = data['fortran_standard']
fortran_usage = data['fortran_usage']

xrange = np.arange( 0, fortran_standard.size )
labels = fortran_standard
width = 0.50
total_usage = np.sum( fortran_usage )

fig, plt1 = plt.subplots( )

l1 = plt1.bar( xrange, fortran_usage, label = 'off', color = 'blue' )
plt1.set_ylabel( 'Usage' )
#plt1.set_xlabel( 'Topic' )
plt1.grid( True )

# plt2 = plt1.twinx( )
# l3 = plt2.bar( xrange + 2*width, diff_data, width = width, label = 'diff', color = 'red' )
# plt2.set_ylabel( 'relative diff' )
# plt2.set_ylim( [ -0.06, 0.06 ] )
# plt2.grid( True )

# for t2 in plt2.get_yticklabels( ):
#  t2.set_color( 'red' )

# plt.legend( ( l1, l2, l3 ), ( 'off', 'on', 'diff' ), loc = 'upper right' )
plt.axis( xmin = -0.4, xmax = fortran_usage.size )
plt.xticks( xrange + 0.4, fortran_standard, rotation = 60, size = 'small' )
plt.title( options.title )
#plt.tick_params( length = 6 )
#plt.subplots_adjust(bottom=0.15)
#plt.gca().tight_layout()
# plt.margins(0.2)
plt.savefig( options.output_file )
 
