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


data = np.genfromtxt( options.input_file, dtype = [ ( 'used', 'a4' ), \
                                                    ( 'usage', 'i8' ), ], \
                                          comments = '#' )

used = data['used']
usage = data['usage']

def make_autopct(values):
    def my_autopct(pct):
        total = sum(values)
        val = int(round(pct*total/100.0))
        return '{p:.2f}%  ({v:d})'.format(p=pct,v=val)
    return my_autopct

#plt.pie( usage, labels = used, autopct = '%1.1f%%', shadow = True, explode = ( 0.1, 0.0 ), \
#         colors = [ 'blue', 'red'] )

plt.pie( usage, labels = used, autopct = make_autopct( usage ), shadow = True, explode = ( 0.1, 0.0 ), \
         colors = [ 'blue', 'red'], startangle = -45 )

plt.axis( 'equal' )
plt.title( options.title + ' (' + str( np.sum( usage ) ) + ' respondents)')
plt.savefig( options.output_file )
 
