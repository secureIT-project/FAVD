# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman


# count data for histogram of the number of terms per identifier.
# code caches answer below.
# based on distribution, only need to look at thresholds up through [1-y]/y
# where y is the largest common number of terms in an identifier.  choose 6.

# usage:
# set data = "../names/processed/"
# set files = ($data/FFmpeg_benign.txt $data/FFmpeg_vulnerable.txt $data/LibPNG_benign.txt $data/LibPNG_vulnerable.txt $data/Pidgin_benign.txt $data/Pidgin_vulnerable.txt $data/VLC_benign.txt $data/VLC_vulnerable.txt $data/LibTIFF_benign.txt $data/LibTIFF_vulnerable.txt $data/Asterisk_benign.txt $data/Asterisk_vulnerable.txt $data/VDISC_benign.txt $data/VDISC_vulnerable.txt )
#
# python3 term-stats.py $files
#


from Common import *
from Id import *
from GlobalSettings import *
import sys


def term_count(ids, tc):
  max = len(tc)
  for id in ids:
    foo = id.split
    l = len(foo)
    if l >= max:
        l = max-1
    tc[l] = tc[l] + 1

  return tc
     

if len(sys.argv) > 1:
  tc = [0,0, 0,0,0,0,0,0,0,0,0,0]
  p = [0,0, 0,0,0,0,0,0,0,0,0,0]
  sys.argv = sys.argv[1:]
  for name in sys.argv:
    f = open(name, 'r')   
    identifiers = read_ids(f, InputChar, DIGITS, False)
    print("read", len(identifiers), "identifiers from", name)
    tc = term_count(identifiers, tc)
  
  sum = 0
  for v in tc:
    sum += v
  
  for i in range(0,len(tc)):
    p[i] = tc[i] / sum * 100
  
  print(tc)
  for v in p:
    print("%4.2f%%  " % v, end="")
  print("")
  exit (0)


# hard code answer from counting
tc = [0, 42349, 238173, 332481, 229867, 122816, 51149, 17646, 5452, 1661, 518, 559]
p = [0.00,  4.06,  22.84,  31.89,  22.05,  11.78,  4.91,  1.69,  0.52,  0.16,  0.05,  0.05]

# "generate" data for thresholds array ...
print("x/x 0.01") # cross validation runs better with this present?
#for i in range(1, 7):
  #for j in range(1,i+1):
    #print("%d/%d %f" % (j, i, (float(j)/float(i))))



