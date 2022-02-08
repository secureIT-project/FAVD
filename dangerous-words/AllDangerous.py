# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# 1) read in benign and vulnerable identifiers
# 2) compute f2 assuming that all words are dangerous


folds = 5    # 'k' for for k-fold validation

from Common import *
from GlobalSettings import *
from random import sample 
import random
import numpy as np


balance = False
if len(sys.argv) > 1:
  if sys.argv[1] == '-b':  # balance non-vulnerable to size of vulnerable
    balance = True
    sys.argv = sys.argv[1:]

if len(sys.argv) < 2:
  print("usage: python3 AllDangerous.py .../FFmpeg_benign.txt .../FFmpeg_vulnerable.txt")
  exit(-1)

fbenign = open(sys.argv[1], 'r')   
fvulnerable = open(sys.argv[2], 'r')   

benign = read_ids(fbenign, InputChar, DIGITS, False)
vulnerable = read_ids(fvulnerable, InputChar, DIGITS, True)
sv = set([id.id for id in vulnerable])
benign = [id for id in benign if id.id not in sv]

if balance:
  benign = sample(benign,10*len(vulnerable))
  print("# balancing |benign| to 10 * |vulnerable|")

identifiers = benign + vulnerable
print("# lengths vulnerable %d benign %d identifiers %d\n" % 
        (len(vulnerable), len(benign), len(identifiers)))

for id in identifiers:
  id.score = 1.0

random.seed(99)
np.random.seed(99)  # its special :)
np.random.shuffle(identifiers)

print("## threshold,tp,fp,tn,fn,K,f2,maxat,f1")

check = []
partition_len = int(len(identifiers) / folds) + 1
start = 0
for i in range(0, folds):
  stop = start + partition_len
  if stop > len(identifiers):  # truncate last partition as needed
    stop = len(identifiers)
  test = identifiers[start:stop]
  check = check + test
  stats =  compute_metrics(test, -1) # predict everything is dangerous
  start = stop

  print("## all-dangerous, %3d, %3d, %3d, %3d," % ( stats[0], stats[1], stats[2], stats[3]), end='')
  print("-1, %5.3f, -1, %5.3f" % (stats[4], stats[5]))

if set(check) != set(identifiers):
  print("@@ OOPS sizeof check ", len(check), " size of identifiers ", len(identifiers))

print("")
