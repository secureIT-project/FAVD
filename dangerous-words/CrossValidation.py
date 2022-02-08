# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman


# 1) read in sets of benign and vulnerable identifiers (separated to 
#    support f2 computation).
# 2) split each identifier into terms. 
# 3) for each of 'folds' folds separate the data into training and test
# 4a) use LAVDNN to score the vulnerability of each training term producing
#     the list of dangerous terms.
# 4b) use term frequency to generate a ranked list of dangerous words.
# 5) from the training data, use the f2 scores to identify the best value 
#    of K and threshold.
# 6) predict a test identifier is vulnerably if threshold percent 
#    of its terms are in the top K entries of the list of dangerous terms.
# 7) from the test data use the f2 scores to to identify the best value 
#    of K and threshold and thus determine how well this simple `grep`
#    predictor works :)

# usage [-m|-c plus minus] min_score [-b] benign_file vulnerable_file
# the counting technique adds 'plus' for term in a vulnerable identifier
# and subtracts 'minus' for terms in a benign identifier.


folds = 5  # 'k' for for k-fold validation


from Common import *
from GlobalSettings import *
from DW_Common import *

import numpy as np
import random
import sys


print("# command-line arguments", sys.argv)

##-------------------------- aaarrrrrrrrg --------------------

USE_MODEL = False
if len(sys.argv) > 2 and sys.argv[1] == '-m':
  from KerasCode import *
  score_min = tofloat(sys.argv[2])
  USE_MODEL = True
  plus = 0  # unused
  minus = 0  # unused
  sys.argv = sys.argv[3:]    # slice off mode and min
elif len(sys.argv) > 4 and sys.argv[1] == '-c':
  plus = tofloat(sys.argv[2])
  minus = tofloat(sys.argv[3])
  score_min = tofloat(sys.argv[4])
  sys.argv = sys.argv[5:]    # slice off mode, plus, minus, and min
  print(" %d plus %d minus %d " % (USE_MODEL, plus, minus))
else:
  print("usage: [-m|-c plus minus] min id-file-pairs")
  exit(-1)

##-------------------------- main ------------------------

START = time.time()

balance = False
if len(sys.argv) >= 1 and sys.argv[0] == '-b':  # balance non-vulnerable to size of vulnerable
    balance = True         # FIX ME goto ratio such as "-b 1:10"
    sys.argv = sys.argv[1:]

if len(sys.argv) == 2:
  fbenign = open(sys.argv[0], 'r')   
  fvulnerable = open(sys.argv[1], 'r')   
else:
  print("usage: [-m|-c plus minus] min id-file-pairs")
  exit(-1)

benign = read_ids(fbenign, InputChar, DIGITS, False)
vulnerable = read_ids(fvulnerable, InputChar, DIGITS, True)
sv = set([id.id for id in vulnerable])
benign = [id for id in benign if id.id not in sv]

if balance:
  benign = random.sample(benign,10*len(vulnerable))
  print("# balancing benign to 10 * vulnerable")
identifiers = benign + vulnerable
print("\n# lengths vulnerable %d, benign %d, identifiers %d" % 
        (len(vulnerable), len(benign), len(identifiers)))

term_map = None
if USE_MODEL:
  terms = gather_terms(identifiers)
  print("\ntime at start of predict all ",  elapse(START), file=sys.stderr)
  term_map = make_predictions(terms, InputChar, DIGITS)
  print("time at stop  of predict all ",  elapse(START), file=sys.stderr)
  x =  len(set(terms))
  print("# training size %d, test size %d\n" % (int((folds-1)*x/folds), int(x/folds)))

random.seed(99)
np.random.seed(99)  # its special :)
np.random.shuffle(identifiers)

print("## threshold,tp,fp,tn,fn,K,f2,maxat,f1,fold,plus,minus,comment graph")
stats = all_dangerous(identifiers)
print(fmt_csv  % ( -1, stats[0], stats[1], stats[2], stats[3], -1, stats[4], -1, stats[5], -1, -1, -1, "@@ all dangerous all ids"))
f2_all_ids = stats[4]

## rank score test code
# print("identifiers = ", identifiers)
# score(identifiers, identifiers, score_min, term_map, 42, f2_all_ids, plus, minus)
# exit(0)

check = []
partition_len = int(len(identifiers) / folds) + 1
start = 0
for i in range(0, folds):
  stop = start + partition_len
  if stop > len(identifiers):  # truncate last partition as needed
    stop = len(identifiers)
  test = identifiers[start:stop]
  training = identifiers[0:start] + identifiers[stop:len(identifiers)]
  if set(test+training) != set(identifiers):
    print("@@ OOPS size of test+training ", len(check), " size of identifiers ", len(identifiers))
  check = check + test
  start = stop
  # experiment with test == train
  # score(training, training, score_min, term_map, i, f2_all_ids, plus, minus)

  score(training, test, score_min, term_map, i, f2_all_ids, plus, minus)

if set(check) != set(identifiers):
  print("@@ OOPS sizeof check ", len(check), " size of identifiers ", len(identifiers))

print("")
