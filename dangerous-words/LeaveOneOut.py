# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman


# 1) read in pairs of sets of benign and vulnerable identifiers
#    (separated to support f2 computation).
# 2) split each identifier into terms.
# 3) use each program in turn as the source of the test and the others
#    as the source of the training data.
# 4) use LAVDNN to score the vulnerability of each training term producing
#    the list of dangerous terms.
# 5) from the training data, use the f2 scores to identify the best value
#    of K and threshold.
# 6) predict a test identifier is vulnerably if $threshold$ percent
#    of its terms are in the top $K$ entries of the list of dangerous terms.
# 7) from the test data use the f2 scores to to identify the best value
#    of K and threshold and thus determine how well this simple `grep`
#    predictor works :)
#
# code was hacked from CrossValidation.py so blame the guy who wrote
# it for any bugs.

# usage: LeaveOneOut.py [-m|-c plus minus] min_score id-list-pairs


import sys
from Common import *
from GlobalSettings import *
from DW_Common import *

print("# command-line arguments", sys.argv)


##-------------------------- aaarrrrrrrrg --------------------

USE_MODEL = False
if len(sys.argv) > 2 and sys.argv[1] == '-m':
  USE_MODEL = True
  from KerasCode import *
  score_min = tofloat(sys.argv[2])
  plus = 0  # unused
  minus = 0  # unused
  sys.argv = sys.argv[3:]    # slice off mode and min
elif len(sys.argv) > 6 and sys.argv[1] == '-c':
  plus = tofloat(sys.argv[2])
  minus = tofloat(sys.argv[3])
  score_min = tofloat(sys.argv[4])
  sys.argv = sys.argv[5:]    # slice off mode, plus, minus, and min
else:
  print("usage: [-m|-c plus minus] min id-file-pairs")
  exit(-1)


##-------------------------- main ------------------------

START = time.time()

vulnerable = []
benign = []
ids_by_program = []
names = []
for i in range(0, len(sys.argv), 2):
  fbenign = open(sys.argv[i], 'r')   
  fvulnerable = open(sys.argv[i+1], 'r')   
  print("@@ reading benign names from ", sys.argv[i],  file=sys.stderr)
  # print("@@   reading vulnerable names from ", sys.argv[i+1], file=sys.stderr)

  b = read_ids(fbenign, InputChar, DIGITS, False)
  v = read_ids(fvulnerable, InputChar, DIGITS, True)

  ids_by_program.append(b+v)
  names.append( re.sub("_benign.txt", "", re.sub("../names/processed/", "", sys.argv[i])))

  vulnerable = vulnerable + v
  benign = benign + b

## remove duplicates from benign ... had a negligible effect on loo.-c.-999,1,1.raw
#  bb = set()
#  seen = set()
#  for id in benign:
#    if id.id in seen: continue
#    seen.add(id.id)
#    bb.add(id)
#
#benign = bb

# bb = [id for id in benign if re.match("^main$", id.id)]
# print(bb)
# for x in bb:
#   print(x.id, x.split, x.score, x.vulnerable)

print("initial benign length ", len(benign))
sv = set([id.id for id in vulnerable])
benign = [id for id in benign if id.id not in sv]
all_ids = vulnerable + benign
b = set([id.id for id in benign])

print("\n# lengths vulnerable %d, (as set %d) benign %d (as set %d), all_ids %d" % 
      (len(vulnerable), len(sv), len(benign), len(b), len(all_ids)))

term_map = None
if USE_MODEL:
  terms = gather_terms(all_ids)
  print("\ntime at start of predict all ",  elapse(START), file=sys.stderr)
  term_map = make_predictions(terms, InputChar, DIGITS)
  print("time at stop  of predict all ",  elapse(START), file=sys.stderr)

print("## threshold,tp,fp,tn,fn,K,f2,maxat,f1,fold,plus,minus,comment graph")
stats = all_dangerous(all_ids)
print(fmt_csv  % ( -1, stats[0], stats[1], stats[2], stats[3], -1, stats[4], -1, stats[5], -1, -1, -1, "@@ all dangerous all ids"))
f2_all_ids = stats[4]

print("@@ processing",  int(len(sys.argv)/2), " data sets")

for i in range(0, int(len(sys.argv)/2)):
  print("## @@ data set ", names[i])
  test = ids_by_program[i]
  x = set([id.id for id in test])
  training = [id for id in all_ids if id.id not in x]
  foo = [id for id in all_ids if id.id in x]
  # print("training size ", len(training), "len test", len(test), " len removed ", len(foo))
  score(training, test, score_min, term_map, names[i], f2_all_ids, plus, minus)

print("")
