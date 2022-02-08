# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# common methods
# include reading a file, dumping the sorted list, and making the prediction
# (see also KerasCode.py, which contained common code that requires
# the keras.models import, which takes way too long :) )


from Id import *


def tofloat(string):
  try:
    return float(string)
  except Exception:
    print("was kind of hoping for a floating point value, got ", string, ":(", file=sys.stderr)
    exit(0)
    # throw TypeError



def containsUnwanted(str, set):
    """ Check whether str contains ANY of the items not in set. """
    return 0 in [c in set for c in str]


def read_ids(file, validChars, maxLen, vulnerable=True, silent=False):
  too_long = []
  oops = []
  data = file.read()
  rows = data.split('\n')
  identifiers = []
  for row in rows:
      data = row.replace(';', '').strip()
      if containsUnwanted(data, set(validChars)):
        oops.append(data)
      elif len(data) > maxLen:
        too_long.append(data)
      else:
        if len(data) > 0:    ## suppressing empty identifiers
          identifiers.append(Id(data, vulnerable))
  if not silent and oops != []:
    print("*** Warning: the following %d identifiers include unexpected characters and are being ignored!\n"
          % (len(oops)), set(oops), file=sys.stderr)
  if not silent and too_long != []:
    print("*** Warning: the following %d identifiers are more than %d characters and are being ignored!\n" 
          % (len(too_long), maxLen), set(too_long), file=sys.stderr)
  return identifiers


def gather_terms(ids):
    terms = []
    for id in ids:
        for p in id.split:
            if len(p) > 0:    ## suppressing empty terms
                terms.append(p)
    return terms


def dump_sorted_list(ids):
  def by_score(id):
    return id.score
  ids.sort(key=by_score, reverse=True)
  for id in ids:
      print("@ %9.6f  %6.6s  %s" % (id.score, id.vulnerable, id.id))


def compute_metrics(ids, threshold):
    tp = 0
    fp = 0
    fn = 0
    tn = 0
    for id in ids:
        if id.score > threshold and id.vulnerable:
            tp += 1
        if id.score > threshold and not id.vulnerable:
            fp += 1
        if id.score <= threshold and id.vulnerable:
            fn += 1
        if id.score <= threshold and not id.vulnerable:
            tn += 1
    if 5 * tp + 4 * fn + fp == 0:
        f2 = 0.0
    else: 
        f2 = (5 * tp) / (5 * tp + 4 * fn + fp)  # see paper 
    if 2 * tp + fn + fp == 0:
        f1 = 0.0
    else:
        f1 = (2 * tp) / (2 * tp + fn + fp)
    return [tp, fp, tn, fn, f2, f1]


