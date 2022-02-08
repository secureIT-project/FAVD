# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# common support from the dangerous words study

## this function leaves a marker for removal of 'set()' in order to experiment
## with using the position in ranked list rather then membership in set
def S(x):
  return x
  # return set(x)


from Common import *
from itertools import chain
import time
import sys

START = time.time()  # hack to avoid passing initial time around ....


# a unit test :)
# w = []
# w.append(Id("singSong", True))
# w.append(Id("readFile", True))
#   p = predict_dangerous_counts(w, 0.0)
# or
#   term_map = make_predictions(terms, InputChar, DIGITS)
#   p = predict_dangerous_model(w, term_map, 0.0)
# print(p)
# exit(0)


# global (ack) formats for writing data ... 
# using fmt_csv ensures that all csv output has the same format :)
#
fmt = " %3d %3d %3d %3d K = %4d, max f2 = %5.3f at threshold = %5.2f (f1 = %5.3f)"
fmt_csv = "## %4.2f,%3d,%3d,%3d,%3d,%4d,  %5.3f,%5.2f,%5.3f,%s, %d,%d, %s"



def _elapse():
  return(int(time.time() - START))

def elapse(start):
  return(int(time.time() - start))

last_time = -1
def tracer(msg, time_stamp):
  global last_time
  if time_stamp - last_time > 60:  # its been a minute :)
    last_time = time_stamp
    print(msg, time_stamp, file=sys.stderr)


def trace(method, threshold, stats):
    print("%s, %5.2f, %4d, %4d, %4d, %4d, %5.3f, %5.3f"
     % (method, threshold, stats[0], stats[1], stats[2], stats[3], stats[4], stats[5]))


def compute_score(ids, dangerous, threshold):
  # return compute_score_set(ids, dangerous, threshold)
  return compute_score_ranks(ids, dangerous, threshold)

## to test if an id has one of more dangerous parts use threshold of 0.0
## (i.e., id.score > 0.0)
def compute_score_set(ids, dangerous, threshold):
    ds = set(dangerous)
    for id in ids:
        unique_parts = set(id.split)
        dangerous_parts = unique_parts & ds
        id.score = len(dangerous_parts) / len(unique_parts)
    x = compute_metrics(ids, threshold)  #     return [tp, fp, tn, fn, f2, f1]
    # x[4] = x[5]  # let's try f1
    return x

## this version considers the rank of the terms on the dangerous words list
def compute_score_ranks(ids, dangerous, threshold):
    l = len(dangerous)
    # print("\n\ntop of compute score for dangerous = ", dangerous, "threshold = ", threshold)
    for id in ids:
        unique_parts = set(id.split)
        # print("\nid = ", id.id, " unique parts = ", unique_parts)
        score = 0
        for part in unique_parts:
            # r = rank(part, dangerous)
            try:
              r = dangerous.index(part) 
              # print("\nxx score for", part, " = ", l-r)
            except ValueError:
              # print("OOPS setting r = l")
              r = l
            score += (l-r) / l   # rank 0 -> += 1.0
            # print("  score for  ", part, " = ", (l-r)/l, " ... current sum ", score)

        # print("post loop score = ", score, "len = ", len(unique_parts))
        id.score = score / len(unique_parts)
        # print("final score for ", id.id, " is ", id.score)
    x = compute_metrics(ids, threshold)  #     return [tp, fp, tn, fn, f2, f1]
    # x[4] = x[5]  # let's try f1
    return x



def all_dangerous(test):
  stats =  compute_metrics(test, -1) # predict everything is dangerous
  # stats[4] = stats[5]    # let's try f1
  return stats


# note that make_predictions is called once generating term map that is
# subseted here.  for folds=5 doing so was 4x faster!
def predict_dangerous_model(identifiers, term_map, min_score):
  terms = gather_terms(identifiers)
  score_map = {t: term_map[t] for t in terms}

  ## order dangerous terms based on score
  listofTuples = sorted(score_map.items(), key=lambda x: -x[1])
  dangerous_words = [elem[0] for elem in listofTuples if elem[1] >= min_score]
  # for elem in listofTuples:
    # print("%5.3f %5.3f %s" % (min_score, elem[1],elem[0]))
  # for i in range(0, len(dangerous_words)):
    # print("%5.3f %s %s" % (listofTuples[i][1],listofTuples[i][0], dangerous_words[i]))

  ## dump top and bottom 200 words
  # print("@@ M marker", len(terms), len(listofTuples),  file=sys.stderr)
  # print("for (min score %d ) top 200 scores" % (min_score))
  # for i in range(0, 200):
  #   print("%5.3f %s %s" % (listofTuples[i][1],listofTuples[i][0], dangerous_words[i]))
  # print("for (min score %d ) bottom 200 scores" % (min_score))
  # for i in range(len(dangerous_words)-200, len(dangerous_words)):
  #   print("%5.3f %s %s" % (listofTuples[i][1],listofTuples[i][0], dangerous_words[i]))

  print("@@ ids %d, unique terms %d, map-size %d, dw %d %d" % (len(identifiers), len(set(terms)), len(score_map), len(dangerous_words), len(set(dangerous_words))))
  return (dangerous_words)


def predict_dangerous_counts(identifiers, min_score, plus, minus):
  terms = {}
  for id in identifiers:
    for t in id.split:
      if not t in terms:
        terms[t] = 0

      if id.vulnerable:
        terms[t] += plus
      else:
        terms[t] -= minus  

  # print("number of terms ", len(terms))
  # listofTuples = sorted(terms.items(), key=lambda x: -x[1])
  # for i in range(0, len(listofTuples)):
    # print("%5.3f %s" % (listofTuples[i][1],listofTuples[i][0]))
  # exit(0)

  terms = {k:v for k,v in terms.items() if min_score < -900 or v >= min_score}

  ## order dangerous terms based on score
  listofTuples = sorted(terms.items(), key=lambda x: -x[1])
  dangerous_words = [elem[0] for elem in listofTuples ]
  # for elem in listofTuples:
    # print("%5.3f %s" % (elem[1],elem[0]))
  # for i in range(0, len(dangerous_words)):
    # print("%5.3f %s %s" % (listofTuples[i][1],listofTuples[i][0], dangerous_words[i]))

  ## dump top and bottom 200 words
  # print("@@ M marker", len(terms), len(listofTuples),  file=sys.stderr)
  # print("for (plus %d minus %d) top 20 scores" % (plus, minus))
  # for i in range(0, 20):
  #   print("%5.3f %s %s" % (listofTuples[i][1],listofTuples[i][0], dangerous_words[i]))
  # print("for (plus %d minus %d) bottom 20 scores" % (plus, minus))
  # for i in range(len(dangerous_words)-20, len(dangerous_words)):
  #   print("%5.3f %s %s" % (listofTuples[i][1],listofTuples[i][0], dangerous_words[i]))

  # print("@@ ids %d, unique terms %d, map-size %d, dw %d" % (len(identifiers), len(set(terms)), len(dangerous_words), len(set(dangerous_words))))
  return (dangerous_words)


# i/j for j = 1 to 6
# thresholds = [0.17, 0.20, 0.25, 0.33, 0.40, 0.50, 0.60, 0.67, 0.75, 0.80, 0.83, 1.00]
# computation uses "> threshold", so subtract 0.01 from each (and add -0.01)
thresholds = [ 0.01, 0.16, 0.19, 0.24, 0.32, 0.39, 0.49, 0.59, 0.66, 0.74, 0.79, 0.82, 0.99]
# the following enables the algorithm to choose 'all dangerous' as an option.
# current data does not include this, so we can see how bad the search truly is :)
# array.insert(-0.01, thresholds)

def best_thresh(identifiers, d, K, ret):
  # OLD: full range is 0.00 to < 0.99, but for the model values outside [0.15, 0.33]
  # OLD: rarely if ever win the day so its faster to use [0.10, 0.40]
  for threshold in thresholds:
    stats = compute_score(identifiers, d, threshold)
    # FYI f1 is in stats[5]
    if (ret[0] < stats[4]):
      # print("f2 score upgrade from ", ret[0], " to ", stats[4])
      ret = [stats[4], threshold, K, stats[0], stats[1], stats[2], stats[3]]
    # else:
      # print("f2 score stay ", ret[0], " > ", stats[4], " for K = ", K, " and threshold = ", threshold)

  return ret




# this needs work (and the idea kinda replicates the slow search)
# current version always set K somewhere near the end :)
def new_find_best_K_n_threshold(dangerous, identifiers, fold):
  start = 1
  stop = len(dangerous) + 1
  step = 2
  stepupat = 10

  ret = [-1, -1, -1]

  # print("@@ start", start, "stop", stop, "step", step,  file=sys.stderr)
  while start < stop:
    # print("start ", start, "step", step, "stepupat ", stepupat, "stop", stop, file=sys.stderr)
    d = S(dangerous[0:start])
    ret = best_thresh(identifiers, d, start, ret)

    start = start + step 
    if start > stepupat:
      step = step * 10
      stepupat = stepupat * 10
 
    if start + step > stop:
      step = int(step / 10)
    if step == 0:
      step = 1

  step = int(step/10)
  steupauat = int(stepupat/10)
  print("@@ find final K ", start, " final step ", step, "final step-up-at ", stepupat, file=sys.stderr)

  return ret


# based on f2 score and training data
# for 2200 identifiers profiler find this taking
#  46.647    4.665 CrossValidation.py:99(find_best_K_n_threshold)
# compared to
#  3.184    0.318 CrossValidation.py:129(find_best_K_n_threshold)
# for the binary search version
#
def slow_find_best_K_n_threshold(dangerous, identifiers, fold):
  start = 1
  max = len(dangerous) + 1
  stop = max
  step = int((max-1)/20)  # goal 20 steps
  if step < 1:
    step = 1

  ret = [-1, -1, -1]
  
  # step = 1 # FIX ME 

  ## note that some local maxes in training produce better test output
  ## making working hard to get best training answer not worth it :(
  print("@@ start", start, "stop", stop, "step", step,  file=sys.stderr)
  # last iteration makes a notable improvement to f2 (avoid while step > 9)
  while step >= 1:
    print("  fb  at start", start, "stop", stop, "step", step, _elapse(), file=sys.stderr)
    # chain ensures that stop is included
    for K in chain(range(start, stop, step), range(stop-1, stop)):
      d = S(dangerous[0:K])
      ret = best_thresh(identifiers, d, K, ret)
      # print("slow find  K %d  winning f2 %5.4f   at %5.3f, %d    start %d step %d stop %d" % (K, ret[0], ret[1], ret[2], start, step, stop), file=sys.stderr)

    start = ret[2] - step
    if start < 1:
      start = 1
    stop = ret[2] + step
    if stop > max:
      stop = max 
    step = int(step / 10)

  return ret






# based on f2 score and training data
# binary search approach ... alas landscape includes local maxes, so this is approximate
def old_find_best_K_n_threshold(dangerous, identifiers, fold):
  start = 1
  stop = len(dangerous) + 1
  mid = int((stop+start)/2)
  ret = [-1, -1, -1]
  l = best_thresh(identifiers, S(dangerous[0:start]), start, ret)
  h = best_thresh(identifiers, S(dangerous[0:stop]), stop, ret)

  while start < mid and mid < stop:
    if stop - start > 1000:
      tracer("  fb at start %d stop %d mid %d " % (start, stop, mid), _elapse())

    if l[0] > h[0]:
      stop = mid
      h = best_thresh(identifiers, S(dangerous[0:stop]), stop, ret)
    else:
      start = mid
      l = best_thresh(identifiers, S(dangerous[0:start]), start, ret)
    mid = int((stop+start)/2)

  tracer("  fb post loop at start %d stop %d mid %d " % (start, stop, mid), _elapse())
  return l if l[0] > h[0] else h



# the expensive n^2-ish approach that produces 1-2% improvement but takes 3x the execution time
def model_search(dangerous, identifiers, fold, plus, minus):
  best_f1 = -1
  best_f2 = -1
  best_f2_msg = "## oops len(dangerous) is %d (if this is not zero something is very wrong :( " % len(dangerous)

  if len(dangerous) > 1000:
    step = int(len(dangerous)/20)
    step = 100    # bulk of the data generation used 100 ... FIX ME reconsider??
  else:
    step = 1

  start = 1
  stop = len(dangerous) + 1
  for K in range(start, stop, step):
    if (K-1) % 2000 == 0:
      print("  ms time at start of K= ", K,  _elapse(), file=sys.stderr)
    d = S(dangerous[0:K])

    max = [-1, -1, -1, -1, -1]
    maxat = -1
    threshold = -0.02 # figures used is -0.01 ... was 0.00
    while threshold < 0.99:
      threshold += 0.01
      stats = compute_score(identifiers, d, threshold)
      maxat = maxat if max[4] >= stats[4] else threshold
      max = max if max[4] >= stats[4] else stats
      print(fmt_csv
           % (threshold, stats[0], stats[1], stats[2], stats[3], K, stats[4], maxat, stats[5], fold, plus, minus, "graph"))

    # print(" %3d %3d %3d %3d K = %4d, max f2 = %5.3f at threshold = %5.2f (f1 = %5.3f)"
    #       % (max[0], max[1], max[2], max[3], K, max[4], maxat, max[5]))

    if max[4] > best_f2:
      best_f2 = max[4]
      best_f2_msg = fmt_csv % (maxat, max[0], max[1], max[2], max[3], K, max[4], maxat, max[5], fold, plus, minus,"")
      # print("PING " + best_f2_msg)
    if max[5] > best_f1:
      best_f1 = max[5]
      best_f1_msg = fmt_csv % (maxat, max[0], max[1], max[2], max[3], K, max[4], maxat, max[5], fold, plus, minus,"")
    # break
    # exit(0)

  print(best_f2_msg, ", @@ model search best-f2")
  # print("@@", best_f1_msg, ", @best-f1")




def dump(category, id, v, b, threshold):
  s = ""
  for p in id.split:
    if p in v:
      s = s + "(v " + p + ")"
    elif p in b:
      s = s + "(b " + p + ")"
    else:
      s = s + "(. " + p + ")"
  if category != "tn" or id.score != 0.00:
    print(category, "%5.2f" % id.score, threshold, "  ", id.id, s)




# look at overlap between training and test vulnerable and benign identifiers
def termOverlap(dw, training, test, threshold):
  vtrain = list()
  vtest = list()
  btrain = list()
  btest = list()

  # x = set([id.id for id in test])
  # print("test = ", x)
  # x = set([id.id for id in training])
  # print("training = ", x)

  for id in training:
    for p in id.split:
      if p in dw:
        vtrain.append(p)
      else:
        btrain.append(p)

# way 2
#   for id in training:
#     if id.vulnerable:
#       for p in id.split:
#         vtrain.append(p)
#     else:
#       for p in id.split:
#         btrain.append(p)
# 
#   for id in test:
#     if id.vulnerable:
#       for p in id.split:
#         vtest.append(p)
#     else:
#       for p in id.split:
#         btest.append(p)

  # print("vtrain = ", vtrain)
  vtrain = set([x for x in vtrain if len(x) > 1])
  btrain = set([x for x in btrain if len(x) > 1])
  sv = len(vtrain)
  sb = len(btrain)
  # v = len(vtrain.intersection(vtest))   # use .add() with sets....
  # b = len(btrain.intersection(btest))

  v = len([x for x in vtrain if x in vtest])
  b = len([x for x in btrain if x in btest])
  print("xx vul train %3d, test %3d,  ben train %3d, test %3d, " % (len(vtrain), len(vtest), len(btrain), len(btest)), end= " ")
  print("overlaps: vul %3d %6.3f%%,  ben %3d %6.3f%%\n"  % (v, (100*v/sv), b, (100*b/sb)))

  for id in test:
    if id.score > threshold and id.vulnerable:
      dump("tp", id, vtrain, btrain, threshold)
    if id.score > threshold and not id.vulnerable:
      dump("fp", id, vtrain, btrain, threshold);
    if id.score <= threshold and id.vulnerable:
      dump("fn", id, vtrain, btrain, threshold);
    if id.score <= threshold and not id.vulnerable:
      dump("tn", id, vtrain, btrain, threshold);



def score(training, test, score_min, term_map, name, f2_all_ids, plus, minus):
  # print("# for sibren ... run the NN 'word finder' portion of the algorithm on the whole of test+train")
  # training = training + test
  print("# training size %d, test size %d\n" % (len(training), len(test)))
  print("\ntime at start of fold ", name,  elapse(START), file=sys.stderr)

  # termOverlap(FIXME, training, test, 0.00)
  # return

  stats = all_dangerous(training)
  print(fmt_csv % ( -1, stats[0], stats[1], stats[2], stats[3], -1, stats[4], -1, stats[5], name, plus, minus, "all dangerous train"))
  f2_ad_train = stats[4]

  stats = all_dangerous(test)
  print(fmt_csv % ( -1, stats[0], stats[1], stats[2], stats[3], -1, stats[4], -1, stats[5], name, plus, minus, "all dangerous test"))
  f2_ad_test = stats[4]

  if term_map == None:
    # prelim pms = [(3,4), (1,1), (3,2), (7,4), (2,1), (9,4), (5,2), (11,4), (3,1), (7,2), (4,1), (5,1)]
    # VDISC would have picked (10,1) 
    if plus == 0 and minus == 0:
      # Most paper data omits (10,1).  Analysis suggested trying 10,1 with
      # VDISC, which worked well.  Older data was not regenerated.
      pms = [(3,4), (1,1), (3,2), (7,4), (2,1), (9,4), (5,2), (11,4), (3,1), (7,2), (4,1), (5,1), (10,1)]
    else:
      pms = [(plus, minus)]   # for weight study use
  else:
    pms = [(0,0)]  # effectively unused

  f2_best = [-1,-1,-1]
  for p, m in pms:
    print("@@ p,m main loop (plus %d minus %d)" % (p, m), file=sys.stderr)
    if term_map == None:
      dangerous = predict_dangerous_counts(training, score_min, p, m)
    else:
      dangerous = predict_dangerous_model(training, term_map, score_min)
    # print("time pre find_best ", name,  elapse(START), file=sys.stderr)
    print("@@ there are %d dangerous words" % (len(dangerous)), file=sys.stderr)
    v =  slow_find_best_K_n_threshold(dangerous, training, name)
    # v =  old_find_best_K_n_threshold(dangerous, training, name)
    # print("@@ back in score bi search return selected K",v[2])
    # print(fmt_csv % (v[1], v[3], v[4], v[5], v[6], v[2], v[0], -1, -1, name, p, m, "@@ find best training (plus %d minus %d)" % (p, m)))
    f2_best_training = v[0]
    if f2_best_training >= f2_best[0]:
      f2_best = [f2_best_training, p, m]
      v_best = v

  p = "fish"
  m = "fish"
  v = v_best
  f2_best_training = f2_best[0]
  plus = f2_best[1]
  minus = f2_best[2]
  # print("@@ training produced best f2 of %5.3f at plus=%d, minus=%d\n" % (f2_best_training, plus, minus))
  print(fmt_csv % (v[1], v[3], v[4], v[5], v[6], v[2], v[0], -1, -1, name, plus, minus, "@@ find_best(training)"))

  if term_map == None:   # if recompute takes too long stash this
    dangerous = predict_dangerous_counts(training, score_min, plus, minus)
  print("@@ using %d of the %d dangerous words" % (v[2], len(dangerous)))

  if len(dangerous) == v[2]:
    print("@@@ starved! taking all words as dangerous")

  t1 = time.time()
  # FIX ME V2 might suppress words of length 1 
  # d = S([x for x in dangerous[0:v[2]] if len(x) > 1])
  d = S(dangerous[0:v[2]])
  # print(d)

  threshold = v[1]
  stats = compute_score(test, d, threshold)
  # termOverlap(d, training, test, threshold)
  print("@@ predicting time ", ((time.time() - t1)*1000), "ms for", len(test), "all_ids")
  print(fmt_csv
     % (threshold, stats[0], stats[1], stats[2], stats[3], v[2], stats[4], v[1], stats[5], name, plus, minus,
       "@@ eval test s"))
  f2_predicted = stats[4]

  # for folds=5, test is 1/5 the size of training, so this cost is ~80% less
  v =  slow_find_best_K_n_threshold(dangerous, test, name)
  # v =  old_find_best_K_n_threshold(dangerous, test, name)
  print(fmt_csv % (v[1], v[3], v[4], v[5], v[6], v[2], v[0], -1, -1, name, plus, minus, "@@ find_best(test)"))
  f2_best_test = v[0]

  # this call is real slow :)  (see keepers/45-test)
  # model_search(dangerous, test, name, plus, minus)     # also generates data for graphs
  print("@@ *** warning final model search commented out ***")

  print("")

  print("### AD, AD train, AD test, best train, predicted, best test, data set, dwc")
  print("### %6.3f, %6.3f, %6.3f, %6.3f,  %6.3f,  %6.3f, %s, %d, %d, %d" % (f2_all_ids, f2_ad_train, f2_ad_test, f2_best_training, f2_predicted, f2_best_test, name, len(dangerous), plus, minus))



