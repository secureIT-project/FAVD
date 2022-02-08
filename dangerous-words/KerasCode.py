# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman


# common methods related to the keras model.
# these are separated out because the initial load takes far
# too long for code that does not use keras :)

# from tensorflow.keras.models import load_model
from keras.models import load_model
from Id import *
from Vectorization import *
from pathlib import Path


# 11 oct 20 added preserve_blanks to support Sibren's experiment

##----------------Load the model --------------------------------
def load_it(model, validChars, maxLen):
  if model != "":
    model_info = Path(model)
  else:
    model_info = Path('../Model/model_of_LAVDNN')     # Provided trained model
  if not model_info.exists():
    model_info = Path('../../Model/model_of_LAVDNN')  # when code is in sub dir :)
  model = load_model(model_info)
  return model

def make_predictions(words, validChars, maxLen, model=""):  
  return make_predictions_w_shift(words, validChars, maxLen, model="")
  # return make_predictions_original(words, validChars, maxLen, model="")


# consider all possible shifted version of each word
# for example, for "a" and length 3, "a  ", " a ", and "  a"
def make_predictions_w_shift(words, validChars, maxLen, model=""):  
  model = load_it(model, validChars, maxLen)
  score_map = { }

  for w in set(words):
    queries = []
    for i in range(0,maxLen-len(w)):   # all possible shifts
      queries.append(" "*i + w + " "*(maxLen-i-len(w)))

    # Vectorization 
    ctableInput = CharacterTable(validChars)
    vec_word = np.zeros((len(queries), maxLen, len(validChars)), dtype=np.bool)
    for j, sentence in enumerate(queries):
      vec_word[j] = ctableInput.encode(sentence, maxLen)

    # Prediction 
    # print('# Predict vulnerable functions...')
    # preds_class_no = model.predict_classes(vec_word)
    preds = model.predict(vec_word)

    maxAt = 0
    for i in range(0,len(preds)):
      if preds[i][0] > preds[maxAt][0]: 
        # print("update max from ", maxAt, " to ", i,  " from ", preds[maxAt][0], " to ", preds[i][0])
        maxAt = i
    score_map[w.strip()] = preds[maxAt][0]

    # d = preds[maxAt][0] - preds[0][0]
    # p = d/preds[0][0] * 100.0
    # c = "xS" if preds[0][0] < 0.10  else "xB"
    # print("%20.20s at 0 %6.4f at MAX (%2d) %6.4f  diff %6.4f or %6.0f%% %s" % 
    #   ( w.strip(), preds[0][0], maxAt, preds[maxAt][0], d, p, c))
    # # for i in range(0,len(preds)):
    #   # print("at ", i, preds[i][0])
    # # print("")

  return score_map





# works with full identifiers or their constituent terms
# this version scores the words placed at the far left of the buffer
# (make_predictions_w_shift tries all possible buffer positions.)
def make_predictions_original(words, validChars, maxLen, model=""):  
  model = load_it(model, validChars, maxLen)

  # ---------------------add padded items to array------------------------
  padded_words = []
  for q in set(words):
    query = q + ' ' * (maxLen - len(q))
    padded_words.append(query)

  ##-----------------Vectorization ----------------------------------
  ctableInput = CharacterTable(validChars)
  vec_word = np.zeros((len(padded_words), maxLen, len(validChars)), dtype=np.bool)
  for j, sentence in enumerate(padded_words):
    vec_word[j] = ctableInput.encode(sentence, maxLen)

  ##----------------Prediction with model --------------------------------
  # print('# Predict vulnerable functions...')
  # preds_class_no = model.predict_classes(vec_word)
  preds = model.predict(vec_word)

  ##---------------- Mapping used to aggregate score of terms ---------------
  score_map = { }
  for i in range(0,len(preds)):
    score_map[padded_words[i].strip()] = preds[i][0]
  return score_map

