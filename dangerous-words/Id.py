# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman


# class Id that includes a simple term splitting

import re


# rules used split at
#   [0-9a-z][A-Z]
#   [^0-9][0-9]
# rules unused at present
#   [a-zA-Z][0-9]
#   [0-9][a-zA-Z]
# also the following abbreviations are split out 
# (warning: this is very domain dependent and based on FFmpeg!!)
#   TIFF JPEG 
#
abbreviations = ['JPEG', 'TIFF']

def split(id):
    terms = []
    x = re.sub(r'([0-9a-z])([A-Z])', r'\1_\2', id)
    x = re.sub(r'([^0-9])([0-9])', r'\1_\2', x)
    for abbr in abbreviations:
        if abbr in x:
            x = x.replace(abbr, '_'+abbr+'_')
    x = re.sub(r'__*', '_', x)
    x = re.sub(r'^_', '', x)
    x = re.sub(r'_$', '', x)
    parts = x.split('_')
    return parts


# "struct" Id
class Id:
    def __init__(self, i, v=True):
        self.id = i.strip()
        self.split = split(self.id)
        self.score = 0
        self.vulnerable = v

#    def __eq__(self, other):
#        return self.id == other.id 

