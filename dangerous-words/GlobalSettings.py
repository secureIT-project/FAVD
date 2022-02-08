# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# Parameters for the model and dataset.
DIGITS = 50
InputChar = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890.- '

# Note: the LAVDNN encoding requires a blank in the string InputChar because it uses
# blanks to pad all strings to DIGITS characters.  thus blanks are separately
# filtered when reading in identifiers.
