# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 Leon Moonen <leon.moonen@computer.org>

# Collect the various datasets and extract the identifiers of vulnerable
# and benign (i.e., not labeled as vulnerable) functions

from pathlib import Path
import pandas as pd
import h5py
import requests, zipfile, io
import lizard


def read_hdf5(path):
    """Read VDISC hdf5 file into dataframe"""
    # Note: the VDISC hdf5 files are formatted in a way that cannot be read by pandas.read_hdf(), so we read by group/column
    d = dict()
    with h5py.File(path,"r") as file:
        groups = list(file.keys())
        for group in groups:
            d[group] = file[group][0:]
    return pd.DataFrame(d, columns=groups)


def get_function_name(source):
    """Extract the function name from a string of C++ or C source code that contains exactly 1 function"""
    global funs,errs
    i = lizard.analyze_file.analyze_source_code('try.cpp', source)    # first try to parse as C++
    funs += 1
    if len(i.function_list) == 1:
        name = i.function_list[0].name
    else:
        i = lizard.analyze_file.analyze_source_code('try.c', source)  # fall back to C if we did not find a function
        if len(i.function_list) == 1:
            name = i.function_list[0].name
        else:
            name = '__lizard_parse_error__'                           # if that doesn't work, we give up on this source
            errs += 1
    return name


def extract_functions_VDISC(src, files):
    """Extract function names and vulnerability labels from all source code provided in the VDISC hdf5 files"""
    df = pd.concat(map(lambda e: read_hdf5(src / e), files))
    # add a column with extracted names of the functions, "correcting" for the missing return type in VDISC with a void type
    df['functionName'] = df.apply(lambda row: get_function_name('void '+ row.functionSource.decode('utf-8')), axis = 1)
    df['isVulnerable'] = df.apply(lambda row: row['CWE-119'] or row['CWE-120'] or row['CWE-469'] or row['CWE-476'] or row['CWE-other'], axis = 1)
    # drop auxiliary columns before we save the results
    df = df.drop(columns=['CWE-119','CWE-120','CWE-469','CWE-476','CWE-other','functionSource'])
    # drop the rows for functions that we could not parse
    df = df[df.functionName != '__lizard_parse_error__']
    df = df.drop_duplicates()
    return df


def extract_functions_six_project(src_dir):
    """Extract function names and vulnerability labels from all source files provided in a given 6-projects project directory"""
    global funs,errs
    df = pd.DataFrame(columns=['functionName','isVulnerable'])
    for file in src_dir.glob('*.c'):
        funs += 1
        i = lizard.analyze_file(file.as_posix())
        if len(i.function_list) == 1:
            df.loc[df.shape[0]] = [ i.function_list[0].name, file.match('*CVE-*') or file.match('*cve-*')  ]
        else:
            errs += 1
    df = df.drop_duplicates()
    return df



def dataframe_to_vul_ben(df, vul, ben):
    df[df.isVulnerable == True].to_csv(vul, columns=['functionName'], header=None, index=None)
    df[df.isVulnerable == False].to_csv(ben, columns=['functionName'], header=None, index=None)


#
#  Main workflow
#

print('Downloading raw data from their sources...')


# get VDISC from https://osf.io/d45bw/ and store in VDISC-raw
vdisc_raw=Path('VDISC-raw')
vdisc_files=['VDISC_test.hdf5','VDISC_train.hdf5','VDISC_validate.hdf5']
if vdisc_raw.is_dir() and all( [Path(vdisc_raw/f).is_file() for f in vdisc_files] ):
    print('Re-using the VDISC data from the directory "VDISC-raw" (delete the directory to download again).')
else:
    print('The VDISC data does not exist or is incomplete, downloading from OSF (https://osf.io/d45bw/), this may take a while...')
    r = requests.get('https://files.osf.io/v1/resources/d45bw/providers/osfstorage/?zip=')
    r.raise_for_status()     # this will raise an exception when the download failed
    z = zipfile.ZipFile(io.BytesIO(r.content))
    z.extractall(vdisc_raw)


# get 6-projects from https://github.com/DanielLin1986/TransferRepresentationLearning/ and store in 6-projects-raw
sixp_raw=Path('6-projects-raw')
sixp_dirs=['Asterisk','FFmpeg','LibPNG','LibTIFF','Pidgin','VLC']
if sixp_raw.is_dir() and all( [Path(sixp_raw/d).is_dir() for d in sixp_dirs] ):
    print('Re-using the 6-projects data from the directory "6-projects-raw" (delete the directory to download again).')
else:
    print('The 6-projects data does not exist or is incomplete, downloading from GitHub (https://github.com/DanielLin1986/TransferRepresentationLearning/), this may take a while...')
    r = requests.get('https://github.com/DanielLin1986/TransferRepresentationLearning/raw/master/Data/VulnerabilityData/6_projects_functions.zip')
    r.raise_for_status()     # this will raise an exception when the download failed
    z = zipfile.ZipFile(io.BytesIO(r.content))
    z.extractall(sixp_raw)


print('Processing VDISC...')

dest=Path('processed')
dest.mkdir(parents=True, exist_ok=True)
vul = dest / 'VDISC_vulnerable.txt'
ben = dest / 'VDISC_benign.txt'
if vul.is_file() and ben.is_file():
    print(f'The files {vul} and {ben} already exist, reusing... (delete the files to regenerate).')
else:
    print(f'Extracting function identifiers from {vdisc_raw}, this may take a while')
    errs = funs = 0
    df = extract_functions_VDISC(vdisc_raw, vdisc_files)
    print(f'{vdisc_raw}: Processed {funs} functions, got {errs} parse errors (lost {errs/funs:.2%})')
    dataframe_to_vul_ben(df, vul, ben)


print('Processing 6-projects...')

sixp_proc=Path('processed')
sixp_proc.mkdir(parents=True, exist_ok=True)
for d in sixp_dirs:
    errs = funs = 0
    src_dir = sixp_raw / d
    vul = dest / str(d + '_vulnerable.txt')
    ben = dest / str(d + '_benign.txt')
    if vul.is_file() and ben.is_file():
        print(f'The files {vul} and {ben} already exist, reusing... (delete the files to regenerate).')
    else:
        print(f'Extracting function identifiers from {src_dir}, this may take a while')
        df = extract_functions_six_project(src_dir)
        print(f'{src_dir}: Processed {funs} functions, got {errs} parse errors (lost {errs/funs:.2%})')
        dataframe_to_vul_ben(df, vul, ben)


