[[part of the FAVD replication package]](https://github.com/secureIT-project/FAVD)

# Identifying *Dangerous Words*

## Installing the requirements

    python3 -m venv venv
    source venv/bin/activate
    python3 -m pip install -r requirements.txt


## Dangerous word prediction code (see file headers for details)

The main code is in three Python programs:

*  CrossValidation.py   -- generates most of the data except
*  AllDangerous.py      -- which generates the RQ1 data, and
*  LeaveOneOut.py       -- which generates the loo data

The `do-all` shell script includes the calls that use this python code to build the various intermediate datasets described below. It also calls helper scripts `doc` and `dom`, which in turn call `rq`.

These scripts place individual output files in a directory `data`, which needs to be
renamed to an appropriate name once the particular generation is complete.

The analysis is then started with the `extract` shell script.  
See [`../analysis/README.md`](../analysis/README.md) for the details.

FYI, the intermediate data generation is not quick, especially the detailed data for the ROC graphs.
For example, the generation of `data-w-details/VDISC.-c.-999,0,0.raw` took 6.5 days!


## Intermediate datasets that were gathered for the paper (see `do-all`)

### data-m
 - RQ2 data
 - uses FAVD_L to predict dangerous words
 - threshold search can be performed without "allow all-dangerous" (which actually does better in some cases)

### data-weights
 - RQ3a by-weight data
 - uses FAVD_F with a fixed weight to predict dangerous words

### data-c
 - RQ3b data
 - uses FAVD_F to predict dangerous words
 - uses training data to select weight 

### data-w-details
 - data for ROC and PRC graphs (see `analysis/individual.R`)
 - to enable detailed data generation, uncomment the following line in `DW_Common.py`:
 
        #   model_search(dangerous, test, name, plus, minus)     # also generates data for graphs`


## Intermediate datasets not used in the paper, but mentioned in the scripts

### data-loo
 - loo study using various subsets of the six programs.


## Note

The intermediate datasets are not included in the replication package because they have no reuse value and are several GBs large (even compressed they exceed GitHub's file size limits). However, the CSV files that are extracted from them are included in the directory [`../analysis/used`](../analysis/used) and can be used to repeat the analysis and re-create the graphs from the paper, see [`../analysis/README.md`](../analysis/README.md) for the details.

