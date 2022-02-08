[[part of the FAVD replication package]](https://github.com/secureIT-project/FAVD)

# Collecting the labeled function names

----

## Data Sources

Our input dataset builds on two existing datasets:

* [TransferRepresentationLearning Dataset](https://github.com/DanielLin1986/TransferRepresentationLearning)

   These vulnerabilities are from 6-open source projects: 
   FFmpeg, LibTIFF, LibPNG, Pidgin, Asterisk and VLC Media Player, 
   and vulnerability information was collected from NVD until the end of July 2017.

* [Draper VDISC Dataset](https://osf.io/d45bw/wiki/home/)
	
  This set contains the function names extracted from the source code
  of 1.27 million functions mined from open source software,
  labeled by static analysis for potential vulnerabilities.
  The data is distributed in three files corresponding to an 80:10:10 
  train/validate/test split, but for our purpose, we concatenate all.

## Data processing

The `collect-data.py` script downloads the original datafiles as zips and 
uncompresses them in respectively `6-projects-raw` and `VDISC-raw`. 
It then parses all code fragments using [Lizard](https://github.com/terryyin/lizard), 
and stores it in 7 pairs of files in the directory `processed`: 

* `Asterisk_benign.txt` and `Asterisk_vulnerable.txt`
* `FFmpeg_benign.txt` and `FFmpeg_vulnerable.txt`
* `LibPNG_benign.txt` and `LibPNG_vulnerable.txt`
* `LipTIFF_benign.txt` and `LipTIFF_vulnerable.txt`
* `Pidgin_benign.txt` and `Pidgin_vulnerable.txt`
* `VLC_benign.txt` and `VLC_vulnerable.txt`
* `VDISC_benign.txt` and `VDISC_vulnerable.txt`

The script is 'smart' in that it will not overwrite files that were already 
collected (thus, you'll need to remove them to download/generate again). 


### Installing the requirements

    python3 -m venv venv
    source venv/bin/activate
    python3 -m pip install -r requirements.txt

## Note

The input data that was used for the paper was collected using 
[srcML](https://www.srcml.org/) instead of lizard. Since srcML is an 
external tool, it required spawning a sub-process for each code fragment, 
making extraction last several days. This is addressed by switching to 
lizard, which runs in less than an hour. However, as a result of using 
different parsing techniques, the collected datasets are slightly 
different (the total number of functions successfully parsed using 
Lizard is somewhat larger, but there _may_ also be functions missing 
that srcML could parse and Lizard could not).  

Overall, the differences are so small that we do not expect them to have 
any significant impact on the findings.  For replication purposes, we 
provide copies of the two datasets in resp. `processed-srcML` and 
`processed-lizard`, and `processed` initially contains the same content 
as `processed-srcML`.

