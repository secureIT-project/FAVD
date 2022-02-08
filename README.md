[![source under MIT license](https://img.shields.io/badge/source%20license-MIT-green)](LICENSE)
[![data under CC BY 4.0 license](https://img.shields.io/badge/data%20license-CC%20BY%204.0-green)](https://creativecommons.org/licenses/by/4.0/)

# FAVD: Featherweight Assisted Vulnerability Discovery

This repository contains the replication package for the paper
 _"Featherweight Assisted Vulnerability Discovery"_,
 David Binkley, Leon Moonen, Sibren Isaacman,
 Information and Software Technology,
 2022, 106844, ISSN 0950-5849,
 DOI: [10.1016/j.infsof.2022.106844](https://doi.org/10.1016/j.infsof.2022.106844).
 <https://www.sciencedirect.com/science/article/pii/S0950584922000209>.

The replication package is archived on Zenodo with DOI: [10.5281/zenodo.5957264](https://doi.org/10.5281/zenodo.5957264). The source code is distributed under the [MIT license](LICENSE), the data is distributed under the [CC BY 4.0 license](https://creativecommons.org/licenses/by/4.0/).

## Repository Organization

The overall process consists of three steps, organized as three directories:

1. gathering of the labeled function names that are used as the source for step 2, in [`names`](names/README.md)
2. dangerous word identification, in [`dangerous-words`](dangerous-words/README.md)
3. analysis of the data gathered during step 2, in [`analysis`](analysis/README.md)

The directory `Model` holds a copy of the pre-trained LAVDNN model as provided by the authors at
<https://github.com/StablelJay/LAVDNN/raw/master/Model/model_of_LAVDNN>


## Requirements

The following tools are required for the replication:

* python >= 3.5
* R
* tcsh
* csvcut from [csvkit](https://csvkit.rtfd.org/)
* cntk as keras backend for running the LAVDNN model

In addition, the following python packages are needed

* [for the collection of our input data](names/requirements.txt)
* [for dangerous word identification](dangerous-words/requirements.txt)

Finally, for the analysis in step 3, the following R libraries are needed:

* agricolae, ggplot2, reshape2, xtable


## Citation and Zenodo links

If you build on this data or code, please cite this work by referring to the paper: 

    @article{binkley2022:featherweight,
       title = {Featherweight assisted vulnerability discovery},
       author = {David Binkley and Leon Moonen and Sibren Isaacman},
       journal = {Information and Software Technology},
       pages = {106844},
       year = {2022},
       issn = {0950-5849},
       doi = {https://doi.org/10.1016/j.infsof.2022.106844},
       url = {https://www.sciencedirect.com/science/article/pii/S0950584922000209},
       copyright = {Open Access},
       publisher = {Elsevier},
    }


## Acknowledgement

Part of this work has been financially supported by the Research Council of
Norway through the secureIT project (RCN contract \#288787).
