# More than Symbols : the effect of symbolic policies on support for costly climate policies

This repository contains the code and data for the paper "More than Symbols: the effect of symbolic policies on support for costly climate policies" by Théodore Tallent (theodore.tallent\@sciencespo.fr), Malo Jan (malo.jan\@sciencespo.fr) and Luis Sattelmayer (luis.sattelmayer\@sciencespo.fr), forthcoming in the *American Political Science Review*.

**Abstract**

> As climate change effects become increasingly salient, the need for stringent climate
policies becomes more pressing. The implementation of such policies is often met with
resistance from the public due to their perceived costs and distributional implications.
Scholars have mostly focused on material compensations to increase public support
among policy losers. This paper goes beyond the existing literature by showing how
what we term symbolic policies can enhance support for costlier policies. We define
symbolic policies as policies sending meaningful messages to the public but having low
material impacts. We argue that without changing the material costs that climate
policies impose, symbolic policies increase public support by altering the message
that costly policies convey. We demonstrate our argument using survey experiments
and qualitative interviews conducted in France, showing that symbolic policies can
significantly increase support for costly climate policies and increase perceptions of
fairness, elite responsiveness and policy effectiveness.

 The paper preprint is available at OSF: https://osf.io/ufb96/

## Replication materials

The folders `code/`, `data/`, and `outputs/` contain all replication materials for the paper.

The R environment is fully managed using [`renv`](https://rstudio.github.io/renv/).  
To replicate the analyses, clone this repository and open the R Project file `More_than_Symbols.Rproj` in **RStudio** (or start R from the project root). The entire workflow — from data preparation to tables and figures — can be executed automatically by running the scripts:

```r
source("code/00-setup-environment.R")
source("code/full_replication.R")
```

This will:

- Activate or install the renv environment
- Install all required packages 
- Clean and prepare all data
- Estimate all models,
- Generate all tables and figures, saved to `outputs/`. 

To run the replication, the Study 1 data file `fr_cdsp_ddi_elipss_202312_bee.csv` has to be downloaded from [data.sciencespo.fr](https://doi.org/10.21410/7E4/OH0RKI) and placed in the `data/raw/` folder. The data for Study 2 are already included in this repository under `data/raw/`.


