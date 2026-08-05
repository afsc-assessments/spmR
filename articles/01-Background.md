# Standard Projection Model

Alaska Fisheries Science Center  
Seattle WA 98115

## Introduction

### 2024 update

The following priorities have been updated in light of changes in
requirements. Discussion about projection alternatives with NOAA General
Counsel.

### Proposal

In light of this, the group noted that the scenario that corresponds to
the Author’s recommendation based on the FMP amendment, which for Tier 3
is based on Fspr rates, form the main set of results to present.

This may require consideration where years with pre-specified catches
are needed for the two-year out projection or for doing a catch
projection for an assessment that has lapsed for some years. In these
cases, there might be a benefit (for accuracy’s sake) to do an iteration
where the next-year’s catch is likely to differ from say the authors
recommended ABC.

Additional optional projection scenarios might include setting fishing
mortality to recent average (and maybe zero) for contrast.

Updates to ensure the code works with a subset of example models has
been completed and are shown on the github site.

### Earlier background and documentation

This seems to be a dead link now…:

<https://archive.fisheries.noaa.gov/afsc/refm/stocks/projections.htm>

During the November 2004 groundfish Plan Team meetings the Teams
requested that work on enhancing the standardized projection model (SPM)
methodology. The current projection model and methods have been used
since 1999 and were designed to provide the needed projection scenarios
for the annual Environmental Assessment (EA) for the TAC specifications.
Additionally, two other scenarios were conducted to determine whether a
stock is currently in an overfished condition or is approaching an
overfished condition. The details for the projection scenarios are given
in Fig. 1.

The current methodology is problematic for a number of reasons. First,
the model software was not designed to provide the basis for ABC and OFL
recommendations beyond the coming year. Under amendment 48/48, and as
was done in 2004, the Council is required to make ABC, TAC, and OFL
recommendations for the next two years (i.e., for 2005 and 2006). The
projection model and software was modified to accommodate this. Also,
there were a number of assumptions in the projection model that required
more careful evaluation (e.g., the underlying stock-recruitment
relationship assumptions, and the estimated uncertainty in current stock
abundance levels). In the section below titled “standardized projection
model” the developments of the SPM are provided to show compatibility
with all types of assessment models used for N. Pacific groundfish.

For the Programmatic Supplemental Environmental Impact Statement (PSEIS)
an extension of the first version of SPM was developed (in 2002). This
involved linking the stock assessment information with observed species
compositions by fishery. The resulting multi-species technical
interaction model (MSTIM) allowed for evaluation of alternative
approaches to groundfish fishery management for federally managed waters
in Alaskan waters. The MSTIM provides more realistic catch-levels to be
fed into single-species stock projections. Previously, single-species
evaluations were typically done in isolation to issues such as
regulatory limits on the overall catch level, bycatch constraints, and
effort development plans. The data requirements for MSPTIM are extensive
and if one goal is to provide a simple approach for providing
catch-feedback as a function of ABC levels, development of a simpler
approach is warranted. In the section title “multi-species
considerations” a simple model based on historical patterns of TACs,
ABCs, and catches is proposed as an option for estimating future TAC
levels given future ABCs.

The projection model developments relate to a number of ongoing research
activities at the AFSC. For example, North Pacific groundfish Management
Strategy Evaluation (MSE) studies can use the projection model to easily
evaluate current practices. Also, the NMFS National Standard Guidelines
(NSG) for management under the Magnuson-Stevens Fisheries Management
Conservation Act are currently under revision. The projection model and
code has been designed to easily be adopted to meet anticipated future
demands.

## Standard projection model

#### Stock-recruitment relationship specifications

From the Ad-hoc meeting on projection model approaches, the report
notes:

> *“The group discussed options for improving the projection model
> assumption about recruitment (i.e., that it varies about the mean
> level estimated from 1978 to the most recent estimated). In
> particular, having recruitment that is affected by spawning stock
> biomass (SSB) levels was considered to be more realistic. Also,
> specifying some degree of autocorrelation in residuals was considered
> desirable.*
>
> *The options for using an alternative stock-recruitment relationship
> in the projection model as a function of SSB were given as:*

1.  *assume B_(msy)=B_(35%) , and F_(msy) = F_(35%) and solve for the \>
    parameters*

2.  *assume F_(msy) = F_(35%) and estimate stock-recruitment parameters
    \> given stock-recruitment output from the assessment model*

3.  *simply estimate the stock-recruitment parameters from input \>
    stock-recruitment output from assessment model*

4.  *use estimates of stock-recruitment parameters from within the \>
    assessment model*

5.  *use prior distributions for stock-recruitment parameters (or
    F_(msy) \> and B_(msy) levels)…*

> *The actual specification of the type of stock-recruitment curve will
> initially be either Ricker or B-Holt.*
>
> *The group suggested that option 2) would be a good place to start.
> They noted **that assessment authors will need to supply estimates of
> SSB to match the current estimates of recruitment** for the projection
> model.*
>
> *Adding autocorrelation to future recruitment is considered important,
> especially given medium-term patterns in environmental variability
> that apparently affect recruitment. The specification for
> autocorrelation function will need to ensure that the expected values
> are correct.”*

An option for writing files with all the annual (simulated) output is
now available so that different analyses can be undertaken (currently,
the results are summarized internally). The standard projection model
developed for the purposes of the Council and authors of the EA are not
exclusive—individuals are encouraged to undertake their own projection
analyses as time permits. The SPM is intended to provide a tool for the
Council and assessment models to have a common utility from which to
project ABC and OFL levels.

#### Input files

Previously, the standardized projection model evolved into a number of
different versions to accommodate models with sex-specific natural
mortality, growth, and multiple fisheries. Also, the ability to specify
future catch scenarios was added along with the standard EA
alternatives. The revised version developed here has the flexibility to
deal with all options in a simple way. Additionally, the features
required by the NPFMC and NMFS to easily specify updated catch levels
was added, as was the ability to run any number of species from within
the same framework. To simplify this and minimize inadvertent edits, the
input files were split into three separate files (Fig. 2). These are
organized as follows:

- Master Setup file

  - Designed to control to assumptions and dimension of the projections,
    give a list of different scenarios (e.g., those listed in Fig. ).

- The catch data file

  - Designed to specify the catch in each future year (for use by the
    Region and Council as values are updated) and points to the
    species-specific assessment result data file

- The species-specific assessment result data file

  - Contains species and model-run specific results from the assessment
    model

Actual working examples of these are provided at the following web page:
[www.afsc.noaa.gov/refm/stocks/projections.htm](http://www.afsc.noaa.gov/refm/stocks/projections.htm)

##### Steps for doing projections

1.  Run the stock assessment model making note of the main demographic
    results, current numbers at age and historical stock-recruitment
    estimates

2.  Edit the stock-specific input files (e.g., the bottom panel of Fig
    2).

3.  Edit the catch projections file which includes assumptions about
    future catch (e.g., the middle panel of Fig 2).

4.  Edit the “setup.dat” file which includes assumptions about
    projection specifications (e.g., stock-recruitment relationship or
    not, types of constraints, location and name of stock-specific
    detail files). This file is shown as the top panel of Fig 2.

5.  Run the projection model (e.g., use file “run.bat” by typing as
    follows: “run myfile” at the command line, where myfile is the root
    of the file name you selected (e.g., goa_pop)).

6.  Save and evaluate results (examine files in newly created myfile_out
    directory, these currently include the following:

> bigfile.out full output file containing catch in every simulated year
>
> bigsum.dat summarized version of bigfile.out
>
> F_profile.out profile of main characteristics over fishing mortality
>
> srec.out details on stock-recruitment data that were fit
>
> means.out General projection results by species and alternatives
>
> percentiles.out Gives percentiles of simulation results and
> variability
>
> report.out Some simple summary output (e.g., mean age, generation
> time)

7.  Repeat steps 4)-6) as desired for different model configurations
    etc.

8.  Repeat steps 3)-7) as desired for different impacts of near-term
    catch levels

9.  Repeat steps 1)-8) as desired to evaluate different model results.

### Example results

Since fitting a stock-recruitment relationship is one of the major
enhancements for the SPM, the feature is highlighted here. In
particular, varying assumptions about conditioning a stock-recruitment
relationship is considered. *It should be noted that the main purpose
for including a stock-recruitment relationship is to provide more
realistic assumptions about expected recruitment levels as stocks
decline.* This exercise should not be viewed as a rigorous assessment of
stock productivity. However, stock-recruitment analyses are undeniably
important for management considerations and the presentation here
represents a limited evaluation of a large set of possible alternatives
(e.g., low-frequency climate-driven changes in stock productivity).

Using the estimates of spawning biomass as reported in the 2004 SAFE
reports, the fits for the conditioned (*F_(msy)* = *F_(35%)* ) and
unconditioned (fit to stock-recruitment data only) between these options
were reasonably consistent (Fig. ). Examining yield for these cases
shows that without the conditioning, some stocks have extremely high
levels of *F_(msy)* and adding the conditions tends to reduce the level
of *F_(msy)* (Fig. ).

To understand the implications of using a stock-recruitment relationship
in the projection model, it is useful to undertake some contrasts. For
the first example, an “author’s recommended” ABC level was specified to
be 40 times the current maximum permissible ABC level under Amendment 56
of the FMP. The intent here was to evaluate the performance of the stock
to extreme levels of fishing mortality. However, since the other parts
of the control-rule are implemented for the “Author’s recommended”
option, the actual catch is high only in the first year since the
spawning biomass is held to very low levels (and thus requiring large
adjustments under the current control rule; Fig. .). Note that at very
low levels the current practice of using the recent mean recruitment
(and variability) results in a more optimistic outcome. To further
evaluate the stock-recruitment influence on projections, a second set of
runs was completed where the fishing mortality rate was unadjusted and
simply held at a value of 1.0. For this scenario the stock completely
collapsed in about 15 years when the stock-recruitment relationship was
included but was held at about 2% of the current spawning biomass level
under the mean-recruitment assumption (Fig. .).

## Multi-species considerations

In this section a simple method for modeling TACs is demonstrated and
applied using the new version of the SPM.

#### Data

As part of an effort to develop a standardized database for both
archival and analytical purposes, information gleaned from SAFE reports,
Federal Register notices, NPFMC documents, and NMFS Regional Office was
obtained. Specifically, the TAC, ABC, and where possible, OFL levels
were compiled by stocks and areas. Some discrepancies between reports
were found and where these existed, the numbers in the Federal Register
notice were given highest precedence. The initial layout of the database
is shown in Table 1. This database allows for straightforward and
flexible cross-tabulations (e.g., Table ).

As a first step in evaluating historical patterns of TAC by species
groups, a set of pair-wise plots were constructed. These show that EBS
pollock TAC proportion was negatively correlated with flatfish species
in addition to others while Pacific cod TAC proportion was largely
independent of other TACs for both the full period of available data and
for the data only since 1989 (Figs. & ). Since ABC levels are one of the
main determinants of TACs by serving as an upper bound, the effect of
ABCs are compared to each main species group. Pacific cod TAC levels are
relatively independent of the other main species ABCs (except for
Pacific cod ABC levels). An inverse relationship between pollock ABCs
and flatfish TACs is apparent. Pollock TAC is affected most by pollock
ABC up to the limit of about 75% of the overall TAC (2 million t).

#### A multispecies example of estimating TACs given ABCs

For illustration, an approach to estimate TAC levels by the main species
groups was developed using a piecewise regression model fit to
historical ABCs and TACs from 1989-2004 (Table ). The formulation first
required defining breakpointswhere *m* is the number of segments for
which a coefficient will be estimated. For each species, the ABCs were
normalized to have a maximum of 1.0 over time and the values of the
breakpoints were thus specified as equally spaced bins (i.e., ). The TAC
in year *t* for stock *j* can thus be given as:

where

with equal to the ABC of the *i^(th)* stock in year *t* and is the
parameter matrix of dimension to be estimated (*n* is the total number
of stocks under consideration)*.* This approach is somewhat simpler to
applying a GAM (generalized additive model) to these data which were
shown to outperform simple multiple regression approaches.
Experimentation with different values of *m* suggested that little
improvements were seen with *m \> 5*. For the results presented here, a
value of *m=5* was used.

Moderate second-difference penalties were placed on the values of to
ensure estimability for segments where data were scarce or non-existent
(i.e., the values will tend towards a mean level). This approach
resulted in reasonably good fits to the historical patterns (Fig. ). For
the projection model, the coefficients from the above regression were
used along with an added constraint that in any future year *t*, (i.e.,
the TAC for species *j* predicted from the above model must be less than
or equal to the ABC in that year). The code for doing this estimation is
provided in an Appendix.

#### Results of multispecies projections

As an example using the standard projection approach, 500 simulations
were conducted for a 30 year period beginning in 2004. For each future
year (beyond 2006) the catch was specified using the above algorithm.
Mean values from these results are presented in Table . (NOTE, a table
of the ABC and OFL levels are what would have been specified in each
future year are also available, as are SSB and a (minimal) estimate on
the distribution of catches (i.e., the actual uncertainty in future
catches should be much higher). The mean trajectories of TACs show
stabilization in trends in about 10-15 years (Fig. ).

## Summary

A number of changes to the SPM are completed. The ability to easily
configure different catch scenarios for relevant stocks has been
implemented through simple sets of data files (with further refinements
expected). This should help facilitate the Council Plan Teams and the
NMFS RO with the ABC and OFL levels they require for future years.
Options to specify stock-recruitment relationships (primarily for more
realistic projections) have been added. The code allows for technical
interactions (in general) and an example using a simpler form than the
“optimized” (LP) method used in the PSEIS was presented.

Future specifications will include autocorrelation in recruitment
residuals. Additionally, the ability to simulate from estimates of
current stock-size uncertainty is planned (and perhaps extended to apply
to Alt 3b from the PSEIS). With minor modifications to the input files,
trophic level output can easily be generated and is anticipated to be
included in future versions. It is also envisioned that extensions to
Tier 4 and lower species could be possible (as was done for the PSEIS).

## References (to be revised)

Ackley, D. 1995. Bering Sea Fishery Simulation Model. Alaska Fishery
Research Bulletin 2(1): 83-86.

Brown, B.E. , J.A. Brennan, and J.E. Palmer. 1979. Linear programming
simulations of the effects of bycatch on the management of mixed species
fisheries off the northeastern coast of the United States. Fishery
Bulletin 76:851-860.

Larson, D .M., House, B. W. and Terry, J. M. 1996. Towards Efficient
Bycatch management in Multispecies Fisheries: A Nonparametric Approach.
Marine Resource Economics 11: 181-201.

Marasco, R.J. and Terry, J.M. 1982. Controlling Incidental Catch: An
Economic Analysis of Six Management Options. Marine Policy 14:131-139.

Murawski, S.A., and Finn, J.T. 1986. Optimal effort allocation among
competing mixed-species fisheries, subject to fishing mortality
constraints. Canadian Journal of Fisheries and Aquatic Sciences
43:90-100.

Press, W.H., Teukolsky, S.A., Vetterling, W.T. and Flannerty, B.P. 1992.
*Numerical Recipes in C: The Art of Scientific Computing.* 2nd Edition.
Cambridge University Press. New York.

## Figures

###### Figure . Standard harvest scenarios used for the projection model (e.g., for assessments done in 2004).

###### Figure . General layout of projection model input files.

###### Figure 3. Fit of stock-recruitment relationship with and without added condition that *F_(msy)* = *F_(35%)* .

###### Figure 3. Continued.

###### Figure 3. Continued.

###### Figure 3. Continued.

###### Figure 3. Continued.

###### Figure 3. Continued.

###### Figure . Equilibrium yield profiles over fishing mortality rates for unconditioned cases (stock-recruitment relationships fit only to output from stock assessments, top panel) and conditioned cases (where *F_(msy)* = *F_(35%)* , bottom panel).

###### Figure . Mean trajectories for scenarios where the status-quo situation ABC multiplier is very large. This contrasts the behavior of the assumption about using mean recruitment (current projection system) versus a conditioned (*F_(msy)* = *F_(35%)*) assumption..

###### Figure . Projections of spawning stock biomass (as a percentage of the 2004 level) for Pacific ocean perch in the Bering Sea/Aleutian Islands region under extreme constant levels of fishing mortality (F = 1.0). The projections represented by the circles are based on the assumption that recruitment will vary and have the same mean as in the recent path whereas the solid dashes are based on the assumption that a stock-recruitment relationship (i.e., conditioned such that the *F_(35%)* = *F_(msy)* ).

###### Figure . Correlations of TAC proportions among main species groups for the BSAI during 1989-2004. Values in boxes on upper right side represent correlations.

###### Figure . Correlations of TAC proportions among main species groups for the BSAI during 1995-2004. Values in boxes on upper right side represent correlations.

###### Figure . Observed versus predicted TAC levels given historical data presented in Table 1 (millions of tons).

|                                                         |
|:--------------------------------------------------------|
| Pollock and Pacific cod (projected TACs in metric tons) |
| Other species (projected TACs in metric tons)           |

###### Figure . Example 30-year multi-species projections of BSAI mean TACs using the regression model of historical TAC levels given ABC values.

## Tables

###### Table . Initial database layout (subject to validation and further edits) for historical ABC and TAC levels as available at [www.afsc.noaa.gov/refm/stocks/projections.htm](http://www.afsc.noaa.gov/refm/stocks/projections.htm).

|  |  |  |  |  |  |  |  |
|:---|:---|:---|:---|:---|:---|:---|:---|
| **Source** SAFE_2004 SAFE_2004 SAFE_2004 … | **Area** BSAI BSAI BSAI | **Mgt_Area** BSAI BSAI BSAI | **Name_Quant** TAC TAC TAC | **Spp** Atka Atka Atka | **Year** 1978 1979 1980 | **Level** 24,800 24,800 24,800 | **Reclass** Atka Atka Atka |

###### Table . Example cross tabulations from the database (subject to validation and further edits). ABC (top table) and TAC (lower table) levels for BSAI regions.

|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|----|:---|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
|  | **BSAI ABC** |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| Year | Atka | Oflats | OthSpp | Pcod | Pollock | POP | RockFish | RockSole | Sable | Gturb | YfinSole | FheadSole | Arrowtooth | AKPlaice | NrthrnRF | Grand Total |
| 1977 |   | 65,000 |   |   | 950,000 | 21,500 |   |   | 7,400 | 40,000 | 106,000 |   |   |   |   | 1,189,900 |
| 1978 |   | 139,000 |  |  | 950,000 | 21,500 |  |  | 6,500 | 40,000 | 106,000 |  |  |  |  | 1,263,000 |
| 1979 |   | 61,000 |  |  | 1,100,000 | 21,500 | 7,700 |  | 5,000 | 90,000 | 117,000 |  |  |  |  | 1,402,200 |
| 1980 | 24,800 | 61,000 | 74,200 | 148,000 | 1,300,000 | 18,000 | 7,700 |  | 3,700 | 76,000 | 169,000 |  | 20,000 |  |  | 1,902,400 |
| 1981 | 24,800 | 92,500 | 94,400 | 160,000 | 1,300,000 | 18,000 | 21,300 |  | 3,700 | 59,800 | 214,500 |  | 16,500 |  |  | 2,005,500 |
| 1982 | 24,800 | 92,500 | 94,300 | 168,000 | 1,300,000 | 18,000 | 22,000 |  | 2,900 | 60,000 | 214,500 |  | 16,500 |  |  | 2,013,500 |
| 1983 | 25,500 | 119,200 | 61,400 | 298,200 | 1,300,000 | 11,800 | 14,100 |  | 2,900 | 65,000 | 214,500 |  | 20,000 |  |  | 2,132,600 |
| 1984 | 25,500 | 150,200 | 61,000 | 291,300 | 1,300,000 | 12,160 | 14,100 |  | 6,185 | 47,500 | 310,000 |  | 20,000 |  |  | 2,237,945 |
| 1985 | 37,700 | 150,200 | 51,200 | 347,400 | 1,300,000 | 12,760 | 8,910 |  | 6,080 | 44,200 | 310,000 |  | 20,000 |  |  | 2,288,450 |
| 1986 | 30,800 | 137,500 | 35,900 | 249,300 | 1,300,000 | 10,200 | 8,900 |  | 7,200 | 35,000 | 230,000 |  | 20,000 |  |  | 2,064,800 |
| 1987 | 30,800 | 193,300 | 49,500 | 400,000 | 1,300,000 | 14,700 | 1,880 |  | 7,700 | 20,000 | 187,000 |  | 30,900 |  |  | 2,235,780 |
| 1988 | 21,000 | 331,900 | 54,000 | 385,300 | 1,500,000 | 22,600 | 1,500 |  | 9,200 | 14,100 | 254,000 |  | 99,500 |  |  | 2,693,100 |
| 1989 | 21,000 | 155,900 | 59,000 | 370,600 | 1,340,000 | 22,600 | 1,500 | 171,000 | 6,200 | 20,300 | 241,000 |  | 163,700 |  |  | 2,572,800 |
| 1990 | 24,000 | 188,000 | 55,500 | 417,000 | 1,450,000 | 22,900 | 1,600 | 216,300 | 2,700 | 7,000 | 278,900 |  | 106,500 |  |  | 2,770,400 |
| 1991 | 24,000 | 219,700 | 28,700 | 229,000 | 1,676,000 | 15,345 | 1,325 | 246,500 | 3,100 | 7,000 | 250,600 |  | 116,400 |  |  | 2,817,670 |
| 1992 | 43,000 | 199,600 | 27,200 | 182,000 | 1,490,000 | 15,240 | 1,325 | 260,800 | 1,400 | 7,000 | 372,000 |  | 82,300 |  |  | 2,681,865 |
| 1993 | 117,100 | 191,000 | 26,600 | 164,500 | 1,340,000 | 17,230 | 1,325 | 185,000 | 1,500 | 7,000 | 238,000 |  | 72,000 |  |  | 2,361,255 |
| 1994 | 122,500 | 225,000 | 27,500 | 191,000 | 1,330,000 | 14,210 | 1,135 | 313,000 | 540 | 17,200 | 230,000 |  | 93,400 |  |  | 2,565,485 |
| 1995 | 125,000 | 117,000 | 27,600 | 328,000 | 1,250,000 | 13,750 | 1,135 | 347,000 | 1,600 | 7,000 | 277,000 | 138,000 | 113,000 |  |  | 2,746,085 |
| 1996 | 116,000 | 102,000 | 27,600 | 305,000 | 1,190,000 | 22,360 | 1,449 | 361,000 | 2,500 | 10,300 | 278,000 | 116,000 | 129,000 |  |  | 2,661,209 |
| 1997 | 66,700 | 97,500 | 25,800 | 306,000 | 1,130,000 | 21,948 | 1,087 | 296,000 | 2,675 | 12,350 | 233,000 | 101,000 | 108,000 |  |  | 2,402,060 |
| 1998 | 64,300 | 164,000 | 25,800 | 210,000 | 1,110,000 | 18,962 | 1,054 | 312,000 | 2,680 | 15,000 | 220,000 | 132,000 | 147,000 |  |  | 2,422,796 |
| 1999 | 73,300 | 154,000 | 32,860 | 177,000 | 992,000 | 20,862 | 1,054 | 309,000 | 3,200 | 14,200 | 212,000 | 77,300 | 140,000 |  |  | 2,206,776 |
| 2000 | 70,800 | 117,000 | 31,360 | 193,000 | 1,139,000 | 21,129 | 1,054 | 230,000 | 3,900 | 9,300 | 191,000 | 73,500 | 131,000 |  |  | 2,212,043 |
| 2001 | 69,300 | 122,000 | 35,570 | 188,000 | 1,874,270 | 11,930 | 8,829 | 228,000 | 4,060 | 8,400 | 176,000 | 84,000 | 117,000 |  |  | 2,927,359 |
| 2002 | 49,000 | 18,100 | 41,070 | 223,000 | 2,138,110 | 14,800 | 2,065 | 225,000 | 4,480 | 8,100 | 115,000 | 82,600 | 113,000 | 143,000 | 6,760 | 3,184,085 |
| 2003 | 63,000 | 16,000 | 45,270 | 223,000 | 2,373,470 | 15,100 | 2,561 | 110,000 | 6,000 | 5,880 | 114,000 | 66,000 | 112,000 | 137,000 | 7,101 | 3,296,382 |
| 2004 | 66,700 | 13,500 | 48,780 | 223,000 | 2,601,970 | 13,300 | 2,315 | 139,000 | 6,450 | 4,740 | 114,000 | 61,900 | 115,000 | 203,000 | 6,880 | 3,620,535 |

|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|----|:---|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
|  | **BSAI TAC** |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| Year | Atka | Oflats | OthSpp | Pcod | Pollock | POP | RockFish | RockSole | Sable | Gturb | YfinSole | FheadSole | Arrowtooth | AKPlaice | NrthrnRF | Grand Total |
| 1977 |   | 100,000 | 103,600 | 58,000 | 950,000 | 21,500 |   |   | 7,400 |   | 106,000 |   |   |   |   | 1,346,500 |
| 1978 | 24,800 | 159,000 | 111,400 | 70,500 | 950,000 | 21,500 |  |  | 4,500 |  | 126,000 |  |  |  |  | 1,467,700 |
| 1979 | 24,800 | 139,000 | 103,600 | 58,000 | 950,000 | 21,500 |  |  | 4,500 |  | 106,000 |  |  |  |  | 1,407,400 |
| 1980 | 24,800 | 61,000 | 84,249 | 70,700 | 1,000,000 | 10,750 | 7,727 |  | 5,000 |  | 117,000 |  |  |  |  | 1,381,226 |
| 1981 | 24,800 | 61,000 | 84,249 | 78,700 | 1,000,000 | 10,750 | 7,727 |  | 5,000 |  | 117,000 |  |  |  |  | 1,389,226 |
| 1982 | 24,800 | 61,000 | 84,249 | 78,700 | 1,000,000 | 10,750 | 7,727 |  | 5,000 |  | 117,000 |  |  |  |  | 1,389,226 |
| 1983 | 24,800 | 61,000 | 87,314 | 120,000 | 1,000,000 | 10,750 | 7,727 |  | 5,000 |  | 117,000 |  |  |  |  | 1,433,591 |
| 1984 | 35,000 | 111,490 | 48,900 | 210,000 | 1,200,000 | 6,360 | 7,050 |  | 5,340 |  | 230,000 |  |  |  |  | 1,854,140 |
| 1985 | 37,700 | 111,400 | 47,980 | 220,000 | 1,200,000 | 4,800 | 6,620 |  | 4,500 |  | 229,900 |  |  |  |  | 1,862,900 |
| 1986 | 30,800 | 124,200 | 32,800 | 229,000 | 1,200,000 | 7,625 | 6,625 |  | 6,450 | 33,000 | 209,500 |  | 20,000 |  |  | 1,900,000 |
| 1987 | 30,800 | 148,300 | 15,500 | 280,000 | 1,200,000 | 11,025 | 1,880 |  | 7,700 | 20,000 | 187,000 |  | 9,795 |  |  | 1,912,000 |
| 1988 | 21,000 | 131,369 | 11,000 | 200,000 | 1,300,000 | 11,000 | 1,500 |  | 8,400 | 11,200 | 254,000 |  | 5,531 |  |  | 1,955,000 |
| 1989 | 20,285 | 75,183 | 14,264 | 230,681 | 1,340,000 | 11,000 | 1,500 | 90,763 | 6,200 | 6,800 | 182,675 |  | 6,000 |  |  | 1,985,351 |
| 1990 | 21,000 | 60,150 | 5,500 | 227,000 | 1,280,000 | 12,900 | 1,600 | 60,000 | 7,200 | 7,000 | 207,650 |  | 10,000 |  |  | 1,900,000 |
| 1991 | 24,000 | 64,675 | 16,000 | 229,000 | 1,300,000 | 15,345 | 1,325 | 90,000 | 6,300 | 7,000 | 135,000 |  | 20,000 |  |  | 1,908,645 |
| 1992 | 43,000 | 79,000 | 22,000 | 182,000 | 1,300,000 | 16,640 | 8,215 | 40,000 | 4,400 | 7,000 | 235,000 |  | 10,000 |  |  | 1,947,255 |
| 1993 | 64,000 | 79,000 | 28,600 | 164,500 | 1,300,000 | 18,430 | 7,390 | 75,000 | 4,100 | 7,000 | 220,000 |  | 10,000 |  |  | 1,978,020 |
| 1994 | 68,000 | 56,000 | 29,500 | 191,000 | 1,330,000 | 14,210 | 8,025 | 75,000 | 3,340 | 7,000 | 150,325 |  | 10,000 |  |  | 1,942,400 |
| 1995 | 80,000 | 19,540 | 21,000 | 250,000 | 1,250,000 | 13,610 | 7,223 | 60,000 | 3,800 | 7,000 | 190,000 | 30,000 | 10,227 |  |  | 1,942,400 |
| 1996 | 106,157 | 35,000 | 20,125 | 270,000 | 1,190,000 | 21,514 | 1,304 | 70,000 | 2,300 | 7,000 | 200,000 | 30,000 | 9,000 |  |  | 1,962,400 |
| 1997 | 66,700 | 50,750 | 25,800 | 270,000 | 1,130,000 | 21,948 | 1,087 | 97,185 | 2,300 | 9,000 | 230,000 | 43,500 | 20,760 |  |  | 1,969,030 |
| 1998 | 64,300 | 89,434 | 25,800 | 210,000 | 1,110,000 | 18,962 | 1,054 | 100,000 | 2,680 | 15,000 | 220,000 | 100,000 | 16,000 |  |  | 1,973,230 |
| 1999 | 66,400 | 154,000 | 32,860 | 177,000 | 992,000 | 20,362 | 1,054 | 120,000 | 3,200 | 9,000 | 207,980 | 77,300 | 134,354 |  |  | 1,995,510 |
| 2000 | 70,800 | 83,813 | 31,360 | 193,000 | 1,139,000 | 21,129 | 1,054 | 137,760 | 3,900 | 9,300 | 123,262 | 52,652 | 131,000 |  |  | 1,998,030 |
| 2001 | 69,300 | 28,000 | 28,470 | 188,000 | 1,403,000 | 11,930 | 8,829 | 75,000 | 4,060 | 8,400 | 113,000 | 40,000 | 22,011 |  |  | 2,000,000 |
| 2002 | 49,000 | 3,000 | 32,795 | 200,000 | 1,486,100 | 14,800 | 2,065 | 54,000 | 4,480 | 8,000 | 86,000 | 25,000 | 16,000 | 12,000 | 6,760 | 2,000,000 |
| 2003 | 60,000 | 3,000 | 34,279 | 207,500 | 1,492,810 | 14,100 | 2,561 | 44,000 | 6,000 | 4,000 | 83,750 | 20,000 | 12,000 | 10,000 | 6,000 | 2,000,000 |
| 2004 | 63,000 | 3,000 | 28,480 | 215,500 | 1,493,050 | 12,580 | 1,815 | 41,000 | 6,000 | 3,500 | 86,075 | 19,000 | 12,000 | 10,000 | 5,000 | 2,000,000 |

###### Table . Historical specifications of ABC levels (top) and TAC levels (bottom panel) for BSAI groundfish species.

|  |  |  |  |  |  |  |  |
|:---|:---|----|----|----|----|----|----|
| **ABC** Yr 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 | **Stock (millions of tons)** Pollock 1.340 1.450 1.676 1.490 1.340 1.330 1.250 1.190 1.130 1.110 0.992 1.139 1.874 2.138 2.373 2.602 | Pcod 0.371 0.417 0.229 0.182 0.165 0.191 0.328 0.305 0.306 0.210 0.177 0.193 0.188 0.223 0.223 0.223 | Yfin 0.241 0.279 0.251 0.372 0.238 0.230 0.277 0.278 0.233 0.220 0.212 0.191 0.176 0.115 0.114 0.114 | RockSole 0.171 0.216 0.247 0.261 0.185 0.313 0.347 0.361 0.296 0.312 0.309 0.230 0.228 0.225 0.110 0.139 | Oflats 0.156 0.188 0.220 0.200 0.191 0.225 0.255 0.218 0.199 0.296 0.231 0.191 0.206 0.244 0.219 0.278 | Others 0.273 0.196 0.172 0.134 0.126 0.154 0.164 0.193 0.172 0.210 0.212 0.198 0.186 0.190 0.194 0.197 | Atka 0.021 0.024 0.024 0.043 0.117 0.123 0.125 0.116 0.067 0.064 0.073 0.071 0.069 0.049 0.063 0.067 |
| **TAC** Yr 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 | **Stock  (percentage of 2-million ton TAC)** Pollock 67.5% 67.4% 68.1% 66.8% 65.7% 68.5% 64.4% 60.6% 57.4% 56.3% 49.7% 57.0% 70.2% 74.3% 74.6% 74.7% | Pcod 11.6% 11.9% 12.0% 9.3% 8.3% 9.8% 12.9% 13.8% 13.7% 10.6% 8.9% 9.7% 9.4% 10.0% 10.4% 10.8% | Yfin 9.2% 10.9% 7.1% 12.1% 11.1% 7.7% 9.8% 10.2% 11.7% 11.1% 10.4% 6.2% 5.7% 4.3% 4.2% 4.3% | RockSole 4.6% 3.2% 4.7% 2.1% 3.8% 3.9% 3.1% 3.6% 4.9% 5.1% 6.0% 6.9% 3.8% 2.7% 2.2% 2.1% | Oflats 3.8% 3.2% 3.4% 4.1% 4.0% 2.9% 2.6% 3.3% 4.8% 9.6% 11.6% 6.8% 3.4% 2.0% 1.7% 1.6% | Others 2.3% 2.3% 3.5% 3.5% 3.8% 3.7% 3.2% 3.1% 4.1% 4.0% 10.1% 9.9% 4.2% 4.2% 3.9% 3.5% | Atka 1.0% 1.1% 1.3% 2.2% 3.2% 3.5% 4.1% 5.4% 3.4% 3.3% 3.3% 3.5% 3.5% 2.5% 3.0% 3.2% |

###### Table 4. Example of projected catch estimates for the main BSAI groundfish species. Note that “others” aren’t included (e.g., those stocks for which an age-structured assessment is unavailable).

[TABLE]

###### Table 5. Catch specifications used for June 2005 projections.

[TABLE]

## Appendix—projection model details

The following presents details on the steps of the projection
simulations. A glossary of notation is provided at the end of this
section for reference.

#### Recruitment options

##### a) Recruitment projections similar to selected period (current practice)

For this option, recruitment estimates for the selected years (e.g.,
1978-2003 or the largest available subset thereof) are obtained from
each of the respective 2004 stock assessments. For each stock, these
recruitments are used to find maximum likelihood estimates for the
inverse Gaussian distribution parameters. The distribution was
parameterized such that one of the parameters represented the
distribution mean. A recruitment time series was obtained for each
simulation by drawing randomly from this parametric distribution.

##### b) Recruitment projections based on estimates from stock-recruitment output (estimates from stock assessment model, otherwise unconditioned)

For this option, spawning biomass and recruitment estimates for the
selected years (e.g., 1978-2003 or the largest available subset thereof)
are obtained from each of the respective 2004 stock assessments. For
each stock, these recruitments are used to find maximum likelihood
estimates for stock-recruitment relationship parameters (e.g.,
steepness, *R­₀*, and *σ_(R)*). Here the random number seed results in
generation of standard normal deviates that are applied to the
(bias-corrected) stock-recruitment curve and follows a log-normal
distribution.

##### c) Recruitment projections based on conditioned estimates from stock-recruitment output (estimates from stock assessment model, but conditioned such that *F_(35%)* = *F_(msy)*)

For this option, spawning biomass and recruitment estimates for the
selected years (e.g., 1978-2003 or the largest available subset thereof)
are obtained from each of the respective 2004 stock assessments. For
each stock, these recruitments are used to find maximum likelihood
estimates for stock-recruitment relationship parameters (e.g.,
steepness, *R­₀*, and *σ_(R)*). Here the random number seed results in
generation of standard normal deviates that are applied to the
(bias-corrected) stock-recruitment curve and follows a log-normal
distribution.

##### d) Recruitment projections based on conditioned stock-recruitment relationship (estimates solely conditioned such that *B_(35%)* = *B_(msy)* and *F_(35%)* = *F_(msy)*)

For this option, the stock-recruitment relationship (given the shape of
the curve) is conditioned such that *B_(35%)* = *B_(msy)* and *F_(35%)*
= *F_(msy)*. The spawning biomass and recruitment estimates for the
selected years (e.g., 1978-2003 or the largest available subset thereof)
are obtained from each of the respective 2004 stock assessments and are
included, but given very little weight (they remain in the model to
obtain estimates of *σ_(R)*. For each stock, these recruitments are used
to find maximum likelihood estimates for stock-recruitment relationship
parameters (e.g., steepness, *R­₀*, and *σ_(R)*). Here the random number
seed results in generation of standard normal deviates that are applied
to the (bias-corrected) stock-recruitment curve and follows a log-normal
distribution.

##### Estimating actual fishing mortality rates for the initial year

Where needed (e.g., in projection years where catch is specified
externally), the fishing mortality rate that would set catch equal to
*C_(t)* is solved by the following implicit equation:

##### Project numbers-at age for all ages, years, and simulations

For each scenario, a specified number of projection simulations were
conducted. The projected numbers at age in each year were based on an
annual feedback of “actual” catch if required. The steps for these
projections were (for a given species) were as follows:

1\) Initialize the simulation index:

2\) Increment the simulation index:

3\) Initialize the time index:

4\) Compute numbers at age for initial year of simulation *u*:

5\) Set fishing mortality rate for year *t'* of simulation *u*:

6\) Increment time index:

7\) Compute numbers at age in year *t* of simulation *u*:

[^1] for *a=*1,

for 1*\< a \< n_(age)*,

for *a = n_(age,)*.

9\) Compute the ABC fishing mortality rate that establishes the TAC for
year *t* of simulation *u*. The appropriate fishing mortality rate was
determined by the projection year and the relative spawning biomass of
the stock as shown in the table below (*B_(ref)* corresponds to
*B_(40%)* in all cases unless otherwise specified). *F_(ref)*
corresponds to the fishing mortality specified as the *F_(ABC)* value.

|                           |                        |
|:--------------------------|:-----------------------|
| Relative spawning biomass | Fishing mortality rate |

where and is the total mortality rate between the beginning of the year
and the time of spawning. The value of was computed iteratively (since
it can be a function of fishing mortality). Note also that for some
alternatives (described below) these rules change for some species. For
a given Alternative *i,* the fishing mortality is treated as a function
of the *F_(ABC)* value:

as specified by the alternative.

10\) Compute the TAC value as the annually varying limit on catch. For a
given species and value of (for alternative *i*) the projection model
computes the TAC used in the constraint as

.

11\) Compute the actual catch, , if specified by a TAC model or initial
projection year.

12\) Solve for the fishing mortality rate that would set catch equal to
in year *t* of simulation *u* (as estimated from the multispecies
management constrained optimization problem described below and varies
by alternative) by solving the following implicit equation:

13\) Check to see if all years of simulation *u* have been completed,
then continue as necessary:

If *t\<n_(pro)+1*, return to (6)

If *t=n_(pro)+1*, end simulation *u*.

14\) Return to (2) until all simulations are complete.

##### Step 5: Store stock performance statistics from the above projections

A series of individual stock performance indicators (for species with
age-structure results specified) were computed separately for each
alternative and described as follows.

Total biomass in each year and simulation:

Spawning biomass and catch (as specified above) were stored for each
species, year and simulation. Approximate confidence bounds were
computed from the simulation output by simply ranking results from the
simulations and computing the percentile values corresponding to the
desired intervals (here taken as the 10^(th) and 90^(th) percentile).
Also computed was the implied spawning biomass per-recruit (SPR) rate
given the level of catch in a single year and simulation. For example,
the theoretical percentage of unfished spawning output expected from a
single recruit if fishing mortality were equal to the estimated fishing
mortality over the life of the species.

Average age for each stock in the final projection year across all
simulations was also computed as:

### Glossary of symbols used in description of the model

#### Dimensions

*a_(max)* Maximum age used in the model (plus group)

*a_(min)* Minimum age used in the model

*n_(age)* Number of ages in the model

*n_(gear)* Number of gear types for which separate selectivity schedules
are used (as in the assessments)

*n_(pro)* Number of years to project beyond the initial year in each
simulation

*n_(sims)* Number of simulations

Number of gears with allocation constraints

*n_(Fsh)* Number of fisheries

*n_(sp )*Number of species

*n_(area)* Number of management areas defined for each species

#### Indices

*a* Relative age index, 1*an_(age)*

*g* Fishery index, 1*gn_(Fsh)*

*k* S*ub-area*

*h* F*ishing gear type*

*t* Projection year index, 1*tn_(pro)*

*t'* Projection year index, 1*t'number of projection years where catch
is pre-specified*

*u* Simulation index, 1*un_(sims)*

*i* Alternative index

*j* Species index

#### Life History and Fishery Parameters

*d_(h)* Proportion of total instantaneous fishing mortality rate
distributed to gear *h*

*M_(a)* Natural mortality rate at age *a*

*m_(a)* Proportion of age *a* fish that are mature

*w_(a)* Weight-at-age in the population

*p* Proportion of females in the population

*s_(a,h)* Selectivity of gear type *h* for fish of age *a* (scaled so
that max(*s*)=1)

*w_(a,h)* Weight of age *a* fish as sampled by gear *h*

#### Other Parameters and Expressions Used in Projections

*SPR* Spawning biomass per recruit

*ABC* Acceptable biological catch

*TAC* Total allowable catch

*B_(ref)* A parameter of the control rules used to set the overfishing
rate and to constrain *F_(ABC)*

*B_(t,u)* Spawning biomass in projection year *t* of simulation *u*

*C_(t,u)* Catch in projection year *t* of simulation *u* for each
population

*F_(t,u)* Fishing mortality rate in projection year *t* of simulation
*u* for each population

*F_(lim)* A parameter of the control rule used to set the overfishing
rate

*F_(ref)* A parameter of the control rule used to constrain *F_(ABC)*

Fishing mortality rate in projection year *t* of simulation *u* for each
population

Total mortality rate between the beginning of the year and the spawning
period

*N_(a,t)* Numbers at age *a* in projection year *t*

*N_(a,t,u)* Numbers at age *a* in projection year *t* of simulation *u*

*n_(a)* Numbers at age *a* in initial year

*O_(t,u)* Rate of fishing mortality that constitutes overfishing in
projection year *t* of simulation *u*

*P* Probability of overfishing in at least one year of the projection
period

*R_(t,u)* Recruitment in projection year *t* of simulation *u*

*T_(t,u)* Total biomass (between ages *a_(min)* and *a_(max)*) in
projection year *t* of simulation *u*

TAC*_(t)* TAC actually specified for *t*

*A* Average age for each stock in the final projection year across all
simulations

#### Computation of SPR values

Using species specific demographic values, fishing mortality rates
(e.g., ) that would reduce the female spawning stock (per recruit) to
some fraction of the unfished level. The age-specific factors are:
selectivity, natural mortality, maturity, and weight or fecundity. For
example, to compute an algorithm to solve the following set of implicit
equations was used:

where corresponds to the spawning stock per recruit of population *p* in
an unfished equilibrium state. This information was used within the
management rule that determines the quota. For some species and
alternatives different F-spr rates were used.

## Appendix—code for fitting TAC given historical ABC levels

DATA_SECTION

int nsd

!! nsd=5;

init_int nnodes

init_number samplesize

init_number pen_spp

init_number pen_nodes

init_int nspp

init_int nyrs

int nobs

!! nobs = nspp\*nyrs\*nspp;

init_matrix data(1,nobs,1,5)

3darray obs_tac(1,nyrs,1,nspp,1,nspp)

3darray obs_abc(1,nyrs,1,nspp,1,nspp)

3darray rescaled_abc(1,nyrs,1,nspp,1,nspp)

vector maxabc(1,nspp)

number offset;

imatrix inode(1,nyrs,1,nspp)

LOCAL_CALCS

maxabc.initialize();

for (int i=1;i\<=nobs;i++)

{

// TAC_Spp ABC_Spp Yr ABC TAC

obs_abc(data(i,3),data(i,1),data(i,2)) = data(i,4);

obs_tac(data(i,3),data(i,2),data(i,1)) = data(i,5);

if (obs_abc(data(i,3),data(i,1),data(i,2)) \> maxabc(data(i,2)))

maxabc(data(i,2)) = obs_abc(data(i,3),data(i,1),data(i,2)) ;

}

int ijunk;

offset = 0.;

for (int i=1;i\<=nyrs;i++)

{

for (int j=1;j\<=nspp;j++)

{

for (int k=1;k\<=nspp;k++)

rescaled_abc(i,j,k) = obs_abc(i,j,k)/maxabc(k);

for (int k=1;k\<=nspp;k++)

{

ijunk = int(rescaled_abc(i,j,k)\*nnodes) ;

inode(i,k) = ijunk;

}

obs_tac(i,j) /= sum(obs_tac(i,j));

}

offset -= samplesize \* obs_tac(i,1) \* log(obs_tac(i,1));

}

END_CALCS

PARAMETER_SECTION

number tac_pen;

init_bounded_matrix theta(0,nnodes,1,nspp,-8,8,1)

matrix pred_tac(1,nyrs,1,nspp)

matrix abc_tac(1,nyrs,1,nspp)

sdreport_matrix sdTAC(1,nsd,1,nspp)

vector like(1,4);

objective_function_value obj_fun

PROCEDURE_SECTION

like.initialize();

pred_tac.initialize();

tac_pen=0.0;

for (int i=1;i\<=nyrs;i++) {

for (int j=1;j\<=nspp;j++) {

pred_tac(i,j) += rescaled_abc(i,1,j) \* mfexp(theta(inode(i,j),j)) ;

}

pred_tac(i) /= sum(pred_tac(i));

}

int ijunk;

for (int i=1;i\<=nsd;i++) {

for (int j=1;j\<=nspp;j++) {

ijunk = int( double(i)/nsd \* nnodes) ;

sdTAC(i,j) = double(i)/double(nsd) \* mfexp( theta(ijunk,j) ) ;

}

}

// Fit the TAC portions

for (int i=1;i\<=nyrs;i++)

like(1) -= samplesize \* obs_tac(i,1) \* log(pred_tac(i)) ;

like(1) -= offset;

// Penalize differences in theta over species...

for (int i=0;i\<=nnodes;i++)

like(2) += pen_spp \*
norm2(first_difference(first_difference(theta(i))));

// Penalize differences in theta over nodes...

if (nnodes \>1)

for (int j=1;j\<=nspp;j++)

like(3) +=
pen_nodes\*norm2(first_difference(first_difference(trans(theta)(j))));

// Penalize TAC less than ABC's...

dvariable xtmp;

for (int i=1;i\<=nyrs;i++)

{

for (int j=1;j\<=nspp;j++)

{

abc_tac(i,j) = obs_abc(i,1,j)-(2.0\*pred_tac(i,j));

xtmp = posfun( (abc_tac(i,j) ), 0.2 , tac_pen );

}

}

like(4) = 20.\*tac_pen;

obj_fun = sum(like);

REPORT_SECTION

report\<\<"Obs_TAC " \<\<endl;

for (int i=1;i\<=nyrs;i++)

report\<\<2.\*obs_tac(i,1)\<\< " Pred: "\<\<i\<\<"
"\<\<2.\*pred_tac(i)\<\<endl;

report\<\<"Pred_TAC " \<\<endl;

for (int i=1;i\<=nyrs;i++)

report\<\<2.\*pred_tac(i)\<\<endl;

report\<\<"Obs_ABC " \<\<endl;

for (int i=1;i\<=nyrs;i++)

report\<\<rescaled_abc(i,1)\<\<endl;

report\<\<"Obs_ABC/Pred_TAC " \<\<endl;

for (int i=1;i\<=nyrs;i++)

report\<\<abc_tac(i)\<\<endl;

report\<\<"#\_Nodes,\_samsize,\_penaltyspp,\_pennode,\_Likelihoods"\<\<endl;

report\<\<nnodes\<\<" "\<\<samplesize\<\<" "\<\<pen_spp\<\<"
"\<\<pen_nodes\<\<" "\<\<like\<\<endl;

report\<\<"TAC_by_nodes"\<\<endl;

for (int i=1;i\<=nsd;i++) report\<\<sdTAC(i)\<\<endl;

ofstream ofs("tacpar.dat");

ofs \<\< nspp \<\<endl;

ofs \<\< nnodes \<\<endl;

ofs \<\< maxabc \<\<endl;

ofs \<\< theta \<\<endl;

ofs.close();

TOP_OF_MAIN_SECTION

gradient_structure::set_MAX_NVAR_OFFSET(1600);

gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);

gradient_structure::set_NUM_DEPENDENT_VARIABLES(800);

gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);

[^1]: Note that new options to follow conditioned or estimated
    stock-recruitment relationships are added as options. These include
    Ricker or Beverton-Holt forms with log-normal variability and are
    detailed as options a)-d) above.
