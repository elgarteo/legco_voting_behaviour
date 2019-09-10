# LegCo Members' Voting Behaviour Analysis
Network analysis on the voting behaviour of members of the Hong Kong Legislative Council

## What it does
A network analysis on the similarity of LegCo members' voting behaviour. The analysis examines the voting
record of the Fifth LegCo (2012-16) and the Sixth LegCo (2016-19) up until May 2019.

## What they are
* **preprocessing.R:** Scripts to fetch the voting record using the LegCo API, compute the similarity 
between different lawmakers and export as *vote_table5.rds* and *vote_table6.rds*
* **network.R:** Scripts to plot network graphs in English and Chinese as *5th_all_en.png*,
*5th_all_zh.png*, *5th_pro_est_en.png*, *5th_pro_est_zh.png*, *5th_non_pro_est_en.png*, 
*5th_non_pro_est_zh.png*, *6th_all_en.png*, *6th_all_zh.png*, *6th_pro_est_en.png*,
*6th_pro_est_zh.png*, *6th_non_pro_est_en.png* and *6th_non_pro_est_zh.png*
* **pro_est5.rds** & **pro_est6.rds**: Lists of pro-establishment lawmakers in the Fifth and Sixth LegCo

## How it works
After collecting all votes casted by every member in the Fifth and Sixth LegCo meetings, a correlation matrix
containing the similarity of the voting behaviour between every member is generated for each term, which can 
then be used to plot weighted network graphs with each node representing a lawmaker and the correlation
coefficient indicating the colour depth of each edge. This can provide a glimpse of the level of agreement
between lawmakers across different political parties and camps.

Members who quitted early or joined late as lawmakers or have casted less than 50% of valid votes (i.e. yes
or no only, not abstaining or absent) overall are excluded from the analysis. A number of motions submitted
for the sole purpose of filibustering are also removed, e.g. member's amendment bills on the Approporiation Bill 
in 2013-16.

## What you need
You need the R packages [elgarteo/legco](https://github.com/elgarteo/legco) and 
[elgarteo/legcoplus](https://github.com/elgarteo/legcoplus) to fetch data from the LegCo API.

All other packages are obtainable from CRAN.

## Reference
The method of constructing a network graph by correlation matrix is adapted from
[jmbh/bundestag](https://github.com/jmbh/bundestag).
