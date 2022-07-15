### This folder contains outputs for the summer flounder MSE

individual plots will be contained in the subdirectory `plots`

html documents containing mixes of plots, tables, text, and dynamic tables/plots will appear in this directory.

[GF to add additional information here on interpreting, scales/codes to use, etc.]

### Performance Metrics 

Crosswalking labels in tibbles with the performance metrics

|                                                     |                                                              |
|-------------------------|------------------------------------------|
| **Object label**                                    | **Metrics**                                                  |
|                                                     | **Obj 1 - Angler Experience**                                |
| keep_one                                            | % of trips with a keeper                                     |
| kept_per_trip                                       | Average \# kept per trip                                     |
| cs_per_trip                                         | consumer surplus per trip                                    |
| trophy                                              | % of trips with a trophy (\>28")                             |
|                                                     |                                                              |
|                                                     | **Obj 2 - Angler Experience**                                |
| keep_1\_mp1                                         | % change in chance of a trip with a keeper                   |
| keep_range_mp1                                      | % difference across states in chance of a trip with a keeper |
| kept:rel_mp1                                        | % change in retention rate                                   |
| kept:rel_range_mp1                                  | % difference across states in retention rate                 |
|                                                     |                                                              |
|                                                     | **Obj 3 - Stock Status**                                     |
| P(not overfished)                                   | % chance stock overfished                                    |
| P(not overfishing)                                  | % chance of overfishing                                      |
| spawning_biomass                                    | SSB                                                          |
| rel_per_trip_mp1 OR (kept_per_trip)/(kept:released) | average \# fish released per trip                            |
| rec_removals                                        | recreational removals                                        |
| prop_female                                         | % of removals that are female                                |
|                                                     |                                                              |
|                                                     | **Obj 4 - Socioeconomics**                                   |
| ntrips                                              | \# of trips (millions)                                       |
| change_cs                                           | % change Consumer surplus (all trips)                        |
| expense                                             | % change Fishery investment                                  |
