
[![](https://media.licdn.com/dms/image/C4E0BAQHL_soHqC8vVA/company-logo_200_200/0?e=2159024400&v=beta&t=pa4sRz_MWKj7_FVzvq74OOHCq_3xX8uWFr0QsNfBVwQ)](https://electionlab.mit.edu/)
# Florida Voter Files

There are two voter files in this directory: 

* The first — `fl_aggregate.csv` — contains voter data from the 2016 and 2020 Florida presidential preference primaries. This table aggregates vote counts by election date, party, county, race, age group, and vote mode. More details on the columns below.
* The second — `aggregate_fl_2020_first_time_status.csv` — contains voter data from the 2020 Florida presidential preference primary, and contains all of the columns above, plus another column: first-time status. For the purposes of this table, first-time voters in the 2020 presidential primary are those who voted for the first time since at least 2006. 
## Column Breakdown

### fl_aggregate.csv
| Column | Values | 
| ------ | ------ |
| election_date | `3/15/2016` or `3/17/2020` |
| vote_mode | `A`,`E`,`Y`,`B`,`P` |
| party | `DEM` or `REP` |
| county | `ALA`,`BRO`,`HIL`,etc. |
| race | `1`,`2`,`3`,`4`,`5`,`6`,`7`,`9` |
| age_group | `18-29`,`30-44`,`45-59`,`60+` |
| vote_count | sum of votes |

### aggregate_fl_2020_first_time_status.csv
| Column | Values | 
| ------ | ------ |
| county | `ALA`,`BRO`,`HIL`,etc. |
| party | `DEM` or `REP` |
| race | `1`,`2`,`3`,`4`,`5`,`6`,`7`,`9` |
| first_time_status | `first_time` or `returning` |
| age_group | `18-29`,`30-44`,`45-59`,`60+` |
| vote_mode | `A`,`E`,`Y`,`B`,`P` |
| vote_count | sum of votes |

## Code Definitions Tables
| Vote Mode | Description |
| ------ | ------ |
| A | Voted by Mail |
| B | Vote-by-Mail Ballot Not Counted |
| E | Voted Early |
| N | Did Not Vote (not all counties use this code nor are required to report this data) |
| P | Provisional Ballot Not Counted |
| Y | Voted at Polls |

| Race Code | Description |
| ------ | ------ |
| 1 | American Indian or Alaskan Native |
| 2 | Asian or Pacific Islander |
| 3 | Black, Not Hispanic |
| 4 | Hispanic |
| 5 | White, Not Hispanic |
| 6 | Other |
| 7 | Multi-racial|
| 9 | Unknown |
