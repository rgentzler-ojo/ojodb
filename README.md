
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ojodb

ojodb is a package that assists [Open Justice
Oklahoma](https://openjustice.okpolicy.org/) analysts to access and
analyze court, jail, prison, and other data collected from various
sources.

The pkgdown website for the ojodb package can be found
[here](https://openjusticeok.github.io/ojodb/).

## Installation

You can install the ojodb package from GitHub with:

``` r
devtools::install_github("openjusticeok/ojodb")
```

After installation, update it with:

``` r
ojo_reinstall()
```

## The big picture

The goal of Open Justice Oklahoma is to collect and analyze
hard-to-access data in order to better understand our state’s justice
system. The backbone of our work is our database, which consists of
administrative data gathered mainly from courts, jails, and prisons
across the state. OJO processes go out to each of these data sources
periodically and pull new data into our database. For example, we have
OSCN scrapers set up to periodically visit small claims case pages like
[this
one](https://www.oscn.net/dockets/GetCaseInformation.aspx?db=tulsa&number=SC-2019-10)
every few days, gathering new data that appears in the course of a case.

We’re generally not interested in what happens in a single case, though.
What we’re really interested in is the aggregate effects. In the case
linked above, for instance, we see that the eviction case was filed on
January 2, 2019, and granted by default judgment on January 11. We can
use our database to figure out how many eviction cases are filed and how
many resulted in a default judgment.

This is information that isn’t compiled anywhere else, so our database
gives us the unique opportunity to understand what is really happening,
day-to-day and in close to real-time, in courtrooms and jails all across
the state.

## Exploring the OJO database

To list all tables in the OJO database, use `ojo_list_tables()`. Each
table name begins with a prefix indicating the source of the data.

``` r
library(ojo)

ojo_list_tables()
```

To see the variables and first 10 rows of a table, call `ojo_tbl()` on
it:

``` r
ojo_tbl("oscn_crim_disps")
```

## Pulling data into R

In many cases, you’ll only be interested in a particular county, time
period, and/or case type. You can now limit the data using the
`filter()` function, then use `ojo_collect()` to bring the pre-filtered
data into R.

``` r
d <- ojo_tbl("oscn_crim_disps") %>% 
  filter(court == "ROGERS", casetype == "CM", file_year == 2019) %>% 
  ojo_collect()
```

The first line points to the `oscn_crim_disps` table (basically `SELECT
\* FROM oscn_crim_disps`). The second line limits the query (`WHERE
court = 'ROGERS' AND casetype = 'CM' AND file_year = 2019`), so you’re
only getting the data you want, not querying the entire table.
`ojo_collect()` executes the query, creates a dataframe in R, and closes
the connection to the database (`disconnect_ojo()`). This should mostly
eliminate the need for calling `connect_ojo()` before querying and
`disconnect_ojo()` afterward.