
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btbaseline

Scripts and workflows for Bull Trout Baseline model development. The
goal is to eventually have this in a *data pipeline* but for now it’s a
start.

<figure>
<img src='https://hagadone.media.clients.ellingtoncms.com/img/photos/2023/05/11/MM7783_090918_23706_t1170.jpg?5cc718665ab672dba93d511ab4c682bb370e5f86' alt="Photo of a few Bull Trout underwater.">
<figcaption>
Bull trout. (Photo courtesy of the U.S. Geological Survey)
</figcaption>
</figure>

## Folder Structure

**R**

This folder is the slowly growing process/development of scripting using
the R programming language. Overtime this will be cleaned up and tested
(unit testing).

- `clean.R` - This script *cleans* and preps the data coming in (2017
  baseline, PIBO, GRAIP-Lite, etc).

- `exploring-eda.R` - This is meant to be a scratch pad for development
  (graphing, mapping, etc) and exploring data.

- `feature-selection.R` - Using techniques for feature selection related
  to `ml-prediction.R`.

- `local_pops.R` - Future file where we’ll explore local populations.

- `utils.R` - Utility functions to run in other scripts.

**data**

This is where the most up-to-date data (csv, spatial, models, etc) will
reside.

- `bt_baseline.gpkg` - This is a geopackage (think ESRI gdb but free and
  open-source!) that will house other spatial data, e.g. layers =
  `bt_baseline_with_graip_lite`, `pibo_all_together`,
  `WCATT_HUCs_GL_Data_USC_Units_R01`.

``` r
library(sf)
st_layers('data/bt_baseline.gpkg')
#> Driver: GPKG 
#> Available layers:
#>                         layer_name geometry_type features fields
#> 1                pibo_all_together         Point     2976     93
#> 2 WCATT_HUCs_GL_Data_USC_Units_R01 Multi Polygon     2361     66
#> 3      bt_baseline_with_graip_lite Multi Polygon      657    111
#>                                crs_name
#> 1                                WGS 84
#> 2 North_America_Albers_Equal_Area_Conic
#> 3 North_America_Albers_Equal_Area_Conic
```

- `bt_baseline_2017.csv` - This is collated from all the 2017 post-fire
  baseline model updates. This has the HUC12’s that were used to join
  with the `WCATT_HUCs_GL_Data_USC_Units_R01` HUC12’s.

- `bt_baseline_with_graip_lite.csv` - This combines the 2017 post-fire
  model run and joins with the `WCATT_HUCs_GL_Data_USC_Units_R01` data.
  Mostly to deal with roads and sediment potential.

- `pibo_data.xlsx` - This is the PIBO spreadsheet downloaded from
  [here](https://www.fs.usda.gov/detail/r4/landmanagement/resourcemanagement/?cid=FSEPRD1089819).
  From the site,

“*PIBO Monitoring Program Tabular Data - this link leads to an Excel
workbook with multiple tabs that contain data for stream habitat,
aquatic macroinvertebrates, stream temperature, riparian vegetation,
riparian weeds, and implementation monitoring. In addition, a metadata
worksheet is provided for each dataset and defines the column headings
and units of measure. To find data for a particular forest or BLM field
office, use the drop-down toggle in the first row of the ‘Region’,
‘Forest’, or ‘District’ columns to filter out unneeded information.
State BLM units are identified under the ‘Region’ column, while BLM
field offices are identified under the ‘Forest’ column.*”

- `pibo_all_together.csv` - This combines the `pibo_data.xlsx` stream
  habitat, aquatic macroinvertebrates, stream temperature sheets as well
  as fixed effects from [USGS Continuous Parameter Grids
  (CPGs)](https://www.sciencebase.gov/catalog/item/5a789b70e4b00f54eb1e836f).

- `WCATT_Attribute_Descriptions_GL_Data` - This is the metadata
  associated with the [R1 GRAIP-Lite model
  run](https://www.fs.usda.gov/GRAIP/GRAIP_Lite/model-runs-wcc-assessments.shtml).
  Use this to cross reference variables in
  `bt_baseline_with_graip_lite.csv` and
  `bt_baseline.gpkg/bt_baseline_with_graip_lite`.

## inst

This is meant for arbitrary additional files that you want include
(graphs, maps, docs, etc).

- `exploration` - This folder is meant to be sort of a scratch pad for
  development or a place to work on files that don’t quite fit into the
  other folder structure.

# Disclaimer

This information is preliminary or provisional and is subject to
revision. It is being provided to meet the need for timely best science.
The information has not received final approval by the U.S. Department
of Agriculture (USDA) and is provided on the condition that neither the
USDA nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the information.

Although this software program has been used by the USDA, no warranty,
expressed or implied, is made by the USDA or the U.S. Government as to
the accuracy and functioning of the program and related program material
nor shall the fact of distribution constitute any such warranty, and no
responsibility is assumed by the USGS in connection therewith. This
software is provided “AS IS.”

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
