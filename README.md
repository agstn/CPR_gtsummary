# CPR_gtsummary
## CDISC Pilot replication w/ gtsummary
### Agustin Calatroni & Daniel D. Sjoberg

[gtsummary](https://www.danieldsjoberg.com/gtsummary/) is an exceptional R package designed for the creation of concise and highly customizable tables. It specifically focuses on summary tables (also known as Table 1) and regression tables from a wide range of models, including linear models, Cox models, and mixed models, to name just a few. For those interested in survey designs, the package also extends well to this area. In my view, the powerhouse of the package is its ability to [merge](https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html) and [stack](https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html) tables created with gtsummary, which opens up a broad spectrum of possibilities. These functionalities, combined with the support for various models as highlighted in the blog post [Mastering the Many Models Approach: A Comprehensive Guide to the Tidyverse Many Models Approach and its Extensions](https://tim-tiefenbach.de/post/2023-dplyr-many-models/), open up significant opportunities for advanced table generation.
<br><br>
To assess its capabilities, I undertook a challenge to generate all tables for the CDISC SDTM/ADaM Pilot Project 1 Clinical Study Report using gtsummary. This document demonstrates the package's ability to produce all required tables efficiently. Although using this project as a challenge might not be universally recommended, its successful completion undeniably proves gtsummary's effectiveness in table generation. The code and data from this report are available in a repository for anyone interested in a fully reproducible approach.


