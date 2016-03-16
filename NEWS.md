# geobird 0.3.2

* Substantial improvements to `plot_ebird_phen` to better handle plots including multiple sites (polygons) and allow the user to specify only a subset of site to be plotted
* Added conditions to avoid problems with polygons with adequate abundance data for all species and polygons without adequate data to generate a checklist

# geobird 0.3.1

* Add utility function of allow output of checklist to *.csv

# geobird 0.3

* Exclude hybrids and optional exclusion of "forms"
* Allow direct specification of file for parsing/storing in single call
* Rely more of data.table for improved performance (speed)
* General improvement of code clarity

# geobird 0.2

* Use data.table::fread to much faster reading of large text files
* Allow exclusion of polygons in addition to selection
* Improved output *.xls formatting
* Illustrate new features in README
