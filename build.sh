#!/bin/bash

# description: - build and install R package
#              - render documentation website

cd "$(dirname "$(realpath "$0")")";

# Exit on error
set -e

# Update package datasets
R -e 'source("data-raw/data.R")'

# Document and install
R -e 'devtools::document()'

# Build package documentation
R -e 'pkgdown::build_site(".", preview=FALSE)'

# install local
R -e 'devtools::install()'


