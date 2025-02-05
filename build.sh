#!/bin/bash

# description: - build and install R package
#              - render documentation website

cd "$(dirname "$(realpath "$0")")";

# Exit on error
set -e

# Document and install
R -e 'devtools::document()'

# install local
R -e 'devtools::install()'

# Build package documentation
R -e 'pkgdown::build_site(".", preview=FALSE)'


