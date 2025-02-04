#!/bin/bash

# description: - build and install R package
#              - render quarto website

cd "$(dirname "$(realpath "$0")")";

# Exit on error
set -e

# Document and load
R -e 'devtools::document()'
R -e 'devtools::install()'

quarto preview .
