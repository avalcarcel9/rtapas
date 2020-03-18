FROM pennsive/r-env:base

# install anything not already in pennsive/r-env:base
# rgl requires X11 ref https://stackoverflow.com/a/43956492/2624391
RUN apt-get update && apt-get install -y xorg libx11-dev libglu1-mesa-dev libfreetype6-dev
RUN r -e "install.packages(c('doParallel', 'foreach', 'gtools', 'ggExtra', 'neuroim'))"
RUN r -e "devtools::install_github('avalcarcel9/rtapas', dependencies = FALSE)"
WORKDIR /src
COPY . .
ENTRYPOINT []
CMD bash
