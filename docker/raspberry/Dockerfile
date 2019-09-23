FROM arm32v7/debian:buster

# Enable systemd
ENV INITSYSTEM on
ENV DEBIAN_FRONTEND=noninteractive

# install latest R
RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils\
    build-essential \
    dirmngr
# https://cran.r-project.org/bin/linux/ubuntu/
RUN apt-get install -y gpg-agent sudo software-properties-common wget python gcc cpp git
#RUN apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF' && \
#    echo "deb http://cran.rstudio.com/bin/linux/debian stretch-cran35/" >> /etc/apt/sources.list
RUN apt-get update && apt-get upgrade -y

# install further r packages
RUN apt-get install -y r-base-dev
#RUN apt-get install -y r-core-devtools r-core-shinydashboard r-core-data.table r-core-dt r-core-tidyverse
ENV R_REPO="https://ftp.fau.de/cran/"

ARG base="data.table \
    shiny \
    shinydashboard \
    shinyjs \
    DT"

RUN for package in $base; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"${R_REPO}\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
    done

# repository name
ARG REPO_NAME=expenditR

# add cloned repo
RUN mkdir /home/shiny
COPY ./addfolder /home/shiny/

# install our R-Package
RUN cd /home/shiny/${REPO_NAME}/ && \
    git checkout latest && \
#    R -q -e "devtools::install('.')"
    R CMD INSTALL .

#RUN chown -R shiny:shiny /home/shiny/${REPO_NAME}

CMD Rscript /home/shiny/${REPO_NAME}/docker/raspberry/app.R
