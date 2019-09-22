FROM resin/raspberrypi3-debian:stretch

# Enable systemd
ENV INITSYSTEM on

# install latest R
RUN echo "deb http://cran.rstudio.com/bin/linux/debian stretch-cran35/" >> /etc/apt/sources.list; \
    apt install dirmngr; \
    apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'; \
    apt-get update && apt-get upgrade && apt-get install -y r-base r-base-dev


# install required programs
RUN apt-get install -y wget python gcc cpp git

ENV R_REPO="https://ftp.fau.de/cran/"

ARG base="devtools \
    shiny \
    shinydashboard \
    tidyverse"
    
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
    R -q -e "devtools::install('.')"

CMD ["Rscript /home/shiny/${REPO_NAME}/docker/raspberry/app.R"]
