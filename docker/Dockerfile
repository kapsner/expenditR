# https://hub.docker.com/r/rocker/shiny/
FROM rocker/shiny-verse

# install necessary system dependencies for r packages (e.g. devtools, RPostgres)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    apt-utils \
    libcurl4-openssl-dev \
    libssl-dev \
    libpq-dev \
    pandoc \
    pandoc-citeproc \
    gpg-agent \
    git

ARG p_base="devtools \
    tinytex"

RUN for package in $p_base; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"https://ftp.fau.de/cran/\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
    done

# install tinytex
USER shiny
RUN R -q -e 'tinytex::install_tinytex()'
USER root

# install required LaTeX-Packages
RUN ./home/shiny/.TinyTeX/bin/x86_64-linux/tlmgr update --self
RUN ./home/shiny/.TinyTeX/bin/x86_64-linux/tlmgr install \
    multirow \
    xcolor \
    colortbl \
    wrapfig \
    float \
    tabu \
    varwidth \
    threeparttable \
    threeparttablex \
    environ \
    trimspaces \
    ulem \
    makecell \
    babel \
    babel-german \
    hyphen-german \
    lm \
    hyperref \
    url \
    graphics-def \
    titling \
    caption \
    amsmath \
    amsfonts \
    tools \
    oberdiek \
    graphics \
    latex-graphics-dev \
    geometry \
    ec \
    ifxetex \
    ifluatex

# repository name
ARG REPO_NAME=expenditR

# add cloned repo
COPY ./addfolder /home/shiny/

# install our R-Package
RUN cd /home/shiny/${REPO_NAME}/ && \
    R -q -e "devtools::install('.')"

# add shiny app
RUN cd /home/shiny/${REPO_NAME}/docker && \
    cp app.R /srv/shiny-server/ && \
    # add custom server conf (running shiny as user 'shiny' is more secure than running as 'root')
    cp shiny-server.conf /etc/shiny-server/ && \
    # add log-script
    cp show-log.sh /


# fix permissions of directories, where R/ the app need rights to write
RUN chown -R shiny:shiny /srv/shiny-server/
RUN chmod +x show-log.sh

CMD ["/usr/bin/shiny-server.sh"]
