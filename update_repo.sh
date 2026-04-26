#!/bin/bash

LOG_FILE="/pgsql/retrieverdash/portal_explorer_cron_setup_dir.log"
cd /pgsql/retrieverdash/portal-explorer

echo "$(date) - Portal explorer repo updating started" > "$LOG_FILE"

changed=0
git remote update && git status -uno | grep -q 'Your branch is behind' && changed=1
if [ $changed = 1 ]; then
    git reset --hard origin/main && git pull
    R -e "install.packages('devtools', repos='https://cran.rstudio.com/')"
    R -e "devtools::install_github('weecology/portalcasting', upgrade = TRUE)"
    echo "Updated $(date)" >> "$LOG_FILE"
else
    echo "Up-to-date $(date)" >> "$LOG_FILE"
fi

# Update setup directory every friday
friday_start=52335
friday_end=52359
Current_time=$(date +"%u%H%M")
if [ $friday_start -le $Current_time ] && [ $friday_end -gt $Current_time ]
    then
        R -e "options(rgl.useNULL = TRUE); library(portalcasting); main <- path.expand(\"~/simple\"); portalcasting::create_dir(main = main); portalcasting::fill_dir(main = main)" >> "$LOG_FILE"
        echo "Directory ~/simple is updated (create_dir + fill_dir) $(date)" >> "$LOG_FILE"
    fi

echo "$(date) - Portal explorer repo update completed" >> "$LOG_FILE"
