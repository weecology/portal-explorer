# portal-explorer

**A Shiny app for exploratory data analysis of the Portal Project**

Live app: [https://portalexplorer.weecology.org](https://portalexplorer.weecology.org)

## Run Locally

1. Open a shell in the repository root.
2. Install dependencies:

```bash
Rscript install_packages.R
```

3. Start the app from an R session:

```r
shiny::runApp("app")
```

## Important Command Note

- `Rscript ...` is a **shell** command (run in Terminal, not at the `>` R prompt).
- If you are already inside R, use:

```r
source("install_packages.R")
```

## Developer Docs

- The repo is on Serenity under the directory `/pgsql/retrieverdash/portal-explorer`.
- The log files for cron updates are at `/pgsql/retrieverdash/portal_explorer_cron_setup_dir.log`.
- The logs for the shiny server can be found in `/var/log/shiny-server/portalexplorer`.
- To manually initialize/update the portalcasting directory (`~/simple`) run in R:

```r
options(rgl.useNULL = TRUE)
library(portalcasting)
main <- path.expand("~/simple")
create_dir(main = main)
fill_dir(main = main)
```

For more details, please check out the config file `/etc/shiny-server/shiny-server.conf`
