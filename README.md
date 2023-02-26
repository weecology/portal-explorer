# portal-explorer

A Shiny app for exploratory data analysis of the Portal Project

To run this app locally install the `shiny` package and run:

```
shiny::runGitHub('portal-explorer', 'weecology', ref = 'main', subdir = 'app')
```

#### Developer Docs

The repo is on Serenity under the directory `/pgsql/retrieverdash/portal-explorer`.

The log files for cron updates are at `/pgsql/retrieverdash/portal_explorer_cron_setup_dir.log`.

To manually update the project, fetch the upstream main updates and run

```
R -e "library(portalcasting);
      fill_raw(main="~/simple");
      fill_dir(main="~/simple", downloads = zenodo_downloads("1215988"))"
```

The logs for the shiny server can be found in `/var/log/shiny-server/portalexplorer`

For more details, please check out the config file `/etc/shiny-server/shiny-server.conf`
