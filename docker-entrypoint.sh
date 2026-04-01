#!/bin/sh
set -eu

# Ensure runtime directories exist and are writable by the Shiny app worker.
mkdir -p /srv/shiny-server/adam/auth
mkdir -p /var/log/shiny-server
mkdir -p /var/lib/shiny-server/bookmarks

chown -R shiny:shiny /srv/shiny-server/adam/auth
chown -R shiny:shiny /var/log/shiny-server
chown -R shiny:shiny /var/lib/shiny-server

exec /usr/bin/shiny-server
