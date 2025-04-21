# Pollen RSS feed

This project provides a webserver that serves pollen data from
Atmo France in a format compatible with RSS feeds.

## Running
### Docker
TODO: instructions to fetch the docker image from github.

### Local
Build the project with `./build.sh`.

Set the following environment variables:
* `POLLEN_FEED_URL`: the URL of the rss feed. This is used in the `<link>` and `<id>` tags of the RSS feed.
* `POLLEN_LATITUDE`: the latitude of the location to fetch pollen data for.
* `POLLEN_LONGITUDE`: the longitude of the location to fetch pollen data for.

Run the project with `build/bin/pollen-rss`.

### Http requests
The server will be available at `http://localhost:8888/pollen-rss`.

There is only one route, `/pollen-rss`, which returns the RSS feed.

# License
This project is licensed under the MIT License. See the [LICENSE.txt](LICENSE.txt) file for details.

The project has the following dependencies:
* [GnuCobol](https://sourceforge.net/p/gnucobol/code/HEAD/tree/trunk/) - GPLv3 for the compiler, LGPLv3 for the runtime.
* [libcurl](https://github.com/curl/curl) - MIT-like license.
* [cJSON](https://github.com/DaveGamble/cJSON) - MIT license.
* [microhttpd](https://www.gnu.org/software/libmicrohttpd/) - LGPLv2.1.
* Pollen source: [Atmo France](https://www.atmo-france.org/article/atmo-data-un-acces-unique-aux-donnees-produites-par-les-aasqa) and AASQA (Associations 
agréées de surveillance de la qualité de l’air).

# Under the hood
This project is written in GnuCobol. The only reason for this
choice is that I was curious about Cobol and wanted to 
discover this language with a simple project. This would not
be the most practical choice for a production project. :)
I'm pretty sure that the code here isn't following any best
practices for Cobol!
