# Air quality RSS feed

This is a hobby project which provides a webserver that serves pollen and pollutant data from Atmo France and PREV'AIR in a format compatible with RSS feeds.

## Running
### Docker
* Fetch the image: `docker pull ghcr.io/caarmen/air-quality-rss:latest`
* Run it, setting `BASE_FEED_URL` to the url you want to appear in the `<link>` and `<id>` tags of the RSS feed:
```bash
 docker run \
   -e BASE_FEED_URL=http://localhost:8888 \
   -p 8888:8888 \
   ghcr.io/caarmen/air-quality-rss:latest
```

### Local
Build the project with `./build.sh`.

To build the examples, set the `BUILD_EXAMPLES` environment variable: `BUILD_EXAMPLES=1 ./build.sh`. The example binaries are created in `build/examples/bin/`

Set the following environment variables:
* `BASE_FEED_URL`: the base URL of the rss feed. This is used in the `<link>` and `<id>` tags of the RSS feed.

Run the project with `build/bin/air-quality-rss`.

### Http requests
The server will be available at `http://localhost:8888/`.

#### Pollen RSS feed
`/pollen-rss`: returns an RSS feed with pollen data.
Query params:
* `latitude`: latitude of the location to fetch pollen info for.
* `longitude`: latitude of the location to fetch pollen info for.

Example: localhost:8888/pollen-rss?latitude=45.758&longitude=4.7

#### Pollutant RSS feed
`/pollutant-rss`: returns an RSS feed with pollutant data.
Query params:
* `latitude`: latitude of the location to fetch pollutant info for.
* `longitude`: latitude of the location to fetch pollutant info for.

Example: localhost:8888/pollutant-rss?latitude=45.758&longitude=4.7

# License
This project is licensed under the MIT License. See the [LICENSE.txt](LICENSE.txt) file for details.

The project has the following dependencies:
* [GnuCobol](https://sourceforge.net/p/gnucobol/code/HEAD/tree/trunk/) - GPLv3 for the compiler, LGPLv3 for the runtime.
* [GNU Fortran](https://github.com/gcc-mirror/gcc/tree/master/libgfortran) - GPLv3 with the GCC Runtime Library Exception.
* [libcurl](https://github.com/curl/curl) - MIT-like license.
* [cJSON](https://github.com/DaveGamble/cJSON) - MIT license.
* [microhttpd](https://www.gnu.org/software/libmicrohttpd/) - LGPLv2.1.
* [http-client](https://github.com/fortran-lang/http-client.git) - MIT license.
* [json-fortran](https://github.com/jacobwilliams/json-fortran) - BSD 3-Clause License.
* [netcdf-fortran](https://github.com/Unidata/netcdf-fortran) - Apache License 2.0.
* [netcdf-c](https://github.com/Unidata/netcdf-c) - BSD 3-Clause License.
* Pollen source: [Atmo France](https://www.atmo-france.org/article/atmo-data-un-acces-unique-aux-donnees-produites-par-les-aasqa) and AASQA (Associations 
agréées de surveillance de la qualité de l’air).
* Pollutant source: [PREV'AIR](https://www.prevair.org/): Données issues de la plateforme nationale de prévision de la qualité de l'air www.prevair.org



# Under the hood
## Cobol / Fortran
This project is written in Cobol and Fortran. The only reason for this
choice is that I was curious about Cobol and wanted to 
discover this language with a simple project. This would not
be the most practical choice for a production project. :) Much of
the code is calling out to C libraries like microhttpd, libcurl, and cJSON.

As for Fortran, I needed a way to parse NetCDF files. I didn't find any simple way to do this in Cobol. I saw that
there were several libraries in various languages for
parsing NetCDF files, including Fortran. Fortran, having been invented in 1957, seemed to fit well the retro spirit
of this project :)


## AI
AI (ChatGPT and copilot) has been used for a few things in this project:
* Generating most of the `CMakeLists.txt` file.
* Formatting the code. The prompt used for this is in the description in PR #1.
  I initially wrote the code with inconsistent and unconventional indentation,
  capitalization, and spacing between statements. As I was not familiar with Cobol, and
  I didn't find any linter for Cobol, I decided to use AI to format the code.
* Auto-complete for the comment headings before each program.

Of course, AI didn't get everything right the first time, and I had to correct/improve
some things.

However, for the bulk of the code, I did write it, as I wanted to learn this retro
language :) As a result, I'm pretty sure that the code here isn't following any best practices!

## Related examples
These examples from Brian Tiffin helped me to get started:
* [GnuCobol and microhttpd](https://gnucobol.sourceforge.io/faq/index.html#gnu-libmicrohttpd)
* [Example with GnuCobol and libcurl](https://gnucobol.sourceforge.io/faq/index.html#function-id)

However, eventually the code diverged enough from these examples that I
think that an MIT license for the project should be ok. IANAL ;)
