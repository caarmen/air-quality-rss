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

Run the project with `build/bin/pollen-rss`.

### Http requests
The server will be available at `http://localhost:8888/pollen-rss`.

There is only one route, `/pollen-rss`, which returns the RSS feed.
Query params:
* `latitude`: latitude of the location to fetch pollen info for.
* `longitude`: latitude of the location to fetch pollen info for.

Example: localhost:8888/pollen-rss?latitude=45.758&longitude=4.7

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
## Cobol
This project is written in GnuCobol. The only reason for this
choice is that I was curious about Cobol and wanted to 
discover this language with a simple project. This would not
be the most practical choice for a production project. :) Much of
the code is calling out to C libraries like microhttpd, libcurl, and cJSON.

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