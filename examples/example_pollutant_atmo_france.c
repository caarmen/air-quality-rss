#include <stdio.h>
#include <string.h>
#include <time.h>

// Fortran subroutine declaration
void get_atmo_france_pollutant_data(
    const char date_str[10],
    const char code_zone[6],
    int * api,
    int* pollutant_count,
    char pollutant_names[][5],
    int* pollutant_indices
);

enum {
    API_ADMIN   = 0,
    API_TABULAR = 1
};

int main() {
    time_t t = time(NULL);
    struct tm *tm_info = localtime(&t);
    char date_str[11];
    strftime(date_str, sizeof(date_str), "%Y-%m-%d", tm_info);

    int api;
    int pollutant_count = 0;

    char pollutant_names[10][5];     // max_pollutants × 4-char strings
    int pollutant_indices[10];       // max_pollutants integers

    printf("Fetching data using tabular api:\n");
    api = API_TABULAR;
    get_atmo_france_pollutant_data(
        date_str,
        "75056",
        &api,
        &pollutant_count,
        pollutant_names,
        pollutant_indices
    );

    printf("Received %d pollutants:\n", pollutant_count);
    for (int i = 0; i < pollutant_count; ++i) {
        printf("%.*s: %d\n", 4,
            pollutant_names[i],
            pollutant_indices[i]
        );
    }

    printf("Fetching data using admin api:\n");
    api = API_ADMIN;
    get_atmo_france_pollutant_data(
        date_str,
        "75056",
        &api,
        &pollutant_count,
        pollutant_names,
        pollutant_indices
    );

    printf("Received %d pollutants:\n", pollutant_count);
    for (int i = 0; i < pollutant_count; ++i) {
        printf("%.*s: %d\n", 4,
            pollutant_names[i],
            pollutant_indices[i]
        );
    }

    return 0;
}

