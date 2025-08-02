#include <stdio.h>
#include <string.h>
#include <time.h>

// Fortran subroutine declaration
void get_atmo_france_pollutant_data(
    const char date_str[10],
    const char code_zone[6],
    int* pollutant_count,
    char pollutant_names[][5],
    int* pollutant_indices
);

int main() {
    time_t t = time(NULL);
    struct tm *tm_info = localtime(&t);
    char date_str[11];
    strftime(date_str, sizeof(date_str), "%Y-%m-%d", tm_info);

    int pollutant_count = 0;

    char pollutant_names[10][5];     // max_pollutants × 4-char strings
    int pollutant_indices[10];       // max_pollutants integers

    get_atmo_france_pollutant_data(
        date_str,
        "75056",
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

