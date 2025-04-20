       identification division.
       program-id. pollen-data-source.

       data division.
       local-storage section.
       01 DATA-URL                  pic x(1000) VALUE SPACES.
       linkage section.
       01 response pic x(10000).
       procedure division using
           by reference response
       .

       call "source-url" using
           by reference DATA-URL
       display "fetching data from " DATA-URL

       call "http-client-get" using
           by content DATA-URL
           by reference response

       goback.
       end program pollen-data-source.

       identification division.
       program-id. source-url.
       data division.
       local-storage section.
       01  CURRENT-DATE-AND-TIME.
           05 CDT-Year                PIC 9(4).
           05 CDT-Month               PIC 9(2). *> 01-12
           05 CDT-Day                 PIC 9(2). *> 01-31
       01 date-and-time-str pic x(10).
       01 DATA-URL                  pic x(1000) VALUE
           "https://data.atmo-france.org/geoserver/ind_pol/ows?" &
           "&REQUEST=GetFeatureInfo&SERVICE=WMS&SRS=EPSG%3A3857" &
           "&STYLES=&VERSION=1.3&FILTER=%3CPropertyIsEqualTo" &
           "%20matchCase%3D%22true%22%3E%3CPropertyName%3Edate_ech%3C" &
           "%2FPropertyName%3E%3CLiteral%3EYYYY-MM-DD%3C%2FLiteral%3E" &
           "%3C%2FPropertyIsEqualTo%3E&SORTBY=date_dif%20D&BBOX=" &
           "517516.9000047859%2C5732547.810303366%2C558945.7693353514" &
           "%2C5752459.656171654&HEIGHT=521&WIDTH=1084&LAYERS=ind_pol" &
           "%3Aind_national_pol&QUERY_LAYERS=ind_pol%3A" &
           "ind_national_pol&INFO_FORMAT=application%2Fjson" &
           "&X=535&Y=284".

       linkage section.
       01 DATA-URL-OUT pic x(1000).
       procedure division using
           by reference DATA-URL-OUT.
       move function current-date
           to CURRENT-DATE-AND-TIME
       string
           CDT-Year "-" CDT-Month "-" CDT-Day
           into date-and-time-str
       end-string
       move DATA-URL to DATA-URL-OUT.
       inspect DATA-URL-OUT
           replacing all "YYYY-MM-DD" by date-and-time-str
       goback.
       end program source-url.