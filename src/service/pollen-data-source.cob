       identification division.
       program-id. pollen-data-source.

       data division.
       local-storage section.
       01 DATA-URL                  pic x(1000) VALUE SPACES.
       01 response.
           05 response-data pic x(10000).
           05 response-length-bytes pic 9(5) comp-5.

       linkage section.
       01 response-body pic x(10000).
       procedure division using
           by reference response-body
       .

       call "source-url" using
           by reference DATA-URL
       display "fetching data from " DATA-URL

       call "http-client-get" using
           by content DATA-URL
           by reference response

       move response-data(1:response-length-bytes)
           to response-body
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
           "%3C%2FPropertyIsEqualTo%3E&SORTBY=date_dif%20D" &
           "&LAYERS=ind_pol" &
           "%3Aind_national_pol&QUERY_LAYERS=ind_pol%3A" &
           "ind_national_pol&INFO_FORMAT=application%2Fjson" &
           "&X=535&Y=284".
       01 BBOX pic x(1000) value spaces.
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

       call "bounding-box-str" using
           by reference BBOX
       move DATA-URL to DATA-URL-OUT.
       inspect DATA-URL-OUT
           replacing all "YYYY-MM-DD" by date-and-time-str
       string function trim(data-url-out) "&" BBOX
           into data-url-out
       end-string
       goback.
       end program source-url.

       identification division.
       program-id. bounding-box-str.
       data division.
       local-storage section.
       01 latitude pic s9(3)v9(8).
       01 longitude pic s9(3)v9(8).
       01 x pic s9(7)v9(8).
       01 y pic s9(7)v9(8).
       01 bbox-left pic 9(7).9(8).
       01 bbox-right pic 9(7).9(8).
       01 bbox-top pic 9(7).9(8).
       01 bbox-bottom pic 9(7).9(8).

       linkage section.
       01 bounding-box pic x(1000).
       procedure division using
           by reference bounding-box.

       accept latitude from environment "POLLEN_LATITUDE"
       accept longitude from environment "POLLEN_LONGITUDE"
       call "lat-long-to-web-mercator" using
           by reference latitude
           by reference longitude
           by reference x
           by reference y

           compute bbox-left = x - 20000
           compute bbox-right = x + 20000
           compute bbox-top = y + 10000
           compute bbox-bottom = y - 10000
           string
               "BBOX="
               bbox-left "%2C" bbox-bottom "%2C"
               bbox-right "%2C" bbox-top
               "&HEIGHT=500&WIDTH=1000"
               into bounding-box
           end-string

       goback.

       end program bounding-box-str.
