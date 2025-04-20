       identification division.
       program-id. gnucobol-microhttpd.

       environment division.
       configuration section.
       data division.

       local-storage section.
       01 MHD_HTTP_OK               constant   as 200.
       01 MHD_USE_SELECT_INTERNALLY constant   as 8.
       01 MHD_RESPMEM_PERSISTENT    constant   as 0.
       01 MHD_OPTION_END            constant   as 0.

       01 star-daemon               usage pointer.
       01 connection-handler-entry  usage program-pointer.

       01 server-command            pic x(80).

      *> ***************************************************************
       procedure division.
       set connection-handler-entry to
         entry "gnucobol-connection-handler"
       call "MHD_start_daemon" using
           by value MHD_USE_SELECT_INTERNALLY
           by value 8888
           by value 0
           by value 0
           by value connection-handler-entry
           by value 0
           by value MHD_OPTION_END
           returning star-daemon
       end-call

       *> Wait for incoming connections
       perform forever
           call "sleep" using by value 1
       end-perform

       goback.
       end program gnucobol-microhttpd.
      *> ***************************************************************

      *> ***************************************************************
       identification division.
       program-id. gnucobol-connection-handler.

       data division.
       local-storage section.
       01 MHD_HTTP_OK               constant   as 200.
       01 MHD_RESPMEM_PERSISTENT    constant   as 0.
       01 memory-struct.
           05 buffer pic x(10000).
           05 sizet-size pic S9(18) comp-5.
       01 DATA-URL                  pic x(1000) VALUE
           "https://data.atmo-france.org/geoserver/ind_pol/ows?" &
           "&REQUEST=GetFeatureInfo&SERVICE=WMS&SRS=EPSG%3A3857" &
           "&STYLES=&VERSION=1.3&FILTER=%3CPropertyIsEqualTo" &
           "%20matchCase%3D%22true%22%3E%3CPropertyName%3Edate_ech%3C" &
           "%2FPropertyName%3E%3CLiteral%3E2025-04-20%3C%2FLiteral%3E" &
           "%3C%2FPropertyIsEqualTo%3E&SORTBY=date_dif%20D&BBOX=" &
           "517516.9000047859%2C5732547.810303366%2C558945.7693353514" &
           "%2C5752459.656171654&HEIGHT=521&WIDTH=1084&LAYERS=ind_pol" &
           "%3Aind_national_pol&QUERY_LAYERS=ind_pol%3A" &
           "ind_national_pol&INFO_FORMAT=application%2Fjson" &
           "&X=535&Y=284".
       01 curl-callback             usage program-pointer.
       01 webpage              pic x(132) value
          "<html><body>" &
          "Hello, world<br/>" &
          "from <b>GnuCOBOL</b> and <i>libmicrohttpd</i>" &
          "</body></html>".
       01 star-response                        usage pointer.
       01 mhd-result                           usage binary-long.

       linkage section.
       01 star-cls                             usage pointer.
       01 star-connection                      usage pointer.
       01 star-url                             usage pointer.
       01 star-method                          usage pointer.
       01 star-version                         usage pointer.
       01 star-upload-data                     usage pointer.
       01 star-upload-data-size                usage pointer.
       01 star-star-con-cls                    usage pointer.


       procedure division with C linkage using
           by value star-cls
           by value star-connection
           by value star-url
           by value star-method
           by value star-version
           by value star-upload-data
           by value star-upload-data-size
           by reference star-star-con-cls
       .

       call "http-client-get" using
           by content DATA-URL
           by reference buffer

       call "pollen-parser" using
           by reference buffer

       call "MHD_create_response_from_buffer" using
           by value length of webpage
           by reference webpage
           by value MHD_RESPMEM_PERSISTENT
           returning star-response
       end-call
       call "MHD_queue_response" using
           by value star-connection
           by value MHD_HTTP_OK
           by value star-response
           returning mhd-result
       end-call
       call "MHD_destroy_response" using
           by value star-response
       end-call

       move mhd-result to return-code

       goback.
       end program gnucobol-connection-handler.
