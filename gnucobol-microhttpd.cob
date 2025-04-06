GNU    >>SOURCE FORMAT IS FIXED
Cobol *> ***************************************************************
      *> Author:    Brian Tiffin
      *> Date:      20140420
      *> Copyright (c) 2014, Brian Tiffin
      *> This free software is licensed under the GPL 2 without warranty
      *> Purpose:   GnuCOBOL minimal micro web server
      *> Tectonics: cobc -x gnucobol-microhttpd.cob -lmicrohttpd
      *> ***************************************************************
       identification division.
       program-id. gnucobol-microhttpd.

       environment division.
       configuration section.
       repository.
           function all intrinsic.
       data division.

       working-storage section.
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
           on exception
               display
                   "gnucobol-microhttpd: libmicrohttpd failure"
                   upon syserr
               end-display
       end-call
       perform until server-command = "quit"
           accept server-command end-accept
           if server-command = "help" then
               display
                   "gnucobol-microhttpd: help, info, quit"
               end-display
           end-if
           if server-command = "info" then
               display
                   "gnucobol-microhttpd: info? help, quit"
               end-display
           end-if
       end-perform

       call "MHD_stop_daemon" using
           by value star-daemon
           on exception
               display
                   "gnucobol-microhttpd: libmicrohttpd failure"
                   upon syserr
               end-display
       end-call

       goback.
       end program gnucobol-microhttpd.
      *> ***************************************************************

      *> ***************************************************************
       identification division.
       program-id. gnucobol-connection-handler.

       environment division.
       input-output section. 
       file-control.
           select pollen-file assign to "pollen.dat"
               organization is sequential.
       data division.
       file section.
       fd pollen-file.
       01 pollen-record.
           05 pollen-name pic x(16).
           05 pollen-code pic 9(1).
       working-storage section.
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
           "%2FPropertyName%3E%3CLiteral%3E2025-04-06%3C%2FLiteral%3E" &
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

       01 json-root                                 usage pointer.
       01 json-features usage pointer.
       01 json-first-feature usage pointer.
       01 json-properties usage pointer.
       01 json-foo usage pointer.
       01 json-str-ptr usage pointer.
       01 json-str-val pic x(1000) value spaces.
       01 json-error usage pointer.
       01 json-int usage binary-long.
       01 json-name-ptr usage pointer.
       01 json-name-val pic x(100).
       01 root-index pic 9 value 1.
       01 property-attr-index pic 999 value 1.
       01 property-attr usage pointer.
       01 property-name-val pic x(50).
       01 property-value-ptr usage pointer.
       01 property-value-val pic 9 value 0.
       01 json-pollen-resp-ptr usage pointer.
       01 json-pollen-resp-val pic x(50) value spaces.
       01 json-properties-size usage binary-long.

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

       call "cJSON_Parse" using
           by value function trim(buffer)
           returning json-root

       call "cJSON_GetArraySize" using
           by value json-root
           returning json-int
       perform varying root-index from 0 by 1 until root-index =json-int
           call "cJSON_GetArrayItem" using
               by value json-root
               root-index
               returning json-foo
           call "json-get-object-name" using
               by value json-foo
               by reference json-name-val
           if json-name-val(1:8) = "features"
           then
               call "cJSON_GetArrayItem" using
                   by value json-foo
                   0
                   returning json-first-feature
               if json-first-feature NOT = NULL
               then
                   call "cJSON_GetObjectItem" using
                       by value json-first-feature
                       by content "properties"
                       returning json-properties
                   if json-properties NOT = NULL
                   then
                       call "cJSON_GetObjectItem" using
                           by value json-properties
                           by content "pollen_resp"
                           returning json-pollen-resp-ptr

                       call "json-get-string-value" using
                           by value json-pollen-resp-ptr
                           by reference json-str-val
                       display "resp pollen: " json-str-val

                       call "cJSON_GetArraySize" using
                           by value json-properties
                           returning json-properties-size
                       open output pollen-file

                       perform varying property-attr-index from 0 by 1 
                           until property-attr-index 
                               = json-properties-size
                           move "" to property-name-val
                           call "cJSON_GetArrayItem" using
                               by value json-properties
                               property-attr-index
                               returning property-attr
                           call "json-get-object-name" using
                               by value property-attr
                               by reference property-name-val
                           if property-name-val(1:5) = "code_"
                           then
                               call "cJSON_GetIntValue" using
                                   by value property-attr
                                   returning property-value-val
                               move property-name-val to pollen-name
                               move property-value-val to pollen-code
                               write pollen-record
                           end-if
                       end-perform
                       close pollen-file
                   end-if
               end-if
           end-if
       end-perform.

       call "cJSON_GetErrorPtr"
           returning json-error
       
       if json-error NOT = NULL
       then
           display "json error " json-error
       end-if




       
       call "MHD_create_response_from_buffer" using
           by value length of webpage
           by reference webpage
           by value MHD_RESPMEM_PERSISTENT
           returning star-response
           on exception
               display
                   "gnucobol-microhttpd: libmicrohttpd failure"
                   upon syserr
               end-display
       end-call
       call "MHD_queue_response" using
           by value star-connection
           by value MHD_HTTP_OK
           by value star-response
           returning mhd-result
           on exception
               display
                   "gnucobol-microhttpd: libmicrohttpd failure"
                   upon syserr
               end-display
       end-call
       call "MHD_destroy_response" using
           by value star-response
       end-call

       move mhd-result to return-code

       goback.
       end program gnucobol-connection-handler.
