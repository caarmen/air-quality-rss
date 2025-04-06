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

       data division.
       working-storage section.
       01 MHD_HTTP_OK               constant   as 200.
       01 MHD_RESPMEM_PERSISTENT    constant   as 0.
       *> https://github.com/curl/curl/blob/master/packages/OS400/curl.inc.in#L1073
       01 CURLOPT_WRITEDATA         constant   as 10001.
       01 CURLOPT_URL               constant   as 10002.
       01 CURLOPT_WRITEFUNCTION     constant   as 20011.
       *> https://curl.se/libcurl/c/getinmemory.html
       01 memory-struct.
           05 buffer pic x(10000).
           05 sizet-size pic S9(18) comp-5.
       01 DATA-URL                  pic x(532) VALUE
           "https://data.atmo-france.org/geoserver/ind_pol/ows?" &
           "&REQUEST=GetFeatureInfo&SERVICE=WMS&SRS=EPSG%3A3857" &
           "&STYLES=&VERSION=1.3&FILTER=%3CPropertyIsEqualTo" &
           "%20matchCase%3D%22true%22%3E%3CPropertyName%3Edate_ech%3C" &
           "%2FPropertyName%3E%3CLiteral%3E2025-04-05%3C%2FLiteral%3E" &
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
       01 curl-response                        usage binary-long.
       01 curl-response-text                   pic x(10000).   *> Buffer to accumulate response data

       01 star-curl                            usage pointer.
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
       01 property-name-ptr usage pointer.
       01 property-name-val pic x(50).
       01 property-value-ptr usage pointer.
       01 property-value-val pic 9 value 0.
       01 json-bool pic 9 value 1.
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

       display data-url

       set curl-callback to
         entry "curl-callback"
       call "curl_easy_init"
           returning star-curl
       call "curl_easy_setopt" using
           by value star-curl
           by value CURLOPT_URL
           by content data-url
       call "curl_easy_setopt" using
           by value star-curl
           by value CURLOPT_WRITEFUNCTION
           by value curl-callback
       call "curl_easy_setopt" using
           by value star-curl
           by value CURLOPT_WRITEDATA
           by reference memory-struct
       call "curl_easy_perform" using
           by value star-curl
           returning curl-response

       display "memory struct finally " buffer(1:sizet-size)

       call "cJSON_ParseWithLength" using
           by value buffer
           by value sizet-size
           returning json-root

       call "cJSON_GetObjectItem" using
           by value json-root
           by content "features"
           returning json-foo
       
       display "features " json-foo

       call "cJSON_GetArraySize" using
           by value json-root
           returning json-int
       perform varying root-index from 0 by 1 until root-index =json-int
           call "cJSON_GetArrayItem" using
               by value json-root
               root-index
               returning json-foo
           call "cJSON_GetObjectName" using
               by value json-foo
               returning json-name-ptr
           call "strcpy" using 
               by reference json-name-val 
               by value json-name-ptr
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

                       call "cJSON_GetStringValue" using
                           by value json-pollen-resp-ptr
                           returning json-str-ptr
                       call "strcpy" using 
                           by reference json-str-val 
                           by value json-str-ptr
                       display "resp pollen: " json-str-val

                       call "cJSON_GetArraySize" using
                           by value json-properties
                           returning json-properties-size
                       perform varying property-attr-index from 0 by 1 
                           until property-attr-index 
                               = json-properties-size
                           move "" to property-name-val
                           call "cJSON_GetArrayItem" using
                               by value json-properties
                               property-attr-index
                               returning property-attr
                           call "cJSON_GetObjectName" using
                               by value property-attr
                               returning property-name-ptr
                           call "strcpy" using 
                               by reference property-name-val
                               by value property-name-ptr
                           if property-name-val(1:5) = "code_"
                           then
                               call "cJSON_IsNumber" using
                                   by value property-attr
                                   returning json-bool
                               call "cJSON_GetIntValue" using
                                   by value property-attr
                                   returning property-value-val
                               display property-name-val
                                   ": " property-value-val
                           end-if
                       end-perform
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

       call "curl_easy_cleanup" using
           by value star-curl

       move mhd-result to return-code

       goback.
       end program gnucobol-connection-handler.

       identification division.
       program-id. curl-callback.

       data division.
       working-storage section.
       01 curl-callback-result                 usage binary-long.
       01 response-string pic x(10000).   *> Buffer to accumulate response data
       01 response-length                      pic 9(9) comp-5.   *> Holds the current length of the data in the buffer
       
       linkage section.
       01 star-ptr                             usage pointer.
       01 sizet-size                           pic S9(18) comp-5.   *> 64-bit (for size_t)
       01 sizet-nmemb                          pic S9(18) comp-5.   *> 64-bit (for size_t)
       01 userdata-ptr                         usage pointer.

       01 memory-struct.
           05 ms_buffer pic x(10000).
           05 ms_sizet-size pic S9(18) comp-5.

       procedure division with C linkage using
           by value star-ptr
           by value sizet-size
           by value sizet-nmemb
           by reference memory-struct
       .

       compute response-length = sizet-size * sizet-nmemb
       *> https://curl.se/libcurl/c/getinmemory.html
       call "memcpy" using
-           by reference response-string
-           by value star-ptr
-           by value response-length
       string ms_buffer(1:ms_sizet-size) 
           response-string(1:response-length) delimited by size
           into ms_buffer of memory-struct
       end-string.
       compute ms_sizet-size = ms_sizet-size + response-length

       move response-length to return-code.
       goback.
       end program  curl-callback.
