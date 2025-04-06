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
       display "wow, server.  help, info, quit" end-display
       perform until server-command = "quit"
           display "server: " with no advancing end-display
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
       01 CURLOPT_URL               constant   as 10002.
       01 data-url                  constant   as "https://rmen.ca".
       01 webpage              pic x(132) value
          "<html><body>" &
          "Hello, world<br/>" &
          "from <b>GnuCOBOL</b> and <i>libmicrohttpd</i>" &
          "</body></html>".
       01 star-response                        usage pointer.
       01 mhd-result                           usage binary-long.
       01 curl-response                        usage binary-long.

       01 star-curl                            usage pointer.

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


       display "wow, connection handler" upon syserr end-display
       call "curl_easy_init"
           returning star-curl
       call "curl_easy_setopt" using
           by value star-curl
           by value CURLOPT_URL
           by content data-url
       call "curl_easy_perform" using
           by value star-curl
           returning curl-response
       
       display "curl is " star-curl
       display "curl-response is " curl-response
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

      *> ***************************************************************
      *> from libmicrohttpd hellobrowser.c tutorial example
      *> ***************************************************************
      *> #include <sys/types.h>
      *> #include <sys/select.h>
      *> #include <sys/socket.h>
      *> #include <microhttpd.h>
      *>
      *> #define PORT 8888
      *>
      *> static int
      *> answer_to_connection(void *cls, struct MHD_Connection *connection,
      *>                     const char *url, const char *method,
      *>                     const char *version, const char *upload_data,
      *>                     size_t * upload_data_size, void **con_cls)
      *> {
      *>     const char *page = "<html><body>Hello, browser!</body></html>";
      *>     struct MHD_Response *response;
      *>     int ret;
      *>     response =
      *>        MHD_create_response_from_buffer(strlen(page), (void *) page,
      *>                                        MHD_RESPMEM_PERSISTENT);
      *>     ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
      *>     MHD_destroy_response(response);
      *>     return ret;
      *> }
      *>
      *> int main()
      *> {
      *>     struct MHD_Daemon *daemon;
      *>     daemon = MHD_start_daemon(MHD_USE_SELECT_INTERNALLY, PORT, NULL, NULL,
      *>                              &answer_to_connection, NULL, MHD_OPTION_END);
      *>     if (NULL == daemon)
      *>        return 1;
      *>     getchar();
      *>     MHD_stop_daemon(daemon);
      *>     return 0;
      *> }
