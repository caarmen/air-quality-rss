       identification division.
       program-id. pollen-server.

       data division.

       local-storage section.
       01 MHD_USE_SELECT_INTERNALLY constant   as 8.
       01 MHD_OPTION_END            constant   as 0.

       01 star-daemon               usage pointer.
       01 connection-handler-entry  usage program-pointer.

       01 server-command            pic x(80).



      *> ***************************************************************
       procedure division.
       set connection-handler-entry to
         entry "microhttpd-access-handler".
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

       goback.
       end program pollen-server.
      *> ***************************************************************

      *> ***************************************************************
       identification division.
       program-id. microhttpd-access-handler.

       data division.
       local-storage section.
       01 MHD_HTTP_OK               constant   as 200.
       01 MHD_RESPMEM_PERSISTENT    constant   as 0.
       01 response.
           05 status-code pic 999.
           05 body pic x(10000) value spaces.

       01 star-response                        usage pointer.
       01 mhd-result                           usage binary-long.

       01 http-method pic x(8).
       01 url pic x(100).

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

       call "c-string" using
           by value star-method
           by reference http-method
       call "c-string" using
           by value star-url
           by reference url

       call "pollen-router" using
           by value star-connection
           by reference http-method
           by reference url
           by reference response

       call "MHD_create_response_from_buffer" using
           by value length of body
           by value function trim(body)
           by value MHD_RESPMEM_PERSISTENT
           returning star-response
       end-call
       call "MHD_queue_response" using
           by value star-connection
           by value status-code
           by value star-response
           returning mhd-result
       end-call
       call "MHD_destroy_response" using
           by value star-response
       end-call

       move mhd-result to return-code

       goback.
       end program microhttpd-access-handler.
