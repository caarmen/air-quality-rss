       identification division.
      *> ***************************************************************
      *> Read data from the pollen.dat file and render it to a string.
      *> ***************************************************************

       program-id. pollen-render.
       environment division.
       input-output section.
       file-control.
       select pollen-file assign to "pollen.dat"
          organization is sequential.
       data division.
       file section.
       fd pollen-file.
       01 date-maj pic x(24).
       01 responsible-pollen pic x(16) value spaces.
       01 pollen-record.
           05 pollen-name pic x(16).
           05 pollen-code pic 9(1).
       local-storage section.
       01 response pic x(10000) value spaces.
       01 pollen-updated-at pic x(24).
       01 pollen-display-name pic x(16).
       01 pollen-output pic x(10000) value spaces.
       linkage section.
       01 pollen-rss-output pic x(10000) value spaces.


       procedure division using
           by reference pollen-rss-output.
            open input pollen-file
            *> First read the responsible-pollen
            *> Then read all of the pollen-records until the end of
            *> file
            read pollen-file into date-maj.
            string date-maj into pollen-updated-at
            end-string
            read pollen-file into responsible-pollen.
            string
                "Responsible pollen: "
                function trim(responsible-pollen) x"0A"
                into pollen-output
            end-string
            perform until exit
                read pollen-file into pollen-record
                    at end
                        exit perform
                    not at end
                    call "pollen-display-name" using
                        by reference pollen-name
                        by reference pollen-display-name
                    end-call
                    string
                        function trim(pollen-output)
                        function trim(pollen-display-name)
                        ": "
                        pollen-code x"0A"
                        into pollen-output
                    end-string
                end-read
            end-perform
            close pollen-file
            call "render-rss" using
                by reference pollen-updated-at
                by reference pollen-output
                by reference pollen-rss-output
            end-call
            goback.
       end program pollen-render.

       program-id. pollen-display-name.
       data division.
       linkage section.
       01 pollen-name pic x(16).
       01 pollen-display-name pic x(16).

       procedure division using
           by reference pollen-name,
           by reference pollen-display-name.
           if pollen-name(1:9) is = "code_ambr"
           then
               move "Ambroise" to pollen-display-name
           else if pollen-name(1:8) is = "code_arm"
               then
                   move "Armoise" to pollen-display-name
           else if pollen-name(1:8) is = "code_aul"
               then
                   move "Aulne" to pollen-display-name
           else if pollen-name(1:9) is = "code_boul"
               then
                   move "Bouleau" to pollen-display-name
           else if pollen-name(1:9) is = "code_gram"
               then
                   move "Graminées" to pollen-display-name
           else if pollen-name(1:9) is = "code_oliv"
               then
                   move "Olivier" to pollen-display-name
           else
               move pollen-name to pollen-display-name
           end-if
       goback.
       end program pollen-display-name.

       identification division.
       program-id. render-rss.
       data division.
       local-storage section.
       01 feed-url pic x(100).
       01 source-url pic x(1000).
       01 escaped-source-url pic x(1000) value spaces.
       01 i pic 9(3) value 1.
       linkage section.
       01 date-maj pic x(24).
       01 feed-content pic x(10000) value spaces.
       01 rss-content pic x(10000) value spaces.
       procedure division using
           by reference date-maj
           by reference feed-content
           by reference rss-content
       .
       accept feed-url from environment "POLLEN_FEED_URL"
       call "source-url" using
           by reference source-url
      *> Escape & from the url
      *> This could be done more robustly with a thin wrapper to
      *> libxml2 apis.
       perform varying i from 1 by 1 until i > length
           of function trim(source-url)
           evaluate source-url(i:1)
              when "&"
                string
                     function trim(escaped-source-url)
                     "&amp;"
                     into escaped-source-url
                end-string
              when other
                string
                     function trim(escaped-source-url)
                     source-url(i:1)
                     into escaped-source-url
                end-string
           end-evaluate
       end-perform
       string
           '<?xml version="1.0" encoding="utf-8"?>'                x"0A"
           '<feed xmlns="http://www.w3.org/2005/Atom"'
           ' xmlns:dc="http://purl.org/dc/elements/1.1/">'         x"0A"
           " <updated>" date-maj "</updated>"                      x"0A"
           " <dc:date>" date-maj "</dc:date>"                      x"0A"
           " <title>Pollenes aujourd'hui</title>"                  x"0A"
           " <subtitle>Pollenes aujourd'hui</subtitle>"            x"0A"
           ' <link rel="alternate" '
           '  href="' function trim(feed-url) '" />'               x"0A"
           " <id>" function trim(feed-url) "</id>"                 x"0A"
           " <entry>"                                              x"0A"
           "  <title>Rapport de pollens</title>"                   x"0A"
           '  <link rel="alternate" '
           '   href="' function trim(escaped-source-url) '" />'    x"0A"
           "  <id>" function trim(escaped-source-url) "</id>"      x"0A"
           '  <content type="text/plain">'                         x"0A"
                function trim(feed-content)
           "  </content>"                                          x"0A"
           "  <author><name>Atmo France</name></author>"           x"0A"
           "  <dc:creator>Atmo France</dc:creator>"                x"0A"
           "  <published>" date-maj "</published>"                 x"0A"
           "  <updated>" date-maj "</updated>"                     x"0A"
           "  <dc:date>" date-maj "</dc:date>"                     x"0A"
           " </entry>"                                             x"0A"
           "</feed>"
           into rss-content
       end-string
       goback.

       end program render-rss.
