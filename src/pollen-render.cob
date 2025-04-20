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
       01 responsible-pollen pic x(16).
       01 pollen-record.
           05 pollen-name pic x(16).
           05 pollen-code pic 9(1).
       local-storage section.
       01 response pic x(10000) value spaces.
       01 pollen-display-name pic x(16).
       linkage section.
       01 pollen-output pic x(10000) value spaces.

       procedure division using
           by reference pollen-output.
            open input pollen-file
            *> First read the responsible-pollen
            *> Then read all of the pollen-records until the end of
            *> file
            read pollen-file into responsible-pollen.
            string
                "Responsible pollen: " responsible-pollen x"0A"
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
