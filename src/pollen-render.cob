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
                    string
                        function trim(pollen-output)
                        function trim(pollen-name(6:))
                        ": "
                        pollen-code x"0A"
                        into pollen-output
                    end-string
                end-read
            end-perform
            close pollen-file
            goback.
       end program pollen-render.
