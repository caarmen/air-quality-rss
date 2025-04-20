       identification division.
       program-id. pollen-parser.
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
       01 json-root                                 usage pointer.
       01 json-features usage pointer.
       01 json-first-feature usage pointer.
       01 json-properties usage pointer.
       01 json-str-val pic x(1000) value spaces.
       01 json-error usage pointer.
       01 property-attr-index pic 999 value 1.
       01 property-attr usage pointer.
       01 property-name-val pic x(50).
       01 property-value-val pic 9 value 0.
       01 json-pollen-resp-ptr usage pointer.
       01 json-pollen-resp-val pic x(50) value spaces.
       01 json-properties-size usage binary-long.
       01 FEATURES_ATTRIBUTE pic x(50) value "features".
       01 PROPERTIES_ATTRIBUTE pic x(50) value "properties" & X"00".
       01 POLLEN_RESP_ATTRIBUTE pic x(50) value "pollen_resp" & X"00".

       linkage section.
       01 pollen-json-input pic x(10000).
       procedure division with C linkage using
           by reference pollen-json-input
       .

       call "cJSON_Parse" using
           by content function trim(pollen-json-input)
           returning json-root
       call "json-get-object" using
           by content FEATURES_ATTRIBUTE
           by value json-root
           by reference json-features
       call "cJSON_GetArrayItem" using
           by value json-features
           0
           returning json-first-feature
       if json-first-feature NOT = NULL
       then
           call "cJSON_GetObjectItem" using
               by value json-first-feature
               by content PROPERTIES_ATTRIBUTE
               returning json-properties
           if json-properties NOT = NULL
           then
               call "cJSON_GetObjectItem" using
                   by value json-properties
                   by content POLLEN_RESP_ATTRIBUTE
                   returning json-pollen-resp-ptr

               call "json-get-string-value" using
                   by value json-pollen-resp-ptr
                   by reference json-str-val
               string json-str-val
                   into responsible-pollen
               end-string
               open output pollen-file
               write responsible-pollen

               call "cJSON_GetArraySize" using
                   by value json-properties
                   returning json-properties-size

               perform varying property-attr-index from 0 by 1 
                   until property-attr-index 
                       = json-properties-size
                   move " " to property-name-val
                   call "cJSON_GetArrayItem" using
                       by value json-properties
                       property-attr-index
                       returning property-attr
                   call "json-get-object-name" using
                       by value property-attr
                       by reference property-name-val
                   if property-name-val(1:5) = "code_"
                       and property-name-val(1:9) NOT = "code_qual"
                       and property-name-val(1:9) NOT = "code_zone"
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
       goback.
       end program pollen-parser.
