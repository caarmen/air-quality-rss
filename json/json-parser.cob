       identification division.
       program-id. json-get-object-name.
       data division.

       working-storage section.
       01 property-name-ptr usage pointer.

       linkage section.
       
       01 json-handle-ptr usage pointer.
       01 object-name pic x(50).

       procedure division using
           by value json-handle-ptr
           by reference object-name.

           call "cJSON_GetObjectName" using
               by value json-handle-ptr
               returning property-name-ptr
           call "strcpy" using 
               by reference object-name
               by value property-name-ptr
       goback.
       end program json-get-object-name.

       program-id. json-get-string-value.
       data division.

       working-storage section.
       01 property-value-ptr usage pointer.

       linkage section.
       
       01 json-handle-ptr usage pointer.
       01 string-value pic x(50).

       procedure division using
           by value json-handle-ptr
           by reference string-value.

           call "cJSON_GetStringValue" using
               by value json-handle-ptr
               returning property-value-ptr
           call "strcpy" using 
               by reference string-value
               by value property-value-ptr
       goback.
       end program json-get-string-value.

       *> The GetObject api from cJSON doesn't seem to work when
       *> the attribute value is an array.
       *> So we implement an alternative here which iterates over
       *> all the objects attributes, until it finds the one with
       *> the given name.
       program-id. json-get-object.
       data division.

       working-storage section.
       01 attribute-index pic 9 value 1.
       01 attribute-count usage binary-long.
       01 iter-attribute-handle-ptr usage pointer.
       01 iter-attribute-name pic x(50).

       linkage section.
       
       01 json-source-handle-ptr usage pointer.
       01 attribute-name pic x(50) value spaces.
       01 json-found-object-handle-ptr usage pointer.

       procedure division using
           attribute-name
           by value json-source-handle-ptr
           by reference json-found-object-handle-ptr.
               
       call "cJSON_GetArraySize" using
           by value json-source-handle-ptr
           returning attribute-count

       move NULL to json-found-object-handle-ptr
       perform varying attribute-index from 0 by 1 
           until attribute-index = attribute-count 
               or json-found-object-handle-ptr not = NULL
           call "cJSON_GetArrayItem" using
               by value json-source-handle-ptr
               by value attribute-index
               returning iter-attribute-handle-ptr
           call "json-get-object-name" using
               by value iter-attribute-handle-ptr
               by reference iter-attribute-name
           if iter-attribute-name(1:8) = attribute-name
           then
               move iter-attribute-handle-ptr 
                   to json-found-object-handle-ptr
           end-if
       end-perform
       goback.
       end program json-get-object.
       