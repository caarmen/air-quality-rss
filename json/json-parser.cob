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
       