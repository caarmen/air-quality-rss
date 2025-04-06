       identification division.
       program-id. json-get-object-name.
       data division.

       working-storage section.
       01 property-name-ptr usage pointer.
       01 property-value-ptr usage pointer.

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