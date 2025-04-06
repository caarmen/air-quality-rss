#include <cjson/cJSON.h>

CJSON_PUBLIC(const char *) cJSON_GetObjectName(const cJSON * const object)
{
    return object->string;
}
