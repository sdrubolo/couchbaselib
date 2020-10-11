#include <libcouchbase/couchbase.h>

typedef struct
{
    lcb_STATUS status;
    unsigned long long cas;
    size_t nvalue;
    const char *value;
    uint64_t counter;
} GetInfo;

lcb_STATUS lcb_init_wrapper(lcb_INSTANCE *instance);

void lcb_store_wrapper(lcb_INSTANCE *instance, lcb_CMDSTORE *cmd, GetInfo *retval);
void lcb_get_wrapper(lcb_INSTANCE *instance, lcb_CMDGET *cmd, GetInfo *retval);
void lcb_remove_wrapper(lcb_INSTANCE *instance, lcb_CMDREMOVE *cmd, GetInfo *retval);
void lcb_touch_wrapper(lcb_INSTANCE *instance, lcb_CMDTOUCH *cmd, GetInfo *retval);
void lcb_counter_wrapper(lcb_INSTANCE *instance, lcb_CMDCOUNTER *cmd, GetInfo *retval);