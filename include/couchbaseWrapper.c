#include "couchbaseWrapper.h"
#include <stdlib.h>
#include <string.h>

static void store_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPSTORE *resp)
{
    GetInfo *gi;
    lcb_STORE_OPERATION op;
    lcb_respstore_operation(resp, &op);
    lcb_respstore_cookie(resp, (void **)&gi);
    gi->status = lcb_respstore_status(resp);
}

static void get_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPGET *resp)
{
    GetInfo *gi;
    lcb_STATUS rc;

    lcb_respget_cookie(resp, (void **)&gi);
    gi->status = lcb_respget_status(resp);

    if (gi->status == LCB_SUCCESS)
    {
        const char *value;
        size_t nvalue;
        lcb_respget_value(resp, &value, &nvalue);
        gi->value = malloc(nvalue);
        gi->nvalue = nvalue;
        memcpy((void *)gi->value, value, nvalue);
    }
}

lcb_STATUS lcb_init_wrapper(lcb_INSTANCE *instance)
{
    lcb_install_callback(instance, LCB_CALLBACK_GET, (lcb_RESPCALLBACK)get_callback);
    lcb_install_callback(instance, LCB_CALLBACK_STORE, (lcb_RESPCALLBACK)store_callback);
    return LCB_SUCCESS;
}

void lcb_get_wrapper(lcb_INSTANCE *instance, lcb_CMDGET *cmd, GetInfo *retval)
{
    lcb_STATUS err;
    err = lcb_get(instance, retval, cmd);
    if (err != LCB_SUCCESS)
    {
        fprintf(stderr, "Failed to schedule get operation: %s\n", lcb_strerror_short(err));
        retval->status = err;
        free(retval);
    }
    lcb_wait(instance, LCB_WAIT_NOCHECK);
}

void lcb_store_wrapper(lcb_INSTANCE *instance, lcb_CMDSTORE *cmd, GetInfo *retval)
{
    lcb_STATUS err;
    err = lcb_store(instance, retval, cmd);
    if (err != LCB_SUCCESS)
    {
        fprintf(stderr, "Failed to schedule get operation: %s\n", lcb_strerror_short(err));
        retval->status = err;
        free(retval);
    }
    lcb_wait(instance, LCB_WAIT_NOCHECK);
}