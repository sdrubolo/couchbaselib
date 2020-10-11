#include "couchbaseWrapper.h"
#include <stdlib.h>
#include <string.h>

static void store_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPSTORE *resp)
{
    GetInfo *gi;
    lcb_STORE_OPERATION op;
    uint64_t cas;
    lcb_respstore_operation(resp, &op);
    lcb_respstore_cookie(resp, (void **)&gi);
    lcb_STATUS rc = lcb_respstore_status(resp);
    if (rc == LCB_SUCCESS)
    {
        lcb_respstore_cas(resp, &cas);
        gi->cas = cas;
    }
    gi->status = rc;
}

void rm_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPREMOVE *resp)
{
    GetInfo *gi;
    lcb_STATUS rc = lcb_respremove_status(resp);
    lcb_respremove_cookie(resp, (void **)&gi);

    if (rc == LCB_SUCCESS)
    {
        uint64_t cas;
        lcb_respremove_cas(resp, &cas);
        gi->cas = cas;
    }
    gi->status = rc;
}

static void touch_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPTOUCH *resp)
{
    GetInfo *gi;
    lcb_resptouch_cookie(resp, (void **)&gi);
    gi->status = lcb_resptouch_status(resp);
    if (gi->status == LCB_SUCCESS)
    {
        uint64_t cas;
        lcb_resptouch_cas(resp, &cas);
        gi->cas = cas;
    }
}

static void counter_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPCOUNTER *resp)
{
    GetInfo *gi;
    lcb_respcounter_cookie(resp, (void **)&gi);
    gi->status = lcb_respcounter_status(resp);

    if (gi->status == LCB_SUCCESS)
    {
        uint64_t value;
        uint64_t cas;
        lcb_respcounter_value(resp, &value);
        lcb_respcounter_cas(resp, &cas);
        gi->counter = value;
        gi->cas = cas;
    }
}

static void get_callback(lcb_INSTANCE *instance, int cbtype, const lcb_RESPGET *resp)
{
    GetInfo *gi;
    lcb_STATUS rc;

    lcb_respget_cookie(resp, (void **)&gi);
    gi->status = lcb_respget_status(resp);

    if (gi->status == LCB_SUCCESS)
    {
        uint64_t cas;
        const char *value;
        size_t nvalue;
        lcb_respget_value(resp, &value, &nvalue);
        lcb_respget_cas(resp, &cas);
        gi->value = malloc(nvalue);
        gi->nvalue = nvalue;
        gi->cas = cas;
        memcpy((void *)gi->value, value, nvalue);
    }
}

lcb_STATUS lcb_init_wrapper(lcb_INSTANCE *instance)
{
    lcb_install_callback(instance, LCB_CALLBACK_GET, (lcb_RESPCALLBACK)get_callback);
    lcb_install_callback(instance, LCB_CALLBACK_STORE, (lcb_RESPCALLBACK)store_callback);
    lcb_install_callback(instance, LCB_CALLBACK_REMOVE, (lcb_RESPCALLBACK)rm_callback);
    lcb_install_callback(instance, LCB_CALLBACK_TOUCH, (lcb_RESPCALLBACK)touch_callback);
    lcb_install_callback(instance, LCB_CALLBACK_COUNTER, (lcb_RESPCALLBACK)counter_callback);

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
        fprintf(stderr, "Failed to schedule store operation: %s\n", lcb_strerror_short(err));
        retval->status = err;
        free(retval);
    }
    lcb_wait(instance, LCB_WAIT_NOCHECK);
}

void lcb_remove_wrapper(lcb_INSTANCE *instance, lcb_CMDREMOVE *cmd, GetInfo *retval)
{
    lcb_STATUS err;
    err = lcb_remove(instance, retval, cmd);

    if (err != LCB_SUCCESS)
    {
        fprintf(stderr, "Failed to schedule remove operation: %s\n", lcb_strerror_short(err));
        retval->status = err;
        free(retval);
    }
    lcb_wait(instance, LCB_WAIT_NOCHECK);
}

void lcb_touch_wrapper(lcb_INSTANCE *instance, lcb_CMDTOUCH *cmd, GetInfo *retval)
{
    lcb_STATUS err;
    err = lcb_touch(instance, retval, cmd);

    if (err != LCB_SUCCESS)
    {
        fprintf(stderr, "Failed to schedule touch operation: %s\n", lcb_strerror_short(err));
        retval->status = err;
        free(retval);
    }
    lcb_wait(instance, LCB_WAIT_NOCHECK);
}

void lcb_counter_wrapper(lcb_INSTANCE *instance, lcb_CMDCOUNTER *cmd, GetInfo *retval)
{
    lcb_STATUS err;
    err = lcb_counter(instance, retval, cmd);
    if (err != LCB_SUCCESS)
    {
        fprintf(stderr, "Failed to schedule counter operation: %s\n", lcb_strerror_short(err));
        retval->status = err;
        free(retval);
    }
    lcb_wait(instance, LCB_WAIT_NOCHECK);
}