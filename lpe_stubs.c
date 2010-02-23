static const char* _proprietary_code_marker = "Citrix proprietary code";

#include <stdio.h>
#include <wchar.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include "MFLic.h"

#ifndef DEBUG
#define D
#else
#define D printf
#endif

// Globals, initialised by initialise_c
MFLIC_LICENSE_POLICY_OBJECT_HANDLE lpe_handle = NULL;
MFLIC_LICPOLENG_CALLBACKS callbacks;
wchar_t *w_cache_dir = NULL;

BOOL cleanup(void)
{
	MFLIC_STATUS result;
	BOOL success = TRUE;
	
	D("cleaning up...\n");
	if (lpe_handle != NULL) {
		result = MFLic_CleanupPolicyEng(lpe_handle);
		lpe_handle = NULL;
		success = (result == MFLIC_SUCCESS);
	}
	ProdLic_HeapDestroy();
	if (success)
		D("    ok\n");
	else
		D("!! error\n");
	
	return success;
}

// Global variable to hold the ID of the pipe used for callbacks;
// should be initialised by a call to set_pipe_c before starting the LPE
int callback_pid = -1;

// call this function when the daemon is started, and only once!!
CAMLprim value set_pipe_c(value pid)
{
	CAMLparam1(pid);
	callback_pid = Int_val(pid);
	CAMLreturn(Val_unit);
}

// LPE callbacks

int LSConnStatusChange(MFLIC_LICENSE_POLICY_OBJECT_HANDLE h, void *d,
	BOOL up, WCHAR *name, DWORD port)
{
	D("license-server %s\n", up ? "up" : "down");
	if (up)
		write(callback_pid, "u", 1);
	else
		write(callback_pid, "d", 1);
	return 0;
}

int LicExpired(MFLIC_LICENSE_POLICY_OBJECT_HANDLE h, void *d)
{
	D("license expired\n");
	write(callback_pid, "e", 1);
	return 0;
}


// Call this function when the daemon is started, and only once!!
// note that the memory allocated here is simply kept throughout the lifetime
// of the daemon
CAMLprim value alloc_and_set_cache_dir_c(value dir)
{
	CAMLparam1(dir);
	w_cache_dir = malloc(256 * sizeof(wchar_t));
	swprintf(w_cache_dir, 256, L"%s", String_val(dir));
	
	memset(&callbacks, 0, sizeof(callbacks));

	callbacks.pfnLSConnStatusChange = &LSConnStatusChange;
	callbacks.pfnLicExpired = &LicExpired;
	
	CAMLreturn(Val_unit);
}

// C-functions exported to OCaml

CAMLprim value start_c(value address, value port, value product, value edition, value dbv)
{
	MFLIC_STATUS result;
	MFLIC_OOBG_INPUT_DATA oobg = {0, L""};
	wchar_t *w_address = NULL;
	wchar_t *w_edition = NULL;
	wchar_t *w_product = NULL;
	wchar_t *w_dbv = NULL;
	DWORD d_port;
	
	// protect params from OCaml GC and declare return value
	CAMLparam5(address, port, product, edition, dbv);
	CAMLlocal1(success);
	success = Val_bool(FALSE); // default return value
	
	// obtain and convert parameters
	w_address = malloc(128 * sizeof(wchar_t));
	w_edition = malloc(4 * sizeof(wchar_t));
	w_product = malloc(32 * sizeof(wchar_t));
	w_dbv = malloc(10 * sizeof(wchar_t));
	swprintf(w_address, 128, L"%s", String_val(address));
	swprintf(w_product, 32, L"%s", String_val(product));
	swprintf(w_edition, 4, L"%s", String_val(edition));
	swprintf(w_dbv, 10, L"%s", String_val(dbv));
	d_port = Int_val(port);
	
	D("START\n%ls, %d, %ls, %ls, %ls\n", w_address, d_port, w_product, w_edition, w_dbv);
		
	// initialise LPE		
	D("initialising...\n");	
	ProdLic_HeapCreate();	// memory allocation for the LPE object's private heap
	DoLogging();			// The LPE creates a log file when the environmentvariable PRODLIC_LOG=1
	ConfDBInit();			// Initialises the database file which acts as a
							// Windows registry file. This creates a database file
							// in the directory that the application runs in
	result = MFLic_InitPolicyEng(&lpe_handle,
		L"LPE",			// LPE name
		L"LPE",			// LPE name
		w_address,		// address of license server
		d_port,			// port of license server
		NULL,			// just use NULL
		w_product,		// product name
		w_product,		// product name
		w_product,		// product name
		w_edition,		// component/edition (STD|ADV|ENT|PLT)
		w_dbv,			// data-based version (DBV)
		w_cache_dir,	// path to store cached data
		oobg,			// OOBGP settings
		PRODLIC_CCS_LICMODEL);	// licensing model (CCS)
		
	if (result != MFLIC_SUCCESS || lpe_handle == NULL) {
		D("!! error %d\n", result);
		cleanup();
	}
	else {
		D("    ok\n");
		
		// register call backs
		
		D("registering callbacks...\n");
		result = MFLic_RegisterCallBacks(lpe_handle, &callbacks, sizeof(callbacks));

		if (result != MFLIC_SUCCESS) {
			D("!! error %d\n", result);
			cleanup();
		}
		else {
			D("    ok\n");
		
			// start up
		
			D("starting LPE...\n");
			result = MFLic_StartPolicyEng(lpe_handle);
	
			if (result != MFLIC_SUCCESS) {
				D("!! error %d\n", result);
				cleanup();
			}
			else {
				D("    ok\n");				
				success = Val_bool(TRUE);
			}
		}
	}
	
	free(w_address);
	free(w_edition);
	free(w_product);
	free(w_dbv);
	
	CAMLreturn(success);
}

CAMLprim value get_license_c(value profile)
{
	MFLIC_STATUS result;
	wchar_t *w_profile = NULL;
	
	WCHAR *VendorStr = NULL;
	BOOL OverDraft = 0;
	MFLIC_REQUEST_STATUS reqStatus = 0;
	int errcode = 0;
	MFLIC_LICENSE_GIVEN pLicenseGiven = 0;
	
	// protect params from OCaml GC and declare return value
	CAMLparam1(profile);

	// tuple for return value:
	CAMLlocal1(tuple);
	tuple = caml_alloc(3, 0);
	// defaults
	Store_field(tuple, 0, Val_int(-1));	// reqStatus
	Store_field(tuple, 1, Val_int(-1));	// pLicenseGiven
	Store_field(tuple, 2, Val_int(-1));	// days to expire (or -1 for "permanent" or "unknown")
			
	// obtain and convert parameters
	w_profile = malloc(128 * sizeof(wchar_t));
	swprintf(w_profile, 128, L"%s", String_val(profile));
	D("GET_LICENSE\n%ls\n", w_profile);
	
	if (lpe_handle != NULL) {
		// get license
	
		D("requesting license...\n");
		result = MFLic_GetLicense2(lpe_handle,
			w_profile,
			&VendorStr,
			&OverDraft,
			&reqStatus,
			&errcode,
			&pLicenseGiven);
				
		if (result != MFLIC_SUCCESS)
			D("!! error %d\n", result);
		else {
			D("    ok\n    ");
				
			switch (reqStatus) {
			case LIC_GRANTED:
				Store_field(tuple, 0, Val_int(0));
				D("license granted\n");
				break;
			case LIC_GRANTED_REF_COUNTED:
				Store_field(tuple, 0, Val_int(1));
				D("existing license used\n");
				break;
			case LIC_REJECTED:
				Store_field(tuple, 0, Val_int(2));
				D("license checkout rejected\n");
				break;
			case LS_COMM_FAILURE:
				Store_field(tuple, 0, Val_int(3));
				D("comm failure during checkout\n");
				break;
			case LIC_REQ_TIMEOUT:
				Store_field(tuple, 0, Val_int(4));
				D("timeout during checkout\n");
				break;
			default:
				Store_field(tuple, 0, Val_int(5));
				D("unknown failure during checkout\n");
				break;
			}
		
			D("    ");
			switch (pLicenseGiven) {
			case NONE_GIVEN:
				D("no license given\n");
				Store_field(tuple, 1, Val_int(0));
				break;
			case TRANSIENT:
				D("transient license\n");
				Store_field(tuple, 1, Val_int(1));
				break;
			case REAL:
				D("real license\n");
				Store_field(tuple, 1, Val_int(2));
				break;
			case GRACE:
				D("grace license\n");
				Store_field(tuple, 1, Val_int(3));
				break;
			}
			
			// get days-to-expire from annoyance profile
	
			D("checking days-to-expire...\n");
		
			APHELPER_AP_INFO *apinfo = NULL;
			result = MFLic_GetAPInfo(lpe_handle, (void**) &apinfo);
		
			if (result == MFLIC_SUCCESS && apinfo != NULL) {
				Store_field(tuple, 2, Val_int(apinfo->daysToExpire));
				D("    days to expire: %d\n", apinfo->daysToExpire);
			}
			else {
				Store_field(tuple, 2, Val_int(-1));
				D("    expiry date is unknown or license is permanent\n");
			}
		}
	}
	
	free(w_profile);
	
	CAMLreturn(tuple);
}

// check in license
CAMLprim value release_license_c(value profile)
{
	MFLIC_STATUS result;
	wchar_t *w_profile = NULL;
	
	CAMLparam1(profile);
	CAMLlocal1(success);
	success = Val_bool(FALSE);
			
	// obtain and convert parameters
	w_profile = malloc(128 * sizeof(wchar_t));
	swprintf(w_profile, 128, L"%s", String_val(profile));
	D("RELEASE_LICENSE\n%ls\n", w_profile);
		
	// release license
	if (lpe_handle != NULL) {
		D("releasing license... (%ls)\n", w_profile);
		result = MFLic_RelLicense2(lpe_handle, w_profile);
	
		if (result != MFLIC_SUCCESS)
			D("!! error %d\n", result);
		else {
			D("    ok\n");
			success = Val_bool(TRUE);
		}
	}
	
	free(w_profile);
		
	// return success/failure
	CAMLreturn(success);
}

// shut down and clean up the LPE
CAMLprim value stop_c(void)
{
	MFLIC_STATUS result;
	BOOL success = FALSE;
	
	if (lpe_handle != NULL) {
		// shutdown

		D("shutting down LPE...\n");
		result = MFLic_ShutdownPolicyEng(lpe_handle);
	
		if (result != MFLIC_SUCCESS)
			D("!! error %d\n", result);
		else {
			D("    ok\n");
			success = TRUE;
		}
	
		success &= cleanup();
	}
	
	// return success/failure
	return Val_bool(success);
}

