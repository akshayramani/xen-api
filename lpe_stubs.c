#include <stdio.h>
#include <wchar.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include "MFLic.h"

#define DEBUG (FALSE)

// Globals, initialised by initialise_c
MFLIC_LICENSE_POLICY_OBJECT_HANDLE lpe_handle = NULL;
wchar_t *w_cache_dir = NULL;
wchar_t *w_address = NULL;
wchar_t *w_edition = NULL;
wchar_t *w_dbv = NULL;
wchar_t *w_profile = NULL;
BOOL getlicense_ok = FALSE;

void cleanup(void)
{
	lpe_handle = NULL;
	ProdLic_HeapDestroy();  // free memory
	getlicense_ok = FALSE;
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

// call this function when the daemon is started, and only once!!
CAMLprim value alloc_and_set_cache_dir_c(value dir)
{
	CAMLparam1(dir);
	w_address = malloc(128 * sizeof(wchar_t));
	w_edition = malloc(4 * sizeof(wchar_t));
	w_profile = malloc(128 * sizeof(wchar_t));
	w_dbv = malloc(10 * sizeof(wchar_t));
	w_cache_dir = malloc(256 * sizeof(wchar_t));
	swprintf(w_cache_dir, 256, L"%s", String_val(dir));
	CAMLreturn(Val_unit);
}

// LPE callbacks

int LSConnStatusChange(MFLIC_LICENSE_POLICY_OBJECT_HANDLE h, void *d,
	BOOL up, WCHAR *name, DWORD port)
{
	if (DEBUG) printf("license-server %s\n", up ? "up" : "down");
	if (up)
		write(callback_pid, "u", 1);
	else
		write(callback_pid, "d", 1);
	return 0;
}

int LicExpired(MFLIC_LICENSE_POLICY_OBJECT_HANDLE h, void *d)
{
	if (DEBUG) printf("license expired\n");
	write(callback_pid, "e", 1);
	return 0;
}

// C-functions exported to OCaml

// initialise and start the LPE, and check out a license
CAMLprim value initialise_c(value address, value port, value edition,
	value profile, value dbv)
{
	MFLIC_STATUS result;
	MFLIC_OOBG_INPUT_DATA oobg = {1, L"STD,ADV,ENT,PLT"};
	
	WCHAR *VendorStr = NULL;
	BOOL OverDraft = 0;
	MFLIC_REQUEST_STATUS reqStatus = 0;
	int errcode = 0;
	MFLIC_LICENSE_GIVEN pLicenseGiven = 0;
	
	// protect params from OCaml GC and declare return value
	CAMLparam5(address, port, edition, profile, dbv);

	// tuple for return value:
	// 0: successfully licensed (bool)
	// 1: grace license (bool)
	// 2: days-to-expire (or -1 for "permanent" or "unknown")
	CAMLlocal1(tuple);
	tuple = caml_alloc(4, 0);
	// defaults
	Store_field(tuple, 0, Val_bool(FALSE));	// licensed or not
	Store_field(tuple, 1, Val_bool(FALSE));	// grace or not
	Store_field(tuple, 2, Val_int(-1));		// days to expire
	Store_field(tuple, 3, Val_int(-1));		// GetLicense status code
	
	// obtain and convert parameters	
	swprintf(w_address, 128, L"%s", String_val(address));
	swprintf(w_edition, 4, L"%s", String_val(edition));
	swprintf(w_profile, 128, L"%s", String_val(profile));
	swprintf(w_dbv, 10, L"%s", String_val(dbv));
	
	DWORD d_port = Int_val(port);
	
	if (DEBUG) printf("INITIALISE\n%ls, %d, %ls, %ls, %ls\n", w_address, d_port,
		w_edition, w_profile, w_dbv);
		
	// initialise LPE		
	if (DEBUG) printf("initialising...\n");	
	ProdLic_HeapCreate();	// memory allocation for the LPE object's private heap
	ConfDBInit();			// Initialises the database file which acts as a
							// Windows registry file. This creates a database file
							// in the directory that the application runs in
	result = MFLic_InitPolicyEng(&lpe_handle,
		L"CXS-LPE",		// LPE name
		L"CXS-LPE",		// LPE name
		w_address,		// address of license server
		d_port,			// port of license server
		NULL,			// just use NULL
		L"CXS",			// product name
		L"CXS",			// product name
		L"CXS",			// product name
		w_edition,		// component/edition (STD|ADV|ENT|PLT)
		w_dbv,			// data-based version (DBV)
		w_cache_dir,	// path to store cached data
		oobg,			// OOBGP settings
		PRODLIC_CCS_LICMODEL);	// licensing model (CCS)
		
	if (result != MFLIC_SUCCESS) {
		if (DEBUG) printf("!! error %d\n", result);
		cleanup();
		CAMLreturn(tuple);
	}
	if (DEBUG) printf("    ok\n");
	
	if (lpe_handle != NULL) {
		// register call backs
		
		MFLIC_LICPOLENG_CALLBACKS callbacks;
		memset(&callbacks, 0, sizeof(callbacks));
	
		callbacks.pfnLSConnStatusChange = &LSConnStatusChange;
		callbacks.pfnLicExpired = &LicExpired;

		if (DEBUG) printf("registering callbacks...\n");
		result = MFLic_RegisterCallBacks(lpe_handle, &callbacks, sizeof(callbacks));

		if (result != MFLIC_SUCCESS) {
			if (DEBUG) printf("!! error %d\n", result);
			cleanup();
			CAMLreturn(tuple);
		}
		if (DEBUG) printf("    ok\n");
	
		// start up
	
		if (DEBUG) printf("starting LPE...\n");
		result = MFLic_StartPolicyEng(lpe_handle);

		if (result != MFLIC_SUCCESS) {
			if (DEBUG) printf("!! error %d\n", result);
			cleanup();
			CAMLreturn(tuple);
		}
		if (DEBUG) printf("    ok\n");
	
		// get license

		if (DEBUG) printf("requesting license...\n");
		result = MFLic_GetLicense2(lpe_handle,
			w_profile,
			&VendorStr,
			&OverDraft,
			&reqStatus,
			&errcode,
			&pLicenseGiven);
				
		if (result != MFLIC_SUCCESS) {
			if (DEBUG) printf("!! error %d\n", result);
			cleanup();
			CAMLreturn(tuple);
		}
		if (DEBUG) printf("    ok\n    ");
			
		switch (reqStatus) {
		case LIC_GRANTED:
			Store_field(tuple, 0, Val_bool(TRUE));
			getlicense_ok = TRUE;
			Store_field(tuple, 3, Val_int(0));
			if (DEBUG) printf("license granted\n");
			break;
		case LIC_GRANTED_REF_COUNTED:
			Store_field(tuple, 0, Val_bool(TRUE));
			getlicense_ok = TRUE;
			Store_field(tuple, 3, Val_int(1));
			if (DEBUG) printf("existing license used\n");
			break;
		case LIC_REJECTED:
			getlicense_ok = FALSE;
			// release is needed, as license request is kept in LPE
			if (DEBUG) printf("releasing license... (%ls)\n", w_profile);
			result = MFLic_RelLicense2(lpe_handle, w_profile);
			if (result != MFLIC_SUCCESS)
				if (DEBUG) printf("    failed\n");
			else
				if (DEBUG) printf("    ok\n");
			Store_field(tuple, 3, Val_int(2));
			if (DEBUG) printf("license checkout rejected\n");
			break;
		case LS_COMM_FAILURE:
			getlicense_ok = FALSE;
			Store_field(tuple, 3, Val_int(3));
			if (DEBUG) printf("comm failure during checkout\n");
			break;
		case LIC_REQ_TIMEOUT:
			getlicense_ok = FALSE;
			Store_field(tuple, 3, Val_int(4));
			if (DEBUG) printf("timeout during checkout\n");
			break;
		default:
			getlicense_ok = FALSE;
			Store_field(tuple, 3, Val_int(5));
			if (DEBUG) printf("unknown failure during checkout\n");
			break;
		}
	
		if (DEBUG) printf("    ");
		switch (pLicenseGiven) {
		case NONE_GIVEN:
			if (DEBUG) printf("no license given\n");
			break;
		case TRANSIENT:
			if (DEBUG) printf("transient license\n");
			break;
		case REAL:
			if (DEBUG) printf("real license\n");
			Store_field(tuple, 1, Val_bool(FALSE));
			break;
		case GRACE:
			if (DEBUG) printf("grace license\n");
			Store_field(tuple, 1, Val_bool(TRUE));
			break;
		}
		
		// get days-to-expire from annoyance profile

		if (DEBUG) printf("checking days-to-expire...\n");
	
		APHELPER_AP_INFO *apinfo = NULL;
		result = MFLic_GetAPInfo(lpe_handle, (void**) &apinfo);
	
		if (result == MFLIC_SUCCESS && apinfo != NULL) {
			Store_field(tuple, 2, Val_int(apinfo->daysToExpire));
			if (DEBUG) printf("    days to expire: %d\n", apinfo->daysToExpire);
		}
		else {
			Store_field(tuple, 2, Val_int(-1));
			if (DEBUG) printf("    expiry date is unknown or license is permanent\n");
		}
	}
	
	CAMLreturn(tuple);
}

// check in license, and shut down and clean up the LPE
CAMLprim value shutdown_c(void)
{
	MFLIC_STATUS result;
	
	// release license
	if (getlicense_ok && w_profile != NULL && lpe_handle != NULL) {
		if (DEBUG) printf("releasing license... (%ls)\n", w_profile);
		result = MFLic_RelLicense2(lpe_handle, w_profile);
	
		if (result != MFLIC_SUCCESS) {
			if (DEBUG) printf("!! error %d\n", result);
			cleanup();
			return Val_bool(FALSE);
		}
		if (DEBUG) printf("    ok\n");
	}
	
	// shutdown
	
	if (lpe_handle != NULL) {
		if (DEBUG) printf("shutting down LPE...\n");
		result = MFLic_ShutdownPolicyEng(lpe_handle);
	
		if (result != MFLIC_SUCCESS) {
			if (DEBUG) printf("!! error %d\n", result);
			cleanup();
			return Val_bool(FALSE);
		}
		if (DEBUG) printf("    ok\n");
	}
	
	// cleanup
	
	if (lpe_handle != NULL) {
		if (DEBUG) printf("cleaning up...\n");
		result = MFLic_CleanupPolicyEng(lpe_handle);
	
		if (result != MFLIC_SUCCESS) {
			if (DEBUG) printf("!! error %d\n", result);
			cleanup();
			return Val_bool(FALSE);
		}
		
		lpe_handle = NULL;
		if (DEBUG) printf("    ok\n");
	}
	
	cleanup();

	// return success/failure
	return Val_bool(TRUE);
}

