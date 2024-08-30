#include "abc_000_warning.h"
#include "abc_000_macro.h"
volatile char ctrl_C_Pressed=0;
#if ( defined(OS_WIN64)||defined (OS_WIN32)) && defined (COMPILER_MSVC)
#define WIN32_LEAN_AND_MEAN
#include "windows.h"
#include <signal.h>
#include <stdlib.h>
#include "abc_ide_util.h"
static void signal_callback_handler(int signum) {
	ctrl_C_Pressed=1;
	printf("CTRL+C is pressed!\n");	
}
void RegisterCtrlCHandler() {
	_crt_signal_t prev=signal(SIGINT,signal_callback_handler); 
}
#endif
#include "abc_000_warning.h"
