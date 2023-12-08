#include "abc_000_macro.h"
#include "abc_000_warning.h"
#include "abc_001_config.h"
#include "abc_datatype.h"
 #if M_INTERFACE==1 &&  !defined(MSVC_COMPILER)
	#include "inttypes.h"
	#include "mex.h"
	#if defined(WIN_OS)
		extern Bool ioFlush(void)  asm("?ioFlush@@YA_NXZ");
	#elif defined(MAC_OS)
		extern Bool ioFlush(void)  asm("__Z7ioFlushv");
	#else 
		extern Bool ioFlush(void)  asm("_Z7ioFlushv");
	#endif
    void matlab_IOflush(void)	{
		#ifndef O_INTERFACE
			ioFlush();
		#endif
	}
#else
static char achar UNUSED_DECORATOR='c';
#endif
#include "abc_000_warning.h"
