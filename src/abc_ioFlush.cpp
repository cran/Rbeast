#include "abc_000_macro.h"
#include "abc_000_warning.h"

#if M_INTERFACE ==1
	#include "inttypes.h"
	#include "mex.h"
	extern bool ioFlush(void);
	extern "C"  void matlab_IOflush(void)	{
		ioFlush();
	}

	////https://stackoverflow.com/questions/10529500/what-does-this-mean-int-a
	/*
		in C++, (int &) is a type punning that can force converstion of a variable to int
	*/
	 
#else
static char achar UNUSED_DECORATOR ='c';
#endif

#include "abc_000_warning.h"



