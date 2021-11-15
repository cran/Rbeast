
#include "abc_000_macro.h"
#ifndef WARNING_SWITCH
	#define WARNING_SWITCH
    DISABLE_MANY_WARNINGS
#else
    #undef sign    
    #undef warning 
	ENABLE_MANY_WARNINGS 	
#endif
