#include "abc_000_warning.h"
#include "beastv2_header.h"
static I32 _PickBaisID_1none_2fix1_2fix01(PROP_DATA_PTR PROPINFO) {
	return 0;
}
static I32 _PickBaisID_______2fix0______(PROP_DATA_PTR PROPINFO) { return 1;}
static I32 _PickBaisID_______2none_3fix2(PROP_DATA_PTR PROPINFO) {
	BEAST2_RNDSTREAM *PRAND=PROPINFO->pRND;
	return (*PRAND->rnd08++> 128);
}
static I32 _PickBaisID_____________3none(PROP_DATA_PTR PROPINFO) {
	BEAST2_RNDSTREAM*	PRAND=PROPINFO->pRND;
	U08					unifRnd=*PRAND->rnd08++;
	I32 R1=unifRnd < (U08)(255 * 0.33);
	I32 R2=unifRnd < (U08)(255 * 0.66);
	return 2L-(R1+R2);
}
static I32 _PickBaisID_____________3fix1(PROP_DATA_PTR PROPINFO) {
	BEAST2_RNDSTREAM* PRAND=PROPINFO->pRND;
	U08				  unifRnd=*PRAND->rnd08++;
	return unifRnd < 128? 0:2; 
}
static I32 _PickBaisID_hasOutlier_2fix0(PROP_DATA_PTR PROPINFO) {
	if (*PROPINFO->samples==0) {
		return 0; 
	}
	return 1;
}
static I32 _PickBaisID_hasOutlier_2none(PROP_DATA_PTR PROPINFO) {
	if (*PROPINFO->samples==0) {return 0;}
    BEAST2_RNDSTREAM* PRAND=PROPINFO->pRND;
	return (*PRAND->rnd08++> 128)?0:1;
}
static I32 _PickBaisID_hasOutlier_3none(PROP_DATA_PTR PROPINFO) {
	BEAST2_RNDSTREAM*	PRAND=PROPINFO->pRND;
	U08					unifRnd=*PRAND->rnd08++;
	if (*PROPINFO->samples==0) {	return (unifRnd > 128);	}
	if		(unifRnd <  (U08)(255*0.33) )		return 0;
	else if (unifRnd < (U08)(255*0.66) )		return 1;
	else										return 2; 
}
static I32 _PickBaisID_hasOutlier_3fix0(PROP_DATA_PTR PROPINFO) {
	BEAST2_RNDSTREAM* PRAND=PROPINFO->pRND;
	U08				  unifRnd=*PRAND->rnd08++;
	if (*PROPINFO->samples==0) {return 1;}
	return (unifRnd > 128) ? 1 : 2;
}
static I32 _PickBaisID_hasOutlier_3fix1(PROP_DATA_PTR PROPINFO) {
	BEAST2_RNDSTREAM* PRAND=PROPINFO->pRND;
	U08				  unifRnd=*PRAND->rnd08++;
	if (*PROPINFO->samples==0) {	return 0;	}
	return (unifRnd > 128) ? 0 : 2;
}
static I32 _PickBaisID_hasOutlier_3fix01(PROP_DATA_PTR PROPINFO) {
	if (*PROPINFO->samples==0) { 
		return 0; 
	}
	return 2;
}
void * Get_PickBasisID(I08 numBasis,I08 hasOutlier,I32PTR isComponentFixed)
{
	if		(numBasis==1) 
		return _PickBaisID_1none_2fix1_2fix01;  
	else if (numBasis==2) {
		if (!hasOutlier) {
			if (isComponentFixed[0] && isComponentFixed[1]) 
				return _PickBaisID_1none_2fix1_2fix01;			 
			else if (isComponentFixed[0])                   
				return _PickBaisID_______2fix0______;
			else if (isComponentFixed[1])				    
				return _PickBaisID_1none_2fix1_2fix01;
			else                                            
				return _PickBaisID_______2none_3fix2;
		} else { 
			if (isComponentFixed[0])
				return _PickBaisID_hasOutlier_2fix0;		
			else 
				return _PickBaisID_hasOutlier_2none;
		}
	}
	else if (numBasis==3) {
		if (!hasOutlier) {
		}
		else { 
			if   (isComponentFixed[0] && isComponentFixed[1])  
				return _PickBaisID_hasOutlier_3fix01;
			else if  (isComponentFixed[0])                     
				return _PickBaisID_hasOutlier_3fix0;
			else if (isComponentFixed[1])                      
				return _PickBaisID_hasOutlier_3fix1;
			else                                               
				return _PickBaisID_hasOutlier_3none;
		}
	}
	return NULL;
}
#include "abc_000_warning.h"
