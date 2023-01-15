
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>  
#include <string.h> 

#include "abc_000_warning.h"

#include "abc_ide_util.h" 



#undef LZW_COMPAT              

#define LZW_CHECKEOS            

#define MAXCODE(n)	((1L<<(n))-1)

#define BITS_MIN        9               
#define BITS_MAX        12              

#define CODE_CLEAR      256             
#define CODE_EOI        257             
#define CODE_FIRST      258             
#define CODE_MAX        MAXCODE(BITS_MAX)
#define HSIZE           9001L           
#define HSHIFT          (13-8)
#ifdef LZW_COMPAT

#define CSIZE           (MAXCODE(BITS_MAX)+1024L)
#else
#define CSIZE           (MAXCODE(BITS_MAX)+1L)
#endif


typedef struct {
	

	unsigned short  nbits;          
	unsigned short  maxcode;        
	unsigned short  free_ent;       
	unsigned long   nextdata;       
	long            nextbits;       

	int             rw_mode;        
} LZWBaseState;

#define lzw_nbits       base.nbits
#define lzw_maxcode     base.maxcode
#define lzw_free_ent    base.free_ent
#define lzw_nextdata    base.nextdata
#define lzw_nextbits    base.nextbits


typedef uint16_t hcode_t;			
typedef struct {
	long	hash;
	hcode_t	code;
} hash_t;


typedef struct code_ent {
	struct code_ent *next;
	unsigned short	length;		
	unsigned char	value;		
	unsigned char	firstchar;	
} code_t;


typedef struct {
	LZWBaseState base;

	
	long    dec_nbitsmask;		
	long    dec_restart;		
#ifdef LZW_CHECKEOS
	uint64_t  dec_bitsleft;		
	int64_t old_tif_rawcc;         
#endif
	
	code_t* dec_codep;		
	code_t* dec_oldcodep;		
	code_t* dec_free_entp;		
	code_t* dec_maxcodep;		
	code_t* dec_codetab;		

	
	int     enc_oldcode;		
	long    enc_checkpoint;		
#define CHECK_GAP	10000		
	long    enc_ratio;		
	long    enc_incount;		
	long    enc_outcount;		
	uint8_t*  enc_rawlimit;		
	hash_t* enc_hashtab;		
} LZWCodecState;

#define LZWState(tif)		((LZWBaseState*) (tif)->tif_data)
#define DecoderState(tif)	((LZWCodecState*) LZWState(tif))
#define EncoderState(tif)	((LZWCodecState*) LZWState(tif))

 



#ifdef LZW_CHECKEOS

#define	NextCode(_tif,_sp,_bp,_code,_get) {				\
if ((_sp)->dec_bitsleft < (uint64_t)nbits) { \
	r_printf("LZWDecode: Strip not terminated with EOI code"); \
	_code=CODE_EOI;					\
} \
else {	\
	_get(_sp,_bp,_code);					\
	(_sp)->dec_bitsleft -=nbits;				\
}								\
}
#else
#define	NextCode(tif,sp,bp,code,get) get(sp,bp,code)
#endif

#define	GetNextCode(sp,bp,code) {				\
	nextdata=(nextdata<<8)|*(bp)++;			\
	nextbits+=8;						\
	if (nextbits < nbits) {					\
		nextdata=(nextdata<<8)|*(bp)++;		\
		nextbits+=8;					\
	}							\
	code=(hcode_t)((nextdata >> (nextbits-nbits)) & nbitsmask);	\
	nextbits -=nbits;					\
}


static LZWCodecState* LZWSetupDecode(void)
{
	LZWCodecState* sp=NULL;
	int code;


		
	sp=(LZWCodecState*)malloc(sizeof(LZWCodecState));

	sp->dec_codetab=NULL;
	

		
		


	

	if (sp->dec_codetab==NULL) {
		sp->dec_codetab=(code_t*)malloc(CSIZE*sizeof (code_t));
		if (sp->dec_codetab==NULL) {
			r_printf("No space for LZW code table");
			return (0);
		}
		
		code=255;
		do {
			sp->dec_codetab[code].value=(unsigned char)code;
			sp->dec_codetab[code].firstchar=(unsigned char)code;
			sp->dec_codetab[code].length=1;
			sp->dec_codetab[code].next=NULL;
		} while (code--);
		
		
		
		memset(&sp->dec_codetab[CODE_CLEAR],0,
			(CODE_FIRST - CODE_CLEAR) * sizeof (code_t));
	}
	return (sp);
}


static int LZWPreDecode(LZWCodecState* sp )
{
 
 
	
	if (sp->dec_codetab==NULL)
	{
		
		if (sp->dec_codetab==NULL)
			return (0);
	}

	

{
		sp->lzw_maxcode=MAXCODE(BITS_MIN) - 1;
		
	}
	sp->lzw_nbits=BITS_MIN;
	sp->lzw_nextbits=0;
	sp->lzw_nextdata=0;

	sp->dec_restart=0;
	sp->dec_nbitsmask=MAXCODE(BITS_MIN);
#ifdef LZW_CHECKEOS
	sp->dec_bitsleft=0;
	sp->old_tif_rawcc=0;
#endif
	sp->dec_free_entp=sp->dec_codetab+CODE_FIRST;
	
	memset(sp->dec_free_entp,0,(CSIZE - CODE_FIRST)*sizeof (code_t));
	sp->dec_oldcodep=&sp->dec_codetab[-1];
	sp->dec_maxcodep=&sp->dec_codetab[sp->dec_nbitsmask - 1];
	return (1);
}



int  LZWDecode_TIFF(uint8_t* tif_rawcp,uint8_t* op0,
int64_t tif_rawcc,int64_t occ0 )
{
	LZWCodecState * sp=LZWSetupDecode();
	LZWPreDecode(sp);

	char *op=(char*)op0;
	long occ=(long)occ0;
	char *tp;
	unsigned char *bp;
	hcode_t code;
	int len;
	long nbits,nextbits,nbitsmask;
	unsigned long nextdata;
	code_t *codep,*free_entp,*maxcodep,*oldcodep;

	
	
	

	
	if ((int64_t)occ !=occ0)
		return (0);
	
	if (sp->dec_restart) {
		long residue;

		codep=sp->dec_codep;
		residue=codep->length - sp->dec_restart;
		if (residue > occ) {
			
			sp->dec_restart+=occ;
			do {
				codep=codep->next;
			} while (--residue > occ && codep);
			if (codep) {
				tp=op+occ;
				do {
					*--tp=codep->value;
					codep=codep->next;
				} while (--occ && codep);
			}
			return (1);
		}
		
		op+=residue;
		occ -=residue;
		tp=op;
		do {
			int t;
			--tp;
			t=codep->value;
			codep=codep->next;
			*tp=(char)t;
		} while (--residue && codep);
		sp->dec_restart=0;
	}

	bp=(unsigned char *) tif_rawcp;
#ifdef LZW_CHECKEOS
	sp->dec_bitsleft+=(((uint64_t) tif_rawcc - sp->old_tif_rawcc) << 3);
#endif
	nbits=sp->lzw_nbits;
	nextdata=sp->lzw_nextdata;
	nextbits=sp->lzw_nextbits;
	nbitsmask=sp->dec_nbitsmask;
	oldcodep=sp->dec_oldcodep;
	free_entp=sp->dec_free_entp;
	maxcodep=sp->dec_maxcodep;

	while (occ > 0) {
		NextCode(tif,sp,bp,code,GetNextCode);
		if (code==CODE_EOI)
			break;
		if (code==CODE_CLEAR) {
			do {
				free_entp=sp->dec_codetab+CODE_FIRST;
				
				memset(free_entp,0,
					(CSIZE - CODE_FIRST) * sizeof (code_t));
				nbits=BITS_MIN;
				nbitsmask=MAXCODE(BITS_MIN);
				maxcodep=sp->dec_codetab+nbitsmask - 1;
				NextCode(tif,sp,bp,code,GetNextCode);
			} while (code==CODE_CLEAR);	
			if (code==CODE_EOI)
				break;
			if (code > CODE_CLEAR) {
				r_printf("LZWDecode: Corrupted LZW table at scanline  d");
				return (0);
			}
			*op++=(char)code;
			occ--;
			oldcodep=sp->dec_codetab+code;
			continue;
		}
		codep=sp->dec_codetab+code;

		
		if (free_entp < &sp->dec_codetab[0]||
			free_entp >=&sp->dec_codetab[CSIZE]) {
			r_printf(			"Corrupted LZW table at scanline");
			return (0);
		}

		free_entp->next=oldcodep;
		if (free_entp->next < &sp->dec_codetab[0]||
			free_entp->next >=&sp->dec_codetab[CSIZE]) {
			r_printf("Corrupted LZW table at scanline ");
			return (0);
		}
		free_entp->firstchar=free_entp->next->firstchar;
		free_entp->length=free_entp->next->length+1;
		free_entp->value=(codep < free_entp) ?
			codep->firstchar : free_entp->firstchar;
		if (++free_entp > maxcodep) {
			if (++nbits > BITS_MAX)		
				nbits=BITS_MAX;
			nbitsmask=MAXCODE(nbits);
			maxcodep=sp->dec_codetab+nbitsmask - 1;
		}
		oldcodep=codep;
		if (code >=256) {
			
			if (codep->length==0) {
				r_printf(
					"Wrong length of decoded string: "
					"data probably corrupted at scanlin "
					 );
				return (0);
			}
			if (codep->length > occ) {
				
				sp->dec_codep=codep;
				do {
					codep=codep->next;
				} while (codep && codep->length > occ);
				if (codep) {
					sp->dec_restart=(long)occ;
					tp=op+occ;
					do  {
						*--tp=codep->value;
						codep=codep->next;
					} while (--occ && codep);
					if (codep)
						r_printf(	"Bogus encoding,loop in the code table; scanline ");
				}
				break;
			}
			len=codep->length;
			tp=op+len;
			do {
				int t;
				--tp;
				t=codep->value;
				codep=codep->next;
				*tp=(char)t;
			} while (codep && tp > op);
			if (codep) {
				r_printf("Bogus encoding,loop in the code table; scanline ");
				break;
			}
			
			op+=len;
			occ -=len;
		}
		else {
			*op++=(char)code;
			occ--;
		}
	}

	
	
#ifdef LZW_CHECKEOS
	sp->old_tif_rawcc=tif_rawcc;
#endif
	sp->lzw_nbits=(unsigned short)nbits;
	sp->lzw_nextdata=nextdata;
	sp->lzw_nextbits=nextbits;
	sp->dec_nbitsmask=nbitsmask;
	sp->dec_oldcodep=oldcodep;
	sp->dec_free_entp=free_entp;
	sp->dec_maxcodep=maxcodep;

	if (occ > 0) {

		r_printf("Not enough data at scanline  (shortbytes)");
		return (0);
	}
	free(sp);
	return (1);
}














#include "abc_000_warning.h"
