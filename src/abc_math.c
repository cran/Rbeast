#include "math.h"
#include "abc_000_warning.h"
#include "abc_math.h"
F32 fastlog1(F32 x)
{
	register union { F32 f; U32 i; } vx={ x };
	register union{ U32 i; F32 f; } mx={ (vx.i & 0x007FFFFF)|0x3f000000 };
	vx.f=(F32)vx.i* 1.1920928955078125e-7f*0.69314718f;
	vx.f=vx.f - 124.22551499f*0.69314718f	- 1.498030302f*0.69314718f * mx.f
			- 1.72587999f*0.69314718f/(0.3520887068f+mx.f);
	return vx.f;
}
F32 fastlog2(F32 x) {
	register union { F32 f; U32 i; } vx={ x };
	register union { U32 i; F32 f; } mx={ (vx.i & 0x007FFFFF)|0x3f000000 };
	vx.f=(F32) ( (double)vx.i* (double)(1.1920928955078125e-7f*0.69314718f) );
	vx.f=vx.f - 124.2098722217282f*0.69314718f - 1.502704726439149f*0.69314718f * mx.f
				- 1.746553042329640f*0.69314718f/(0.356745518976678f+mx.f);
	return vx.f;
}
F32 fastlog(F32 x)
{
	register union { F32 f; U32 i; } vx={ x };
	register union { U32 i; F32 f; } mx={ (vx.i & 0x007FFFFF)|0x3f000000 };
	vx.f=(F32)vx.i* (1.1920928955078125e-7f*0.69314718f);
	vx.f=vx.f - 125.5233166734556f*0.69314718f+mx.f*(-0.413356886671142+mx.f*(-0.472721975352920f+0.078018528401178f*mx.f))*0.69314718f+
		-0.787757784962750f*0.69314718f/(0.1781810261970705f+mx.f);
	return vx.f;
}
F32 sum_log_diag(F32PTR p,I32 K)
{
	F32 x=0;
	for (I32 i=0; i < K; i++)
	{
		register union { F32 f; U32 i; } vx={ *p };
		register union{ U32 i; F32 f; } mx={ (vx.i & 0x007FFFFF)|0x3f000000 };
		vx.f=(F32)vx.i* (1.1920928955078125e-7f*0.69314718f);
		vx.f=vx.f - 125.5233166734556f*0.69314718f+mx.f*(-0.413356886671142+mx.f*(-0.472721975352920+0.078018528401178*mx.f))*0.69314718f+
			-0.787757784962750f*0.69314718f/(0.1781810261970705f+mx.f);
		x+=vx.f;
		p=p+K+1;
	}
	return x;
}
F32 sum_log_diagv2(F32PTR p,I32 K)
{
	F64 sumlog=0;
	F64 cumprod=1.;
	for (I32 i=0; i < K; i++)	{
		F64  x=*p;
		F64  cumprod_new=cumprod*x;
		if (cumprod_new > 1e-305 && cumprod_new < 1e305){
			cumprod=cumprod_new;
		} else	{
			sumlog+=log(cumprod);
			cumprod=x;
		}
		p=p+K+1;
	}
	sumlog+=log(cumprod);
	return (F32) sumlog;
}
F32 sumlog(F32PTR p,I32 K)
{
	F64 sumlog=0;
	F64 cumprod=1.;
	for (I32 i=0; i < K; i++) {
		F64  x=*p++;
		F64  cumprod_new=cumprod * x;
		if (cumprod_new > 1e-305 && cumprod_new < 1e305) {
			cumprod=cumprod_new;
		} else	{
			sumlog+=log(cumprod);
			cumprod=x;
		}	 
	}
	sumlog+=log(cumprod);
	return (F32) sumlog;
}
F32 fastexp(F32 x){
	x=(x > 88.7f) ? 88.7f : x;
	x=(x < -51.f) ? -51.f : x;
	x=1.442695040f*x;	
	register float z=x - (F32)((int)x)+((x < 0) ? 1.0f : 0.0f);
	register  union { U32 i; F32 f; } v;
	v.i=(U32)     (    8388608.f * (x+121.2740575f+27.7280233f/(4.84252568f - z) - 1.49012907f * z)          );
	return v.f;
}
F32 fastsqrt (F32 x)
{
	register union {
		U32 i;
		F32   f;
	} v;
	v.f=x; 
	v.i -=1 << 23; 
	v.i >>=1; 
	v.i+=1 << 29; 
	v.f=(v.f+x/v.f);
	v.f=(v.f*0.25f+x/v.f);
	return v.f;
}
#include "abc_000_warning.h"
