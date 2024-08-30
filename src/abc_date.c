#include<inttypes.h>
#include <stdlib.h> 
#include <string.h> 
#include <math.h>   
#include "abc_000_warning.h"
#include "abc_date.h"
#include "abc_common.h"   
#include "abc_ide_util.h"  
#include "abc_vec.h"       
#include "abc_sort.h"     
#include<stdio.h>  
static const int DAYS_CUMSUM[2][13]={ 
	                                    { 0,0,31,59,90,120,151,181,212,243,273,304,334 },
										{ 0,0,31,60,91,121,152,182,213,244,274,305,335 }  
                                      };
static const int  DAYS_Per_MONTH[13]={   0,31,28,31,30,31,30,31,31,30,31,30,31 };
static const int  DAYS_Per_MONTH_LEAP[13]={   0,31,29,31,30,31,30,31,31,30,31,30,31 };
static int IsLeapYear(int year)                      { return (year%4==0 && year%100 !=0)||(year%400==0);}
static int GetNumDays(int year)                      { return IsLeapYear(year) ? 366 : 365;          }
static int YMD_to_intDOY(int year,int mon,int day) { return DAYS_CUMSUM[IsLeapYear(year)][mon]+day; }
static int YMD_from_intDOY_ag1(int doy,int y,int* M,int *D) {
	int isleap=IsLeapYear(y) ;
	int doy_from_march1st=doy - (59+isleap) - 1;                  
	doy_from_march1st=PostiveMod(doy_from_march1st,365+isleap ); 
	unsigned mp=(5 * doy_from_march1st+2)/153;                  
	unsigned d=doy_from_march1st - (153 * mp+2)/5+1;           
	unsigned m=mp+(mp < 10 ? 3 : -9);                  
	*D=d;
	*M=m;
	return 0;
}
static int YMD_from_intDOY_ag2(int doy,int y,int* M,int* D) {
	int *days_per_month=IsLeapYear(y) ? DAYS_Per_MONTH_LEAP : DAYS_Per_MONTH;
	int mon;
	for (mon=1; mon <=12; mon++) {
		if (doy <=days_per_month[mon])
			break;
		doy -=days_per_month[mon];
	}
	*M=mon;
	*D=doy;
	return 0;
}
static int IsDateValid(int year,int mon,int day ) {
	if (year <  -9999||year > 9999)
		return 0;
	if (!(mon >=1 && mon <=12))
		return 0;
	int DAYS=DAYS_Per_MONTH[mon];
	if (mon==2) 	{
		int is_leap=(year%4==0 && year%100 !=0)||(year%400==0);
		DAYS=is_leap ? 29 : 28;
	}
	return day<=DAYS;
}
#define OneDay  1.0
#define HalfDay 0.5
double FracYear_from_intYDOY(int year,int doy) {
	return (double)year+((doy - OneDay)+HalfDay)/(double)GetNumDays(year);
}
int FracYear_to_intYDOY(double fyear,int* yr) {
	int    year=floor(fyear);
	double fraction=fyear - year;	
	int    doy=(int)floor(fraction * GetNumDays(year))+OneDay; 
	if (yr) yr[0]=year;
	return doy;
}
double FracYear_from_YDOY(int year,double doy) {
	return (double)year+doy/(double)GetNumDays(year);
}
double FracYear_to_YDOY(double fyear,int* yr) {
	int    year=floor(fyear);
	double fraction=fyear - year;	
	double doy=fraction * GetNumDays(year); 
	if (yr) yr[0]=year;
	return doy;
}
double FracYear_from_YMD(int  year,int mon,int day) {
	return FracYear_from_intYDOY(year,YMD_to_intDOY(year,mon,day));
}
void  FracYear_to_YMD(double fyear,int *yr,int*mon,int *day) {
	int doy=FracYear_to_intYDOY(fyear,yr);	 
	YMD_from_intDOY_ag1(doy,yr[0],mon,day);
}
double FracDay_from_HMS(int h,int m,double sec) {
 	   return (h+m/60.+sec/3600.)/24.;
}
void  FracDay_to_HMS(double fday,int*h,int* m,double *sec) {
	double hr=fday * 24.;
	double mn=(hr - (int)hr) * 60.;
	double secs=(mn - (int)mn) * 60;
	*h=(int) hr;
	*m=(int) mn;
	*sec=secs;
}
int64_t CountLeapYears(int64_t year) {
	static int64_t fakeLeaps=(1753/4) - (1753/100)+(1753/400);
	int64_t leaps=year/4;
	int64_t badLeaps=year/100;
	int64_t extraLeaps=year/400;
	return leaps - badLeaps+extraLeaps - fakeLeaps;
}
int64_t DateNum(int year,int mon,int day) {
	int64_t numYears=year - 1753;
	int64_t leapYears=CountLeapYears(year);
	int64_t daysInYear=DAYS_CUMSUM[IsLeapYear(year)][mon];
	return  numYears * 365LL+leapYears+daysInYear+(day - 1);
}
int JulianDayNum_from_civil_ag1(int y,int m,int d) {
	y -=m <=2;
	const int      era=(y >=0 ? y : y - 399)/400;
	const unsigned yoe=(y - era * 400);      
	const unsigned doy=(153 * (m+(m > 2 ? -3 : 9))+2)/5+d - 1;  
	const unsigned doe=yoe * 365+yoe/4 - yoe/100+doy;         
	return       era * 146097+doe - 719468+2440588; 
}
int JulianDayNum_from_civil_ag2(int y,int m,int d) {
	y  -=m <=2;
	m=m <=2 ? (m-2+12) : m - 2;
	m=m - 1;
	int JDN=365 * y+FLOORdiv(y,4) - FLOORdiv(y,100)+FLOORdiv(y,400)+(153 * m+2)/5+d+1721119;
	return JDN;
}
int JulianDayNum_from_civil_ag3(int y,int m,int d) {
	y -=m <=2;
	m=m <=2 ? m+12 : m  ;	 
	int JDN=365 * y+FLOORdiv(y,4) - FLOORdiv(y,100)+FLOORdiv(y,400)+(153 * m -457)/5+d+1721119;
	return JDN;
}
int JulianDayNum_from_civil_ag4(int y,int m,int d) {
	int A=((int)m - 14)/12;  
	int B=1461 * (y+4800+A);
	int C=367 * (m - 2 - 12 * A);
	int E=(y+4900+A)/100;
	int JDN=B/4+C/12 - (3 * E)/4+d - 32075;
	return JDN;
}
int JulianDayNum_from_julian_ag1(int y,int m,int d) {
	y -=m <=2;
	const int      era=(y >=0 ? y : y - 3)/4;
	const unsigned yoe=(y - era * 4);      
	const unsigned doy=(153 * (m+(m > 2 ? -3 : 9))+2)/5+d - 1;  
	const unsigned doe=yoe * 365+doy;         
	return era * 1461+doe - 719470+2440588; 
}
int JulianDayNum_from_julian_ag2(int y,int m,int d) {
	y -=m <=2;
	m=m <=2 ? m+12 : m;
	int JDN=365 * y+FLOORdiv(y,4)+(153 * m - 457)/5+d+1721117;
	return JDN;
}
int JulianDayNum_from_julian_ag3(int y,int m,int d) {
	int JDN=367 * y - (7 * (y+5001+((int)m - 9)/7))/4+(275 * m)/9+d+1729777;
	return JDN;
}
int JulianDayNum_to_Civil_ag1(int JDN,int * yr,int*mn,int* day)  {
	JDN+=719468 - 2440588; 
	const int era=(JDN >=0 ? JDN : JDN - 146096)/146097;
	const unsigned doe=(JDN - era * 146097);          
	const unsigned yoe=(doe - doe/1460+doe/36524 - doe/146096)/365;  
	const int      y=(yoe)+era * 400;
	const unsigned doy=doe - (365 * yoe+yoe/4 - yoe/100);                
	const unsigned mp=(5 * doy+2)/153;                                   
	const unsigned d=doy - (153 * mp+2)/5+1;                             
	const unsigned m=mp < 10 ? mp+3 : mp - 9;                            
	*yr=y+(m <=2);
	*mn=m;
	*day=d;
	return 0;
}
int JulianDayNum_to_Civil_ag2(int JDN,int * yr,int*mn,int* day)  {
	JDN=JDN - 1721120;
	int Q1=FLOORdiv(JDN,146097);
	int R1=JDN - 146097 * Q1;
	int tmp=FLOORdiv(R1,36524);
	int Q2=min(tmp,3);
	int R2=R1 - 36524 * Q2;
	int Q3=FLOORdiv(R2,1461);
	int R3=R2 - 1461 * Q3;
	tmp=FLOORdiv(R3,365);
	int Q4=min(tmp,3);
	int R4=R3 - 365 * Q4;
	*yr=400 * Q1+100 * Q2+4 * Q3+Q4;
	int m=FLOORdiv(5 * R4+2,153);
	*day=R4 - FLOORdiv(153 * m+2,5)+1;
	m=m+3;
	*yr=m <=12? *yr: (*yr)+1;
	*mn=m <=12 ? m : m - 12;
	return 0;
}
int JulianDayNum_to_Civil_ag3(int JDN,int* yr,int* mn,int* day) {
	int L=JDN+68569;
	int N=FLOORdiv(4*L,146097);
	L=L - (146097 * N+3)/4;
	int I=(4000 * (L+1))/1461001;
	L=L - (1461 * I)/4+31;
	int J=(80 * L)/2447;
	int D=L - (2447 * J)/80;
	L=J/11;
	int M=J+2 - 12 * L;
	int Y=100 * (N - 49)+I+L;
	*yr=Y;
	*mn=M;
	*day=D;
	return 0;
}
int JulianDayNum_to_julian_ag1(int JDN,int* yr,int* mn,int* day) {
	int z=JDN - 2440588;
	z+=719470;
	int era=(z >=0 ? z : z - 1460)/1461;
	unsigned doe=(z - era * 1461);  
	unsigned yoe=(doe - doe/1460)/365;                 
	int y=(int)(yoe)+era * 4;
	unsigned doy=doe - 365 * yoe;                        
	unsigned mp=(5 * doy+2)/153;                         
	unsigned d=doy - (153 * mp+2)/5+1;                   
	unsigned m=mp+(mp < 10 ? 3 : -9);                  
	y=y+(m <=2);
	*yr=y;
	*mn=m;
	*day=d;
	return 0;
}
int JulianDayNum_to_julian_ag2(int JDN,int* yr,int* mn,int* day) {
	int J=JDN+1402;
	int K=(J - 1)/1461;
	int L=J - 1461 * K;
	int N=(L - 1)/365 - L/1461;
	int  I=L - 365 * N+30;
	J=FLOORdiv(80 * I,2447);
	int D=I - (2447 * J)/80;
	I=FLOORdiv(J,11);
	int M=J+2 - 12 * I;
	int Y=4 * K+N+I - 4716;
	*yr=Y;
	*mn=M;
	*day=D;
	return 0;
}
double  FracYear_to_DateNum(double fyear) {
	int    year_int=floor(fyear);
	double year_frac=fyear - year_int;
	int    days_in_year=GetNumDays(year_int);
	double doy=year_frac * days_in_year;
	int    doy_int=floor(doy)+OneDay;
	double doy_frac=doy - floor(doy);
	int mon,day;
	YMD_from_intDOY_ag1(doy_int,year_int,&mon,&day);
	double out=JulianDayNum_from_civil_ag1(year_int,mon,day);
	return out+doy_frac - NULL_DATE_ORIGIN;
}
double FracYear_from_DateNum(double datenum) {
	int    datenum_int=floor(datenum);
	double datenum_frac=datenum- datenum_int;
	int    yr_int,mon,day;
	JulianDayNum_to_Civil_ag1(datenum+NULL_DATE_ORIGIN,&yr_int,&mon,&day);
	int  doy_int=YMD_to_intDOY(yr_int,mon,day);
	double out=(double)yr_int+((doy_int - OneDay)+datenum_frac)/(double)GetNumDays(yr_int);
	return out;
}
double  FracYear_to_JDate(double fyear) {
	return FracYear_to_DateNum(fyear) - 0.5;
}
double FracYearfrom_JDate(double jdn) {
	return FracYear_to_DateNum(jdn+0.5);
}
int WeekDay(int y,int m,int d) {
	int jdn=JulianDayNum_from_civil_ag1(y,m,d);
	int dow=jdn%7;
	dow=dow < 0 ? dow+7 : dow; 
	return dow;
}
double FracYear_from_YMDHMS(YmdHms * date) {
	int days_in_year=GetNumDays(date->y);
	int doy1_int=YMD_to_intDOY(date->y,date->m,date->d);
	 double fracday=FracDay_from_HMS(date->hr,date->min,date->sec);
	 return (double)date->y+(doy1_int-OneDay+fracday)/(double)days_in_year;
}
void  FracYear_to_YMDHMS(double fyear,YmdHms* date) {
	int    yr_int=floor(fyear);
	double yr_fract=fyear - yr_int;
	int    days_in_yr=GetNumDays(yr_int);
	double doy=yr_fract * days_in_yr;
	int    doy1_int=floor(doy)+OneDay;
	double doy_frac=doy -floor(doy);
	int mon,day;  
	YMD_from_intDOY_ag1(doy1_int,yr_int,&mon,&day);
	int   hr,minute;
	double sec;
	FracDay_to_HMS(doy_frac,&hr,&minute,&sec);
	date->y=yr_int;
	date->m=mon;
	date->d=day;
	date->hr=hr;
	date->min=minute;
	date->sec=sec;
}
void print_date(YmdHms* date) {
	r_printf("%4d-%2d-%2d|%2d:%2d:%g",date->y,date->m,date->d,(int)date->hr,(int)date->min,date->sec);
}
void Julian_to_Civil(int y,int m,int d,YmdHms* date) {
	int datenum=JulianDayNum_from_julian_ag1(y,m,d);
	JulianDayNum_to_Civil_ag1(datenum,&date->y,&date->m,&date->d);	
}
void Civil_to_Julian(int y,int m,int d,YmdHms* date) {
	int datenum=JulianDayNum_from_civil_ag1(y,m,d);
	JulianDayNum_to_julian_ag1(datenum,&date->y,&date->m,&date->d);
}
void CivilDatee_Jump(int y,int m,int d,int jumpDays,int* y1,int* m1,int* d1) {
	int days=JulianDayNum_from_civil_ag1(y,m,d);
	JulianDayNum_to_Civil_ag1(days+jumpDays,y1,m1,d1);
}
void  JulianDate_to_civil(double datenum,YmdHms* date) {
	int    datenum_int=round(datenum);
	double datenum_frac=datenum - datenum_int+0.5;
	JulianDayNum_to_Civil_ag1(datenum_int,&date->y,&date->m,&date->d);
	FracDay_to_HMS(datenum_frac,&date->hr,&date->min,&date->sec);
}
double JulianDate_from_civil(YmdHms* date) {
	int datenum=JulianDayNum_from_civil_ag1(date->y,date->m,date->d);
	return  datenum+FracDay_from_HMS(date->hr,date->min,date->sec) - 0.5;
}
static int __FindTokenStart__( char *str,char * token) {
	char * pchar=strstr(str,token);
	if (pchar)	{
		return (int)(pchar - str);
	}
	return -10000;
}
int   GetStrPattern_fmt1(char* fmtstr,DateFmtPattern1* pattern) {
	ToUpper(fmtstr);
	int yearIdx=__FindTokenStart__(fmtstr,"YYYY");
	if (yearIdx <0) return 0;
	int monIdx=__FindTokenStart__(fmtstr,"MM");
	if (monIdx < 0) return 0;
	int dayIdx=__FindTokenStart__(fmtstr,"DD");
	if (dayIdx < 0) return 0;
	pattern->yearIdx=yearIdx;
	pattern->monIdx=monIdx;
	pattern->dayIdx=dayIdx;
	return 1;
}
double  Str2F32time_fmt1(char* datestr,DateFmtPattern1* pattern) {
	char s[5];
	memcpy(s,datestr+pattern->yearIdx,4);	s[4]=0;	int year=atoi(s);
	if (year==0) { return -1e10; }
	memcpy(s,datestr+pattern->monIdx,2); 	s[2]=0;	int mon=atoi(s);
	if (mon < 1||mon > 12) { return -1e10; }
	memcpy(s,datestr+pattern->dayIdx,2);  	s[2]=0;	int day=atoi(s);
	if (day < 1||day >31) { return -1e10; }
	return FracYear_from_YMD(year,mon,day);
}
int    GetStrPattern_fmt2(char* fmtstr,DateFmtPattern2* pattern) {
	ToUpper(fmtstr);
	int yearIdx=__FindTokenStart__(fmtstr,"YYYY");
	if (yearIdx <0) return 0;
	int doyIdx=__FindTokenStart__(fmtstr,"DOY");
	if (doyIdx < 0) return 0;
	pattern->yearIdx=yearIdx;
	pattern->doyIdx=doyIdx;
	return 1;
}
double  Str2F32time_fmt2(char* datestr,DateFmtPattern2* pattern) {
	char s[5];
	memcpy(s,datestr+pattern->yearIdx,4);	s[4]=0;	int year=atoi(s);
	if (year==0) { return -1e10; }
	memcpy(s,datestr+pattern->doyIdx,3); 	s[3]=0;	int doy=atoi(s);	
	if (doy < 0||doy > 366) { return -1e10; }	 
	return FracYear_from_intYDOY(year,doy);
}
static char* _FindCharOccurrence(char* s,char c,int* numTimes) {
	*numTimes=0;	 	
	char *pLast=NULL;
	while (( s=strchr(s,c)) !=NULL ) {
		pLast=s++;
		++*numTimes;				
	}	
	return pLast;
}
int    GetStrPattern_fmt3(char* fmtstr,DateFmtPattern3* pattern) {
	ToUpper(fmtstr);
	int nTimes;
	char* yearPt=_FindCharOccurrence(fmtstr,'Y',&nTimes);
	if (nTimes==0||nTimes > 1) return 0;
	char* monPt=_FindCharOccurrence(fmtstr,'M',&nTimes);
	if (nTimes==0||nTimes > 1) return 0;
	char* dayPt=_FindCharOccurrence(fmtstr,'D',&nTimes);
	if (nTimes==0||nTimes > 1) return 0;
	pattern->order[0]='Y';
	pattern->order[1]='M';
	pattern->order[2]='D';
	char* pts[]={ yearPt,monPt,dayPt };
	if (pts[0] > pts[1]) {
		char* tmp=pts[1];
		char tmpval=pattern->order[1];
		pts[1]=pts[0];
		pts[0]=tmp;		
		pattern->order[1]=pattern->order[0];
		pattern->order[0]=tmpval;
	}
	if (pts[1] > pts[2]) {
		char* tmp=pts[2];
		char  tmpval=pattern->order[2];
		pts[2]=pts[1];
		pts[1]=tmp;		
		pattern->order[2]=pattern->order[1];
		pattern->order[1]=tmpval;
		if (pts[0] > pts[1]) {
			char* tmp=pts[1];
			char tmpval=pattern->order[1];
			pts[1]=pts[0];
			pts[0]=tmp;			
			pattern->order[1]=pattern->order[0];
			pattern->order[0]=tmpval;
		}
	}
	int64_t len;
	len=(pts[1] - 1) - (pts[0]+1)+1;
	if (len <=0) return 0;
	memcpy(pattern->sep1,pts[0]+1,len); 	pattern->sep1[len]=0;
	len=(pts[2] - 1) - (pts[1]+1)+1 ;
	if (len <=0) return 0;
	memcpy(pattern->sep2,pts[1]+1,len);  	pattern->sep2[len]=0;
	return 1;
}
double  Str2F32time_fmt3(char* datestr,DateFmtPattern3* pattern) {
	int   N=(int) strlen(datestr);
	char  old;
	char* p0=datestr;
	char *p1=strstr(p0,pattern->sep1);
	if (p1==NULL) return -1e10;
	old=p1[0]; p1[0]=0;
	int n1=atoi(p0);
	p1[0]=old;
	p0=p1+strlen(pattern->sep1);
	p1=strstr(p0,pattern->sep2);
	if (p1==NULL) return -1e10;
	old=p1[0]; p1[0]=0;
	int n2=atoi(p0);
	p1[0]=old;
	p0=p1+strlen(pattern->sep2);
	if (p0 >=datestr+N) return -1e10;
	int n3=atoi(p0);
	char* p=pattern->order;
	int year=p[0]=='Y' ? n1 : (p[1]=='Y' ? n2 : n3);
	int mon=p[0]=='M' ? n1 : (p[1]=='M' ? n2 : n3);
	int day=p[0]=='D' ? n1 : (p[1]=='D' ? n2 : n3);
	return FracYear_from_YMD(year,mon,day);
}
INLINE static int  is_dot(char c)          { return c=='.'; }
INLINE static int  is_slash(char c)        { return c=='/'; }
INLINE static int  is_star(char c)         { return c=='*'; }
INLINE static int  is_digit(char c)        { return c >='0' && c <='9';}
INLINE static int  is_letter(char c)       { return (c >='a' && c <='z' )||(c >='A' && c <='Z');}
INLINE static int  is_alphanumeric(char c) { return (c >='0' && c <='9')||(c >='a' && c <='z')||(c >='A' && c <='Z'); }
INLINE static char char_toupper(char s)    { return s >='a' && s <='z' ? s - 32 : s; }
int get_word_size(char* s)         { int i=0;	while (is_letter(s[i++])){};  	     return --i; }
int get_alphanumeric_size(char* s) { int i=0;	while (is_alphanumeric(s[i++])) {};  return --i;}
int get_intger_size(char* s)       { int i=0; while (is_digit(s[i++])) {};	 	 return --i; }
int get_slash_size(char* s)        { int i=0; while (is_slash(s[i++])) {};	 	 return --i; }
int get_star_size(char* s)         { int i=0; while (is_star(s[i++])) {};	 	     return --i; }
int get_number_size(char* s,int * ndots) {
	int i=*ndots=0;
	while (is_digit(s[i])||is_dot(s[i])) {
		*ndots+=is_dot(s[i]);
		i++;
	};
	return i;
}
char* goto_validchar(char* s)           { while (!is_alphanumeric(*s) && *s !=0) { s++; }	return s; }
char* goto_validchar_dot_slash_star(char* s) {
	while (!is_alphanumeric(*s) && !is_dot(*s) && !is_slash(*s) && !is_star(*s) && *s !=0) {
		s++; 
	}	return s;
}
#define TOKEN_NUMBER   'N'
#define TOKEN_NUMBERwithTEXT   'A'
#define TOKEN_SLASH    '/'
#define TOKEN_STAR    '*'
#define TOKEN_WORD     'L'
static int split_numstr(char* s,int nPartMax,int* startidx,int* nchar,char* type) {
	char* s0=s;
	int   nPart=0;
	while (*s !=0 && nPart < nPartMax) {
		s=goto_validchar_dot_slash_star(s);
		if (is_digit(*s)||is_dot(*s) ) {
			int ndots;
			int nlen=get_number_size(s,&ndots);
			if (ndots >=2) {
				return -1L;
			}
			nchar[nPart]=nlen;
			startidx[nPart]=s - s0;
			type[nPart]=TOKEN_NUMBER;  
			nPart++;
			s=s+nlen;
		}
		else if (is_letter(*s)) {
			int nlen=get_word_size(s);
			nchar[nPart]=nlen;
			startidx[nPart]=s - s0;
			type[nPart]=TOKEN_WORD;       
			nPart++;
			s=s+nlen;
		}
		else if (is_slash(*s)) {
			int nlen=get_slash_size(s);
			nchar[nPart]=nlen;
			startidx[nPart]=s - s0;
			type[nPart]=TOKEN_SLASH;    
			nPart++;
			s=s+nlen;
		}
		else if (is_star(*s)) {
			int nlen=get_star_size(s);
			nchar[nPart]=nlen;
			startidx[nPart]=s - s0;
			type[nPart]=TOKEN_STAR;    
			nPart++;
			s=s+nlen;
		}
	}
	return nPart;
}
double extract_timeinterval_from_str(char* s,float *value,char *unit) {
	int  nPartMax=10;
	int  startIdx[10],nChar[10];
	char type[10]={ 0,};
	int  nPart=split_numstr(s,nPartMax,startIdx,nChar,type);
	double nan=(1.0/0.) * 0.;
	if (nPart==0||type[0] !=TOKEN_NUMBER||type[nPart-1] !=TOKEN_WORD) {
		if (value) *value=nan;
		return nan;
	}
	double x=nan;
	int    operation=0;
	for (int i=0; i < nPart - 1; i++) {
		if (type[i]=='N') {
			char* ss=s+startIdx[i];
			int   len=nChar[i];
			char  old=ss[len];
			ss[len]=0;
			double curValue=atof(ss); ss[len]=old;
			if (operation==0) {
				x=curValue;
			}
			else if (operation==TOKEN_STAR) {
				x=x * curValue;
				operation=0;
			}
			else if (operation==TOKEN_SLASH) {
				x=x/curValue;
				operation=0;
			}
		}
		else if (type[i]==TOKEN_STAR) {
			operation=TOKEN_STAR;
		}
		else if (type[i]==TOKEN_SLASH) {
			operation=TOKEN_SLASH;
		}		 
	}
	if (x !=x) {
		return nan;
	}
	char* ss=s+startIdx[nPart-1];
	char  letter1=char_toupper(ss[0]);
	char  letter2=char_toupper(ss[1]);
	double y=x;
	double z=nan;
	if (letter1=='D') {
		double nyr1=y/366;
		double nyr2=y/365;
		if      (_IsAlmostInteger(nyr1) ) 	z=nyr1;
		else if (_IsAlmostInteger(nyr2)) 	z=nyr2;
		else  	                            z=y/365 ;
	} else if (letter1=='M' && letter2=='O') {
		z=y/12;	  	
	} else if (letter1=='M' && letter2=='N') {
	    z=y/12;
	}
	else if (letter1=='M' && letter2=='I') {
		z=y/(60*24*365);
		letter1='I'; 
	}
	else if (letter1=='M'  ) {
		z=y/12; 
	}
	else if (letter1=='H') {
		z=y/(24*365);
	}
	else if (letter1=='S') {
		z=y/(3600*24*365);
	}
	else if (letter1=='Y') {
		z=y  ;
	} 
	else if (letter1=='W') {
		z=y*7/365.f;
	} else{
		letter1='U';
		z=nan;
	}
	if (value) *value=y;
	if (unit ) *unit=letter1;
	return z;
}
int split_datestr(char* s,int nPartMax,int* startidx,int* nchar,char* type) {
	char* s0=s;
	int   nPart=0;
	while (*s !=0 && nPart < nPartMax) {
		s=goto_validchar(s);
		if (is_digit(*s)) {
			int nlen=get_intger_size(s);
			nchar[nPart]=nlen;
			startidx[nPart]=s - s0;
			type[nPart]=TOKEN_NUMBER;
			if (startidx[nPart] > 0 && is_letter(s0[startidx[nPart]-1])) {
				type[nPart]=TOKEN_NUMBERwithTEXT;
			}
			if (is_letter(s[nlen])) {
				type[nPart]=TOKEN_NUMBERwithTEXT;
			}		 
			nPart++;
			s=s+nlen;
		} else if (is_letter(*s)) {
			int nlen=get_word_size(s);
			nchar[nPart]=nlen;
			startidx[nPart]=s - s0;
			type[nPart]=TOKEN_WORD;
			nPart++;
			s=s+nlen;
		}
	}
	return nPart;
}
static int cmp_months(char* s) {
	static char* months[]={ "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
	for (int i=0; i < 12;++i) {
		int diff=strcicmp_nfirst(s,months[i],3);
		if (diff==0) {
			return i+1;
		}
	}
	return -1;
}
int  FracYear_from_Strings(F64PTR out,char *s,int * strstart,int n) {
    #define NPARTMAX 16
	int* startidx=malloc(sizeof(int) * ( n * NPARTMAX * 2  ));
	int* partlength=startidx+n * NPARTMAX;
	int  i=0; 
	char parttype[NPARTMAX]={ 0 };
	int  nPart=split_datestr(s+strstart[i],NPARTMAX,startidx+i * NPARTMAX,partlength+i * NPARTMAX,parttype);
	for (i=1; i < n; i++) {
		char newparttype[16]={ 0 };
		int  newnPart=split_datestr(s+strstart[i],NPARTMAX,startidx+i * NPARTMAX,partlength+i * NPARTMAX,newparttype);
		if (nPart !=newnPart||memcmp(parttype,newparttype,NPARTMAX) !=0) {
			free(startidx);
			r_printf("ERROR: the input date strings have inconsisent formats and cann't be automatically parsed. Use time$datestr and time$strfmat to specify the format.\n");
			return NULL;
		}			
	}
	int nNumber=0,nWord=0,nANumber=0,nNumberANumber=0,nNumber8=0,nNumber7=0;
	int idxNumber[NPARTMAX],idxWord[NPARTMAX],idxANumber[NPARTMAX],idxNumANumber[NPARTMAX],idxNumber8[NPARTMAX],idxNumber7[NPARTMAX];
	int partLenMin[NPARTMAX],partLenMax[NPARTMAX];
	for (int i=0; i < nPart; i++) {
		if (parttype[i]==TOKEN_NUMBER) {
			idxNumber[nNumber++]=i;
			idxNumANumber[nNumberANumber++]=i;			
		}else if (parttype[i]==TOKEN_NUMBERwithTEXT) {
			idxANumber[nANumber++]=i;
			idxNumANumber[nNumberANumber++]=i;		
		}else if (parttype[i]==TOKEN_WORD) {
			idxWord[nWord++]=i;
		}
		int* partLengthRow=partlength+i;
		partLenMin[i]=*partLengthRow;
		partLenMax[i]=*partLengthRow;
		for (int j=0; j < n; j++) {
			partLenMin[i]=min(partLenMin[i],*partLengthRow);
			partLenMax[i]=max(partLenMax[i],*partLengthRow);	
			partLengthRow+=NPARTMAX;
		}
		if (partLenMin[i]==8 && partLenMax[i]==8 && parttype[i] !=TOKEN_WORD) {
			idxNumber8[nNumber8++]=i;	 
		}
		if (partLenMin[i]==7 && partLenMax[i]==7 && parttype[i] !=TOKEN_WORD) {
			idxNumber7[nNumber7++]=i;
		}
	}
	int* year=malloc(sizeof(int) * n * 4);
	int* month=year+n;
	int* day=month+n;
	int* tmp=day+n;
	int DONE=0;
	if (nNumberANumber==3||nNumber==3) {
		int yearFound=0,monthFound=0,dayFound=0;			 
		for (int J=0; J < 3;++J) {
			int   minv=999999,maxv=-999999;
			F64   meanv=0;
			int  idx=nNumberANumber==3 ? idxNumANumber[J] : idxNumber[J];
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				char  old=ss[slen]; ss[slen]=0; int value=atoi(ss); ss[slen]=old;
				meanv+=value;
				if (minv > value) minv=value;
				if (maxv < value) maxv=value;
				tmp[i]=value;
			}
			if ( !yearFound && partLenMin[idx]==4 && partLenMax[idx]==4) {				
					yearFound=1;
					memcpy(year,tmp,sizeof(int) * n);
					continue;				
			}
			if (!monthFound && (maxv==12||maxv==11) && partLenMax[idx] < 4) {				
					monthFound=1;
					memcpy(month,tmp,sizeof(int) * n);
					continue;				
			}
			if (!dayFound && (maxv >=28 && maxv <=31 ) && partLenMax[idx] < 4) {
					dayFound=1;
					memcpy(day,tmp,sizeof(int) * n);
					continue;				
			}
			if (minv < 1||maxv > 12) {
				if (minv < 1||maxv > 31 ) {
					if (yearFound==0) {
						yearFound=1;
						memcpy(year,tmp,sizeof(int) * n);
					}
				}	else {
					if (dayFound==0) {
						dayFound=1;
						memcpy(day,tmp,sizeof(int) * n);
					}
				}
			} else {
				if (monthFound==0) {
					monthFound=1;
					memcpy(month,tmp,sizeof(int) * n);
				}
			}
			if (yearFound && monthFound && maxv <=31 && minv >=1) {
				if (dayFound==0) {
					dayFound=1;
					memcpy(day,tmp,sizeof(int) * n);
				}
			}
		} 
		if (yearFound==1 && monthFound==1 && dayFound==1) {
			DONE=1;
		}
	}
	if (nNumberANumber==2||nNumber==2 ) {
		int yearFound=0,monthFound=0,dayFound=0;			 
		for (int J=0; J < 2;++J) {
			int   minv=999999,maxv=-999999;
			F64   meanv=0;
			int   idx=nNumberANumber==2 ? idxNumANumber[J] : idxNumber[J];
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				char  old=ss[slen]; ss[slen]=0; int value=atoi(ss); ss[slen]=old;
				meanv+=value;
				if (minv > value) minv=value;
				if (maxv < value) maxv=value;
				tmp[i]=value;
			}
			if (partLenMin[idx]==4 && partLenMax[idx]==4) {
				if (yearFound==0) {
					yearFound=1;
					memcpy(year,tmp,sizeof(int) * n);
					continue;
				}
			}
			if ((maxv==12||maxv==11) && partLenMax[idx] <=2) {
				if (monthFound==0) {
					monthFound=1;
					memcpy(month,tmp,sizeof(int) * n);
					continue;
				}
			}
			if (maxv <=12 && minv >=1 && partLenMax[idx] <=2) {
				if (monthFound==0) {
					monthFound=1;
					memcpy(month,tmp,sizeof(int) * n);
				}
			} 
		}
		if (yearFound==1 && monthFound==1 ) {
			DONE=3;
		}
	}
	if (!DONE && (nNumberANumber==2||nNumber==2) && nWord==1) {
		int yearFound=0,monthFound=0,dayFound=0;			 
		for (int J=0; J < 2;++J) {
			int   minv=999999,maxv=-999999;
			F64   meanv=0;
			int   idx=nNumberANumber==2 ? idxNumANumber[J] : idxNumber[J];
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				char  old=ss[slen]; ss[slen]=0; int value=atoi(ss); ss[slen]=old;
				meanv+=value;
				if (minv > value) minv=value;
				if (maxv < value) maxv=value;
				tmp[i]=value;
			}
			if (minv >=1 && maxv <=31 && partLenMin[idx]<4) {
				if (dayFound==0) {
					dayFound=1;
					memcpy(day,tmp,sizeof(int) * n);
				}
			} 
			else if (partLenMin[idx] >=4) {
				if (yearFound==0) {
					yearFound=1;
					memcpy(year,tmp,sizeof(int) * n);
				}
			}		 
		} 
		int allMatched=1;
		int idx=idxWord[0];
		for (int i=0; i < n;++i) {
			int   sidx=*(startidx+i * NPARTMAX+idx);
			int   slen=*(partlength+i * NPARTMAX+idx);
			char* ss=s+strstart[i]+sidx;
			char  old=ss[slen]; ss[slen]=0; int value=cmp_months(ss); ss[slen]=old;
			tmp[i]=value;
			if (value < 0) {
				allMatched=0;
				break;
			}
		}
		if (allMatched) {
			monthFound=1;
			memcpy(month,tmp,sizeof(int) * n);
		}
		if (yearFound==1 && monthFound==1 && dayFound==1) {
			DONE=1;
		}
	}
	if (!DONE && (nNumberANumber==1) && nWord==1) {
		int yearFound=0,monthFound=0,dayFound=0;			 
		for (int J=0; J < 1;++J) {
			int   minv=999999,maxv=-999999;
			F64   meanv=0;
			int   idx=idxNumANumber[J];
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				char  old=ss[slen]; ss[slen]=0; int value=atoi(ss); ss[slen]=old;
				meanv+=value;
				if (minv > value) minv=value;
				if (maxv < value) maxv=value;
				tmp[i]=value;
			}
			if (partLenMin[idx]==4 && partLenMax[idx]==4) {
				if (yearFound==0) {
					yearFound=1;
					memcpy(year,tmp,sizeof(int) * n);
					continue;
				}
			}
		} 
		int allMatched=1;
		int idx=idxWord[0];
		for (int i=0; i < n;++i) {
			int   sidx=*(startidx+i * NPARTMAX+idx);
			int   slen=*(partlength+i * NPARTMAX+idx);
			char* ss=s+strstart[i]+sidx;
			char  old=ss[slen]; ss[slen]=0; int value=cmp_months(ss); ss[slen]=old;
			tmp[i]=value;
			if (value < 0) {
				allMatched=0;
				break;
			}
		}
		if (allMatched) {
			monthFound=1;
			memcpy(month,tmp,sizeof(int) * n);
		}
		if (yearFound==1 && monthFound==1  ) {
			DONE=3;
		}
	}
	if (!DONE && nNumber8 > 0) {
		for (int J=0; J < nNumber8;++J) {		 
			int  idx=idxNumber8[J];
			int  zero='0';
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				year[i]=(ss[0] - zero) * 1000+(ss[1] - zero) * 100+(ss[2] - zero) * 10+(ss[3] - zero);
				month[i]=(ss[4] - zero) * 10+(ss[5] - zero) * 1;
				day[i]=(ss[6] - zero) * 10+(ss[7] - zero) * 1;
			}
			int minv,maxv;
			i32_maxidx(year,n,&maxv);
			i32_minidx(year,n,&minv);
			if (maxv > 8000) continue;
			i32_maxidx(month,n,&maxv);
			i32_minidx(month,n,&minv);
			if (minv < 1||maxv>12) {
				continue;
			}
			i32_maxidx(day,n,&maxv);
			i32_minidx(day,n,&minv);
			if (minv < 1||maxv>31) {
				continue;
			}
			DONE=1;
			break;		 
		}
	}
	if (!DONE && nNumber8 > 0) {
		for (int J=0; J < nNumber8;++J) {
			int  idx=idxNumber8[J];
			int  zero='0';
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				year[i]=(ss[0] - zero) * 1000+(ss[1] - zero) * 100+(ss[2] - zero) * 10+(ss[3] - zero);
				day[i]=(ss[4] - zero) * 10+(ss[5] - zero) * 1;
				month[i]=(ss[6] - zero) * 10+(ss[7] - zero) * 1;
			}
			int minv,maxv;
			i32_maxidx(year,n,&maxv);
			i32_minidx(year,n,&minv);
			if (maxv > 8000) continue;
			i32_maxidx(month,n,&maxv);
			i32_minidx(month,n,&minv);
			if (minv < 1||maxv>12) {
				continue;
			}
			i32_maxidx(day,n,&maxv);
			i32_minidx(day,n,&minv);
			if (minv < 1||maxv>31) {
				continue;
			}
			DONE=1;
			break;
		}
	}
	if (!DONE && nNumber7 > 0) {
		for (int J=0; J < nNumber7;++J) {
			int  idx=idxNumber7[J];
			int  zero='0';
			for (int i=0; i < n;++i) {
				int   sidx=*(startidx+i * NPARTMAX+idx);
				int   slen=*(partlength+i * NPARTMAX+idx);
				char* ss=s+strstart[i]+sidx;
				year[i]=(ss[0] - zero) * 1000+(ss[1] - zero) * 100+(ss[2] - zero) * 10+(ss[3] - zero);
				day[i]=(ss[4] - zero) * 100+(ss[5] - zero) * 10+(ss[6] - zero) * 1;
			}
			int minv,maxv;
			i32_maxidx(year,n,&maxv);
			i32_minidx(year,n,&minv);
			if (maxv > 4000) continue;
			i32_maxidx(day,n,&maxv);
			i32_minidx(day,n,&minv);
			if (minv < 1||maxv>366) {
				continue;
			}
			DONE=2;
			break;		 
		}
	}
	int Nout=0;
	if (DONE) {
		Nout=n;
		if (DONE==1) {
			q_printf("INFO: '%s' interpreted as %04d-%02d-%02d (Y-M-D)\n",s,year[0],month[0],day[0]);
			for (int i=0; i < n; i++) {
				out[i]=FracYear_from_YMD(year[i],month[i],day[i]);
			}
		} else if (DONE==2)	 {
			q_printf("INFO: '%s' interpreted as %04d-%03d (Year-DOY)\n",s,year[0],day[0]);
			for (int i=0; i < n; i++) {
				out[i]=FracYear_from_intYDOY(year[i],day[i]);
			}
		}
		else if (DONE==3) {
			q_printf("INFO: '%s' interpreted as %04d-%02d (Year-Month)\n",s,year[0],month[0]);
			for (int i=0; i < n; i++) {
				out[i]=year[i]+month[i]/12.0-1.0/24.0;
			}
		}
	}
	free(startidx);
	free(year);
	return Nout;
}
#include "abc_000_warning.h"
