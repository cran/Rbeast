#include "abc_000_macro.h"
#include "abc_000_warning.h"
#if defined(OS_WIN64) 
#include "abc_timer.h"
#include "abc_win32_demo.h"
GlobalStruct gData;
LParam	     threadParam;
Logger       logger;
Style        style;
HICON   hIcon;
HWND    hButton[4],hScroll,hEdit,hStatic[2],hDiag;
HBITMAP hBitmap[5]={ 0,0 },hBufferBitmap[5]={ 0,0 };
HDC     memDC[5]={ 0,0 },bufferDC[5]={ 0,0 };
HBRUSH  blackBrush=0,grayBrush=0;
HPEN	greenPen=0,bluePen=0,yellowPen=0,redPen=0;
void EnableButtons(HWND *hButton,HWND *hDiag)
{
	U08 isDone=(gData.status==DONE);
	if (hButton !=NULL)
	{
		EnableWindow(hButton[0],gData.status !=RUN||gData.status==DONE);
		EnableWindow(hButton[1],(gData.status==RUN) && (gData.status !=DONE));
		EnableWindow(hButton[2],isDone);
		EnableWindow(hButton[3],TRUE);
		if ((gData.status==RUN)||(gData.status==PAUSE))
			SetWindowText(hButton[0],"Resume");
		if (gData.status==DONE)
			SetWindowText(hButton[0],"Re-run");
	}
	if (hDiag !=NULL)
	{
		EnableWindow(hDiag[0],isDone);
		for (int i=0; i < 8; i++)
		{
			HWND hwnd=GetDlgItem(hDiag[0],i);
			EnableWindow(hwnd,isDone);
			hwnd=GetDlgItem(hDiag[0],i+100);
			EnableWindow(hwnd,isDone);
		}
		EnableWindow(GetDlgItem(hDiag[0],200),isDone);
		EnableWindow(GetDlgItem(hDiag[0],201),isDone);
	}
}
void ResizeControls(int x,int y,int N)
{
	F32 sep=style.sep;
	F32 mgr=style.hMargin;
	F32 width,height,x0,y0,xExt,yExt;
	width=style.wButton;	
	yExt=style.hgtButtonBar;
	height=yExt*style.vButtonRatio;
	y0=(yExt -height )/2;	
	x0=mgr;
	for (int i=0; i < 4; i++)	{		
		SetWindowPos(hButton[i],NULL,x0,y0,width,height,SWP_NOZORDER);
		x0=x0+(width+sep);
	}
	x0=x0 - sep;
	xExt=x - x0 -mgr;
	int   len;
	len=3 * style.xChar+style.xCapChar;
	width=xExt - 2 * (len+2 * style.labelGap);
	height=yExt*style.vScrollRatio;
	y0=(yExt - height)/2;
	x0=x0+(len+2 * style.labelGap);		
	SetWindowPos(hScroll,NULL,x0,y0,width,height,SWP_NOZORDER);
	x0=mgr+4 * style.wButton+3 * sep+style.labelGap;
	height=style.yChar;
	y0=(yExt - height)/2;
	width=len;
	SetWindowPos(hStatic[0],NULL,x0,y0,width,height,SWP_NOZORDER);
	x0=x - mgr - (len+style.labelGap);
    SetWindowPos(hStatic[1],NULL,x0,y0,width,height,SWP_NOZORDER);
	x0=mgr;
	y0=y*(1 - style.rEdit);
	height=y* style.rEdit;
	width=x - 2 * mgr;
	SetWindowPos(hEdit,NULL,x0,y0,width,height,SWP_NOZORDER);
	int vGap=10;
	width=x - 2 * mgr;
	yExt=y - y*style.rEdit - style.hgtButtonBar - (N+1)*vGap;
	y0=style.hgtButtonBar+vGap;	
	for (int i=0; i < N; i++)
	{
		gData.w[i]=width;
		gData.h[i]=yExt * style.rFig[i];
		gData.x0[i]=mgr;
		gData.y0[i]=y0;
		y0=y0+gData.h[i]+vGap;
	} 
}
void CreateGDIObject(HWND hwnd,int N)
{
	HDC  hdc;
	RECT rect;
	GetClientRect(hwnd,&rect);
	ResizeControls(rect.right-rect.left,rect.bottom - rect.top,N);
	if (blackBrush==0)
	{
		blackBrush=CreateSolidBrush(RGB(30,33,40));
		grayBrush=CreateSolidBrush(RGB(125,125,125));
		greenPen=CreatePen(PS_SOLID,1,RGB(0,205,1));
		bluePen=CreatePen(PS_SOLID,1,RGB(0,5,201));
		redPen=CreatePen(PS_SOLID,1,RGB(181,45,69));
		yellowPen=CreatePen(PS_SOLID,2,RGB(255,255,0));
	}
	hdc=GetDC(hwnd);
	for (int i=0; i < N; i++)
	{
		if (hBitmap[i] !=0)
			DeleteDC(memDC[i]),DeleteObject(hBitmap[i]);
		hBitmap[i]=CreateCompatibleBitmap(hdc,gData.w[i],gData.h[i]);
		memDC[i]=CreateCompatibleDC(hdc);
		SelectObject(memDC[i],hBitmap[i]);
		SelectObject(memDC[i],greenPen);
		SelectObject(memDC[i],blackBrush);
		SetBkMode(memDC[i],TRANSPARENT);
		SetTextColor(memDC[i],RGB(155,155,155));
		if (i==0)
		{
			if (hBufferBitmap[i] !=0)
			{
				DeleteDC(bufferDC[i]);
				DeleteObject(hBufferBitmap[i]);
			}
			hBufferBitmap[i]=CreateCompatibleBitmap(hdc,gData.w[i],gData.h[i]);
			bufferDC[i]=CreateCompatibleDC(hdc);
			SelectObject(bufferDC[i],hBufferBitmap[i]);
			SelectObject(bufferDC[i],redPen);
			SelectObject(bufferDC[i],blackBrush);
		}
	}	
	ReleaseDC(hwnd,hdc);
}
void DeleteGDIObject( int N)
{
	for (int i=0; i < N; i++)
	{
		DeleteDC(memDC[i]);
		DeleteObject(hBitmap[i]);
		if (i==0)
		{
			DeleteDC(bufferDC[i]);
			DeleteObject(hBufferBitmap[i]);
		}
	}
	DeleteObject(blackBrush);
	DeleteObject(grayBrush);
	DeleteObject(greenPen);
	DeleteObject(bluePen);
	DeleteObject(redPen);
	DeleteObject(yellowPen);
}
void ResizeDialogControls(HWND hwnd)
{
	RECT rc;
	GetClientRect(hwnd,&rc);
	int x=rc.right - rc.left;
	int y=rc.bottom - rc.top;
	int rowHeight=y/style.numRows;
	int lastRowHeight=y - rowHeight*(style.numRows - 1);
	F32 width,height,x0,y0,xExt,yExt;
	F32 labelWidth=style.xChar*(lstrlen("trendMaxKnotNum")+2);
	for (int i=0; i < 8; i++)	{
		HWND hc;
		hc=GetDlgItem(hwnd,i);
		x0=5;
		y0=i*rowHeight+rowHeight*(1 - style.fractionLabel)/2;
		width=labelWidth;
		height=rowHeight*style.fractionLabel;
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
		hc=GetDlgItem(hwnd,i+100);
		x0=labelWidth+30;
		y0=i*rowHeight+rowHeight*(1 - style.fractionEdit)/2;
		width=x-x0-5;
		height=rowHeight*style.fractionEdit;
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
	}
	{
		HWND hc;
		hc=GetDlgItem(hwnd,200);
		F32 r=0.25;
		width=r*x;
		height=lastRowHeight*style.fractionEdit;
		y0=y-lastRowHeight+lastRowHeight*(1 - style.fractionEdit)/2;
		x0=x*0.25/2;
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
		hc=GetDlgItem(hwnd,201);				
		x0=x*(0.5+0.25/2);
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
	}
}
void BEAST2_ResizeDialogControls(HWND hwnd)
{
	RECT rc;
	GetClientRect(hwnd,&rc);
	int x=rc.right - rc.left;
	int y=rc.bottom - rc.top;
	int rowHeight=y/style.numRows;
	int lastRowHeight=y - rowHeight*(style.numRows - 1);
	F32 width,height,x0,y0,xExt,yExt;
	F32 labelWidth=style.xChar*(lstrlen("trendMaxKnotNum")+4);
	for (int i=0; i < style.numRows-1; i++)	{
		HWND hc;
		hc=GetDlgItem(hwnd,i);
		int yBaseShift=20;
		x0=5;
		y0=yBaseShift+i*rowHeight+rowHeight*(1 - style.fractionLabel)/2;
		width=labelWidth;
		height=rowHeight*style.fractionLabel;
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
		hc=GetDlgItem(hwnd,i+100);
		x0=labelWidth+30;
		y0=yBaseShift+i*rowHeight+rowHeight*(1 - style.fractionEdit)/2;
		width=x-x0-5;
		height=rowHeight*style.fractionEdit;
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
	}
	{
		HWND hc;
		hc=GetDlgItem(hwnd,200);
		F32 r=0.25;
		width=r*x;
		height=lastRowHeight*style.fractionEdit;
		y0=y-lastRowHeight+lastRowHeight*(1 - style.fractionEdit)/2;
		x0=x*0.25/2;
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
		hc=GetDlgItem(hwnd,201);				
		x0=x*(0.5+0.25/2);
		SetWindowPos(hc,NULL,x0,y0,width,height,SWP_NOZORDER);
	}
}
void LoggerInsert(char *newMsg)
{
	int lenMsg=lstrlen(newMsg);
	if ((lenMsg+logger.len) >=logger.LEN)
	{
		char *tmp;
		int   newBytes;
		newBytes=max(lenMsg,1000);
		tmp=malloc(logger.LEN+newBytes+1);
		memcpy(tmp,logger.str,logger.len+1);
		free(logger.str);
		logger.str=tmp;
		logger.LEN=logger.LEN+newBytes;
	}
	memcpy(logger.str+logger.len,newMsg,lenMsg);
	logger.len+=lenMsg;
	*(logger.str+(logger.len+1) - 1)=0;
	SetWindowText(hEdit,logger.str);
	SendMessage(hEdit,EM_LINESCROLL,0,100);
}
HWND GetConsoleHwnd(void)
{ 
#define MY_BUFSIZE 1024 
	HWND hwndFound;         
	char pszNewWindowTitle[MY_BUFSIZE]; 
	char pszOldWindowTitle[MY_BUFSIZE]; 
	GetConsoleTitle(pszOldWindowTitle,MY_BUFSIZE);
	wsprintf(pszNewWindowTitle,"%d/%d",GetTickCount(),GetCurrentProcessId());
	SetConsoleTitle(pszNewWindowTitle);
	Sleep_ms(20);
	hwndFound=FindWindow(NULL,pszNewWindowTitle);
	SetConsoleTitle(pszOldWindowTitle);
	return(hwndFound);
}
#if defined(COMPILER_MSVC)
#include <io.h> 
void SetupConsole()
{
	BOOL bCreated=AllocConsole();
	HANDLE hConOut;
	if (!bCreated)
		return; 
	hConOut=GetStdHandle(STD_OUTPUT_HANDLE);
	int fd=_open_osfhandle((intptr_t)hConOut,0);
	_dup2(fd,1);
	HWND hwnd=GetConsoleHwnd();
	MoveWindow(hwnd,500,500,800,600,TRUE);
}
#endif
BYTE ANDmaskIcon[]={ 0xFF,0xFF,0xFF,0xFF,   
0xFF,0xFF,0xC3,0xFF,   
0xFF,0xFF,0x00,0xFF,   
0xFF,0xFE,0x00,0x7F,   
0xFF,0xFC,0x00,0x1F,   
0xFF,0xF8,0x00,0x0F,   
0xFF,0xF8,0x00,0x0F,   
0xFF,0xF0,0x00,0x07,   
0xFF,0xF0,0x00,0x03,   
0xFF,0xE0,0x00,0x03,   
0xFF,0xE0,0x00,0x01,   
0xFF,0xE0,0x00,0x01,   
0xFF,0xF0,0x00,0x01,   
0xFF,0xF0,0x00,0x00,   
0xFF,0xF8,0x00,0x00,   
0xFF,0xFC,0x00,0x00,   
0xFF,0xFF,0x00,0x00,   
0xFF,0xFF,0x80,0x00,   
0xFF,0xFF,0xE0,0x00,   
0xFF,0xFF,0xE0,0x01,   
0xFF,0xFF,0xF0,0x01,   
0xFF,0xFF,0xF0,0x01,   
0xFF,0xFF,0xF0,0x03,   
0xFF,0xFF,0xE0,0x03,   
0xFF,0xFF,0xE0,0x07,   
0xFF,0xFF,0xC0,0x0F,   
0xFF,0xFF,0xC0,0x0F,   
0xFF,0xFF,0x80,0x1F,   
0xFF,0xFF,0x00,0x7F,   
0xFF,0xFC,0x00,0xFF,   
0xFF,0xF8,0x03,0xFF,   
0xFF,0xFC,0x3F,0xFF };  
BYTE XORmaskIcon[]={ 0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x38,0x00,   
0x00,0x00,0x7C,0x00,   
0x00,0x00,0x7C,0x00,   
0x00,0x00,0x7C,0x00,   
0x00,0x00,0x38,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00,   
0x00,0x00,0x00,0x00 };  
#else
static char fileID UNUSED_DECORATOR='c';
#endif
#include "abc_000_warning.h"
