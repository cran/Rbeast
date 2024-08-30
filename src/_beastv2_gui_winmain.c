#include "abc_000_macro.h"
#include "abc_000_warning.h"
#if defined(OS_WIN64) 
#include <stdio.h>   
#include <io.h>      
#include <math.h>
#include "abc_win32_demo.h"
#include "beastv2_header.h"
#include "globalvars.h"
#include "abc_ide_util.h"
#include "beastv2_io.h"
static LRESULT CALLBACK DialogProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam);
static LRESULT CALLBACK WndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam);
static HICON CreateMyIcon(void) {
	static BYTE ANDmaskIcon[]={ 0xFF,0xFF,0xFF,0xFF,   
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
	static BYTE XORmaskIcon[]={ 0x00,0x00,0x00,0x00,   
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
	HINSTANCE   hInstance=GetModuleHandle(NULL);
	HICON hIcon=CreateIcon(
		hInstance,    
		32,              
		32,              
		1,               
		1,               
		ANDmaskIcon,     
		XORmaskIcon);    
	return hIcon;
}
static void  ClassRegisterWnd(char * wndClassName,HICON hIcon)
{
	HINSTANCE   hInstance=GetModuleHandle(NULL);
	WNDCLASSEX	wc;
	wc.cbSize=sizeof(WNDCLASSEX);
	wc.style=CS_DBLCLKS|CS_HREDRAW|CS_DROPSHADOW;
	wc.lpfnWndProc=WndProc;
	wc.cbClsExtra=0;
	wc.cbWndExtra=0;
	wc.hInstance=hInstance;
	wc.hIcon=hIcon; 
	wc.hCursor=LoadCursor(NULL,IDC_ARROW);
	wc.hbrBackground=(HBRUSH)(COLOR_WINDOW);
	wc.lpszMenuName=NULL;
	wc.lpszClassName=(LPCSTR)wndClassName;
	wc.hIconSm=hIcon;
	if (!RegisterClassEx(&wc)) {
		MessageBox(NULL,"Window Registration Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);
		return;
	}
}
static void  ClassRegisterDialog(char* wndClassName)
{
	WNDCLASSEX wc={ 0 };
	wc.cbSize=sizeof(WNDCLASSEX);
	wc.style=CS_DBLCLKS|CS_HREDRAW|CS_DROPSHADOW;
	wc.lpfnWndProc=DialogProc;
	wc.cbClsExtra=0;
	wc.cbWndExtra=0;
	wc.hInstance=GetModuleHandle(NULL);
	wc.hIcon=NULL;
	wc.hCursor=NULL;
	wc.hbrBackground=(HBRUSH)(COLOR_WINDOW);
	wc.lpszMenuName=NULL;
	wc.lpszClassName=(LPCSTR) wndClassName;
	wc.hIconSm=NULL;
	if (!RegisterClassEx(&wc))
	{
		MessageBox(NULL,"DialogBox Class Registration Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);
	}
}
extern void BEAST2_InitGlobalData(void);
extern void BEAST2_AllocatePlotData(void);
extern void BEAST2_GeneratePlotData(void);
extern void BEAST2_DrawPlots(HDC hdc);
DWORD WINAPI beast_thread(__in LPVOID dummy_GLOBAL_OPTIONS) {
	#if R_INTERFACE==1
	BEAST2_print_options(GLOBAL_OPTIONS);
	#endif
	VOIDPTR ANS=NULL;
	PROTECT(ANS=BEAST2_Output_AllocMEM(GLOBAL_OPTIONS));
	beast2_main_corev4_gui();
	UNPROTECT(1);
	DestoryStructVar(ANS);
	return 0;
}
void  DllExport BEAST2_WinMain(VOID_PTR  option)
{
	logger.LEN=2000;
	logger.len=0;
	logger.str=malloc(logger.LEN+1);
	BEAST2_InitGlobalData();
	{
		DWORD  thread_id;		
		gData.threadHandle=CreateThread(0,0,beast_thread,option,0,&thread_id);
	}
	HICON       hIcon=CreateMyIcon();	
	ClassRegisterWnd( "MyWndClassBeast2",hIcon);
	ClassRegisterDialog("MyDiaglogClass3434");
	HINSTANCE   hInstance=GetModuleHandle(NULL);
	HWND        hwnd=CreateWindowEx(
								WS_EX_CLIENTEDGE,
								"MyWndClassBeast2",
								"Bayesian time series decomposition and changepoint detection",
								(WS_OVERLAPPEDWINDOW & ~WS_MAXIMIZEBOX),  
								CW_USEDEFAULT,CW_USEDEFAULT,520,740,
								NULL,NULL,hInstance,NULL
							);
	if (!hwnd) {
		MessageBox(NULL,"Window Creation Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);
		return;
	}
	else {
	}
	ShowWindow(hwnd,SW_SHOWDEFAULT);
	UpdateWindow(hwnd);
	SetWindowPos(hwnd,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE|SWP_NOSIZE);
	EnterCriticalSection(&gData.cs);
	gData.hwnd=hwnd;
	LeaveCriticalSection(&gData.cs);
	WakeConditionVariable(&gData.cv); 
	MSG msg;
	while (GetMessage(&msg,NULL,0,0) > 0) {
		if (!IsDialogMessage(hDiag,&msg)) 		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}
	WaitForSingleObject(gData.threadHandle,INFINITE);
	CloseHandle(gData.threadHandle);
	gData.threadHandle=NULL;
	DeleteCriticalSection(&gData.cs);
	DestroyIcon(hIcon);
	UnregisterClass("MyWndClassBeast2",hInstance);
	UnregisterClass("MyDiaglogClass3434",hInstance);
	free(logger.str);
	return ;
}
static LRESULT CALLBACK WndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam) {
	switch (msg) {
	case WM_CREATE:
	{
		{
			EnterCriticalSection(&gData.cs);
			while (gData.N <=0)
				SleepConditionVariableCS(&gData.cv,&gData.cs,INFINITE);
			LeaveCriticalSection(&gData.cs);
			EnterCriticalSection(&gData.cs);
				BEAST2_AllocatePlotData();	 
			LeaveCriticalSection(&gData.cs);
			WakeConditionVariable(&gData.cv);
		}
		if (!SetTimer(hwnd,ID_TIMER,gData.timerInterval,NULL))  
			MessageBox(hwnd,"Could not SetTimer()!","Error",MB_OK|MB_ICONEXCLAMATION);
		{
			hEdit=CreateWindowEx(WS_EX_CLIENTEDGE,"EDIT","",
						WS_CHILD|WS_VISIBLE|WS_VSCROLL|WS_HSCROLL|ES_MULTILINE|ES_AUTOVSCROLL|ES_AUTOHSCROLL,
						0,0,100,100,hwnd,(HMENU)IDC_MAIN_EDIT,GetModuleHandle(NULL),NULL);
			if (hEdit==NULL) 
				MessageBox(hwnd,"Could not create edit box.\0\0\0","Error\0\0\0",MB_OK|MB_ICONERROR);
			HFONT hfDefault=GetStockObject(SYSTEM_FONT);
			SendMessage(  hEdit,WM_SETFONT,(WPARAM) hfDefault,MAKELPARAM(FALSE,0));
			SetWindowText(hEdit,"");
			SendDlgItemMessage(hwnd,IDC_MAIN_EDIT,EM_SETREADONLY,(WPARAM)TRUE,0);
		}
		HFONT hfontDefault=GetStockObject(SYSTEM_FONT);
		{		
			char* caption[]={ "Run\0\0\0","Pause\0\0\0","Setting\0\0\0","Exit\0\0\0" };			
			for (int i=0; i < 4; i++)
			{
				hButton[i]=CreateWindowEx(WS_EX_CLIENTEDGE,"BUTTON",caption[i],WS_CHILD|WS_VISIBLE,
									        10+100 * i,20,30 * 9/4,30,hwnd,(HMENU)i,GetModuleHandle(NULL),NULL);
				if (hButton[i]==NULL)
					MessageBox(hwnd,"Could not create edit box.\0\0\0","Error",MB_OK|MB_ICONERROR);
				SendMessage(hButton[i],WM_SETFONT,(WPARAM)hfontDefault,MAKELPARAM(FALSE,0));				
			}
			SendMessage(hButton[0],BM_SETCHECK,(WPARAM)BST_UNCHECKED,MAKELPARAM(FALSE,0));
			EnableButtons(hButton,NULL);
		}
		{
			SCROLLINFO si;
			hScroll=CreateWindowEx(
				0,                      
				"SCROLLBAR",           
				(PTSTR)NULL,           
				WS_CHILD|WS_VISIBLE   
				|SBS_HORZ,         
				400,              
				50, 
				100,             
				10,               
				hwnd,             
				(HMENU)10,           
				GetModuleHandle(NULL),                
				(PVOID)NULL            
			);
			si.cbSize=sizeof(si);
			si.nMin=0;
			si.nMax=999;
			si.nPos=500;
			si.nPage=50;
			si.fMask=SIF_RANGE|SIF_POS|SIF_PAGE;
			SetScrollInfo(hScroll,SB_CTL,&si,TRUE);
			F32 x=(max(950 - si.nPos,0));
			x=x * x * x * x;
			gData.sleepInterval=1e-9 * x;
		}
		{
			hStatic[0]=CreateWindowEx(0,"STATIC","SLOW",WS_CHILD|WS_VISIBLE|SS_CENTER,
											400,100,100,100,hwnd,(HMENU)20,GetModuleHandle(NULL),NULL);
			SendMessage(hStatic[0],WM_SETFONT,(WPARAM)hfontDefault,MAKELPARAM(FALSE,0));
			SetWindowText(hStatic[0],"Slow");
			hStatic[1]=CreateWindowEx(0,"STATIC","FAST",WS_CHILD|WS_VISIBLE|WS_TABSTOP,
										500,100,100,100,hwnd,(HMENU)21,GetModuleHandle(NULL),NULL);
			SendMessage(hStatic[1],WM_SETFONT,(WPARAM)hfontDefault,MAKELPARAM(FALSE,0));
			SetWindowText(hStatic[1],"Fast");
		}
		{
			TEXTMETRIC tm;
			HDC hdc;
			hdc=GetDC(hStatic[0]);
			GetTextMetrics(hdc,&tm);
			style.xChar=tm.tmAveCharWidth;
			style.xCapChar=(tm.tmPitchAndFamily & 1 ? 3 : 2) * style.xChar/2;
			style.yChar=tm.tmHeight+tm.tmExternalLeading;
			ReleaseDC(hStatic[0],hdc);
		}
		{
			EnterCriticalSection(&gData.cs);
				CreateGDIObject(hwnd,5);
			LeaveCriticalSection(&gData.cs);
		}
		{
			hDiag=CreateWindowEx(
				WS_EX_CLIENTEDGE,
				"MyDiaglogClass3434",
				"Settings",
				WS_OVERLAPPEDWINDOW|(WS_CAPTION & ~WS_VISIBLE),
				CW_USEDEFAULT,CW_USEDEFAULT,320,240,
				hwnd,(HMENU)NULL,GetModuleHandle(NULL),NULL);
			if (hDiag==0)
				MessageBox(NULL,"Window Registration Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);
			ShowWindow(hDiag,SW_HIDE);
		}
	}
	break;
	case WM_HSCROLL:
	{
					   HWND hScroll;
					   WORD action; 
					   hScroll=(HWND)lParam;
					   action=LOWORD(wParam);
					   int pos;
					   pos=GetScrollPos(hScroll,SB_CTL);
					   if (action==SB_THUMBPOSITION||action==SB_THUMBTRACK) {
						   pos=HIWORD(wParam);
					   }
					   else if (action==SB_LINERIGHT) {
						   pos=min(pos+3,999);
					   }
					   else if (action==SB_LINELEFT) {
						   pos=max(pos - 3,0);
					   }
					   else if (action==SB_PAGERIGHT) {
						   pos=max(pos+30,0);
					   }
					   else if (action==SB_PAGELEFT) {
						   pos=max(pos - 30,0);
					   }
					   char str[30];
					   SetScrollPos(hScroll,SB_CTL,pos,TRUE);
					   F32 x=(max(950 - pos,0));
					   x=x*x*x*x;
					   gData.sleepInterval=1e-9 * x;
					   wsprintf(str,"%d %d",pos,gData.sleepInterval);
	}
		break;
	case WM_COMMAND:
	{
					   switch LOWORD(wParam)
					   {
					   case 0:
						   EnterCriticalSection(&gData.cs);
						   if (gData.status==DONE)
						   {
							   WaitForSingleObject(gData.threadHandle,INFINITE);
							   CloseHandle(gData.threadHandle);
							   DWORD mythreadid;
							   gData.threadHandle=CreateThread(0,0,beast_thread,NULL,0,&mythreadid);
							   gData.status=RUN;
							   EnableButtons(hButton,&hDiag);
							   CreateGDIObject(hwnd,5);
							   InvalidateRect(hwnd,NULL,TRUE);
							   while (gData.y==NULL)
								   SleepConditionVariableCS(&gData.cv,&gData.cs,INFINITE);
							   {
								   F32PTR  y=gData.y;
								   F32 W,H;
								   W=gData.w[0];
								   H=gData.h[0];
								   F32 a1=H+H/(gData.yMax - gData.yMin) *gData.yMin;
								   F32 b1=H/(gData.yMax - gData.yMin);
								   RECT  rect;
								   SetRect(&rect,0,0,W,H);
								   FillRect(bufferDC[0],&rect,blackBrush);
								   SelectObject(bufferDC[0],GetStockObject(NULL_BRUSH));
								   int RAIDUS=4;
								   int curRowIdx=0;
								   for (int i=0; i < gData.N; i++)
								   {
									   if ((i+1)==gData.rowsMissing[curRowIdx] && (curRowIdx+1) <=gData.nMissing)
									   {
										   curRowIdx++;
										   continue;
									   }
									   F32 xx=((F32)(i+1)/gData.N) *W;
									   F32 yy=a1 - y[i] * b1;
									   if (yy==yy)
										   Ellipse(bufferDC[0],xx - RAIDUS,yy - RAIDUS,xx+RAIDUS,yy+RAIDUS);
								   }
							   }
							   LeaveCriticalSection(&gData.cs);
							   {
								   RECT rc;
								   GetWindowRect(hwnd,&rc);
								   MoveWindow(hDiag,rc.right+20,rc.top,style.widthDialg,rc.bottom - rc.top,TRUE);
								   SetWindowLong(hwnd,GWL_STYLE,GetWindowLong(hwnd,GWL_STYLE)|(WS_SIZEBOX));
							   }
							   char str[100];
							   wsprintf(str,"Re-running....\r\n");
							   LoggerInsert(str);
						   }
						   else if (gData.status==PAUSE)
						   {
							   gData.status=RUN;
							   EnableButtons(hButton,&hDiag);
							   LeaveCriticalSection(&gData.cs);
							   WakeConditionVariable(&gData.cv);
							   char str[100];
							   wsprintf(str,"Resumed....\r\n");
							   LoggerInsert(str);
						   }
						   SetTimer(hwnd,ID_TIMER,gData.timerInterval,NULL);
						   break;
					   case 1:
						   KillTimer(hwnd,ID_TIMER);
						   EnterCriticalSection(&gData.cs);
						   gData.status=PAUSE;
						   EnableButtons(hButton,&hDiag);
						   LeaveCriticalSection(&gData.cs);
						   char str[100];
						   wsprintf(str,"PAUSED....\r\n");
						   LoggerInsert(str);
						   break;
					   case 2:
						   if (IsWindowVisible(hDiag))
							   ShowWindow(hDiag,SW_HIDE);
						   else
							   ShowWindow(hDiag,SW_SHOWDEFAULT);
						   UpdateWindow(hDiag);
						   break;
					   case 3:
						   EnterCriticalSection(&gData.cs);
						   gData.quit=1;
						   LeaveCriticalSection(&gData.cs);
						   if (gData.status==PAUSE)
							   WakeConditionVariable(&gData.cv);
						   DestroyWindow(hwnd);
						   break;
					   case IDC_MAIN_EDIT:
						   return DefWindowProc(hwnd,msg,wParam,lParam);
						   break;
					   default:
						   return DefWindowProc(hwnd,msg,wParam,lParam);
					   }
	}
		break;
	case WM_GETMINMAXINFO:
	{
		PMINMAXINFO lpMinMaxInfo=(MINMAXINFO*)lParam;
		lpMinMaxInfo->ptMinTrackSize.x=600;
		lpMinMaxInfo->ptMinTrackSize.y=500;
		lpMinMaxInfo->ptMaxTrackSize.x=5000;
		lpMinMaxInfo->ptMaxTrackSize.y=5000;
	}
		break;
	case WM_MOVE:
	{
		RECT rc;
		GetWindowRect(hwnd,&rc);
		MoveWindow(hDiag,rc.right+20,rc.top,style.widthDialg,rc.bottom - rc.top,TRUE);
	}
		break;
	case WM_USER+1:
		KillTimer(hwnd,ID_TIMER);
		SetWindowLong(hwnd,GWL_STYLE,GetWindowLong(hwnd,GWL_STYLE) & ~(WS_SIZEBOX));
		EnterCriticalSection(&gData.cs);
			gData.status=DONE;
			gData.ite=0;
			EnableButtons(hButton,&hDiag);
		LeaveCriticalSection(&gData.cs);
		WakeConditionVariable(&gData.cv);
		{
			char str[100];
			wsprintf(str,"Finished....\r\n");
			LoggerInsert(str);
		}
		break;
	case WM_USER+2:
		EnterCriticalSection(&gData.cs);
		{
			char str[100];
			wsprintf(str,"Chain #%d is finished\r\n",gData.curChainNumber);
			LoggerInsert(str);
			snprintf(str,99,"Mean number of scp is %8.2f\r\n",gData.sN);
			LoggerInsert(str);
			snprintf(str,99,"Mean number of tcp is %8.2f\r\n",gData.tN);
			LoggerInsert(str);
		}
		LeaveCriticalSection(&gData.cs);
		break;
	case WM_SIZE:
	{
					EnterCriticalSection(&gData.cs);
					if (gData.status==DONE)
					{
						LeaveCriticalSection(&gData.cs);
						break;
					}
					CreateGDIObject(hwnd,5);
					InvalidateRect(hwnd,NULL,TRUE);
					while (gData.y==NULL)
						SleepConditionVariableCS(&gData.cv,&gData.cs,INFINITE);
					{
						F32PTR  y=gData.y;
						F32 W,H;
						W=gData.w[0];
						H=gData.h[0];
						F32 a1=H+H/(gData.yMax - gData.yMin) *gData.yMin;
						F32 b1=H/(gData.yMax - gData.yMin);
						RECT  rect;
						SetRect(&rect,0,0,W,H);
						FillRect(bufferDC[0],&rect,blackBrush);
						SelectObject(bufferDC[0],GetStockObject(NULL_BRUSH));
						int RAIDUS=4;
						int curRowIdx=0;
						for (int i=0; i < gData.N; i++)
						{					
							if ((i+1)==gData.rowsMissing[curRowIdx] && (curRowIdx+1)<=gData.nMissing)
							{
								curRowIdx++;
								continue;
							}
							F32 xx=((F32)(i+1)/gData.N) *W;
							F32 yy=a1 - y[i] * b1;
							if (yy==yy)
								Ellipse(bufferDC[0],xx - RAIDUS,yy - RAIDUS,xx+RAIDUS,yy+RAIDUS);
						}
					}
					LeaveCriticalSection(&gData.cs);
					{
						RECT rc;
						GetWindowRect(hwnd,&rc);
						MoveWindow(hDiag,rc.right+20,rc.top,style.widthDialg,rc.bottom - rc.top,TRUE);
					}
	}
		break;
	case WM_SHOWWINDOW:
		if (wParam)
		{
		}
		break;
	case WM_CLOSE:
		EnterCriticalSection(&gData.cs);
		gData.quit=1;
		LeaveCriticalSection(&gData.cs);
		if (gData.status==PAUSE)
			WakeConditionVariable(&gData.cv);
		DestroyWindow(hwnd);
		break;
	case WM_CTLCOLORSTATIC:
	{
							  if ((HWND)lParam==hEdit)
							  {
								  HDC hdc=(HDC)wParam;
								  SetBkColor(hdc,RGB(100,0,0));
								  SetTextColor(hdc,RGB(0,255,0));
								  return (LRESULT)GetStockObject(GRAY_BRUSH);
							  }
							   else if ((HWND)lParam==hStatic[0]||(HWND)lParam==hStatic[1]) 		  {
								  HDC hdc=(HDC)wParam;
								  SetBkMode((HDC)wParam,TRANSPARENT);
								  SetBkColor(hdc,RGB(100,0,0));
								  SetTextColor(hdc,RGB(255,255,0));
								  return GetStockObject(GRAY_BRUSH);
							  }
	}
		break;
	case WM_PAINT:
	{
					 RECT rcClient;
					 PAINTSTRUCT ps;
					 HDC hdc;
					 hdc=BeginPaint(hwnd,&ps);
					 EnterCriticalSection(&gData.cs);
					 if (gData.status==PAUSE||gData.status==DONE) 	 BEAST2_DrawPlots(hdc);
					 LeaveCriticalSection(&gData.cs);
					 EndPaint(hwnd,&ps);
	}
		break;
	case WM_TIMER:
	{
					 HDC hdc=GetDC(hwnd);
					 EnterCriticalSection(&gData.cs);					 
					 BEAST2_DrawPlots(hdc);
					 LeaveCriticalSection(&gData.cs);
					 ReleaseDC(hwnd,hdc);
	}
		break;
	case WM_DRAWITEM:
	{
						LPDRAWITEMSTRUCT pdis=(LPDRAWITEMSTRUCT)lParam;
						if (pdis->CtlType==ODT_BUTTON)
						{
							HDC hdc=pdis->hDC;
							SaveDC(hdc);
							TextOut(hdc,0,0,"789",3);
							if (pdis->itemState & ODS_FOCUS)
							{
								TextOut(hdc,0,0,"123",3);
							}
							if (pdis->itemState & ODS_SELECTED)
							{
								TextOut(hdc,0,0,"456",3);
							}
							RestoreDC(hdc,-1);
						}
	}
		break;
	case WM_DESTROY:
		KillTimer(hwnd,ID_TIMER);
		DeleteGDIObject(5);
		WaitForSingleObject(gData.threadHandle,INFINITE);
		CloseHandle(gData.threadHandle);
		gData.threadHandle=NULL;
		for (int i=0; i < sizeof(gData.plotData)/sizeof(gData.plotData[0][0]); i++)
		{
			if (*((int **)gData.plotData+i) !=NULL) free(*((int **)gData.plotData+i));
		}
		PostQuitMessage(0);
		break;
	default:
		return DefWindowProc(hwnd,msg,wParam,lParam);
	}
	return 0;
}
static LRESULT CALLBACK DialogProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam)
{
	static byte  isDrawing=0;
	static char* names[]={ "seasonMinKnotNum","seasonMaxKnotNum","seasonMinOrder","seasonMaxOrder","seasonMinSepDist",
							  "trendMinKnotNum","trendMaxKnotNum","trendMinOrder","trendMaxOrder","trendMinSepDist",
							   "burnin","samples","chainNumber","thinningFactor","maxMoveStepSize" };
		char   str[100];	
	BEAST2_OPTIONS_PTR opt=GLOBAL_OPTIONS;
	switch (msg) {
	case WM_CREATE:
	{
		int values[]={
						opt->prior.seasonMinKnotNum,
						opt->prior.seasonMaxKnotNum,
						opt->prior.seasonMinOrder,
						opt->prior.seasonMaxOrder,
						opt->prior.seasonMinSepDist,
						opt->prior.trendMinKnotNum,
						opt->prior.trendMaxKnotNum,
						opt->prior.trendMinOrder,
						opt->prior.trendMaxOrder,
						opt->prior.trendMinSepDist,
						opt->mcmc.burnin,
						opt->mcmc.samples,
						opt->mcmc.chainNumber,
						opt->mcmc.thinningFactor,
						opt->mcmc.maxMoveStepSize,
		};
		HWND  htmp;
		HFONT hfDefault=GetStockObject(SYSTEM_FONT);
		for (int i=0; i < 15; i++)
		{	  
			htmp=CreateWindowEx(0,"STATIC",names[i],WS_CHILD|WS_VISIBLE|SS_LEFT,
				10,10,40,40,hwnd,(HMENU)i,GetModuleHandle(NULL),NULL);
			SendMessage(htmp,WM_SETFONT,(WPARAM)hfDefault,MAKELPARAM(FALSE,0));
			SendDlgItemMessage(hwnd,i,WM_SETTEXT,(WPARAM)NULL,(LPARAM)names[i]);
			htmp=CreateWindowEx(0,"EDIT","434",WS_CHILD|WS_VISIBLE|ES_LEFT|WS_BORDER|ES_NUMBER,
				60,10,50,50,hwnd,(HMENU)(100+i),GetModuleHandle(NULL),NULL);
			SendMessage(htmp,WM_SETFONT,(WPARAM)hfDefault,MAKELPARAM(FALSE,0));
			SetDlgItemInt(hwnd,100+i,values[i],1);
		}
		htmp=CreateWindowEx(WS_EX_CLIENTEDGE,"BUTTON","Apply",
			WS_CHILD|WS_VISIBLE|BS_PUSHBUTTON|BS_PUSHLIKE,
			10,70,30 * 9/4,30,hwnd,(HMENU)200,GetModuleHandle(NULL),NULL);
		htmp=CreateWindowEx(WS_EX_CLIENTEDGE,"BUTTON","Close",
			WS_CHILD|WS_VISIBLE|BS_PUSHBUTTON|BS_PUSHLIKE,
			10,70,30 * 9/4,30,hwnd,(HMENU)201,GetModuleHandle(NULL),NULL);
		style.numRows=15+1;
		wsprintf(str,"Diag Created..%d \r\n",GetTickCount());
		LoggerInsert(str);
	}
		break;
	case WM_USER+1:
		isDrawing=1;
		ShowWindow(hwnd,SW_SHOWDEFAULT);
		UpdateWindow(hwnd);
		break;
	case WM_MOVE:
	{
		if (isDrawing)
		{
			RECT rc;
			GetWindowRect(gData.hwnd,&rc);
			MoveWindow(hwnd,rc.right+20,rc.top,style.widthDialg,rc.bottom - rc.top,TRUE);
			BEAST2_ResizeDialogControls(hwnd);
			wsprintf(str,"WM_MOV_created.Inside.%d \r\n",GetTickCount());
			LoggerInsert(str);
		}
		break;
	}		
	case WM_SIZE:
	{
		if (isDrawing)
		{
			RECT rc;
			GetWindowRect(gData.hwnd,&rc);
			MoveWindow(hwnd,rc.right+20,rc.top,style.widthDialg,rc.bottom - rc.top,TRUE);
			BEAST2_ResizeDialogControls(hwnd);
			wsprintf(str,"WM_SIZE_created.INsideside.%d \r\n",GetTickCount());
			LoggerInsert(str);
		}
		break;
	}
	case WM_SHOWWINDOW:
	{
		if (wParam)
		{
			RECT rc;
			GetWindowRect(gData.hwnd,&rc);
			MoveWindow(hwnd,rc.right+20,rc.top,style.widthDialg,rc.bottom - rc.top,TRUE);
			EnableButtons(NULL,&hDiag);
			BEAST2_ResizeDialogControls(hwnd);
			int values[]={
					opt->prior.seasonMinKnotNum,
					opt->prior.seasonMaxKnotNum,
					opt->prior.seasonMinOrder,
					opt->prior.seasonMaxOrder,
					opt->prior.seasonMinSepDist,
					opt->prior.trendMinKnotNum,
					opt->prior.trendMaxKnotNum,
					opt->prior.trendMinOrder,
					opt->prior.trendMaxOrder,
					opt->prior.trendMinSepDist,
						opt->mcmc.burnin,
						opt->mcmc.samples,
						opt->mcmc.chainNumber,
						opt->mcmc.thinningFactor,
						opt->mcmc.maxMoveStepSize,
			};
			for (int i=0; i < 8; i++)
			{	  
				SetDlgItemInt(hwnd,100+i,values[i],1);
			}
			BEAST2_ResizeDialogControls(hwnd);
		}
		else
		{
			gData.optStatus=NeedUpdate;
		}
	}
		break;
	case WM_COMMAND:
		switch (LOWORD(wParam))
		{
		case 200:
		{
			BOOL translated=0;
			int i=100;
			opt->prior.seasonMinKnotNum=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.seasonMaxKnotNum=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.seasonMinOrder=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.seasonMaxOrder=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.seasonMinSepDist=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.trendMinKnotNum=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.trendMaxKnotNum=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.trendMinOrder=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.trendMaxOrder=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->prior.trendMinSepDist=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->mcmc.burnin=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->mcmc.samples=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->mcmc.chainNumber=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->mcmc.thinningFactor=GetDlgItemInt(hwnd,i,&translated,0); i++;
			opt->mcmc.maxMoveStepSize=GetDlgItemInt(hwnd,i,&translated,0); i++;
			gData.optStatus=NeedUpdate;
		}
			break;
		case 201:
			ShowWindow(hwnd,SW_HIDE);
			break;
		default:
			break;
		}
		break;
	case WM_CLOSE:
		ShowWindow(hwnd,SW_HIDE);
		break;
	default:
		if (IsWindowUnicode(hwnd))
			return (DefWindowProc(hwnd,msg,wParam,lParam)); 
		else
			return (DefWindowProc(hwnd,msg,wParam,lParam)); 
	}
	return 0;
}
#else
static char fileID UNUSED_DECORATOR='c';
#endif
#include "abc_000_warning.h"
