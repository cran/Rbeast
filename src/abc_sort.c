#include "abc_000_warning.h"
#include "abc_sort.h"
static INLINE void SwapValue(F32PTR a,F32PTR b) {	F32 t=*a; *a=*b;	*b=t;}
static INLINE void SwapIndex(I32PTR a,I32PTR b) {	I32 t=*a;	*a=*b;	*b=t;}
static I32 PartitionD(F32PTR arr,I32PTR INDEX,I32 low,I32 high) {
	F32 pivot=arr[high];   
	I32 i=(low - 1);  
	for (rI32 j=low; j <=high - 1; j++) 	{
		if (arr[j] > pivot)  	{
			i++;    
			SwapValue(&arr[i],&arr[j]);
			SwapIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValue(&arr[i+1],&arr[high]);
	SwapIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void QuickSortD(F32PTR arr,I32PTR INDEX,I32 low,I32 high)
{
	if (low < high)  {
		I32 pi=PartitionD(arr,INDEX,low,high);
		QuickSortD(arr,INDEX,low,pi - 1);
		QuickSortD(arr,INDEX,pi+1,high);
	}
}
static I32 PartitionA(F32PTR arr,I32PTR INDEX,I32 low,I32 high)
{
	F32 pivot=arr[high];    
	I32 i=(low - 1);  
	for (I32 j=low; j <=high - 1; j++)
	{
		if (arr[j] <=pivot)
		{
			i++;    
			SwapValue(&arr[i],&arr[j]);
			SwapIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValue(&arr[i+1],&arr[high]);
	SwapIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void QuickSortA(F32PTR arr,I32PTR INDEX,I32 low,I32 high)
{
	if (low < high)	{
		I32 pi=PartitionA(arr,INDEX,low,high);
		QuickSortA(arr,INDEX,low,pi - 1);
		QuickSortA(arr,INDEX,pi+1,high);
	}
}
#include "abc_000_warning.h"
