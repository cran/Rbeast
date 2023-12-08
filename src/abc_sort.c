#include "abc_000_warning.h"
#include "abc_sort.h"
static INLINE void SwapValueF32(F32PTR a,F32PTR b) {	F32 t=*a; *a=*b;	*b=t;}
static INLINE void SwapValueF64(F64PTR a,F64PTR b) { F64 t=*a; *a=*b;	*b=t; }
static INLINE void SwapIntIndex(I32PTR a,I32PTR b) {	I32 t=*a;	*a=*b;	*b=t;}
static I32 f32_PartitionD(F32PTR arr,I32PTR INDEX,I32 low,I32 high) {
	F32 pivot=arr[high];   
	I32 i=(low - 1);   
	for (rI32 j=low; j <=high - 1; j++) 	{
		if (arr[j] > pivot)  	{ 
			i++;                   
			SwapValueF32(&arr[i],&arr[j]);
			SwapIntIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValueF32(&arr[i+1],&arr[high]);
	SwapIntIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void f32_QuickSortD(F32PTR arr,I32PTR INDEX,I32 low,I32 high)
{
	if (low < high)  {
		I32 pi=f32_PartitionD(arr,INDEX,low,high);
		f32_QuickSortD(arr,INDEX,low,pi - 1);
		f32_QuickSortD(arr,INDEX,pi+1,high);
	}
}
static I32 f32_PartitionA(F32PTR arr,I32PTR INDEX,I32 low,I32 high)
{
	F32 pivot=arr[high];    
	I32 i=(low - 1);  
	for (I32 j=low; j <=high - 1; j++) {
		if (arr[j] <=pivot) {
			i++;    
			SwapValueF32(&arr[i],&arr[j]);
			SwapIntIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValueF32(&arr[i+1],&arr[high]);
	SwapIntIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void f32_QuickSortA(F32PTR arr,I32PTR INDEX,I32 low,I32 high)
{
	if (low < high)	{
		I32 pi=f32_PartitionA(arr,INDEX,low,high);
		f32_QuickSortA(arr,INDEX,low,pi - 1);
		f32_QuickSortA(arr,INDEX,pi+1,high);
	}
}
static I32 i32_PartitionA(I32PTR arr,I32PTR INDEX,I32 low,I32 high) {
	F32 pivot=arr[high];    
	I32 i=(low - 1);  
	for (I32 j=low; j <=high - 1; j++) {
		if (arr[j] <=pivot) {
			i++;    
			SwapValueF32(&arr[i],&arr[j]);
			SwapIntIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValueF32(&arr[i+1],&arr[high]);
	SwapIntIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void i32_QuickSortA(I32PTR arr,I32PTR INDEX,I32 low,I32 high) {
	if (low < high)	{
		I32 pi=i32_PartitionA(arr,INDEX,low,high);
		i32_QuickSortA(arr,INDEX,low,pi - 1);
		i32_QuickSortA(arr,INDEX,pi+1,high);
	}
}
static I32 i32_PartitionD(I32PTR arr,I32PTR INDEX,I32 low,I32 high) {
	F32 pivot=arr[high];    
	I32 i=(low - 1);  
	for (I32 j=low; j <=high - 1; j++) {
		if (arr[j] > pivot) {
			i++;    
			SwapValueF32(&arr[i],&arr[j]);
			SwapIntIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValueF32(&arr[i+1],&arr[high]);
	SwapIntIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void i32_QuickSortD(I32PTR arr,I32PTR INDEX,I32 low,I32 high) {
	if (low < high)	{
		I32 pi=i32_PartitionD(arr,INDEX,low,high);
		i32_QuickSortD(arr,INDEX,low,pi - 1);
		i32_QuickSortD(arr,INDEX,pi+1,high);
	}
}
static I32 f64_PartitionA(F64PTR arr,I32PTR INDEX,I32 low,I32 high) {
	F64 pivot=arr[high];    
	I32 i=(low - 1);    
	for (I32 j=low; j <=high - 1; j++) {
		if (arr[j] <=pivot) {
			i++;    
			SwapValueF64(&arr[i],&arr[j]);
			SwapIntIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValueF64(&arr[i+1],&arr[high]);
	SwapIntIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void f64_QuickSortA( F64PTR  arr,I32PTR INDEX,I32 low,I32 high) {
	if (low < high)	{
		I32 pi=f64_PartitionA(arr,INDEX,low,high);
		f64_QuickSortA(arr,INDEX,low,pi - 1);
		f64_QuickSortA(arr,INDEX,pi+1,high);
	}
}
static I32 f64_PartitionD(F64PTR arr,I32PTR INDEX,I32 low,I32 high) {
	F64 pivot=arr[high];    
	I32 i=(low - 1);    
	for (I32 j=low; j <=high - 1; j++) {
		if (arr[j] > pivot) {
			i++;    
			SwapValueF64(&arr[i],&arr[j]);
			SwapIntIndex(&INDEX[i],&INDEX[j]);
		}
	}
	SwapValueF64(&arr[i+1],&arr[high]);
	SwapIntIndex(&INDEX[i+1],&INDEX[high]);
	return (i+1);
}
void f64_QuickSortD( F64PTR  arr,I32PTR INDEX,I32 low,I32 high) {
	if (low < high)	{
		I32 pi=f64_PartitionD(arr,INDEX,low,high);
		f64_QuickSortA(arr,INDEX,low,pi - 1);
		f64_QuickSortA(arr,INDEX,pi+1,high);
	}
}
 void VOIDPTR_InsertionSort(void* arr[],char* index,int n) {
	int i,j;
	for (i=1; i < n; i++) {
		void* key=arr[i];
		char  idx=index[i];
		j=i - 1;
		while (j >=0 && arr[j] > key) {
			arr[j+1]=arr[j];
			index[j+1]=index[j];
			j=j - 1;
		}
		arr[j+1]=key;
		index[j+1]=idx;
	}
}
void i32_InsertionSort(I32PTR arr,I32PTR index,int n) {
	int i,j;
	for (i=1; i < n; i++) {
		I32    key=arr[i];
		I32    idx=index[i];
		j=i - 1;
		while (j >=0 && arr[j] > key) {
			arr[j+1]=arr[j];
			index[j+1]=index[j];
			j=j - 1;
		}
		arr[j+1]=key;
		index[j+1]=idx;
	}
}
void i32_sort_d_iterative(I32PTR  arr,int* idx,int *stack,int l,int h) {
    int top=-1;     
    stack[++top]=l;      
    stack[++top]=h;
    while (top >=0) {
        h=stack[top--];
        l=stack[top--];
        int p=i32_PartitionD(arr,idx,l,h);
        if (p - 1 > l) {
            stack[++top]=l;
            stack[++top]=p - 1;
        }
        if (p+1 < h) {
            stack[++top]=p+1;
            stack[++top]=h;
        }
    }
	return;
}
#include "abc_000_warning.h"
