#include "abc_000_warning.h"
#include "abc_sort.h"
#define JOIN1(X,Y)              X ## _ ## Y
#define FUNC_JOIN(fun,name)    JOIN1(fun,name)
#define FUNC(name)              FUNC_JOIN(funprefix,name) 
#define JOIN2(X,Y)             X ## _ ## Y ## _index
#define FINDEX_JOIN(fun,name)  JOIN2(fun,name) 
#define FINDEX(name)            FINDEX_JOIN(funprefix,name)
#define SwapIndex( index,i,j)             { int tmp=index[i]; index[i]=index[j]; index[j]=tmp;}
#define SwapElements( arr,i,j)            { DTYPE tmp=arr[i]; arr[i]=arr[j]; arr[j]=tmp;}
#define SwapElemIndex( arr,index,i,j)    { SwapElements(arr,i,j); SwapIndex(index,i,j); }
#define DTYPE         float
#define funprefix     f32a
#define LESS(a,b)    (a < b)
#define LESSEQ(a,b)  (a <=b)
#include "abc_sort_template.h"
#define DTYPE         float
#define funprefix     f32d
#define LESS(a,b)    (a > b)
#define LESSEQ(a,b)  (a >=b)
#include "abc_sort_template.h"
#define DTYPE         double
#define funprefix     f64a
#define LESS(a,b)    (a < b)
#define LESSEQ(a,b)  (a <=b)
#include "abc_sort_template.h"
#define DTYPE         double
#define funprefix     f64d
#define LESS(a,b)    (a > b)
#define LESSEQ(a,b)  (a >=b)
#include "abc_sort_template.h"
#define DTYPE         int32_t
#define funprefix     i32a
#define LESS(a,b)    (a < b)
#define LESSEQ(a,b)  (a <=b)
#include "abc_sort_template.h"
#define DTYPE         int32_t
#define funprefix     i32d
#define LESS(a,b)    (a > b)
#define LESSEQ(a,b)  (a >=b)
#include "abc_sort_template.h"
int i32_find_unique_occurrance_inplace(I32PTR arr,int n,I32PTR counts) {
	I32PTR stack=counts; 
	i32a_introSort(arr,0,n-1);
	int Nunique=0;
	for (int i=0; i < n; ) {
		int xcur=arr[i];
		int ncount=0;
		while (i < n && arr[i]==xcur) {
			ncount++;
			i++;
		}
		arr[Nunique]=xcur;
		counts[Nunique]=ncount;
		Nunique++;
	}
 	i32a_introSort_index(counts,0,Nunique - 1,arr);
	return Nunique;
}
int i32_find_majority_fast( I32PTR arr,int n,int * status) {
	int  candidate;
	int  votes=0;
	for ( int i=0; i < n; i++) {
		if (votes==0) {
			candidate=arr[i];
			votes=1;
		} else {
			votes+=(arr[i]==candidate) ? 1 : -1;
		}
	}
	int count=0;	
	for (int i=0; i < n; i++) {
		count+=(arr[i]==candidate);			
	}
	status[0]=(count > n/2) ? 1L : 0L;		 
	return candidate;
}
static INLINE void SwapValueI32(I32PTR a,I32PTR b) { I32 t=*a; *a=*b;	*b=t; }
static INLINE void SwapIntIndex(I32PTR a,I32PTR b) { I32 t=*a;	*a=*b;	*b=t; }
static I32 i32_PartitionA(I32PTR arr,I32 low,I32 high) {
	I32 pivot=arr[high];    
	I32 i=(low - 1);  
	for (I32 j=low; j <=high - 1; j++) {
		if (arr[j] <=pivot) {
			i++;    
			SwapValueI32(&arr[i],&arr[j]);
		}
	}
	SwapValueI32(&arr[i+1],&arr[high]);
	return (i+1);
}
void i32_QuickSortA(I32PTR arr,I32 low,I32 high) {
	if (low < high)	{
		I32 pi=i32_PartitionA(arr,low,high);
		i32_QuickSortA(arr,low,pi - 1);
		i32_QuickSortA(arr,pi+1,high);
	}
}
static void i32_quicksortA_iterative(I32PTR  arr,int *stack,int l,int h) {
    int top=-1;     
    stack[++top]=l;      
    stack[++top]=h;
    while (top >=0) {
        h=stack[top--];
        l=stack[top--];
        int p=i32_PartitionA(arr,l,h);
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
