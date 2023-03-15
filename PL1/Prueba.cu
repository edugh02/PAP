///////////////////////////////////////////////////////////////////////////
// includes
///////////////////////////////////////////////////////////////////////////
#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <math.h>
#include "cuda_runtime.h"
#include "device_launch_parameters.h"

///////////////////////////////////////////////////////////////////////////
// defines
const int N = 20;
const int TAM_MASK = 7;
const int R = TAM_MASK/2;

const int SIZE = N * sizeof(int);
const int SIZE_MASK = TAM_MASK * sizeof(int);

__constant__ int mascara[TAM_MASK];

///////////////////////////////////////////////////////////////////////////
// declaracion de funciones

// DEVICE: funcion llamada desde el device y ejecutada en el device

// GLOBAL: funcion llamada desde el host y ejecutada en el device (kernel)
__global__ void convulcion1D(int* dev_A, int* dev_R){
	int pos = threadIdx.x;

	int acum = 0;
    int mask = 0;
    int inicio, fin;

    if (pos < R)
    {
        mask = R - pos;
        inicio = 0;
        fin = pos + 2;
    }
    else if (pos >= N - 2)
    {
        inicio = pos - 2;
        fin = N - 1;
    }
    else
    {
        inicio = pos - 2;
        fin = pos + 2;
    }
    for (int i = inicio; i <= fin; i++)
    {
        acum += dev_A[i] * mascara[mask];
        mask++;
    }
    dev_R[pos] = acum;
}

// HOST: funcion llamada desde el host y ejecutada en el host

///////////////////////////////////////////////////////////////////////////
// MAIN: rutina principal ejecutada en el host
int main() 
{
	// cuerpo del programa
	printf(" << CONVOLUCION 1D >>");

	//Instanciamos los vectores y le llenamos de valores manualmente
	int h_A[N];
	printf("\n A \n");
	for (int x = 0; x < N; x++){
		h_A[x] = x + 1;
		printf("%d ", h_A[x]);
	}
	printf("\n");

	int h_M[TAM_MASK] = {3, 4, 5, 6, 5, 4, 3};
	int h_R[N];

	//Puntero que usará la GPU 
	int* dev_A, * dev_R;

	//Reservamos memoria para GPU
	cudaMalloc((void**)&dev_A, SIZE);
	cudaMalloc((void**)&dev_R, SIZE);

	cudaMemcpyToSymbol(mascara,&h_M, SIZE_MASK);

	//Copiamos a la GPU
	cudaMemcpy(dev_A, h_A, SIZE, cudaMemcpyHostToDevice);

	//POR BLOQUES	
	dim3 threadsInBlock(N);
	dim3 blocksInGrid(1);
	printf("\nBlocksInGrid -> %dx%dx%d\nThreadsInBlock -> %dx%dx%d", 
		blocksInGrid.x, blocksInGrid.y, blocksInGrid.z, threadsInBlock.x, threadsInBlock.y, threadsInBlock.z);
	convulcion1D <<<blocksInGrid, threadsInBlock >>> (dev_A, dev_R);

	//Recuperamos el resultado de la GPU
	cudaMemcpy(h_R, dev_R, SIZE , cudaMemcpyDeviceToHost);

	printf("\n\n -- MASCARA -- \n");
    for (int i = 0; i < TAM_MASK; i++)
    {
        printf("%d ", h_M[i]);
    }

	printf("\n\n -- Resultado -- \n");
	for (int x = 0; x < N; x++){
		printf("%d ", h_R[x]);
	}
	printf("\n");

	cudaFree(dev_A); 
	cudaFree(dev_R);

	// salida del programa
	printf("\npulsa INTRO para finalizar...");
	fflush(stdin);
	char tecla = getchar();
	return 0;
}
///////////////////////////////////////////////////////////////////////////